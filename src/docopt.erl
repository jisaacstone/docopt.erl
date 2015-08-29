-module(docopt).

%% docopt: docopt library's entry point.

-export([tokenize/1,parse/1,parse/2]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(flag(Bin), binary_part(Bin,0,1) == "-").

%% API

tokenize(DocString) ->
    {ok, Tokens} = docopt_tokenizer:tokenize(DocString),
    Tokens.

parse(DocString) ->
    {ok, Result} = docopt_parser:parse(tokenize(DocString)),
    Result.

parse(ArgList,DocString) ->
    {UsageLines,Opts} = parse(DocString),
    parse(UsageLines,ArgList,Opts).

%% Internals

%% Parsing %%

parse([],_,_) ->
    {error,invalid_input};
parse([H|T],ArgList,Opts) ->
    case parse_line(ArgList,H,Opts,#{}) of
        {success,Result} -> set_defaults(Result, Opts);
        {error,Reason} -> {error,Reason};
        fail -> parse(T,ArgList,Opts)
    end.

parse_line([],U,_,R) -> check_optional(U,R);
parse_line(Args,Usage,Opts,Result) ->
    case consume_next(Args,Usage,Opts,Result) of
        {NewArgs,NewUsage,NewOpts,NewResult} ->
            parse_line(NewArgs,NewUsage,NewOpts,NewResult);
        nomatch -> fail
    end.

maybe_consume(Args,[],O,R) ->
    {Args,O,R};
maybe_consume(Args,[{required,RElem}|T],O,R) ->
    case consume_all(Args,RElem,O,R) of
        {NewArgs,NewOpts,NewResult} ->
            maybe_consume(NewArgs,T,NewOpts,NewResult);
        {nomatch,NewResult} -> maybe_consume(Args,T,O,NewResult)
    end;
maybe_consume(Args,OElem,O,R) ->
    case consume_next(Args,OElem,O,R) of
        {NewArgs,NewOElem,NewOpts,NewResult} ->
            maybe_consume(NewArgs,NewOElem,NewOpts,NewResult);
        {nomatch,NewResult} -> {Args,O,NewResult}
    end.

consume_all(Args,[],O,R) -> {Args,O,R};
consume_all(Args,RElem,O,R) ->
    case consume_next(Args,RElem,O,R) of
        {NewArgs,NewRElem,NewOpts,NewResult} ->
            consume_all(NewArgs,NewRElem,NewOpts,NewResult);
        {nomatch,_}=NM -> NM
    end.

consume_choice(_,[],_,_) -> nomatch;
consume_choice(Args,[C|Choices],Opts,Result) ->
    case consume_all(Args,C,Opts,Result) of
        {NewArgs,NewOpts,NewResult} ->
            {NewArgs,NewOpts,NewResult};
        {nomatch,NewResult} -> consume_choice(Args,Choices,Opts,NewResult)
    end.

%% Choice: A | B
consume_next(A,[{choice,C}|U],O,R) ->
    case consume_choice(A,C,O,R) of
        {NewArgs,NewOpts,NewResult} ->
            {NewArgs,U,NewOpts,NewResult};
        {nomatch,_}=NM -> NM
    end;
%% Optional: [A,B]
consume_next(Args,[{optional,OElem},{'...',_}|Rest]=U,Opts,Result) ->
    case maybe_consume(Args,OElem,Opts,Result) of
        {Args,Opts,NewResult} -> {Args,Rest,Opts,NewResult};
        {NewArgs,NewOpts,NewResult} -> {NewArgs,U,NewOpts,NewResult}
    end;
consume_next(A,[{optional,OElem}|U],O,R) ->
    {NewArgs,NewOpts,NewResult} = maybe_consume(A,OElem,O,R),
    {NewArgs,U,NewOpts,NewResult};
%% Required: (A,B)
consume_next(Args,[{required,RElem},{'...',L}|Rest],Opts,Result) ->
    case consume_all(Args,RElem,Opts,Result) of
        {NewArgs,NewOpts,NewResult} ->
            {NewArgs,[{optional,RElem},{'...',L}|Rest],NewOpts,NewResult};
        {nomatch,_}=NM -> NM
    end;
consume_next(Args,[{required,RElem}|Rest],Opts,Result) ->
    case consume_all(Args,RElem,Opts,Result) of
        {NewArgs,NewOpts,NewResult} -> {NewArgs,Rest,NewOpts,NewResult};
        {nomatch,_}=NM -> NM
    end;
%% Positional: a <b> C
consume_next([H|Args],[{word,_,Word}|U],O,R) ->
    case H == Word of
        true -> {Args,U,O,maps:put(to_atom(Word),true,R)};
        false -> {nomatch,maps:put(to_atom(Word),false,R)}
    end;
consume_next([H|T],[{argument,_,ArgName},{'...',_}|Rest]=U,O,R) ->
    Key = to_atom(ArgName),
    case not flag(H) of
        true -> {T,U,O,add_value(R,Key,H)};
        false -> case maps:is_key(Key, R) of
            true -> {T,Rest,O,R};
            false -> {nomatch, maps:put(Key,[])}
        end
    end;
consume_next([H|T],[{argument,_,ArgName}|U],O,R) ->
    Key = to_atom(ArgName),
    case not flag(H) of
        true -> {T,U,O,maps:put(Key,H,R)};
        false -> {nomatch, maps:put(Key,nil)}
    end;
%% Options: -a --b
consume_next([<<$-,C,_/binary>>=A|T],U,O,R) when C =/= $- ->
    consume_next([expand_alias(A,O)|T],U,O,R);
consume_next(Args,[{short_flag,_,_}=A|Rest],Opts,Result) ->
    consume_next(Args,[expand_alias(A,Opts)|Rest],Opts,Result);
consume_next([F|T]=Args,[{long_flag,L,F}|Rest],Opts,Result) ->
    Key = to_atom(F);
    {Nargs,NewRest,NewOpts} = get_nargs(Rest,Key,L,Opts,Result),
    case consume_nargs(Nargs,T) of
        {NewT,Value} -> {NewT,NewRest,NewOpts,maps:put(Key,Value,Result)};
        {nomatch,_} = NM -> NM
    end;

consume_next(_,_,_,_) -> nomatch.

get_nargs(Tokens,Key,L,Opts,Result) ->
    case lists:keyfind(Key,1,Tokens) of
        false    -> perror(L,Key)
        {Key,KW} -> case lists:keyfind(nargs,1,KW) of
            {nargs,Nargs} -> {Nargs,Opts};
            false ->
                Nargs = guess_nargs(Tokens);
                {Nargs,lists:keyreplace(Key,1,Opts,{Key,[{nargs,Nargs}|KW]})
            end
    end.
guess_nargs([{'...',_}|_]) ->
    {count,infinity};
guess_nargs([{argument,_,_}

to_atom(Bin) -> binary_to_atom(Bin,utf8).
add_value(Map,Key,Value) ->
    Atom = to_atom(Key),
    maps:put(Atom,case maps:find(Atom,Map) of
        {ok,V} when is_list(V) -> [Value|V];
        {ok,V}                 -> [Value,V];
        error                  -> [Value]
    end,Map).

flag(<<$-,_/binary>>) -> true;
flag(_) -> false.

%% ensure any remaining elements in the usage line are marked optional
check_optional([],R) -> {success,R};
check_optional([{optional,_}|T],R) -> check_optional(T,R);
check_optional(_,_) -> fail.

%% expand arguments
expand_alias(<<$-,C,Rest/binary>>,Opts) ->
    case option_name(to_atom(<<$-,C>>),Opts) of
        unknown -> perror(0,<<$-,C>>);
        Opt     -> [Opt|expand_alias(Rest,Opts)]
    end;
expand_alias(<<>>,_) -> [];
expand_alias(<<C,Rest/binary>>,Opts) ->
    case option_name(to_atom(<<$-,C>>),Opts) of
        unknown -> [<<C,Rest/binary>>];
        Opt     -> [Opt|expand_alias(Rest,Opts)]
    end;
%% expand parsed tokens
expand_alias({short_flag,L,<<$-,C,Rest/binary>>},Opts) ->
    case option_name(to_atom(<<$-,C>>),Opts) of
        unknown -> perror(L,<<$-,C>>);
        Opt     -> [{long_flag,L,Opt}|expand_alias(L,Rest,Opts)]
    end.
expand_alias(_,<<>>,_) -> [];
expand_alias(L,<<C,Rest/binary>>,Opts) ->
    io:fwrite("token:~p~nresult:~p~n", [[C],option_name(to_atom(<<$-,C>>),Opts)]),
    case option_name(to_atom(<<$-,C>>),Opts) of
        unknown -> io:fwrite(<<C,Rest/binary>>),[{argument,L,<<C,Rest/binary>>}|[]];
        Opt     -> [{long_flag,L,Opt}|expand_alias(L,Rest,Opts)]
    end.

option_name(Alias, Opts) ->
    case lookup_alias(Alias, Opts) of
        {found,Opt} -> atom_to_binary(Opt,utf8);
        notfound    -> unknown
    end.

lookup_alias(_, []) ->
    notfound;
lookup_alias(A, [{Opt,K}|Opts]) ->
    case lists:keyfind(alias,1,K) of
        {alias,A} -> {found,Opt};
        _ -> lookup_alias(A,Opts)
    end.

%% Errors %%

perror(Line, Token) ->
    throw({error,{Line,?MODULE,["Syntax Error: ", Token]}}).

%% Tests

-ifdef(TEST).

simple_tes_() ->
    #{bar := true} = parse([<<"bar">>], "usage: foo bar").

expand_alias_test() ->
    Opts = [{'--foo',[{alias,'-f'}]},{'--bar',[{alias,'-b'}]}],
    [{long_flag,_,<<"--foo">>}] = expand_alias(
        {short_flag,1,<<"-f">>}, Opts),
    [{long_flag,_,<<"--foo">>},{long_flag,_,<<"--bar">>}] = expand_alias(
        {short_flag,1,<<"-fb">>}, Opts),
    [{long_flag,_,<<"--foo">>},{argument,_,<<"r">>}] = expand_alias(
        {short_flag,1,<<"-fr">>}, Opts),
    [<<"--foo">>] = expand_alias(<<"-f">>, Opts),
    [<<"--foo">>, <<"--bar">>] = expand_alias(<<"-fb">>, Opts),
    [<<"--foo">>, <<"r">>] = expand_alias(<<"-fr">>, Opts).


-endif.

%% End of Module.
