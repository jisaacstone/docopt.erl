-module(docopt).

%% docopt: docopt library's entry point.

-export([parse/2]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(is_flag(Name), Name==short_flag; Name==long_flag; Name==alias).

%% API

parse(ArgList,DocString) ->
    {ok, Tokens} = docopt_tokenizer:tokenize(DocString),
    {ok, {UsageLines,Opts}} = docopt_parser:parse(Tokens),
    {ArgList,parse_usage(UsageLines,Opts)}.

%% Internals

%% Parsing %%

parse_usage(UsageLines, Opts) -> parse_usage(UsageLines, [], Opts).

parse_usage([], Usage, Opts) -> {Usage, Opts};
parse_usage([H|T], Usage, Opts) ->
    parse_usage(T, [usage_line(H, Opts)|Usage], Opts).

usage_line(Args, Opts) -> usage_line(Args, {[],[]}, Opts).

usage_line([], Usage, Opts) -> {Usage, Opts};
%% `[options]` is a shortcut for all options.
usage_line([{'[',_,_},{_,_,<<"options">>},{']',_,_}|T], {P,_}, O) ->
    usage_line(T,{P,lists:nth(1,O)},O);
usage_line([{short_flag,_,_}=F|T], U, O) ->
    usage_line([maybe_expand(F,O)|T], U, O);
usage_line([A,B|T], {P,F}, O) when ?is_flag(A); ?is_flag(B) ->
    Name = binary_to_existing_atom(element(3,A),utf8),
    Opts = set_nargs(O,Name,boolean),
    usage_line([B|T],{P,[Name|F]},Opts).

maybe_expand({short_flag,L,<<$-,_>>=F},Opts) ->
    case check_alias(F,Opts) of
        unknown -> error(L,F);
        Alias -> [{alias,L,Alias}]
    end;
maybe_expand({short_flag,L,<<$-,Rest/binary>>},Opts) ->
    maybe_expand(L,Rest,Opts).
maybe_expand(_,<<>>,_) -> [];
maybe_expand(L,<<C,Rest/binary>>,Opts) ->
    case check_alias(<<$-,C>>, Opts) of
        unknown -> [{argument,L,<<C,Rest/binary>>}];
        Alias   -> [{alias,L,Alias}|maybe_expand(L,Rest,Opts)]
    end.

check_alias(A, Opts) ->
    Alias = binary_to_atom(A,utf8),
    case knownalias(Alias, Opts) of
        true -> Alias;
        false -> unknown
    end.
knownalias(A, Opts) ->
    lists:any(fun({_,K}) -> lists:keyfind(alias,1,K) == {alias,A} end, Opts).

%% Errors %%

error(Line, Token) ->
    throw({error,{Line,?MODULE,["Syntax Error: ", Token]}});

%% Tests

-ifdef(TEST).

simple_test() ->
    ?assertNot(2 + 2 == 5).

-endif.

%% End of Module.
