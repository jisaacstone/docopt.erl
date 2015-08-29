-module(docopt_tokenizer).
-export([tokenize/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

tokenize(String) when is_list(String) -> tokenize(list_to_binary(String));
tokenize(Bin) when is_binary(Bin) ->
    {UStart, Line0} = skipto(Bin, comp("^(usage:\\s*)"), 0),
    {Tokens, Rest, Line1} = extract_usage(UStart, Line0),
    {ok, Tokens ++ extract_options(Rest, Line1)}.

comp(S) -> comp(S, []).
comp(S, Opts) ->
    {ok, R} = re:compile(S,
        [unicode, caseless, multiline, {newline, any}] ++ Opts),
    R.

newline_re() -> comp("$.?.?^", [dotall]).

skipto(String, Pattern, Line) ->
    case re:run(String, Pattern) of
        nomatch ->
            {"", Line};
        {match, [{Start,Len}|_]} ->
            End = Start + Len,
            {binary_part(String,End,byte_size(String)-End),
             Line + numlines(binary_part(String,0,End))}
    end.

numlines(S) ->
    case re:run(S, newline_re()) of
        {match, TL} -> length(TL);
        nomatch     -> 0
    end.

extract_usage(String, Line) ->
    W = next_word(String),
    extract_usage(String,Line,W).

next_word(S) ->
    [Word, _] = re:split(S, "[ \t]+", [{parts, 2}, {return, binary}]),
    Word.

extract_usage(String, Line, Word) ->
    R = comp("^\\s*" ++ binary_to_list(Word) ++ "\\s*"),
    tokenize_usage(tl(re:split(String, R)), [], Line, Word).

tokenize_usage([Bin], Usage, Line, Word) ->
    case re:split(Bin, newline_re(), [{parts, 2}, {return, binary}]) of
        [Final, Rest] ->
            {Usage ++ tokenize_usage_line(Final, Line, Word), Rest, Line+1};
        [Final] ->
            {Usage ++ tokenize_usage_line(Final, Line, Word), <<>>, Line}
    end;
tokenize_usage([H|T], Usage, Line, Word) ->
    Tokens = Usage ++ tokenize_usage_line(H, Line, Word),
    tokenize_usage(T, Tokens, Line + 1, Word).

tokenize_usage_line(String, Line, Word) ->
    [{usage, Line, Word}|tokenize_line(String, Line)].

extract_options(String, Line) ->
    extract_options(re:split(String, newline_re()), [], Line).

extract_options([<<>>|T], Tokens, Line) ->
    extract_options(T, Tokens, Line);
extract_options([], Tokens, _) ->
    Tokens;
extract_options([<<$-,_/binary>>=H|T], Tokens, Line) ->
    extract_options(T, Tokens ++ extract_option_line(H, Line), Line + 1);
extract_options([H|T], Tokens, Line) ->
    case re:run(H, "^\\s+") of
        {match, [{0,E}]} when E == byte_size(H) ->
            extract_options(T, Tokens, Line + numlines(H));
        {match, [{0,E}]} ->
            extract_options([binary_part(H,E,byte_size(H)-E)|T], Tokens, Line);
        nomatch ->
            case re:split(H, comp("options:"), [{parts, 2}]) of
                [_,R] -> extract_options([R|T], Tokens, Line);
                _     -> Tokens
            end
    end.

extract_option_line(Bin, Line) ->
    case re:split(Bin, comp("\t|  +"), [{parts, 2}]) of
        [Bin]     -> tokenize_line(Bin, Line);
        [B, Desc] -> tokenize_line(B, Line, maybe_default(Desc, Line))
    end.

maybe_default(Desc, Line) ->
  case re:run(Desc, "\\[default: ([^\\]]+)\\]", [{capture, [1], binary}]) of
      {match, [M]} -> [{default, Line, M}];
      nomatch    -> []
  end.

tokenize_line(String, Line) -> tokenize_line(String, Line, []).
tokenize_line(String, Line, Tokens) ->
    {ok, R} = re:compile("\\s+|([()[\\]|]|\\.\\.\\.)"),
    lists:foldr(fun(P, T) -> token(P, Line) ++ T end,
                Tokens ++ [{eol, Line}],
                re:split(String, R, [{return, binary}])).

token(<<>>, _) -> [];
token(T, L) when T==<<"(">>; T==<<")">>; T==<<"[">>; T==<<"]">>;
                 T==<<"|">>; T==<<"...">>; T==<<"[-]">>; T==<<"[--]">> -> 
    [{binary_to_atom(T, utf8), L}];
token(<<$<,_/binary>>=B, L) ->
    case binary:last(B) of
        $> -> [{argument,L,B}]
    end;
token(<<$-,$-,_/binary>>=F, L) ->
    flag_maybe_arg(long_flag, L, F);
token(<<$-,_/binary>>=F, L) ->
    flag_maybe_arg(short_flag, L, F);
token(B, L) ->
    case re:run(B, "^[^a-z]*[A-Z][^a-z]*$") of
        {match, _} -> [{argument,L,B}];
        nomatch    -> [{word,L,B}]
    end.

flag_maybe_arg(FlagName, L, Bin) ->
    case re:split(Bin, "=", [{return, binary}]) of
        [Flg, Arg] -> [{FlagName,L,Flg},
                       {argument,L,Arg}];
        [Bin]      -> [{FlagName,L,Bin}]
    end.

%% Tests

-ifdef(TEST).

tokenize_test() ->
    {ok,[{usage,_,_},{word,_,_},{eol,_}]} = tokenize(
        "usage: foo f"),
    {ok,[{usage,_,_},{word,_,_},{eol,_}]} = tokenize(
        "bla bla bla \nusage: foo f\n bla bla bla"),
    {ok,[{usage,_,_},{argument,_,_},{short_flag,_,_},{long_flag,_,_},
        {argument,_,_},{eol,_}]} = tokenize(
        "usage: foo <bar> -baz --biz BOZ"),
    {ok,[{usage,_,_},{'[',_},{word,_,_},{']',_},{eol,_},
        {short_flag,_,_},{argument,_,_},{eol,_},
        {short_flag,_,_},{long_flag,_,_},{eol,_}]} = tokenize(
        "usage: foo [options]\n\noptions:\n-f FOO  foo\n-b, --bar  bar"),
    {ok,[{usage,_,_},{'[',_},{'(',_},{word,_,_},{word,_,_},{')',_},
        {'|',_},{long_flag,_,_},{argument,_,_},{']',_},{eol,_}]} = tokenize(
        "usage: foo [(bar baz) | --fin=RAKE]"),
    {ok,[{usage,_,_},{'[',_},{short_flag,_,_},{']',_},{eol,_},
        {short_flag,_,_},{default,_,_},{eol,_]} = tokenize(
        "usage: foo [-b]\n\n-b  Bean [default: lima]").

-endif.

%% End of Module.
