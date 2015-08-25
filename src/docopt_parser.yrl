Header "%% Parser for docopt style options"
"%% http://docopt.org/"
"".


Nonterminals docopt
usage_line usage_lines elements element nargs paren_expr
option_lines option_line flagopts short long.


Terminals '[' ']' '(' ')' '|' '...' '[-]' '[--]' usage eol default
long_flag short_flag word argument.


Rootsymbol docopt.


Right 100 '|'.
Unary 200 '...'.


docopt -> usage_lines              : {'$1', []}.
docopt -> usage_lines option_lines : {'$1', '$2'}.

usage_lines -> usage_line             : ['$1'].
usage_lines -> usage_line usage_lines : ['$1'|'$2'].

usage_line -> usage elements eol : ['$1'|'$2'].

elements -> element          : ['$1'].
elements -> element elements : ['$1'|'$2'].

element -> short_flag          : maybe_expand('$1').
element -> long_flag           : '$1'.
element -> argument            : '$1'.
element -> word                : '$1'.
element -> '...'               : '$1'.
element -> '[-]'               : '$1'.
element -> '[--]'              : '$1'.
element -> paren_expr          : '$1'.
element -> element '|' element : choice('$1', '$3').

paren_expr -> '(' elements ')' : {required, '$2'}.
paren_expr -> '[' elements ']' : {optional, '$2'}.

option_lines -> option_line              : ['$1'].
option_lines -> option_line option_lines : ['$1'] ++ '$2'.

option_line -> flagopts eol         : '$1'.
option_line -> flagopts default eol : setdefault('$1', '$2').

flagopts -> short      : name('$1').
flagopts -> long       : name('$1').
flagopts -> short long : alias(name('$1'), name('$2')).

short -> short_flag       : '$1'.
short -> short_flag nargs : {'$1', '$2'}.

long -> long_flag       : '$1'.
long -> long_flag nargs : {'$1', '$2'}.

nargs -> argument       : {1, 1}.
nargs -> argument nargs : nargs('$2', '$1').
nargs -> '[' nargs ']'  : nargs(optional, '$2').
nargs -> nargs '...'    : nargs(ellipses, '$1').


Erlang code.

name({{_,_,Opt}, Nargs}) -> {Opt, [{nargs,Nargs}]};
name({_,_,Opt}) -> {Opt, []}.

alias({_,[{nargs,A}]},{T,[{nargs,B}]}) when A =/= B ->
    throw({error, {0, ?MODULE, ["Syntax Error: ", T]}});
alias({Short,_},{Long,KW}) -> {Long,[{alias,Short}|KW]}.

setdefault({Arg,KW}, {default,_,Dft}) -> {Arg, [{default,Dft}|KW]}.

nargs({Min, infinity}, _Arg) -> {Min + 1, infinity};
nargs({Min, Max}, _Arg) -> {Min + 1, Max + 1};
nargs(optional, {_, Max}) -> {0, Max};
nargs(ellipses, {Min, _}) -> {Min, infinity}.

choice({_,_,_}=A,{_,_,_}=B) -> {choice,[A,B]};
choice(A,{choice,L}) -> {choice,[A|L]}.

maybe_expand(Short={_,Line,Value}, Opts) ->
    case atom_to_list(Value) of
        [_]   -> Short;
        Flags -> expand(
            lists:partition(fun(C) -> knownalias(C, Opts) end, Flags), Line)
    end.

expand({[], Rest}, L) ->
    throw({error,{L,?MODULE,["Syntax Error: ", Rest]}});
expand({Flags, []}, L) ->
    lists:foldr(fun(C,T) -> [{short_flag,L,list_to_atom([C])}|T] end,
                [], Flags);
expand({Flags, Rest}, L) ->
    lists:foldr(fun(C,T) -> [{short_flag,L,list_to_atom([C])}|T] end,
                {argument,L,Rest}, Flags);

knownalias(C, Opts) when is_number(C) ->
    knownalias(list_to_atom([C]));
knownalias(A, Opts) ->
    lists:any(fun({_,K}) -> lists:keyfind(alias,1,K) == {alias,A} end, Opts).
