Header "%% Parser for docopt style options"
"%% http://docopt.org/"
"".


Nonterminals docopt
usage_line usage_lines elements element nargs paren_expr choiceexpr
option_lines option_line flagopts short long.


Terminals '[' ']' '(' ')' '|' '...' usage eol default
long_flag short_flag word argument.


Rootsymbol docopt.


Right 100 '|'.
Unary 200 '...'.


docopt -> usage_lines              : {'$1', []}.
docopt -> usage_lines option_lines : {'$1', '$2'}.

usage_lines -> usage_line             : ['$1'].
usage_lines -> usage_line usage_lines : ['$1'|'$2'].

usage_line -> usage elements eol : '$2'.

elements -> element          : ['$1'].
elements -> element elements : ['$1'|'$2'].

element -> short_flag          : '$1'.
element -> long_flag           : '$1'.
element -> argument            : '$1'.
element -> word                : '$1'.
element -> '...'               : '$1'.
element -> paren_expr          : '$1'.
element -> element '|' element : choice('$1', '$3').

paren_expr -> '(' elements ')'                : {required, '$2'}.
paren_expr -> '[' elements ']'                : {optional, '$2'}.
paren_expr -> '(' elements '|' choiceexpr ')' : {required, choice('$2', '$4')}.
paren_expr -> '[' elements '|' choiceexpr ']' : {optional, choice('$2', '$4')}.

choiceexpr -> elements                : '$1'.
choiceexpr -> elements '|' choiceexpr : choice('$1', '$3').

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
nargs -> '...'          : {count, infinity}.


Erlang code.

%% Options %%

name({Opt, Nargs}) -> {name(Opt), [{nargs,Nargs}]};
name({_,_,Opt}) -> {binary_to_atom(Opt,utf8), []}.

alias({_,[{nargs,A}]},{T,[{nargs,B}]}) when A =/= B ->
    throw({error, {0, ?MODULE, ["Syntax Error: ", T]}});
alias({Short,_},{Long,KW}) -> {Long,[{alias,Short}|KW]}.

setdefault({Arg,KW}, {default,_,Dft}) -> {Arg, [{default,Dft}|KW]}.
nargs({Min, infinity}, _Arg) -> {Min + 1, infinity};
nargs({Min, Max}, _Arg) -> {Min + 1, Max + 1};
nargs(optional, {_, Max}) -> {0, Max};
nargs(ellipses, {Min, _}) -> {Min, infinity}.

%% PreParsing %%

choice({_,_,_}=E1,{_,_,_}=E2) -> {choice,[[E1],[E2]]};
choice(E1,E2) when is_list(E1), is_list(E2) ->
    {choice,[E1,E2]};
choice(Element,{choice,ChoiceList}) when is_list(Element) ->
    {choice,[Element|ChoiceList]};
choice(Element,{choice,ChoiceList}) ->
    {choice,[[Element]|ChoiceList]}.
