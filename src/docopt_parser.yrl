Header "%% Parser for docopt style options"
"%% http://docopt.org/"
"".


Nonterminals docopt usage_block usage_line usage_lines
elements element
option_blocks option_block option_lines option_line opt_start words
flagopts short long


Terminals '[' ']' '(' ')' '|' '...' '[-]' '[--]' usage eol default
long_flag short_flag word argument


Rootsymbol docopt.


Right 100 '|'.
Unary 200 '...'.


docopt -> usage_lines : {'$1', []}.
docopt -> usage_lines option_lines : {'$1', '$2'}.

usage_lines -> usage_line : ['$1'].
usage_lines -> usage_line usage_lines : ['$1'|'$2'].

usage_line -> usage elements eol : ['$1'|'$2'].

elements -> element : ['$1'].
elements -> element elements : ['$1'|'$2'].

element -> short_flag           : '$1'.
element -> long_flag            : '$1'.
element -> argument             : '$1'.
element -> '[-]'                : '$1'.
element -> '[--]'               : '$1'.
element -> '(' elements ')'     : {required, '$2'}.
element -> '[' elements ']'     : {optional, '$2'}.
element -> element '|' element  : {choice, ['$1'|'$3']}.
element -> element '...'        : {'...', '$1'}.

option_lines -> option_line : ['$1'].
option_lines -> option_line option_lines : ['$1'|'$2'].

option_line -> flagopts : {'$1', nil}.
option_line -> flagopts default : {'$1', '$2'}.

flagopts -> short      : {'$1', nil}.
flagopts -> long       : {nil, '$1'}.
flagopts -> short long : {'$1', '$2'}.

short -> short_flag                : '$1'.
short -> short_flag argument       : {'$1', '$2'}.
short -> short_flag argument '...' : {'$1', '$2', '$3'}.

long -> long_flag                : '$1'.
long -> long_flag argument       : {'$1', '$2'}.
long -> long_flag argument '...' : {'$1', '$2', '$3'}.


Erlang code.

to_atom({_, _, Chars}) when is_list(Chars) -> list_to_atom(Chars);
to_atom(Other) -> io:fwrite("~p~n", [Other]), Other.
