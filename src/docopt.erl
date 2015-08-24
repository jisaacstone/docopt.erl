-module(docopt).

%% docopt: docopt library's entry point.

-export([my_func/0]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% API

my_func() ->
    ok().

%% Internals

ok() ->
    ok.

%% Tests

-ifdef(TEST).

simple_test() ->
    ?assertNot(2 + 2 == 5).

-endif.

%% End of Module.
