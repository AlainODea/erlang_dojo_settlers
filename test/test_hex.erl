-module(test_hex).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([run/0]).

run() ->
    Hex = hex:start(),
    testInitialState(Hex).

assert(Actor, Status) ->
    case status:check(Actor) of
    Status -> true;
    BadStatus -> throw({expect, Status, was, BadStatus})
    end.

testInitialState(Hex) ->
    assert(Hex, {waiting, for, [intersection, 3]}).