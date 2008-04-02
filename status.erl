-module(status).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([check/1]).

check(Pid) when is_pid(Pid) ->
    Pid ! {self(), status},
    receive
    Status -> Status
    after 100 -> timeout
    end.
