-module(path).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([between/2,start/0]).

between(I1, I2) when is_pid(I1), is_pid(I2) ->
    Path = start(),
    I1 ! {path, Path},
    I2 ! {path, Path},
    I1 ! {intersection, I2},
    I2 ! {intersection, I1},
    Path ! {intersection, I1},
    Path ! {intersection, I2},
    Path.

start() ->
    spawn(fun waiting/0).

waiting() ->
    receive
    {intersection, IntersectionA} when is_pid(IntersectionA) ->
        waiting(IntersectionA);
    {From, status} when is_pid(From) ->
        From ! {waiting, for, [{intersection, 2}]},
        waiting();
    BadMsg ->
        exit({badMsg, BadMsg})
    end.

waiting(IntersectionA) ->
    receive
    {intersection, IntersectionB} when is_pid(IntersectionB) ->
        disconnected(IntersectionA, IntersectionB);
    {From, status} when is_pid(From) ->
        From ! {waiting, for, [{intersection, 1}]},
        waiting(IntersectionA);
    BadMsg ->
        exit({badMsg, BadMsg})
    end.

disconnected(IntersectionA, IntersectionB) ->
    receive
    connect ->
        connected(IntersectionA, IntersectionB);
    {From, status} when is_pid(From) ->
        From ! disconnected,
        disconnected(IntersectionA, IntersectionB);
    _ ->
        disconnected(IntersectionA, IntersectionB)
    end.

connected(IntersectionA, IntersectionB) ->
    receive
    {road, Owner} when is_pid(Owner) ->
        case stockpile:build_road(Owner) of
        true ->
            IntersectionA ! connect,
            IntersectionB ! connect,
            road(Owner);
        _ -> connected(IntersectionA, IntersectionB)
        end;
    {From, status} when is_pid(From) ->
        From ! connected,
        connected(IntersectionA, IntersectionB);
    _ ->
        connected(IntersectionA, IntersectionB)
    end.

road(Owner) ->
    receive
    {From, status} when is_pid(From) ->
        From ! {road, Owner},
        road(Owner);
    _ ->
        road(Owner)
    end.
