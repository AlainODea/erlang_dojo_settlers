-module(path).
-behaviour(actor).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([new/0]).

new() ->
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
