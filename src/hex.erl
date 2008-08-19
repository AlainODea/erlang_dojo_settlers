-module(hex).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([start/1]).

% argument value checking
% disallow unsupported resource types
start(lumber)  -> start1(lumber);
start(brick)   -> start1(brick);
start(grain)   -> start1(grain);
start(wool)    -> start1(wool);
start(ore)     -> start1(ore);
start(desert)  -> start1(desert).

start1(ResourceType) ->
    % create a hex which is waiting for six intersections
    spawn(fun() -> waiting(6, [], ResourceType) end).

% waiting for intersections
waiting(0, Intersections, ResourceType) ->
    hex(Intersections, ResourceType);
waiting(N, Intersections, ResourceType) ->
    receive
    {intersection, Intersection} when is_pid(Intersection) ->
        waiting(N-1, [Intersection|Intersections], ResourceType);
    {From, status} when is_pid(From) ->
        From ! {waiting, for, [{intersections, N}]},
        waiting(N, Intersections, ResourceType);
    BadMsg ->
        exit({badMsg, BadMsg})
    end.

hex(_, desert) -> desert();
hex(Intersections, ResourceType) ->
    receive
    produce ->
        [Intersection ! {produce, ResourceType} || Intersection <- Intersections],
        hex(Intersections, ResourceType);
    {robber, arrives} ->
        blocked(Intersections, ResourceType);
    {From, status} when is_pid(From) ->
        From ! {hex, ResourceType},
        hex(Intersections, ResourceType);
    _ ->
        hex(Intersections, ResourceType)
    end.

blocked(Intersections, ResourceType) ->
    receive
    {robber, leaves} ->
        hex(Intersections, ResourceType);
    {From, status} when is_pid(From) ->
        From ! blocked,
        blocked(Intersections, ResourceType);
    _ ->
        blocked(Intersections, ResourceType)
    end.

desert() ->
    receive
    {From, status} when is_pid(From) ->
        From ! desert,
        desert();
    _ -> desert()
    end.
