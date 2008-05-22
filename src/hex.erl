-module(hex).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([new/0]).

new() ->
    % TODO: create a registry process to generate terrain type
    new(lumber).

% argument value checking
% disallow unsupported resource types
new(lumber) -> new1(lumber);
new(brick)  -> new1(brick);
new(grain)  -> new1(grain);
new(wood)   -> new1(wood);
new(ore)    -> new1(ore).

new1(ResourceType) ->
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
