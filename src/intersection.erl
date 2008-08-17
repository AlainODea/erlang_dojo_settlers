-module(intersection).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([start/0]).

start() ->
    % create an intersection waiting for
    % 3 intersections and 3 paths
    spawn(fun() -> waiting(3, 3, [], []) end).

% waiting for nothing
waiting(0, 0, Intersections, Paths) ->
    disconnected(Intersections, Paths);
% waiting for intersections
waiting(I, 0, Intersections, Paths) ->
    receive
    {intersection, Intersection} when is_pid(Intersection) ->
        waiting(I-1, 0, [Intersection|Intersections], Paths);
    {From, status} when is_pid(From) ->
        From ! {waiting, for, [{intersection, I}]},
        waiting(I, 0, Intersections, Paths);
    BadMsg ->
        exit({badMsg, BadMsg})
    end;
% waiting for paths
waiting(0, P, Intersections, Paths) ->
    receive
    {path, Path} when is_pid(Path) ->
        waiting(0, P-1, Intersections, [Path|Paths]);
    {From, status} when is_pid(From) ->
        From ! {waiting, for, [{path, P}]},
        waiting(0, P, Intersections, Paths);
    BadMsg ->
        exit({badMsg, BadMsg})
    end;
% waiting for intersections and paths
waiting(I, P, Intersections, Paths) ->
    receive
    {intersection, Intersection} when is_pid(Intersection) ->
        waiting(I-1, P, [Intersection|Intersections], Paths);
    {path, Path} when is_pid(Path) ->
        waiting(I, P-1, Intersections, [Path|Paths]);
    {From, status} when is_pid(From) ->
        From ! {waiting, for, [{path, P}, {intersection, I}]},
        waiting(I, P, Intersections, Paths);
    BadMsg ->
        exit({badMsg, BadMsg})
    end.

disconnected(Intersections, Paths) ->
    receive
    block ->
        % Intersections are no longer relevant state
        % discard by omission
        blocked(Paths);
    connect ->
        connect(Paths),
        % Paths are no longer relevant state
        % discard by omission
        connected(Intersections);
    {basecamp, Owner} when is_pid(Owner) ->
        block(Intersections),
        connect(Paths),
        % Intersections and Paths are no longer relevant state
        % discard by omission
        settlement(Owner);
    {From, status} when is_pid(From) ->
        From ! disconnected,
        disconnected(Intersections, Paths);
    _ ->
        disconnected(Intersections, Paths)
    end.

block(Intersections) ->
    [Intersection ! block || Intersection <- Intersections].

connect(Paths) ->
    [Path ! connect || Path <- Paths].

connected(Intersections) ->
    receive
    block ->
        % Discard all state by omission since a blocked
        % intersection does nothing but report its status
        % and discard other messages
        blocked();
    {settlement, Owner} when is_pid(Owner) ->
        case stockpile:build_settlement(Owner) of
        true ->
            block(Intersections),
            % Intersections are no longer relevant state
            % discard by omission
            settlement(Owner);
        _ -> connected(Intersections)
        end;
    {basecamp, Owner} when is_pid(Owner) ->
        block(Intersections),
        % Intersections are no longer relevant state
        % discard by omission
        settlement(Owner);
    {From, status} when is_pid(From) ->
        From ! connected,
        connected(Intersections);
    _ ->
        connected(Intersections)
    end.

blocked(Paths) ->
    receive
    connect ->
        connect(Paths),
        % Paths are no longer relevant state
        % discard by omission
        blocked();
    {From, status} when is_pid(From) ->
        From ! blocked,
        blocked(Paths);
    _ ->
        blocked(Paths)
    end.

blocked() ->
    receive
    {From, status} when is_pid(From) ->
        From ! blocked,
        blocked();
    _ ->
        blocked()
    end.

settlement(Owner) ->
    receive
    {produce, ResourceType} ->
        Owner ! {produce, 1, ResourceType},
        settlement(Owner);
    {city, Owner} ->
        case stockpile:build_city(Owner) of
        true -> city(Owner);
        _ -> settlement(Owner)
        end;
    {From, status} when is_pid(From) ->
        From ! {settlement, Owner},
        settlement(Owner);
    _ ->
        settlement(Owner)
    end.

city(Owner) ->
    receive
    {produce, ResourceType} ->
        Owner ! {produce, 2, ResourceType},
        city(Owner);
    {From, status} when is_pid(From) ->
        From ! {city, Owner};
    _ ->
        city(Owner)
    end.
