-module(game).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([start/0]).

start() ->
    OuterHexes = startn(12, hex),
    OuterIntersections = startn(30, intersection),
    outer_perimeter:connect(OuterHexes, OuterIntersections),
    MiddleIntersections = startn(18, intersection),
    inner_perimeter:connect(OuterHexes, MiddleIntersections),
    MiddleHexes = startn(6, hex),
    outer_perimeter:connect(MiddleHexes, MiddleIntersections),
    CenterIntersections = startn(6, intersection),
    center_perimeter:connect(MiddleHexes, CenterIntersections),
    CenterHex = hex:start(),
    [CenterHex ! {intersection, Intersection} || Intersection <- CenterIntersections],
    Intersections = lists:append([OuterIntersections, MiddleIntersections, CenterIntersections]),
    WalkablePaths = walkable_paths:build(Intersections),
    SpecialPaths = special_paths:build(Intersections),
    % TODO: need random way of choosing the desert hex
    % TODO: need way of randomly dealing out terrain types onto hexes
    Robber = CenterHex,
    Hexes = lists:append([OuterHexes, MiddleHexes, CenterHex]),
    special_intersections:connect(Hexes, Intersections),
    Players = [player:start() || _ <- lists:seq(1, 4)],
    Paths = WalkablePaths ++ SpecialPaths,
    spawn(fun() -> turn(Players, Hexes, Intersections, Paths, Robber) end).

startn(N, Mod) -> [Mod:start() || _ <- lists:seq(1, N)].

turn([CurrentPlayer|OtherPlayers], Hexes, Intersections, Paths, Robber) ->
    production(CurrentPlayer, Hexes, Robber),
    trading(CurrentPlayer),
    building(CurrentPlayer),
    turn(OtherPlayers ++ CurrentPlayer, Hexes, Robber, Intersections, Paths).

production(CurrentPlayer, Hexes, Robber) ->
    Dice = random:uniform(6) + random:uniform(6),
    if
    Dice =:= 7 ->
        moving_robber(CurrentPlayer, Robber);
    true ->
        [Hex ! produce || Hex <- Hexes]
    end.

moving_robber(CurrentPlayer, Robber) ->
    CurrentPlayer ! {self(), move, Robber},
    receive
    {done, moving, robber} ->
        done
    end.

trading(CurrentPlayer) ->
    CurrentPlayer ! {self(), trade},
    receive
    {done, trading} ->
        done
    end.

building(CurrentPlayer) ->
    CurrentPlayer ! {self(), build},
    receive
    {done, building} ->
        done
    end.
