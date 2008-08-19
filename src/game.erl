-module(game).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([start/0]).

start() ->
    HexFactory = hex_factory:start(),
    OuterHexes = hex_factory:hexes(12, HexFactory),
    OuterIntersections = startn(30, intersection),
    outer_perimeter:connect(OuterHexes, OuterIntersections),
    OuterPaths = walkable_paths:build(OuterIntersections),
    MiddleIntersections = startn(18, intersection),
    inner_perimeter:connect(OuterHexes, MiddleIntersections),
    MiddleHexes = hex_factory:hexes(6, HexFactory),
    outer_perimeter:connect(MiddleHexes, MiddleIntersections),
    MiddlePaths = walkable_paths:build(MiddleIntersections),
    CenterIntersections = startn(6, intersection),
    center_perimeter:connect(MiddleHexes, CenterIntersections),
    CenterPaths = walkable_paths:build(CenterIntersections),
    CenterHex = hex_factory:hex(HexFactory),
    [CenterHex ! {intersection, Intersection} || Intersection <- CenterIntersections],
    Intersections = lists:append([OuterIntersections, MiddleIntersections, CenterIntersections]),
    SpecialPaths = special_paths:build(Intersections),
    % TODO: need random way of choosing the desert hex
    % TODO: need way of randomly dealing out terrain types onto hexes
    Robber = CenterHex,
    Hexes = lists:append([OuterHexes, MiddleHexes, CenterHex]),
    Players = [player:start() || _ <- lists:seq(1, 4)],
    Paths = lists:append([OuterPaths, MiddlePaths, CenterPaths, SpecialPaths]),
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
