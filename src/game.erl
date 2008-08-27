-module(game).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([start/0]).

start() ->
    HexFactory = hex_factory:start(),
    OuterHexes = hex_factory:hexes(12, HexFactory),
    OuterIntersections = startn(30, intersection),
    outer_perimeter:connect(OuterHexes, OuterIntersections),
    shoreline:connect(OuterIntersections),
    OuterPaths = walkable_paths:build(OuterIntersections),
    MiddleIntersections = startn(18, intersection),
    inner_perimeter:connect(OuterHexes, MiddleIntersections),
    MiddleHexes = hex_factory:hexes(6, HexFactory),
    middle_perimeter:connect(MiddleHexes, MiddleIntersections),
    MiddlePaths = walkable_paths:build(MiddleIntersections),
    CenterIntersections = startn(6, intersection),
    center_perimeter:connect(MiddleHexes, CenterIntersections),
    CenterPaths = walkable_paths:build(CenterIntersections),
    CenterHex = hex_factory:hex(HexFactory),
    [CenterHex ! {intersection, Intersection} || Intersection <- CenterIntersections],
    Intersections = lists:append([OuterIntersections, MiddleIntersections, CenterIntersections]),
    SpokePaths = spoke_paths:build(OuterIntersections, MiddleIntersections, CenterIntersections),
    % TODO: need random way of choosing the desert hex
    % TODO: need way of randomly dealing out terrain types onto hexes
    Robber = CenterHex,
    Hexes = lists:append([OuterHexes, MiddleHexes, [CenterHex]]),
    Players = [player:start() || _ <- lists:seq(1, 4)],
    Paths = lists:append([OuterPaths, MiddlePaths, CenterPaths, SpokePaths]),
    HexNumbering = hex_numbers:assign(Hexes),
    spawn(fun() -> game(Players, HexNumbering, Intersections, Paths, Robber) end).

startn(N, Mod) -> [Mod:start() || _ <- lists:seq(1, N)].

game(Players, HexNumbering, Intersections, Paths, Robber) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    turn(Players, HexNumbering, Intersections, Paths, Robber).

turn([CurrentPlayer|OtherPlayers], HexNumbering, Intersections, Paths, Robber) ->
    production(CurrentPlayer, HexNumbering, Robber),
    trading(CurrentPlayer),
    building(CurrentPlayer),
    turn(OtherPlayers ++ CurrentPlayer, HexNumbering, Robber, Intersections, Paths).

production(CurrentPlayer, HexNumbering, Robber) ->
    Dice = random:uniform(6) + random:uniform(6),
    io:format("~w rolled ~w~n", [CurrentPlayer, Dice]),
    production(Dice, CurrentPlayer, HexNumbering, Robber).

production(7, CurrentPlayer, _, Robber) ->
    moving_robber(CurrentPlayer, Robber);
production(Roll, _, HexNumbering, _) ->
    %% Need a mechanism for assigning die rolls to hexes
    [Hex ! produce || Hex <- element(Roll, HexNumbering)].

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
