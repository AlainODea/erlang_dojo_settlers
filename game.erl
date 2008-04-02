-module(game).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([new/0]).

new() ->
    Hexes = make_hexes(),
    Intersections = make_intersections(),
    Paths = make_paths(),
    {OuterHexes, InnerHexes} = lists:split(12, Hexes),
    outer_perimeter:connect(OuterHexes, Intersections),
    InnerIntersections = lists:nthtail(30, Intersections),
    inner_perimeter:connect(OuterHexes, InnerIntersections),
    {MiddleHexes, [CenterHex]} = lists:split(6, InnerHexes),
    outer_perimeter:connect(MiddleHexes, InnerIntersections),
    CenterIntersections = lists:nthtail(18, InnerIntersections),
    center_perimeter:connect(MiddleHexes, CenterIntersections),
    lists:map(
        fun(Intersection) ->
            CenterHex ! {intersection, Intersection}
        end, CenterIntersections),
    special_intersections:connect(Hexes, Intersections),
    special_paths:connect(Hexes, Intersections),
    Players = make_players(),
    Robber = CenterHex,
    spawn(fun() -> turn(Players, Hexes, Intersections, Paths, Robber) end).

make_players()       -> make_n(player,         4).
make_hexes()         -> make_n(hex,           35).
make_intersections() -> make_n(intersection,  54).
make_paths()         -> make_n(path,         117).

make_n(Actor, N) -> lists:map(fun(_) -> Actor:new() end, lists:seq(1, N)).

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
