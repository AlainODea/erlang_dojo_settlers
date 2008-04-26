-module(outer_perimeter).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Hexes, Intersections) ->
    connect_hex_to_two(Hexes, Intersections).

connect_hex_to_two([],_) -> done;
connect_hex_to_two([Hex|Hexes],[Intersection1,Intersection2|Intersections]) ->
    Hex ! {intersection, Intersection1},
    Hex ! {intersection, Intersection2},
    connect_shared_hexes(fun connect_hex_to_one/2, Hex, Hexes, Intersections).

connect_shared_hexes(Fun, Hex1, [Hex2|Hexes], [Intersection|Intersections]) ->
    Hex1 ! {intersection, Intersection},
    Hex2 ! {intersection, Intersection},
    Fun(Hexes, Intersections).

connect_hex_to_one([],_) -> done;
connect_hex_to_one([Hex|Hexes],[Intersection|Intersections]) ->
    Hex ! {intersection, Intersection},
    connect_shared_hexes(fun connect_hex_to_two/2, Hex, Hexes, Intersections).
