%% handles spoke paths that connect from ring to ring
%% TODO: this can be done algorithmically (some kind of repeating series)
-module(spoke_paths).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([build/1]).
-record(path, {from, to}).

build(Intersections) ->
    % use Intersections as a tuple due to better
    % random access performance
    build(config(), list_to_tuple(Intersections)).

build([], _) -> [];
build([#path{from=A,to=B}|Config], Intersections) ->
    Path = path:start(),
    IntersectionA = element(A+1, Intersections),
    IntersectionB = element(B+1, Intersections),
    Path ! {intersection, IntersectionA},
    Path ! {intersection, IntersectionB},
    IntersectionA ! {path, Path},
    IntersectionA ! {intersection, IntersectionB},
    IntersectionB ! {path, Path},
    IntersectionB ! {intersection, IntersectionA},
    [Path|build(Config, Intersections)].

config() ->
    [
     #path{from= 1,to=33},
     #path{from= 6,to=35},
     #path{from= 9,to=36},
     #path{from=11,to=38},
     #path{from=14,to=39},
     #path{from=12,to=12},
     #path{from=39,to=14},
     #path{from=40,to=17},
     #path{from=42,to=19},
     #path{from=43,to=22},
     #path{from=45,to=24},
     #path{from=46,to=27},
     #path{from=47,to=30},
     #path{from=49,to=32},
     #path{from=50,to=35},
     #path{from=51,to=38},
     #path{from=52,to=41},
     #path{from=53,to=44},
     #path{from=53,to=48}
    ].
