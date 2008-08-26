%% handles spoke paths that build from ring to ring
%% TODO: this can be done algorithmically (some kind of repeating series )
-module(spoke_paths).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([build/3]).

build(Outers, Middles, Centers) ->
    outer_spokes(Outers, Middles)++inner_spokes(Middles, Centers).

outer_spokes(Outers, [_|Inners]) -> build3(Outers, Inners).

build3([Outer,_,_|Outers],[Inner|Inners]) ->
    [path:between(Outer, Inner)|build2(Outers, Inners)].

build2([Outer,_],[Inner]) -> [path:between(Outer, Inner)];
build2([Outer,_|Outers],[Inner,_|Inners]) ->
    [path:between(Outer, Inner)|build3(Outers, Inners)].

inner_spokes([], []) -> [];
inner_spokes([Outer,_,_|Outers], [Inner|Inners]) ->
    [path:between(Outer, Inner)|inner_spokes(Outers, Inners)].
