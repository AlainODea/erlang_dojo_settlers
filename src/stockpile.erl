-module(stockpile).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([start/0]).

% RPC API
-export([build_road/1,
         build_settlement/1,
         build_city/1,
         build_development_card/1]).

build_road(Stockpile) when is_pid(Stockpile) ->
    build(Stockpile, road).

build_settlement(Stockpile) when is_pid(Stockpile) ->
    build(Stockpile, settlement).

build_city(Stockpile) when is_pid(Stockpile) ->
    build(Stockpile, city).

build_development_card(Stockpile) when is_pid(Stockpile) ->
    build(Stockpile, development_card).

build(Stockpile, Buildable) ->
    Stockpile ! {self(), build, Buildable},
    receive
    {did, build, Buildable} ->
        true;
    {didNot, build, Buildable} ->
        false
    end.

start() ->
    spawn(fun() -> stockpile(0, 0, 0, 0, 0) end).

stockpile(L, B, G, W, O) ->
    receive
    {produce, lumber, Amt} ->
        stockpile(L+Amt, B, G, W, O);
    {produce, brick, Amt} ->
        stockpile(L, B+Amt, G, W, O);
    {produce, grain, Amt} ->
        stockpile(L, B, G+Amt, W, O);
    {produce, wool, Amt} ->
        stockpile(L, B, G, W+Amt, O);
    {produce, ore, Amt} ->
        stockpile(L, B, G, W, O+Amt);
    {From, build, road} when is_pid(From) ->
        build_road(From, L, B, G, W, O);
    {From, build, settlement} when is_pid(From) ->
        build_settlement(From, L, B, G, W, O);
    {From, build, city} when is_pid(From) ->
        build_city(From, L, B, G, W, O);
    {From, build, development_card} when is_pid(From) ->
        buy_development_card(From, L, B, G, W, O);
    {Monopolist, monopoly, lumber} when is_pid(Monopolist) ->
        Monopolist ! {produce, lumber, L},
        stockpile(0, B, G, W, O);
    {Monopolist, monopoly, brick} when is_pid(Monopolist) ->
        Monopolist ! {produce, brick, B},
        stockpile(L, 0, G, W, O);
    {Monopolist, monopoly, grain} when is_pid(Monopolist) ->
        Monopolist ! {produce, grain, G},
        stockpile(L, B, 0, W, O);
    {Monopolist, monopoly, wool} when is_pid(Monopolist) ->
        Monopolist ! {produce, wool, W},
        stockpile(L, B, G, 0, O);
    {Monopolist, monopoly, ore} when is_pid(Monopolist) ->
        Monopolist ! {produce, ore, O},
        stockpile(L, B, G, W, 0);
    road_building ->
        road_building(2),
        stockpile(L, B, G, W, O);
    {From, status} when is_pid(From) ->
        From ! {stockpile, [
            {lumber, L}, {brick, B}, {grain, G}, {wool, W}, {ore, O}
        ]},
        stockpile(L, B, G, W, O);
    _ ->
        stockpile(L, B, G, W, O)
    end.

road_building(0) -> done;
road_building(N) ->
    receive
    {From, build, road} ->
        From ! {did, build, road},
        road_building(N-1)
    end.

% 1xL+1xB are needed to build a Road
build_road(From, L, B, G, W, O) ->
    L_new = L - 1,
    B_new = B - 1,
    case lists:all(fun non_negative/1,
            [L_new, B_new]) of
    true ->
        From ! {did, build, road},
        stockpile(L_new, B_new, G, W, O);
    false ->
        From ! {didNot, build, road},
        stockpile(L, B, G, W, O)
    end.

% 1xL+1xB+1xG+1xW are needed to build a Settlement
build_settlement(From, L, B, G, W, O) ->
    L_new = L - 1,
    B_new = B - 1,
    G_new = G - 1,
    W_new = W - 1,
    case lists:all(fun non_negative/1,
            [L_new, B_new, G_new, W_new]) of
    true ->
        From ! {did, build, settlement},
        stockpile(L_new, B_new, G_new, W_new, O);
    false ->
        From ! {didNot, build, settlement},
        stockpile(L, B, G, W, O)
    end.

% 2xG+3xO are needed to build a City
build_city(From, L, B, G, W, O) ->
    G_new = G - 2,
    O_new = G - 3,
    case lists:all(fun non_negative/1,
            [G_new, O_new]) of
    true ->
        From ! {did, build, city},
        stockpile(L, B, G_new, W, O_new);
    false ->
        From ! {didNot, build, city},
        stockpile(L, B, G, W, O)
    end.

% 1xG+1xW+1xO are needed to buy a Development Card
buy_development_card(From, L, B, G, W, O) ->
    G_new = G - 1,
    W_new = W - 1,
    O_new = G - 1,
    case lists:all(fun non_negative/1,
            [G_new, W_new, O_new]) of
    true ->
        From ! {did, build, development_card},
        stockpile(L, B, G_new, W_new, O_new);
    false ->
        From ! {didNot, build, development_card},
        stockpile(L, B, G, W, O)
    end.

non_negative(X) ->
    X >= 0.
