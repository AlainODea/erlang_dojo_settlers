-module(player).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([start/0]).

start() -> spawn(fun() -> player(stockpile:start()) end).

player(Stockpile) ->
    receive
    {Game, move, Robber} ->
        move_robber(Stockpile, Game, Robber);
    {Game, trade} ->
        trade(Stockpile, Game);
    {Game, build} ->
        build(Stockpile, Game);
    _ ->
        player(Stockpile)
    end.

trade(Stockpile, Game) ->
    % TODO: get the player to trade if they wish to
    Game ! {done, trading},
    player(Stockpile).

build(Stockpile, Game) ->
    receive
        {done, building} ->
            Game ! {done, building},
            player(Stockpile)
    end.

move_robber(Stockpile, Game, Robber) ->
    receive
        {move, robber, NewRobber} ->
            Robber ! {robber, leaves},
            NewRobber ! {robber, arrives},
            Game ! {done, moving, robber},
            player(Stockpile)
    end.
