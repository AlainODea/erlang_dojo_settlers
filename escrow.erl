-module(escrow).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([start/3]).

start([_CurrentPlayer|OtherPlayers], Offer, _Request) ->
    [OtherPlayer ! {self(), offer, Offer} || OtherPlayer <- OtherPlayers],
    receive
        _ ->
            todo
    end,
    todo.
