-module(hex_numbers).
-export([assign/1]).

%% Hexes <- [pid()] -> {Robber, {[],[pid()],[pid()],...}}
assign(Hexes) ->
     HexNumbering = assign(numbers(), Hexes),
     Sorted = lists:keysort(1, HexNumbering),
     list_to_tuple([[]|coalesce(Sorted)]).

coalesce([]) -> [];
coalesce([{N,Hex1},{N,Hex2}|HexNumbering]) ->
    [[Hex1, Hex2]|coalesce(HexNumbering)];
coalesce([{_,Hex}|HexNumbering]) ->
    [[Hex]|coalesce(HexNumbering)].

assign([], []) -> [];
assign([Number|Numbers], [Hex|Hexes]) ->
    case status:check(Hex) of
        desert -> [{7, Hex}|lists:zip([Number|Numbers], Hexes)];
        _ -> [{Number, Hex}|assign(Numbers, Hexes)]
    end.

numbers() -> [5, 2, 6, 3, 8, 10, 9, 12, 11, 4, 8, 10, 9, 4, 5, 6, 3, 11].
