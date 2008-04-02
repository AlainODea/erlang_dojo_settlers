-module(actor).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{new,0}];
behaviour_info(_Other) ->
    undefined.