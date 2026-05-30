-module(maps_util).

-export([all/2]).


all(Fun, Map) ->
    maps:fold(fun(Key, Value, Acc) -> Acc andalso Fun(Key, Value) end, true, Map).