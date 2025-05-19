-module(proxy).
-export([start/0, start/1]).


start() -> start(false).


start(Verbose) ->
    erlang:register(proxy, self()),
    listen(Verbose).


listen(Verbose) ->
    receive
        {Dst, Msg} ->
            case Verbose of
                true -> io:format("Forwarding: ~p ! ~p~n", [Dst, Msg]);
                _ -> ok
            end,
            Dst ! Msg,
            listen(Verbose)
    end.

