-module(dbg_log).
-export([init/0, log/1, print/0]).

init() -> 
    Pid = spawn(fun() -> process_events([], false) end),
    % register(dbg_log, Pid),
    ok.
    % case file:open("backend-log.log", [write]) of
    %     {ok, _IODevice} -> ok
    % end.

log(Msg) -> 
    % if 
    %     element(2, Msg) == "Stimulation" orelse element(3, Msg) == "value" orelse element(4, Msg) == "value" -> 
    %        dbg_log ! {log, {utils:get_timestamp_str(), erlang:unique_integer([monotonic])}, Msg},
    %     true -> ok
    % end,
    ok.
    % Timestamp = utils:get_timestamp_str(),
    % case file:open("backend-log.log", [append]) of
    %     {ok, IODevice} ->
    %         io:fwrite(IODevice, "~s ~p~n", [Timestamp, Msg]),
    %         file:close(IODevice)
    % end.

print() ->
    % dbg_log ! print,
    ok.


process_events(Msgs, AlreadyPrinted) ->
    receive
        {log, Timestamp, Msg} -> process_events([{Timestamp, Msg}] ++ Msgs, AlreadyPrinted);
        
        print -> 
            Mode = case AlreadyPrinted of
                true -> append;
                false -> write
            end,

            case file:open("backend-log.log", [Mode]) of
                {ok, IODevice} -> 
                    lists:foreach(
                        fun ({{Timestamp, _Mon}, Msg}) ->
                            io:fwrite(IODevice, "~s ~p~n", [Timestamp, Msg])
                        end,
                        lists:sort(
                            fun({TimestampA, _MesA}, {TimestampB, _MesB}) ->
                                TimestampA =< TimestampB
                            end,
                            Msgs
                        )
                    ),
                    file:close(IODevice)
            end,

            process_events([], true)
    end.

