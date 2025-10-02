-module(report).
-export([   %% control
    start/1,
    stop/1
]).
-export([   %% topology
    node_group_creation/4, 
    node_creation/6, 
    connection_formed/4, 
    connection_broken/4,
    node_killed/3
]).
-export([   %% stimulation
    node_group_modes/5,
    node_stimulated/9,
    node_poisoned/4
]).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

start(ReportConfig) ->
    spawn(fun() -> init_reporter(ReportConfig) end).


node_group_creation(Id, NodeGroupName, NodeGroupKind, Reporter) -> report(topology, na, node_group_creation, {Id, NodeGroupName, NodeGroupKind}, Reporter).

%%% report creation of a single node
%%% NodeKind = <vn | on | ...>, NodeParameters - depend on NodeKind
node_creation(Id, NodeKind, NodeParameters, NodeGroupId, ExperimentStep, Reporter) -> report(topology, ExperimentStep, node_creation, {Id, NodeKind, NodeParameters, NodeGroupId}, Reporter).


connection_formed(SourceNodeId, DestNodeId, ExperimentStep, Reporter) -> report(topology, ExperimentStep, connection_formed, {SourceNodeId, DestNodeId}, Reporter).


connection_broken(SourceNodeId, DestNodeId, ExperimentStep, Reporter) -> report(topology, ExperimentStep, connection_broken, {SourceNodeId, DestNodeId}, Reporter).


node_killed(NodeId, ExperimentStep, Reporter) -> report(topology, ExperimentStep, node_killed, {NodeId}, Reporter).


node_group_modes(false, _NodeGroupModes, _ExperimentStep, _StimulationName, _Reporter) -> ok;

node_group_modes(true, NodeGroupModes, ExperimentStep, StimulationName, Reporter) ->
    report(stimulation, ExperimentStep, StimulationName, na, node_group_modes, {NodeGroupModes}, Reporter).


node_stimulated(false, _StimulatedNodeId, _SourceNodeId, _NewExcitation, _Stimulus, _ExperimentStep, _StimulationName, _Depth, _Reporter) -> ok;

node_stimulated(true, StimulatedNodeId, SourceNodeId, NewExcitation, Stimulus, ExperimentStep, StimulationName, Depth, Reporter) -> 
    report(stimulation, ExperimentStep, StimulationName, Depth, node_stimulated, {StimulatedNodeId, SourceNodeId, NewExcitation, Stimulus}, Reporter).


node_poisoned(_NodeId, _NewAccPoisonLvl, _ExperimentStep, _Reporter) -> ok. % report(stimulation, ExperimentStep, na, na, node_poisoned, {NodeId, NewAccPoisonLvl}, Reporter).


stop(Reporter) -> Reporter ! stop.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

report(topology, ExperimentStep, EventType, Args, Reporter) ->
    send_msg({topology, ExperimentStep, EventType, Args}, Reporter).

report(stimulation, ExperimentStep, StimulationName, Depth, EventType, Args, Reporter) ->
    send_msg({stimulation, {ExperimentStep, StimulationName, Depth}, EventType, Args}, Reporter).


send_msg(Msg, Reporter) -> Reporter ! {send, Msg}.


init_reporter(ReportConfig = #{structure_id := StructureId}) ->
    IdStr = structure_id_to_list(StructureId),
    FileName = lists:flatten(["reporter_diag_", IdStr, ".log"]),
    DiagFile = case file:open(FileName, [append]) of
        {ok, F} -> safe_write(F, io_lib:format("~n[~p] reporter started (id=~s)~n", [ts(), IdStr])), F;
        {error, _} -> none
    end,
    process_events(ReportConfig#{diag_file => DiagFile, max_size => 0, msg_count => 0}).


process_events(State = #{mode := Mode, structure_id := StructureId, max_size := Max, msg_count := Cnt}) ->
    receive
        {send, Msg} ->
            %% Size measurement guarded (defensive; term_to_binary should not fail for valid terms)
            Size = try byte_size(term_to_binary(Msg)) catch _:E -> maybe_log(State, io_lib:format("[~p] size_fail ~p~n", [ts(), E])), -1 end,
            NewCnt = Cnt + 1,
            %% Log only when a new maximum size appears or every 10k msgs (heartbeat)
            State1 = case Size > Max orelse (NewCnt rem 10000 =:= 0) of
                true ->
                    MemInfo = case process_info(self(), memory) of {memory, M} -> M; _ -> undefined end,
                    maybe_log(State, io_lib:format("[~p] msg#~p size=~p bytes (max was ~p) proc_mem=~p~n", [ts(), NewCnt, Size, Max, MemInfo])),
                    case Size > Max of true -> maybe_log(State, io_lib:format("    shallow=~s~n", [io_lib:format("~P", [Msg, 3])])); false -> ok end,
                    %% NOTE: Avoid boolean short-circuit idiom to stay compatible with strict boolean semantics in OTP 25
                    %% Original: (Size > Max andalso Size) orelse Max
                    %% Replaced with explicit erlang:max/2 for clarity and to prevent badarg when Size is non-boolean
                    State#{max_size := erlang:max(Size, Max), msg_count := NewCnt};
                false -> State#{msg_count := NewCnt}
            end,
            case Mode of
                pyrlang -> {StructureId, aas_vis@Beast} ! {log, Msg};
                silent -> ok
            end,
            process_events(State1);

        stop ->
            maybe_log(State, io_lib:format("[~p] reporter stopping (max_size=~p, total_msgs=~p)~n", [ts(), Max, Cnt])),
            close_if_open(State),
            stopped
    end.
maybe_log(#{diag_file := none}, _IOData) -> ok;
maybe_log(#{diag_file := F}, IOData) -> safe_write(F, IOData).

close_if_open(#{diag_file := none}) -> ok;
close_if_open(#{diag_file := F}) -> file:close(F).

structure_id_to_list(A) when is_atom(A) -> atom_to_list(A);
structure_id_to_list(B) when is_binary(B) -> binary_to_list(B);
structure_id_to_list(L) when is_list(L) -> L;
structure_id_to_list(Int) when is_integer(Int) -> io_lib:format("~p", [Int]);
structure_id_to_list(Other) -> io_lib:format("~p", [Other]).

safe_write(none, _Data) -> ok;
safe_write(F, Data) ->
    try file:write(F, Data) catch _:Reason -> io:format("reporter log write failed: ~p~n", [Reason]) end.

ts() -> erlang:system_time(millisecond).
