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
    spawn(fun() -> process_events(ReportConfig) end).


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


node_poisoned(NodeId, NewAccPoisonLvl, ExperimentStep, Reporter) -> ok. % report(stimulation, ExperimentStep, na, na, node_poisoned, {NodeId, NewAccPoisonLvl}, Reporter).


stop(Reporter) -> Reporter ! stop.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

report(topology, ExperimentStep, EventType, Args, Reporter) ->
    send_msg({topology, ExperimentStep, EventType, Args}, Reporter).

report(stimulation, ExperimentStep, StimulationName, Depth, EventType, Args, Reporter) ->
    send_msg({stimulation, {ExperimentStep, StimulationName, Depth}, EventType, Args}, Reporter).


send_msg(Msg, Reporter) -> profiling. %Reporter ! {send, Msg}.


process_events(#{mode := Mode, structure_id := StructureId} = ReportConfig) ->
    receive
        {send, Msg} ->
            case Mode of
                pyrlang -> {StructureId, aas_vis@Beast} ! {log, Msg};
                silent -> ok
            end,
            process_events(ReportConfig);

        stop -> stopped
    end.
