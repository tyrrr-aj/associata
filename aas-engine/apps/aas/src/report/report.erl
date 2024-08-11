-module(report).
-export([start/1, 
        % structure_creation/2, 
        node_group_creation/4, 
        node_creation/5, 
        connection_formed/3, 
        connection_broken/3,
        node_stimulated/3, 
        node_poisoned/7,
        node_killed/2,
        stimuli_propagated/4,
        experiment_end/1,
        stop/1]).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

start(ReportConfig) -> spawn(fun() -> init(ReportConfig) end).

%%% report creation of AAS structure, e.g. AGDS
% structure_creation(StructureKind, Reporter) -> report_timestamped(structure_creation, {StructureKind}, Reporter).

%%% report creation of group of nodes, e.g. VNG, ONG
node_group_creation(Id, NodeGroupName, NodeGroupKind, Reporter) -> report_timestamped(node_group_creation, {Id, NodeGroupName, NodeGroupKind}, Reporter).

%%% report creation of a single node
%%% NodeKind = <vn | on | ...>, NodeParameters - depend on NodeKind
node_creation(Id, NodeKind, NodeParameters, NodeGroupId, Reporter) -> report_timestamped(node_creation, {Id, NodeKind, NodeParameters, NodeGroupId}, Reporter).


connection_formed(SourceNodeId, DestNodeId, Reporter) -> report_timestamped(connection_formed, {SourceNodeId, DestNodeId}, Reporter).


connection_broken(SourceNodeId, DestNodeId, Reporter) -> report_timestamped(connection_broken, {SourceNodeId, DestNodeId}, Reporter).


node_stimulated(NodeId, NewExcitation, Reporter) -> report_timestamped(node_stimulated, {NodeId, NewExcitation}, Reporter).


node_poisoned(NodeId, NewAccPoisonLvl, NewTmpPoisonLvl, NewTmpPoisonMultiplier, Source, Stimuli, Reporter) -> 
    report_timestamped(node_poisoned, {NodeId, NewAccPoisonLvl, NewTmpPoisonLvl, NewTmpPoisonMultiplier, Source, Stimuli}, Reporter).

node_killed(NodeId, Reporter) -> report_timestamped(node_killed, {NodeId}, Reporter).


%%% displays propagation of stimuli through given connection
%%% calling this function does NOT indicate stimulation of destination node (node_stimulated/3 should be called separately)
stimuli_propagated(SourceNodeId, DestNodeId, Stimuli, Reporter) -> report_timestamped(stimuli_propagated, {SourceNodeId, DestNodeId, Stimuli}, Reporter).


experiment_end(Reporter) -> report_timestamped(experiment_end, {none}, Reporter).


stop(Reporter) -> Reporter ! stop.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

report_timestamped(EventType, Args, Reporter) ->
    Reporter ! {EventType, {erlang:monotonic_time(millisecond), erlang:unique_integer([monotonic])}, Args}.


init(ReportConfig) ->
    process_events(ReportConfig).


process_events(#{mode := Mode, structure_id := StructureId} = ReportConfig) ->
    receive
        {EventType, Timestamp, Params} -> 
            case Mode of
                pyrlang -> {StructureId, aas_vis@Beast} ! {Timestamp, EventType, Params}
            end,
            process_events(ReportConfig);

        stop -> ok
    end.


