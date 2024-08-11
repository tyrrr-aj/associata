-module(on).
-export([
    create_ON/4, 
    connect_VN/2, 
    disconnect_VN/2, 
    stimulate/7, 
    get_excitation/2, 
    reset_excitation/1, 
    wait_for_reset_excitation/1, 
    get_neighbours/1, 
    get_index/1, 
    delete/1
    , report_stimulations_count/2
]).

-include("config.hrl").

-record(state, {
    self_index, 
    ong, 
    connected_vns, 
    last_excitation, 
    curr_stimulation_id, 
    stimulated_neighs,
    acc_poison_lvl, 
    tmp_poison_lvl, 
    tmp_poison_multiplier, 
    tmp_poisoning_id, 
    agds, 
    global_cfg,
    dbg_stimulations_count = 0
}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_ON(ONG, ONIndex, AGDS, GlobalCfg) -> spawn(fun() -> init(ONG, ONIndex, AGDS, GlobalCfg) end).


connect_VN(ON, VN) -> ON ! {connect, VN}.


disconnect_VN(ON, VN) -> ON ! {disconnect, VN}.


stimulate(ON, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind, StimuliFromActionVNG) -> 
    ON ! {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind, StimuliFromActionVNG}.


get_excitation(ON, LastStimulationId) -> 
    ON ! {get_excitation, self(), LastStimulationId},
    receive
        {excitation, Excitation} -> Excitation;
        {remove_killed_ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            none
    end.


reset_excitation(ON) -> 
    ON ! {reset_excitation, self()}.

wait_for_reset_excitation(ON) ->
    receive
        {reset_excitation_finished, ON} -> ok;
        {remove_killed_ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            ok
    end.


get_neighbours(ON) -> ON ! {get_neighbours, self()},
    receive
        {neighbours, Neighbours} -> Neighbours;
        {remove_killed_ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            []
    end.


get_index(ON) -> ON ! {get_index, self()},
    receive
        {on_index, Index} -> Index;
        {remove_killed_ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            ONIndex
    end.


report_stimulations_count(ON, ONIndex) -> 
    ON ! {report_stimulations_count, self()},
    receive
        {stimulations_count_reported, ON} -> ok;
        {remove_killed_ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            ok
    end.


delete(ON) -> ON ! delete.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(ONG, ONIndex, AGDS, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), on, ONIndex, ONG, Reporter),
    process_events(#state{
        self_index=ONIndex, 
        ong=ONG, 
        connected_vns=[], 
        last_excitation=0.0, 
        curr_stimulation_id=none, 
        stimulated_neighs=#{},
        acc_poison_lvl=0.0, 
        tmp_poison_lvl=0.0, 
        tmp_poison_multiplier=0.0, 
        tmp_poisoning_id=-1, 
        agds=AGDS, 
        global_cfg=GlobalCfg
}).


process_events(#state{
    self_index=ONIndex, 
    ong=ONG, 
    connected_vns=ConnectedVNs, 
    last_excitation=LastExcitation, 
    curr_stimulation_id=CurrStimulationId, 
    stimulated_neighs=StimulatedNeighs, 
    acc_poison_lvl=AccPoisonLvl, 
    tmp_poison_lvl=TmpPoisonLvl, 
    tmp_poison_multiplier=TmpPoisonMultiplier, 
    tmp_poisoning_id=TmpPoisoningId, 
    agds=AGDS, 
    global_cfg=#global_cfg{reporter=Reporter, timestep_ms=TimestepMs}=GlobalCfg
    , dbg_stimulations_count=DbgStimulationsCount
} = State) -> 

    receive
        {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind, StimuliFromActionVNG} ->
            NewInferenceDepth = CurrInferenceDepth + 1,

            StimulatedVNs = if 
                NewInferenceDepth < MaxInferenceDepth -> [VN || VN <- ConnectedVNs, VN /= Source];
                true -> []
            end,

            NewStimulatedNeighs = case StimulatedVNs of
                [] -> 
                    Source ! {stimulation_finished, self(), CurrInferenceDepth}, % TODO: hide message in function, in common "agds_node" module
                    StimulatedNeighs;
                _ -> 
                    case StimulatedNeighs of
                        #{NewInferenceDepth := {AlreadyStimulatedNeighs, SourcesAtInfDepth}} -> 
                            #{NewInferenceDepth => {AlreadyStimulatedNeighs ++ StimulatedVNs, SourcesAtInfDepth ++ [Source]}};
                        _ -> 
                            StimulatedNeighs#{NewInferenceDepth => {StimulatedVNs, [Source]}}
                    end
            end,

            % agds:notify_node_stimulated(AGDS, length(StimulatedVNs)),
            lists:foreach(fun(VN) -> vn:stimulate(VN, self(), Stimuli, NewInferenceDepth, MaxInferenceDepth, StimulationKind) end, StimulatedVNs),

            case StimulationKind of
                {infere, StimulationId} ->
                    dbg_counter:add_stimulations(on, 1, StimulationId, GlobalCfg#global_cfg.dbg_counter),
                    case StimulationId of
                        CurrStimulationId ->
                            NewExcitation = LastExcitation + Stimuli,
                            NewStimulationId = CurrStimulationId;
                        UnknownStimulationId ->
                            NewExcitation = Stimuli,
                            NewStimulationId = UnknownStimulationId
                    end,
                    process_events(State#state{last_excitation=NewExcitation, curr_stimulation_id=NewStimulationId, stimulated_neighs=NewStimulatedNeighs, dbg_stimulations_count = DbgStimulationsCount + 1});

                {poison, DeadlyDose, MinimumAccumulatedDose, PoisoningId} -> 
                    dbg_counter:add_stimulations(on, 1, PoisoningId, GlobalCfg#global_cfg.dbg_counter),
                    case PoisoningId of 
                        TmpPoisoningId -> 
                            NewTmpPoisoningId = TmpPoisoningId,
                            case StimuliFromActionVNG of 
                                true ->
                                    if
                                        (TmpPoisonLvl * TmpPoisonMultiplier) >= MinimumAccumulatedDose ->   % minimum acc dose has already been exceeded
                                            NewAccPoisonLvl = AccPoisonLvl + (TmpPoisonLvl * Stimuli);
                                        (TmpPoisonLvl * (TmpPoisonMultiplier + Stimuli)) >= MinimumAccumulatedDose ->   % minimum acc dose is exceeded in current stimulation
                                            NewAccPoisonLvl = AccPoisonLvl + (TmpPoisonLvl * (TmpPoisonMultiplier + Stimuli));
                                        true ->     % minimum acc dose has not been reached
                                            NewAccPoisonLvl = AccPoisonLvl
                                    end,
                                    NewTmpPoisonLvl = TmpPoisonLvl,
                                    NewTmpPoisonMultiplier = TmpPoisonMultiplier + Stimuli;
                                false ->
                                    if
                                        (TmpPoisonLvl * TmpPoisonMultiplier) >= MinimumAccumulatedDose ->   % minimum acc dose has already been exceeded
                                            NewAccPoisonLvl = AccPoisonLvl + TmpPoisonMultiplier * Stimuli;
                                        (TmpPoisonLvl * TmpPoisonMultiplier) >= MinimumAccumulatedDose ->   % minimum acc dose is exceeded in current stimulation
                                            NewAccPoisonLvl = AccPoisonLvl + (TmpPoisonLvl * (TmpPoisonMultiplier + Stimuli));
                                        true ->     % minimum acc dose has not been reached
                                            NewAccPoisonLvl = AccPoisonLvl
                                    end,
                                    NewTmpPoisonLvl = TmpPoisonLvl + Stimuli,
                                    NewTmpPoisonMultiplier = TmpPoisonMultiplier
                            end;

                        NewPoisoningId ->
                            
                            NewTmpPoisoningId = NewPoisoningId,
                            NewAccPoisonLvl = AccPoisonLvl,

                            case StimuliFromActionVNG of
                                true ->
                                    NewTmpPoisonLvl = 0.0,
                                    NewTmpPoisonMultiplier = Stimuli;
                                false ->
                                    NewTmpPoisonLvl = Stimuli,
                                    NewTmpPoisonMultiplier = 0.0
                            end
                    end,

                    report:node_poisoned(self(), NewAccPoisonLvl, NewTmpPoisonLvl, NewTmpPoisonMultiplier, Source, Stimuli, Reporter),

                    if 
                        NewAccPoisonLvl >= DeadlyDose -> 
                            report:node_killed(self(), Reporter),
                            [vn:disconnect_ON(VN, self()) || VN <- ConnectedVNs],
                            ong:remove_killed_ON(ONG, ONIndex),
                            % dbg_counter:add_stimulations(on, DbgStimulationsCount + 1, GlobalCfg#global_cfg.dbg_counter),
                            zombie_wait_for_orphan_stimulations(StimulatedNeighs);
                        true ->
                            process_events(State#state{
                                acc_poison_lvl=NewAccPoisonLvl, 
                                tmp_poison_lvl=NewTmpPoisonLvl, 
                                tmp_poison_multiplier=NewTmpPoisonMultiplier, 
                                tmp_poisoning_id=NewTmpPoisoningId, 
                                stimulated_neighs=NewStimulatedNeighs,
                                dbg_stimulations_count=DbgStimulationsCount + 1
                            })
                    end
            end;

        
        {stimulation_finished, StimulatedNode, InferenceDepth} ->
            % io:format("ON(~p) received stimulation finished from ~p at depth ~p, current StimulatedNeighs: ~p ~n", [ONIndex, StimulatedNode, InferenceDepth, StimulatedNeighs]),
            NewStimulatedNeighs = case StimulatedNeighs of
                #{InferenceDepth := {[StimulatedNode], SourcesAtInfDepth}} -> 
                    lists:foreach(fun(Source) -> Source ! {stimulation_finished, self(), InferenceDepth - 1} end, SourcesAtInfDepth),
                    maps:remove(InferenceDepth, StimulatedNeighs);
                #{InferenceDepth := {StimulatedNeighsAtInfDepth, SourcesAtInfDepth}} -> 
                    StimulatedNeighs#{InferenceDepth => {lists:delete(StimulatedNode, StimulatedNeighsAtInfDepth), SourcesAtInfDepth}}
            end,
            process_events(State#state{stimulated_neighs=NewStimulatedNeighs});


        {connect, VN} ->
            process_events(State#state{connected_vns=[VN | ConnectedVNs]});


        {disconnect, VN} ->
            process_events(State#state{connected_vns=lists:delete(VN, ConnectedVNs)});


        {get_excitation, Asker, LastStimulationId} -> 
            Excitation = case LastStimulationId of 
                CurrStimulationId -> LastExcitation;
                _ -> 0.0
            end,
            Asker ! {excitation, Excitation},
            process_events(State);
        

        {reset_excitation, Asker} -> 
            % report:node_stimulated(self(), 0.0, Reporter),
            Asker ! {reset_excitation_finished, self()},
            process_events(State#state{last_excitation=0.0});


        {get_neighbours, Asker} -> 
            NeighbouringVNs = [vn:get_repr_value_and_vng_name(VN) || VN <- ConnectedVNs],
            Response = [{vn, VNGName, ReprValue} || {ReprValue, VNGName} <- NeighbouringVNs],
            Asker ! {neighbours, Response},
            process_events(State);


        {get_index, Asker} ->
            Asker ! {on_index, ONIndex},
            process_events(State);


        % {report_stimulations_count, Asker} ->
        %     dbg_counter:add_stimulations(on, DbgStimulationsCount, GlobalCfg#global_cfg.dbg_counter),
        %     Asker ! {stimulations_count_reported, self()},
        %     process_events(State);


        delete -> ok
    end.


% zombie_wait_for_orphan_stimulations(#{}) -> killed;

zombie_wait_for_orphan_stimulations(StimulatedNeighs) ->
    receive
    {stimulate, Source, _Stimuli, CurrInferenceDepth, _MaxInferenceDepth, _StimulationKind, _StimuliFromActiveVNG} ->
        % agds:notify_node_stimulated(AGDS, 0),
        Source ! {stimulation_finished, self(), CurrInferenceDepth},
        zombie_wait_for_orphan_stimulations(StimulatedNeighs);
    {stimulation_finished, StimulatedNode, InferenceDepth} -> 
        NewStimulatedNeighs = case StimulatedNeighs of
            #{InferenceDepth := {[StimulatedNode], SourcesAtInfDepth}} -> 
                lists:foreach(fun(Source) -> Source ! {stimulation_finished, self(), InferenceDepth - 1} end, SourcesAtInfDepth),
                maps:remove(InferenceDepth, StimulatedNeighs);
            #{InferenceDepth := {StimulatedNeighsAtInfDepth, SourcesAtInfDepth}} -> 
                StimulatedNeighs#{InferenceDepth => {lists:delete(StimulatedNode, StimulatedNeighsAtInfDepth), SourcesAtInfDepth}}
        end,
        zombie_wait_for_orphan_stimulations(NewStimulatedNeighs)
    after 5000 -> killed
    end.
