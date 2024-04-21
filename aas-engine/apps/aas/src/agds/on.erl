-module(on).
-export([create_ON/3, connect_VN/2, disconnect_VN/2, stimulate/7, get_excitation/1, reset_excitation/1, get_neighbours/1, get_index/1, delete/1]).

-include("config.hrl").

-record(state, {self_index, ong, connected_vns, last_excitation, acc_poison_lvl, tmp_poison_lvl, tmp_poison_multiplier, tmp_poisoning_id, global_cfg}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_ON(ONG, ONIndex, GlobalCfg) -> spawn(fun() -> init(ONG, ONIndex, GlobalCfg) end).


connect_VN(ON, VN) -> ON ! {connect, VN}.


disconnect_VN(ON, VN) -> ON ! {disconnect, VN}.


stimulate(ON, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind, StimuliFromActionVNG) -> 
    ON ! {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind, StimuliFromActionVNG}.


get_excitation(ON) -> 
    ON ! {get_excitation, self()},
    receive
        {excitation, Excitation} -> Excitation;
        {remove_killed_ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            none
    end.


reset_excitation(ON) -> 
    ON ! {reset_excitation, self()},
    receive
        reset_excitation_finished -> ok;
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


delete(ON) -> ON ! delete.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(ONG, ONIndex, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), on, ONIndex, ONG, Reporter),
    process_events(#state{self_index=ONIndex, ong=ONG, connected_vns=[], last_excitation=0.0, acc_poison_lvl=0.0, tmp_poison_lvl=0.0, tmp_poison_multiplier=0.0, tmp_poisoning_id=-1, global_cfg=GlobalCfg}).


process_events(#state{self_index=ONIndex, ong=ONG, connected_vns=ConnectedVNs, last_excitation=LastExcitation, acc_poison_lvl=AccPoisonLvl, tmp_poison_lvl=TmpPoisonLvl, tmp_poison_multiplier=TmpPoisonMultiplier, tmp_poisoning_id=TmpPoisoningId, global_cfg=#global_cfg{reporter=Reporter, timestep_ms=TimestepMs}} = State) -> 
    receive
        {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind, StimuliFromActionVNG} ->
            NewInferenceDepth = CurrInferenceDepth + 1,

            % NEEDS TO BE MOVED - NewExcitation no longer available here
            % case StimulationKind of
            %     infere -> report:node_stimulated(self(), NewExcitation, Reporter);
            %     {poison, DeadlyDoseRep} -> report:node_poisoned(self(), NewExcitation, DeadlyDoseRep, Reporter)
            % end,

            if 
                NewInferenceDepth < MaxInferenceDepth ->
                    timer:sleep(TimestepMs),
                    [vn:stimulate(VN, self(), Stimuli, NewInferenceDepth, MaxInferenceDepth, StimulationKind) || VN <- ConnectedVNs, VN /= Source];
                true -> 
                    % NEEDS TO BE MOVED - NewExcitation no longer available here
                    % case StimulationKind of
                    %     infere -> report:node_stimulated(self(), NewExcitation, Reporter);
                    %     {poison, DeadlyDoseRep} -> report:node_poisoned(self(), NewExcitation, DeadlyDoseRep, Reporter)
                    % end
                    ok
            end,

            case StimulationKind of
                infere -> 
                    NewExcitation = LastExcitation + Stimuli,
                    process_events(State#state{last_excitation=NewExcitation});

                {poison, DeadlyDose, MinimumAccumulatedDose, PoisoningId} -> 
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
                            % report:node_poisoned(self(), AccPoisonLvl, DeadlyDose, Reporter),
                            
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

                    % report:node_poisoned(self(), NewAccPoisonLvl, NewTmpPoisonLvl, TmpPoisonMultiplier, Source, Stimuli, Reporter),

                    if 
                        NewAccPoisonLvl >= DeadlyDose -> 
                            report:node_killed(self(), Reporter),
                            [vn:disconnect_ON(VN, self()) || VN <- ConnectedVNs],
                            ong:remove_killed_ON(ONG, ONIndex);
                            killed;
                        true ->
                            process_events(State#state{acc_poison_lvl=NewAccPoisonLvl, tmp_poison_lvl=NewTmpPoisonLvl, tmp_poison_multiplier=NewTmpPoisonMultiplier, tmp_poisoning_id=NewTmpPoisoningId})
                    end
            end;


        {connect, VN} ->
            process_events(State#state{connected_vns=[VN | ConnectedVNs]});


        {disconnect, VN} ->
            process_events(State#state{connected_vns=lists:delete(VN, ConnectedVNs)});


        {get_excitation, Asker} -> 
            Asker ! {excitation, LastExcitation},
            process_events(State);
        

        {reset_excitation, Asker} -> 
            % report:node_stimulated(self(), 0.0, Reporter),
            Asker ! reset_excitation_finished,
            process_events(State#state{last_excitation=0.0});


        {get_neighbours, Asker} -> 
            NeighbouringVNs = [vn:get_repr_value_and_vng_name(VN) || VN <- ConnectedVNs],
            Response = [{vn, VNGName, ReprValue} || {ReprValue, VNGName} <- NeighbouringVNs],
            Asker ! {neighbours, Response},
            process_events(State);


        {get_index, Asker} ->
            Asker ! {on_index, ONIndex},
            process_events(State);


        delete -> ok
    end.
