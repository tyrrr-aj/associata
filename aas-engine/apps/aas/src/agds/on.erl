-module(on).
-export([create_ON/3, connect_VN/2, disconnect_VN/2, stimulate/6, get_excitation/1, reset_excitation/1, get_neighbours/1, get_index/1, delete/1]).

-include("config.hrl").

-record(state, {self_index, ong, connected_vns, last_excitation, global_cfg}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_ON(ONG, ONIndex, GlobalCfg) -> spawn(fun() -> init(ONG, ONIndex, GlobalCfg) end).


connect_VN(ON, VN) -> ON ! {connect, VN}.


disconnect_VN(ON, VN) -> ON ! {disconnect, VN}.


stimulate(ON, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind) -> 
    ON ! {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind}.


get_excitation(ON) -> 
    ON ! {get_excitation, self()},
    receive
        {excitation, Excitation} -> Excitation
    end.


reset_excitation(ON) -> ON ! reset_excitation.


get_neighbours(ON) -> ON ! {get_neighbours, self()},
    receive
        {neighbours, Neighbours} -> Neighbours
    end.


get_index(ON) -> ON ! {get_index, self()},
    receive
        {on_index, Index} -> Index
    end.


delete(ON) -> ON ! delete.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(ONG, ONIndex, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), on, ONIndex, ONG, Reporter),
    process_events(#state{self_index=ONIndex, ong=ONG, connected_vns=[], last_excitation=0.0, global_cfg=GlobalCfg}).


process_events(#state{self_index=ONIndex, ong=ONG, connected_vns=ConnectedVNs, last_excitation=LastExcitation, global_cfg=#global_cfg{reporter=Reporter, timestep_ms=TimestepMs}} = State) -> 
    receive
        {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind} ->
            NewInferenceDepth = CurrInferenceDepth + 1,
            NewExcitation = LastExcitation + Stimuli,

            case StimulationKind of
                infere -> report:node_stimulated(self(), NewExcitation, Reporter);
                {poison, DeadlyDoseRep} -> report:node_poisoned(self(), NewExcitation, DeadlyDoseRep, Reporter)
            end,

            if 
                NewInferenceDepth < MaxInferenceDepth ->
                    timer:sleep(TimestepMs),
                    [vn:stimulate(VN, self(), Stimuli, NewInferenceDepth, MaxInferenceDepth, StimulationKind) || VN <- ConnectedVNs, VN /= Source];
                true -> ok
            end,

            case StimulationKind of
                infere -> process_events(State#state{last_excitation=NewExcitation});
                {poison, DeadlyDose} -> 
                    if 
                        NewExcitation >= DeadlyDose -> 
                            report:node_killed(self(), Reporter),
                            [vn:disconnect_ON(VN, self()) || VN <- ConnectedVNs],
                            ong:remove_killed_ON(ONG, ONIndex);
                            killed;
                        true -> 
                            process_events(State#state{last_excitation=NewExcitation})
                    end
            end;


        {connect, VN} ->
            process_events(State#state{connected_vns=[VN | ConnectedVNs]});


        {disconnect, VN} ->
            process_events(State#state{connected_vns=lists:delete(VN, ConnectedVNs)});


        {get_excitation, Asker} -> 
            Asker ! {excitation, LastExcitation},
            process_events(State);
        

        reset_excitation -> process_events(State#state{last_excitation=0.0});


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
