-module(ong).
-export([create_ONG/2, new_ON/1, stimulate/3, get_excitation/2, get_neighbours/2, delete/1]).
-export([remove_killed_ON/2]).

-include("config.hrl").
-include("stimulation.hrl").

-record(state, {ons, next_on_index, stimulated_ons, agds, global_cfg}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_ONG(AGDS, GlobalCfg) ->     
    spawn(fun() -> init(AGDS, GlobalCfg) end).


new_ON(ONG) -> 
    ONG ! {new_ON, self()},
    receive
        {new_ON, NewON, NewONIndex} -> {NewON, NewONIndex}
    end.

%  ONIndex is 0-based
stimulate(ONG, Stimuli, StimulationSpec) -> 
    ONG ! {stimulate, Stimuli, StimulationSpec}.


remove_killed_ON(ONG, ONIndex) -> 
    ONG ! {remove_killed_ON, ONIndex}.


get_excitation(ONG, LastStimulationId) -> 
    ONG ! {get_excitation, self(), LastStimulationId},
    receive 
        {ons_excitation, ONsExcitation} -> ONsExcitation
    end.


get_neighbours(ONG, ONIndex) ->
    ONG ! {get_neighbours, ONIndex, self()},
    receive
        {neighbours, Neighbours} -> Neighbours
    end.


delete(ONG) -> ONG ! delete.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%
 
init(AGDS, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_group_creation(self(), "ONG", ong, Reporter),
    process_events(#state{ons=#{}, next_on_index=0, stimulated_ons=[], agds=AGDS, global_cfg=GlobalCfg}).


process_events(#state{ons=ONs, next_on_index=NextONIndex, stimulated_ons=StimulatedONs, agds=AGDS, global_cfg=GlobalCfg} = State) ->
    receive
        {new_ON, Sender} ->
            NewON = on:create_ON(self(), NextONIndex, GlobalCfg),
            Sender ! {new_ON, NewON, NextONIndex},
            process_events(State#state{ons=ONs#{NextONIndex => NewON}, next_on_index=NextONIndex + 1});


        {stimulate, Stimuli, #stim_spec{node_group_modes=NodeGroupModes}=StimulationSpec} ->
            case maps:get(ong, NodeGroupModes) of
                passive -> 
                    stimulation:send_stimulation_finished(AGDS, 0),
                    process_events(State);

                _ -> 
                    maps:foreach(fun(ONIndex, Stimulus) -> on:stimulate(maps:get(ONIndex, ONs), Stimulus, 0, StimulationSpec) end, Stimuli),
                    NewStimulatedONs = maps:keys(Stimuli),

                    case NewStimulatedONs of
                        [] -> stimulation:send_stimulation_finished(AGDS, 0);
                        _ -> ok
                    end,

                    process_events(State#state{stimulated_ons=NewStimulatedONs})
            end;

        
        {stimulation_finished, StimulatedON, 0, 1} ->
            % io:format("~s    ONG: sitmulation_finished received from ~p, stimulated ons: ~p~n", [utils:get_timestamp_str(), StimulatedON, StimulatedONs]),
            NewStimulatedONs = lists:delete(StimulatedON, StimulatedONs),
            case NewStimulatedONs of
                [] -> stimulation:send_stimulation_finished(AGDS, 0);
                _ -> ok
            end,
            process_events(State#state{stimulated_ons=NewStimulatedONs});


        {remove_killed_ON, ONIndex} ->
            NewONs = maps:remove(ONIndex, ONs),
            process_events(State#state{ons=NewONs});


        {get_excitation, Sender, LastStimulationId} ->
            ONsResponses = maps:map(fun(_Index, ON) -> on:get_excitation(ON, LastStimulationId) end, ONs),
            ONsExcitation = maps:filter(fun(_ONIndex, Exc) -> Exc /= none end, ONsResponses),
            Sender ! {ons_excitation, ONsExcitation},
            process_events(State);


        {get_neighbours, ONIndex, Asker} ->
            case maps:get(ONIndex, ONs, none) of
                none -> Asker ! {neighbours, {badkey, ONIndex}};
                ON -> Asker ! {neighbours, on:get_neighbours(ON)}
            end,
            
            process_events(State);


        delete -> 
            maps:foreach(fun(_Index, ON) -> on:delete(ON) end, ONs)
    end.
