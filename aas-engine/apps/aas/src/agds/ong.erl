-module(ong).
-export([create_ONG/2, new_ON/1, stimulate/5, get_excitation/1, reset_excitation/1, get_neighbours/2, delete/1]).
-export([remove_killed_ON/2]).

-include("config.hrl").

-record(state, {ons, next_on_index, agds, global_cfg}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_ONG(AGDS, GlobalCfg) ->     
    spawn(fun() -> init(AGDS, GlobalCfg) end).


new_ON(ONG) -> 
    ONG ! {new_ON, self()},
    receive
        {new_ON, NewON} -> NewON
    end.

%  ONIndex is 0-based
stimulate(ONG, ONIndex, Stimulation, MaxDepth, StimulationKind) -> 
    ONG ! {stimulate, ONIndex, Stimulation, MaxDepth, StimulationKind},
    1.  % ONG alway stimulates a single node; function should never be called on an empty ONG (there is no valid ONIndex then).


remove_killed_ON(ONG, ONIndex) -> 
    % io:format("ONG: remove killed ON (~f)~n", [erlang:monotonic_time() / 1.0]),
    ONG ! {remove_killed_ON, ONIndex}.


get_excitation(ONG) -> 
    % io:format("ONG: get excitation (~f)~n", [erlang:monotonic_time() / 1.0]),
    ONG ! {get_excitation, self()},
    receive 
        {ons_excitation, ONsExcitation} -> ONsExcitation
    end.


reset_excitation(ONG) -> 
    ONG ! {reset_excitation, self()},
    receive
        reset_excitation_finished -> ok
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
    process_events(#state{ons=#{}, next_on_index=0, agds=AGDS, global_cfg=GlobalCfg}).


process_events(#state{ons=ONs, next_on_index=NextONIndex, agds=AGDS, global_cfg=GlobalCfg} = State) ->
    receive
        {new_ON, Sender} ->
            NewON = on:create_ON(self(), NextONIndex, AGDS, GlobalCfg),
            Sender ! {new_ON, NewON},
            process_events(State#state{ons=ONs#{NextONIndex => NewON}, next_on_index=NextONIndex + 1});


        {stimulate, ONIndex, Stimulation, MaxDepth, StimulationKind} ->
            % TODO: check if ONIndex is valid (requires setting up an error channel from backend to client)
            StimulatedON = maps:get(ONIndex, ONs),
            on:stimulate(StimulatedON, self(), Stimulation, 0, MaxDepth, StimulationKind, false),
            process_events(State);


        {remove_killed_ON, ONIndex} ->
            NewONs = maps:remove(ONIndex, ONs),
            process_events(State#state{ons=NewONs});


        {get_excitation, Sender} ->
            ONsResponses = maps:to_list(maps:map(fun(_Index, ON) -> on:get_excitation(ON) end, ONs)),
            ONsExcitation = lists:filter(fun({_ONIndex, Exc}) -> Exc /= none end, ONsResponses),
            Sender ! {ons_excitation, ONsExcitation},
            process_events(State);


        {reset_excitation, Asker} ->
            maps:foreach(fun(_Index, ON) -> on:reset_excitation(ON) end, ONs),
            Asker ! reset_excitation_finished,
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
