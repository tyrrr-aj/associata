-module(ong).
-export([create_ONG/1, new_ON/1, stimulate/5, get_excitation/1, reset_excitation/1, get_neighbours/2, delete/1]).
-export([remove_killed_ON/2]).

-include("config.hrl").


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_ONG(GlobalCfg) ->     
    spawn(fun() -> init(GlobalCfg) end).


new_ON(ONG) -> 
    ONG ! {new_ON, self()},
    receive
        {new_ON, NewON} -> NewON
    end.

%  ONIndex is 0-based
stimulate(ONG, ONIndex, Stimulation, MaxDepth, StimulationKind) -> ONG ! {stimulate, ONIndex, Stimulation, MaxDepth, StimulationKind}.


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
 
init(#global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_group_creation(self(), "ONG", ong, Reporter),
    process_events(#{}, GlobalCfg, 0).


process_events(ONs, GlobalCfg, NextONIndex) ->
    receive
        {new_ON, Sender} ->
            NewON = on:create_ON(self(), NextONIndex, GlobalCfg),
            Sender ! {new_ON, NewON},
            process_events(ONs#{NextONIndex => NewON}, GlobalCfg, NextONIndex + 1);


        {stimulate, ONIndex, Stimulation, MaxDepth, StimulationKind} ->
            % TODO: check if ONIndex is valid (requires settimg up an error channel from backend to client)
            StimulatedON = maps:get(ONIndex, ONs),
            on:stimulate(StimulatedON, self(), Stimulation, 0, MaxDepth, StimulationKind),
            process_events(ONs, GlobalCfg, NextONIndex);


        {remove_killed_ON, ONIndex} ->
            NewONs = maps:remove(ONIndex, ONs),
            process_events(NewONs, GlobalCfg, NextONIndex);


        {get_excitation, Sender} ->
            ONsResponses = maps:to_list(maps:map(fun(_Index, ON) -> on:get_excitation(ON) end, ONs)),
            ONsExcitation = lists:filter(fun({_ONIndex, Exc}) -> Exc /= none end, ONsResponses),
            Sender ! {ons_excitation, ONsExcitation},
            process_events(ONs, GlobalCfg, NextONIndex);


        {reset_excitation, Asker} ->
            maps:foreach(fun(_Index, ON) -> on:reset_excitation(ON) end, ONs),
            Asker ! reset_excitation_finished,
            process_events(ONs, GlobalCfg, NextONIndex);


        {get_neighbours, ONIndex, Asker} ->
            case maps:get(ONIndex, ONs, none) of
                none -> Asker ! {neighbours, {badkey, ONIndex}};
                ON -> Asker ! {neighbours, on:get_neighbours(ON)}
            end,
            
            process_events(ONs, GlobalCfg, NextONIndex);


        delete -> 
            maps:foreach(fun(_Index, ON) -> on:delete(ON) end, ONs)
    end.
