-module(benchmark).
-export([setup/0, run/2]).
-compile(export_all).

-define(COOKIE, aas_cookie).
-define(AAS_ENGINE_NAME, aas@Beast).
-define(CTRL_PROC_NAME, ctrl).
-define(STRUCTURE_ID, agds_benchmark).

-define(CTRL, {?CTRL_PROC_NAME, ?AAS_ENGINE_NAME}).
-define(AGDS, {?STRUCTURE_ID, ?AAS_ENGINE_NAME}).

-define(SEED, 42).


setup() ->
    erlang:set_cookie(node(), ?COOKIE),
    pong = net_adm:ping(?AAS_ENGINE_NAME),
    io:format("~p ~p ~p ~p ~p~n", [?AAS_ENGINE_NAME, ?CTRL_PROC_NAME, ?STRUCTURE_ID, ?CTRL, ?AGDS]),
    ok.


run(NCores, BenchmarkSize) ->
    setup_agds().


measure_random_calls(N, Function) ->
    % % Seed the random number generator with a constant seed
    % rand:seed(exsplus, ?SEED),
    
    % % Generate N random values
    % RandomData = [rand:uniform(1000) for _ <- lists:seq(1, N)],
    
    % % Measure the execution time of N calls
    % {Time, _} = timer:tc(fun() ->
    %     lists:foreach(fun(Value) ->
    %         Function(Value)
    %     end, RandomData)
    % end),
    
    % % Convert microseconds to milliseconds
    % ExecutionTimeMs = Time / 1000,
    
    % ExecutionTimeMs.
    tbi.


setup_agds() ->
    ?CTRL ! {new_structure, agds, atom_to_list(?STRUCTURE_ID)},
    ?AGDS ! {add_vng, "Plato", numerical, 0.5},
    ?AGDS ! {add_vng, "IBU", numerical, 5},
    ?AGDS ! {add_vng, "EBC", numerical, 10},
    ?AGDS ! {add_vng, "Style", categorical},
    ok.




add_element() ->
    ?AGDS ! {add_observation, 0, #{"Plato" => 12, "IBU" => 25, "EBC" => 30, "Style" => "Weizen"}}.


infere() ->
    tbi.



call(Msg) ->
    tbi.
