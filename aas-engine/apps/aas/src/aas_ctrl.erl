-module(aas_ctrl).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {log_file :: any, structures = #{}}).


start_link() -> gen_server:start_link({local, ctrl}, ?MODULE, [], []).


init([]) -> 
    {ok, LogFile} = file:open("aas_ctrl.log", [write]),
    file:write(LogFile, "Started\n"),
    io:format("Number of cores: ~p~n", [erlang:system_info(schedulers_online)]),

    pyrlang:send_client(ctrl, started),
    
    {ok, #state{log_file = LogFile}}.


handle_call(_Cmd, _From, State) ->
    {reply, ok, State}.
    

handle_cast(_Cmd, State) ->
    {noreply, State}.


handle_info(Info, #state{log_file = LogFile, structures = Structures} = State) ->
    file:write(LogFile, io_lib:format("Received ctrl message: ~p~n", [Info])),

    case Info of
        subscription_init_ok ->
            NewState = State;

        {new_structure, agds, StructureId} -> 
            file:write(LogFile, io_lib:format("Received new_structure message: ~p~n", [agds])),

            AGDS = agds:create(StructureId),
            NewState = State#state{structures = maps:put(StructureId, AGDS, Structures)};

        {set_n_cores, NCores} ->
            erlang:system_flag(schedulers_online, NCores),
            file:write(LogFile, io_lib:format("Number of cores: ~p~n", [erlang:system_info(schedulers_online)])),
            pyrlang:send_client(ctrl, n_cores_set),
            NewState = State;

        stop -> 
            gen_server:stop(?MODULE),
            NewState = State;

        _ -> 
            NewState = State
    end,

    {noreply, NewState}.



terminate(_Reason, #state{log_file = LogFile, structures = Structures}) ->
    maps:foreach(fun (_StructureId, AGDS) -> agds:end_experiment(AGDS) end, Structures),
    file:close(LogFile),
    % init:stop().
    ok.
