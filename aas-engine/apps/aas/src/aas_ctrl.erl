-module(aas_ctrl).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {rabbitmq_connection :: any, ctrl_channel :: any, log_file :: any, structures = #{}}).


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) -> 
    {ok, LogFile} = file:open("aas_ctrl.log", [write]),
    file:write(LogFile, "Started\n"),
    
    Connection = rabbitmq:connect(),
    CtrlChannel = rabbitmq:setup_channel(Connection, "ctrl"),
    rabbitmq:respond("started", CtrlChannel),
    
    {ok, #state{rabbitmq_connection = Connection, ctrl_channel = CtrlChannel, log_file = LogFile}}.


handle_call(_Cmd, _From, State) ->
    {reply, ok, State}.
    

handle_cast(_Cmd, State) ->
    {noreply, State}.


handle_info(Info, #state{rabbitmq_connection = Connection, ctrl_channel = CtrlChannel, log_file = LogFile, structures = Structures} = State) ->
    Message = rabbitmq:decode_and_ack_message(Info, CtrlChannel),
    
    file:write(LogFile, io_lib:format("Received ctrl message: ~p~n", [Message])),

    case Message of
        subscription_init_ok ->
            NewState = State;

        {new_structure, agds, StructureId} -> 
            file:write(LogFile, io_lib:format("Received new_structure message: ~p~n", [agds])),

            AGDS = agds:create(StructureId, Connection),
            NewState = State#state{structures = maps:put(StructureId, AGDS, Structures)};


        stop -> 
            gen_server:stop(?MODULE),
            NewState = State;


        _ -> 
            NewState = State
    end,

    {noreply, NewState}.



terminate(_Reason, #state{rabbitmq_connection = Connection, ctrl_channel = CtrlChannel, log_file = LogFile, structures = Structures}) ->
    maps:foreach(fun (_StructureId, AGDS) -> agds:end_experiment(AGDS) end, Structures),

    rabbitmq:respond("stopped", CtrlChannel),
    rabbitmq:teardown_channel(CtrlChannel),
    rabbitmq:disconnect(Connection),
    
    file:close(LogFile),
    ok.
