-module(rabbitmq).
-export([connect/0, disconnect/1, setup_channel/2, teardown_channel/1, respond/2, log/4, decode_and_ack_message/2]).

-include_lib("amqp_client/include/amqp_client.hrl").


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%
 

connect() ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    Connection.


disconnect(Connection) -> 
    amqp_connection:close(Connection).


setup_channel(Connection, ChannelName) -> 
    AmqpChannel = open_amqp_channel(Connection),
    Topology = setup_topology(AmqpChannel, ChannelName),
    Channel = #{amqp_channel => AmqpChannel, topology => Topology},
    listen(Channel),
    Channel.


teardown_channel(#{amqp_channel := AmqpChannel} = Channel) ->
    destroy_topology(Channel),
    close_amqp_channel(AmqpChannel),
    ok.


respond(ResponseBody, #{amqp_channel := AmqpChannel, topology := #{exchange := Exchange, res_routing_key := ResRoutingKey}}) ->
    send(io_lib:format("~0p", [ResponseBody]), AmqpChannel, Exchange, ResRoutingKey),
    ok.


log(Timestamp, EventType, EventInfo, #{amqp_channel := Channel, topology := #{exchange := Exchange, log_routing_key := RoutingKey}}) -> 
    send(io_lib:format("~0p#~0p#~0p", [Timestamp, EventType, EventInfo]), Channel, Exchange, RoutingKey),
    ok.


decode_and_ack_message(#'basic.consume_ok'{}, _Channel) -> subscription_init_ok;

decode_and_ack_message(#'basic.cancel_ok'{}, _Channel) -> stop;

decode_and_ack_message(Message, #{amqp_channel := AmqpChannel} = _Channel) ->
    {Tag, Content} = decode_message(Message),
    ack_message(AmqpChannel, Tag),
    Content.



%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

open_amqp_channel(Connection) ->
    {ok, AmqpChannel} = amqp_connection:open_channel(Connection),
    AmqpChannel.


close_amqp_channel(AmqpChannel) ->
    amqp_channel:close(AmqpChannel).


setup_topology(AmqpChannel, ChannelName) ->
    ExchangeName = list_to_binary([<<"aas[">>, ChannelName, <<"]">>]),
    CmdQueueName = list_to_binary([<<"backend-">>, ExchangeName, <<"-cmd">>]),
    CmdRoutingKey = <<"cmd.backend">>,
    ResRoutingKey = <<"res.backend">>,
    LogRoutingKey = <<"log">>,

    ExchangeDeclare = #'exchange.declare'{exchange = ExchangeName, auto_delete = true},
    #'exchange.declare_ok'{} = amqp_channel:call(AmqpChannel, ExchangeDeclare),

    QueueDeclare = #'queue.declare'{queue = CmdQueueName, exclusive = true, auto_delete = true},
    #'queue.declare_ok'{} = amqp_channel:call(AmqpChannel, QueueDeclare),

    Binding = #'queue.bind'{queue = CmdQueueName, exchange = ExchangeName, routing_key = CmdRoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(AmqpChannel, Binding),

    #{
        exchange => ExchangeName, 
        cmd_queue => CmdQueueName, 
        cmd_routing_key => CmdRoutingKey,
        res_routing_key => ResRoutingKey,
        log_routing_key => LogRoutingKey
    }.


listen(#{amqp_channel := AmqpChannel, topology := #{cmd_queue := CmdQueue}}) ->
    #'basic.consume_ok'{consumer_tag = _Tag} = amqp_channel:call(AmqpChannel, #'basic.consume'{queue = CmdQueue}).      % contrary to Pika (Python), in Erlang RabbitMQ client library messeges are auto-acked by default


destroy_topology(#{
        amqp_channel := Channel, 
        topology := #{
            exchange := CtrlExchangeName, 
            cmd_queue := CtrlCmdQueueName, 
            cmd_routing_key := CmdRoutingKey
        }
    }) ->

    Unbind = #'queue.unbind'{queue = CtrlCmdQueueName, exchange = CtrlExchangeName, routing_key = CmdRoutingKey},
    #'queue.unbind_ok'{} = amqp_channel:call(Channel, Unbind),

    QueueDelete = #'queue.delete'{queue = CtrlCmdQueueName},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, QueueDelete),

    % ExchangeDelete = #'exchange.delete'{exchange = CtrlExchangeName},
    % #'exchange.delete_ok'{} = amqp_channel:call(Channel, ExchangeDelete),

    ok.


send(Message, Channel, ExchangeName, RoutingKey) ->
    Payload = list_to_binary(Message),
    Publish = #'basic.publish'{exchange = ExchangeName, routing_key = RoutingKey},
    Props = #'P_basic'{delivery_mode = 2},      %% 2 = persistent message
    AmqpMsg = #amqp_msg{props = Props, payload = Payload},
    amqp_channel:cast(Channel, Publish, AmqpMsg).


ack_message(AmqpChannel, DeliveryTag) ->
    amqp_channel:cast(AmqpChannel, #'basic.ack'{delivery_tag = DeliveryTag}).


decode_message({#'basic.deliver'{delivery_tag = Tag}, Content}) ->
    #amqp_msg{payload = Payload} = Content,
    StrContent = binary_to_list(Payload),
    {ok,Tokens,_EndLine} = erl_scan:string(StrContent),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
    {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    {Tag, Value}.
