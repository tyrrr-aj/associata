import amqpstorm


def connect():
    return amqpstorm.Connection("localhost" , "TheWatcher", "Beware!")


def disconnect(connection):
    connection.close()

class Channel:
    def __init__(self, connection, channel_name, receiving_routing_key='log'):
        self._connection = connection

        self._sending_channel = self._connection.channel()
        self._receiving_channel = self._connection.channel()
        
        self._sending_routing_key = 'res.vis'
        self._receiving_routing_key = receiving_routing_key
        
        self._exchange_name = f'aas[{channel_name}]'
        self._queue_name = f'vis-{self._exchange_name}-{receiving_routing_key}'

        self._sending_channel.exchange.declare(exchange=self._exchange_name, exchange_type='direct', auto_delete=True)
        self._receiving_channel.exchange.declare(exchange=self._exchange_name, exchange_type='direct', auto_delete=True)
        
        self._receiving_channel.queue.declare(queue=self._queue_name, exclusive=True, auto_delete=True)
        self._receiving_channel.queue.bind(exchange=self._exchange_name, queue=self._queue_name, routing_key=receiving_routing_key)


    def respond(self, message):
        self._sending_channel.basic.publish(exchange=self._exchange_name, routing_key=self._sending_routing_key, body=message)


    def listen(self, callback):
        self._receiving_channel.basic.consume(queue=self._queue_name, no_ack=True, callback=lambda message: callback(message.body))
        self._receiving_channel.start_consuming()


    def close(self):
        try:
            self._receiving_channel.close()
            self._sending_channel.close()
        except amqpstorm.AMQPConnectionError as ex:
            print(f'WARNING: Channel for {self._exchange_name} could not be closed! Error: {ex}')
