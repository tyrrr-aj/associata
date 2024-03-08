import pika


def connect():
    return pika.BlockingConnection(pika.ConnectionParameters('localhost'))


def disconnect(connection):
    connection.close()


class Channel:
    def __init__(self, connection, channel_name):
        if connection is None:
            raise ValueError("Connection is not established.")

        self._connection = connection
        
        self._exchange_name = f'aas[{channel_name}]'

        self._sending_vis_routing_key = 'cmd.vis'
        self._sending_backend_riouting_key = 'cmd.backend'

        self._receiving_vis_routing_key = 'res.vis'
        self._receiving_vis_queue_name = f'master-{self._exchange_name}-{self._receiving_vis_routing_key}'

        self._receiving_backend_routing_key = 'res.backend'
        self._receiving_backend_queue_name = f'master-{self._exchange_name}-{self._receiving_backend_routing_key}'


        self._sending_channel = self._connection.channel()
        self._sending_channel.exchange_declare(exchange=self._exchange_name, exchange_type='direct', auto_delete=True)

        self._receiving_vis_channel = self._connection.channel()
        self._receiving_vis_channel.queue_declare(queue=self._receiving_vis_queue_name, exclusive=True, auto_delete=True)
        self._receiving_vis_channel.queue_bind(queue=self._receiving_vis_queue_name, exchange=self._exchange_name, routing_key=self._receiving_vis_routing_key)

        self._receiving_backend_channel = self._connection.channel()
        self._receiving_backend_channel.queue_declare(queue=self._receiving_backend_queue_name, exclusive=True, auto_delete=True)
        self._receiving_backend_channel.queue_bind(queue=self._receiving_backend_queue_name, exchange=self._exchange_name, routing_key=self._receiving_backend_routing_key)



    def send_vis(self, message):
        self._send(message, self._sending_vis_routing_key)


    def send_backend(self, message):
        self._send(message, self._sending_backend_riouting_key)


    def _send(self, message, routing_key):
        self._sending_channel.basic_publish(exchange=self._exchange_name, routing_key=routing_key, body=message)



    def receive_vis(self, timeout, on_message_callback, on_timeout_callback=lambda: None, should_ack_message=lambda _: True):
        self._receive(self._receiving_vis_channel, self._receiving_vis_queue_name, timeout, on_message_callback, on_timeout_callback, should_ack_message)


    def receive_backend(self, timeout, on_message_callback, on_timeout_callback=lambda: None, should_ack_message=lambda _: True):
        self._receive(self._receiving_backend_channel, self._receiving_backend_queue_name, timeout, on_message_callback, on_timeout_callback, should_ack_message)


    def _receive(self, channel, queue_name, timeout, on_message_callback, on_timeout_callback=lambda: None, should_ack_message=lambda _: True):
        for method, properties, body in channel.consume(queue=queue_name, inactivity_timeout=timeout):
            if method is not None or properties is not None or body is not None:
                if should_ack_message(body):
                    channel.basic_ack(method.delivery_tag)
                if on_message_callback(body):
                    break
            else:
                on_timeout_callback()
                break



    def close(self):
        self._sending_channel.close()
        self._receiving_vis_channel.close()
        self._receiving_backend_channel.close()
