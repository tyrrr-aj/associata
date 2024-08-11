from pyrlang import Node
from pyrlang.process import Process
from term import Atom
from collections import deque
import asyncio


_node_name = None


def connect(own_node_name, cookie):
    global _node_name

    _node_name = own_node_name

    return Node(node_name=own_node_name, cookie=cookie)


def disconnect(connection):
    connection.destroy()


class Channel():
    def __init__(self, connection, channel_name, remote_receiver_node_name, callback=None) -> None:
    # def __init__(self, connection, channel_name, remote_receiver_node_name, callback) -> None:
        self._node = connection
        self._channel_name = channel_name
        self._remote_node_name = remote_receiver_node_name

        self._res_buffer = deque()
        self._res_semaphore = asyncio.Semaphore(0)

        if callback is None:
            self._receiver = Receiver(channel_name, 
                                          self._res_buffer, 
                                          self._res_semaphore)
        else:
            self._receiver = ReceiverWithCallback(channel_name, callback)
        
        self._sender_pid = self._node.register_new_process()


    # def send(self, message):
    #     self._node.send_nowait(sender=self._sender_pid,
    #                 receiver=(Atom(self._remote_node_name), Atom(self._channel_name)),
    #                 message=message)
        
    
    # def send_self(self, message):
    #     self._node.send_nowait(sender=self._sender_pid,
    #                 receiver=self._receiver.pid_,
    #                 message=message)


    async def send(self, message):
        await self._node.send(sender=self._sender_pid,
                    receiver=(Atom(self._remote_node_name), Atom(self._channel_name)),
                    message=message)
        
    
    async def send_self(self, message):
        await self._node.send(sender=self._sender_pid,
                    receiver=self._receiver.pid_,
                    message=message)
        

    async def receive_async(self):
        await self._res_semaphore.acquire()
        return self._res_buffer.popleft()


    def close(self):
        self._receiver.exit(Atom('normal'))


class Receiver(Process):
    def __init__(self, name, buffer, semaphore):
        super().__init__()
        self.get_node().register_name(self, Atom(name))
        self._buffer = buffer
        self._semaphore = semaphore
    
    def handle_one_inbox_message(self, msg):
        print(f'Msg received: {msg}')
        self._buffer.append(msg)
        self._semaphore.release()


class ReceiverWithCallback(Process):
    def __init__(self, name, callback):
        super().__init__()
        self.get_node().register_name(self, Atom(name))
        self._callback = callback
    
    def handle_one_inbox_message(self, msg):
        self._callback(msg)

