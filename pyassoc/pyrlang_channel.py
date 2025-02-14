from pyrlang import Node
from pyrlang.process import Process
from term import Atom
from asyncio import wait_for, TimeoutError, Semaphore
from collections import deque


_backend_node_name = None
_vis_node_name = None


def connect(client_node_name, backend_node_name, vis_node_name, cookie):
    global _backend_node_name, _vis_node_name

    _backend_node_name = backend_node_name
    _vis_node_name = vis_node_name

    return Node(node_name=client_node_name, cookie=cookie)


def disconnect(connection):
    connection.destroy()


class Channel():
    def __init__(self, connection, channel_name) -> None:
        self._node = connection
        self._channel_name = channel_name

        self._res_buffer = deque()
        self._res_semaphore = Semaphore(0)
        self._receiver = Receiver(f'{channel_name}', 
                                    self._res_buffer, 
                                    self._res_semaphore)


    async def send_vis_async(self, message):
        # print(f'Channel: sending message to vis: {message} (node: {_vis_node_name}, channel: {self._channel_name})')
        await self._send(self._receiver.pid_, _vis_node_name, self._channel_name, (Atom('ctrl'), message))
        # print(f'Channel: message sent to vis')


    async def send_backend_async(self, message):
        await self._send(self._receiver.pid_, _backend_node_name, self._channel_name, message)


    async def receive_async(self, timeout):
        try:
            await wait_for(self._res_semaphore.acquire(), timeout=timeout)
            return self._res_buffer.popleft()
        except TimeoutError:
            return None

    async def _send(self, sender_pid, target_node_name, target_process_name, msg):
        await self._node.send(sender=sender_pid,
                    receiver=(Atom(target_node_name), Atom(target_process_name)),
                    message=msg)


    def close(self):
        self._receiver.exit()


class Receiver(Process):
    def __init__(self, name, buffer, semaphore):
        super().__init__()
        self.get_node().register_name(self, Atom(name))
        self._buffer = buffer
        self._semaphore = semaphore
    
    def handle_one_inbox_message(self, msg):
        self._buffer.append(msg)
        self._semaphore.release()

