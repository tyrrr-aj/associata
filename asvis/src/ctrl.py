import pyrlang_channel
from term import Atom
from agds_logger import AgdsLogger
import asyncio


class Ctrl:
    def __init__(self, out_path, client_node_name, vis_node_name, cookie):
        self._out_path = out_path
        self._client_node_name = client_node_name

        self._structure_observers = []
        self._finishing_semaphore = asyncio.Semaphore(0)

        self._vis_node_name = vis_node_name
        self._cookie = cookie
            

    async def wait_until_finished(self):
        self._connection = pyrlang_channel.connect(self._vis_node_name, self._cookie)
        self._ctrl_channel = pyrlang_channel.Channel(self._connection, 'ctrl', self._client_node_name)

        await self._ctrl_channel.send(Atom('started'))

        while (cmd := await self._ctrl_channel.receive_async()) != Atom('stop'):
            await self.on_ctrl_message(cmd)

        print('Waiting for structure observers to finish')
        await asyncio.gather(*[observer.wait_until_finished() for observer in self._structure_observers])
        print('All structure observers finished')

        await self.stop()


    async def on_ctrl_message(self, message):
        print(f'Ctrl received cmd: {message}')

        match message:
            case (Atom('new_structure'), structure_type, structure_id):
                print(f'New structure: {structure_type} ({structure_id})')
                if structure_type == Atom('agds'):
                    print(f'Creating AGDS observer for {structure_id}')
                    observer = AgdsLogger(self._connection, structure_id, self._out_path, self._client_node_name)
                    await observer.start()
                    self._structure_observers.append(observer)

            case _:
                print(f'Unknown cmd: {message}')

    async def stop(self):
        for observer in self._structure_observers:
            await observer.stop()

        print('Waiting for structure observers to finish')
        await asyncio.gather(*[observer.wait_until_finished() for observer in self._structure_observers])
        print('All structure observers finished')

        self._ctrl_channel.close()
        pyrlang_channel.disconnect(self._connection)
