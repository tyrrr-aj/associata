import rabbitmq
from agds_logger import AgdsLogger


class Ctrl:
    def __init__(self, out_path):
        self._out_path = out_path

        self._structure_observers = []

        self._connection = rabbitmq.connect()
        self._ctrl_channel = rabbitmq.Channel(self._connection, 'ctrl', 'cmd.vis')


    def start(self):
        self._ctrl_channel.respond(b'"started"')
        self._ctrl_channel.listen(self.on_ctrl_message)


    def on_ctrl_message(self, message):
        ctrl_message = message.replace('{', '').replace('}', '').replace('"', '')[:-1]
        cmd = ctrl_message.split(',')[0]

        print(f'Ctrl received message: {ctrl_message} (raw: {message})')
        print(f'Cmd: {cmd}')

        if cmd == 'new_structure':
            structure_type = ctrl_message.split(',')[1]
            structure_id = ctrl_message.split(',')[2]

            print(f'New structure: {structure_type} ({structure_id})')
            if structure_type == 'agds':
                print(f'Creating AGDS observer for {structure_id}')
                observer = AgdsLogger(self._connection, structure_id, self._out_path)
                observer.start()
                self._structure_observers.append(observer)

        elif cmd == 'stop':
            for observer in self._structure_observers:
                observer.finish()

            self._ctrl_channel.close()
            rabbitmq.disconnect(self._connection)

            exit(0)
