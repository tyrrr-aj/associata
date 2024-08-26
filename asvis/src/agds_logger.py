import parse
import graph
import pyrlang_channel
from term import Atom
import asyncio
import datetime
import os
from pathlib import Path


class AgdsLogger:
    def __init__(self, connection, structure_id, out_path, client_node_name):
        self._connection = connection
        self._out_path = out_path
        self._structure_id = structure_id
        
        self._time_origin = None
        self._seq_origin = None
        self._last_known_timestamp = None

        self._observed_graph = graph.Graph('AGDS')
        self._node_groups = {}

        self._started = False
        self._shutdown = False
        self._shutdown_semaphore = asyncio.Semaphore(0)

        self._channel = pyrlang_channel.Channel(self._connection, structure_id, client_node_name, self.on_log_message)

        self.completed = None


    async def start(self):
        print(f'AGDS Logger for {self._structure_id} started')
        self._started = True
        await self._channel.send('vis_setup_done')
        self.completed = asyncio.create_task(self.wait_until_finished())


    async def stop(self):
        await self._channel.send_self(Atom('stop'))


    async def wait_until_finished(self):
        print(f'AGDS Logger for {self._structure_id} waiting for shutdown')
        await self._shutdown_semaphore.acquire()
        print(f'AGDS Logger for {self._structure_id} finished')

        await self._channel.send('vis_stopped')
        self._channel.close()


    def on_log_message(self, message):
        match message:

            case (timestamp_info, Atom('node_group_creation'), (ng_id, ng_name, _ng_type)):
                self._node_groups[ng_id] = ng_name
                print(f'Node group {ng_name} ({ng_id}) created')

            case (timestamp_info, Atom('node_creation'), (n_id, n_type, n_value, n_group)):
                    n_value = round(n_value, 3)
                    attributes = {
                        'type': str(n_type), 
                        'node_group': self._node_groups[n_group],
                        'value': n_value
                    }
                    self._observed_graph.add_node(n_id, attributes, self._get_timestamp(timestamp_info))
                    # print(f'Node {n_id} created')

            case (timestamp_info, Atom('connection_formed'), (source_node_id, dest_node_id)):
                # print(f'Connection formed: {source_node_id} <-> {dest_node_id}')
                self._observed_graph.add_connection(source_node_id, dest_node_id, self._get_timestamp(timestamp_info))

            case (timestamp_info, Atom('connection_broken'), (source_node_id, dest_node_id)):
                # print(f'Connection broken: {source_node_id} <-> {dest_node_id}')                
                self._observed_graph.remove_connection(source_node_id, dest_node_id, self._get_timestamp(timestamp_info))

            case (timestamp_info, Atom('node_stimulated'), (node_id, excitation)):
                self._observed_graph.add_node_stimulation(node_id, excitation, self._get_timestamp(timestamp_info))

            case (timestamp_info, Atom('node_poisoned'), (node_id, acc_poison_lvl, excitation, stimulus, source_node)):
                acc_poison_lvl = round(float(acc_poison_lvl), 3)
                excitation = round(float(excitation), 3)
                stimulus = round(float(stimulus), 3)
                
                self._observed_graph.add_node_poisoning(node_id, acc_poison_lvl, excitation, stimulus, source_node, self._get_timestamp(timestamp_info))

            case (timestamp_info, Atom('node_killed'), (node_id,)):
                # print(f'Node {node_id} killed')
                self._observed_graph.add_node_killing(node_id, self._get_timestamp(timestamp_info))

            case (timestamp_info, Atom('stimuli_propagated'), (source_node_id, dest_node_id, stimuli)):
                ...

            case (timestamp_info, Atom('experiment_end'), (Atom('none'),)):
                self._finish(self._get_timestamp(timestamp_info))

            case (Atom('stop')):
                print(f'AGDS Logger for {self._structure_id} received message "stop"')
                self._finish()

            case (timestamp_info, event_type, _event_data):
                print(f'Unknown event type: {event_type}')


    def _finish(self, timestamp=None):        
        print(f'AGDS Logger for {self._structure_id} finishing')

        if self._started:
            if timestamp is None:
                if self._last_known_timestamp is not None:
                    timestamp = self._last_known_timestamp + 1.0
                else:
                    timestamp = self._time_origin + 1.0
            self._observed_graph.close_all_time_ranges(timestamp)
            
            out_dir_name = datetime.datetime.now().strftime("%Y-%m-%d")
            out_file = f'{datetime.datetime.now().strftime("%H-%M-%S")} agds[{self._structure_id}].gexf'
            out_file_path = os.path.join(self._out_path, out_dir_name, out_file)

            Path(os.path.join(self._out_path, out_dir_name)).mkdir(parents=True, exist_ok=True)
            self._observed_graph.export(out_file_path)

            # TODO: find a suitable place to call this send asynchronously
            # self._channel.send('vis_stopped')
            # self._channel.close()

            self._started = False
        
        self._shutdown = True
        self._shutdown_semaphore.release()


    def _get_timestamp(self, timestamp_info):
        if (self._time_origin is None or self._seq_origin is None):
            self._set_time_origin(timestamp_info)
            return float(0.0)
        
        (time, seq) = timestamp_info # self._parse_timestamp_info(timestamp_info)
        rel_time = time - self._time_origin
        rel_seq = seq - self._seq_origin

        timestamp = rel_time + rel_seq / 10**9  # TODO: may fail for too long experiments
        self._last_known_timestamp = timestamp

        return timestamp


    def _set_time_origin(self, timestamp_info):
        (time, seq) = timestamp_info # self._parse_timestamp_info(timestamp_info)
        self._time_origin = time
        self._seq_origin = seq

    def _parse_timestamp_info(self, timestamp_info):
        timestamp_format = '{{{},{}}}'
        [time, seq] = [int(n) for n in parse.parse(timestamp_format, timestamp_info)]
        return (time, seq)
