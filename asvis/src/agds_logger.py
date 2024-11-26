import parse
import graph
import pyrlang_channel
from term import Atom
import asyncio
import os
import datetime
from pathlib import Path



class AgdsLogger:
    def __init__(self, connection, structure_id, out_path, client_node_name):
        self._connection = connection
        self._out_path = out_path
        self._structure_id = structure_id
        
        self._last_known_experiment_step = 0

        self._observed_graph = graph.Graph('AGDS')
        self._node_groups = {}

        self._started = False
        self._shutdown = False
        self._shutdown_semaphore = asyncio.Semaphore(0)

        self._channel = pyrlang_channel.Channel(self._connection, structure_id, client_node_name, self.on_message)

        self.completed = None

        self._experiment_timestamp = datetime.datetime.now()

        self._unprocessed_log_msgs = []


    async def start(self):
        # print(f'AGDS Logger for {self._structure_id} started')
        self._started = True
        await self._channel.send('vis_setup_done')
        self.completed = asyncio.create_task(self.wait_until_finished())


    async def stop(self):
        await self._channel.send_self(Atom('stop'))


    async def wait_until_finished(self):
        # print(f'AGDS Logger for {self._structure_id} waiting for shutdown')
        await self._shutdown_semaphore.acquire()
        # print(f'AGDS Logger for {self._structure_id} finished')

        await self._channel.send('vis_stopped')
        self._channel.close()


    def on_message(self, message):
        match message:
            case (Atom('ctrl'), Cmd):
                self.on_ctrl_message(Cmd)
            
            case (Atom('log'), LogMsg):
                self.on_log_message(LogMsg)

            case msg:
                print(f'Unknown message: {msg}')


    def on_ctrl_message(self, cmd):
        match cmd:
            case (Atom('export_stimulation'), experiment_step, stimulation_name):
                self._process_log_messages()
                try:
                    self._observed_graph.export_stimulation(self._get_output_path(), experiment_step, stimulation_name)
                except Exception as e:
                    print(f'Error exporting stimulation: {e}')

            case Atom('export_topology'):
                self._process_log_messages()
                try:
                    self._observed_graph.export_topology(self._get_output_path(), self._last_known_experiment_step)
                except Exception as e:
                    print(f'Error exporting topology: {e}')

            case Atom('stop'):
                # print(f'AGDS Logger for {self._structure_id} received message "stop"')
                self._finish()

            case msg:
                print(f'Unknown ctrl msg: {msg}')


    def on_log_message(self, message):
        self._unprocessed_log_msgs.append(message)


    def _process_log_messages(self):
        if len(self._unprocessed_log_msgs) == 0:
            return

        print(f'Processing log messages: {len(self._unprocessed_log_msgs)} messages to process...')
        for i, msg in enumerate(self._unprocessed_log_msgs):
            self.process_single_log_message(msg)
            if (i + 1) % 5000 == 0:
                print(f'Processed {i} messages ({(i + 1) / len(self._unprocessed_log_msgs):.0%})')
        
        print(f'All {len(self._unprocessed_log_msgs)} log messages processed successfuly')
        self._unprocessed_log_msgs = []



    def process_single_log_message(self, message):
        match message:

            case (Atom('topology'), Atom('na'), Atom('node_group_creation'), (ng_id, ng_name, _ng_type)):
                self._node_groups[ng_id] = ng_name
                # print(f'Node group {ng_name} ({ng_id}) created')

            case (Atom('topology'), experiment_step, Atom('node_creation'), (n_id, n_type, n_value, n_group)):
                self._update_last_known_experiment_step(experiment_step)
                n_value = round(float(n_value), 3)  # TODO: assumes numerical values only
                node_group = self._node_groups[n_group]
                attributes = {
                    'type': str(n_type), 
                }
                self._observed_graph.add_node(n_id, node_group, n_value, attributes, experiment_step)
                # print(f'Node {n_id} created')

            case (Atom('topology'), experiment_step, Atom('connection_formed'), (source_node_id, dest_node_id)):
                self._update_last_known_experiment_step(experiment_step)
                # print(f'[{experiment_step}] Connection formed: {source_node_id} <-> {dest_node_id}')
                self._observed_graph.add_connection(source_node_id, dest_node_id, experiment_step)

            case (Atom('topology'), experiment_step, Atom('connection_broken'), (source_node_id, dest_node_id)):
                self._update_last_known_experiment_step(experiment_step)
                # print(f'[{experiment_step}] Connection broken: {source_node_id} <-> {dest_node_id}')
                self._observed_graph.remove_connection(source_node_id, dest_node_id, experiment_step)

            case (Atom('topology'), experiment_step, Atom('node_killed'), (node_id,)):
                self._update_last_known_experiment_step(experiment_step)
                # print(f'Node {node_id} killed')
                self._observed_graph.add_node_killing(node_id, experiment_step)

            case (Atom('stimulation'), (experiment_step, stimulation_name, Atom('na')), Atom('node_group_modes'), (node_group_modes,)):
                ng_modes_repr = {ng_id: str(ng_mode) if not isinstance(ng_mode, tuple) else "_".join([str(ng_mode_part) for ng_mode_part in ng_mode]) for ng_id, ng_mode in node_group_modes.items()}
                self._observed_graph.add_node_group_modes(ng_modes_repr, experiment_step, stimulation_name)

            case (Atom('stimulation'), (experiment_step, stimulation_name, depth), Atom('node_stimulated'), (stimulated_node_id, source_node_id, new_excitation, stimulus)):
                # print(f'[{experiment_step}, {stimulation_name}] Node {stimulated_node_id} stimulated at depth {depth} by {source_node_id} with stimulus {stimulus} resulting in excitation {new_excitation}')
                self._update_last_known_experiment_step(experiment_step)
                self._observed_graph.add_node_stimulation(stimulated_node_id, source_node_id, new_excitation, stimulus, experiment_step, stimulation_name, depth)

            case (Atom('stimulation'), (experiment_step, Atom('na'), Atom('na')), Atom('node_poisoned'), (node_id, new_acc_poison_level)):
                self._update_last_known_experiment_step(experiment_step)
                self._observed_graph.add_node_poisoning(node_id, new_acc_poison_level, experiment_step)

            case msg:
                print(f'Unknown log msg: {msg}')


    def _update_last_known_experiment_step(self, experiment_step):
        if experiment_step > self._last_known_experiment_step:
            self._last_known_experiment_step = experiment_step


    def _finish(self, timestamp=None):        
        print(f'AGDS Logger for {self._structure_id} finishing')

        if self._started:
            self._observed_graph.close_all_time_ranges(self._last_known_experiment_step + 1)

            # TODO: find a suitable place to call this send asynchronously
            # self._channel.send('vis_stopped')
            # self._channel.close()

            self._started = False
        
        self._shutdown = True
        self._shutdown_semaphore.release()


    def _get_output_path(self):
        out_dir_day = self._experiment_timestamp.strftime("%Y-%m-%d")
        out_dir_structure = f'{self._experiment_timestamp.strftime("%H-%M-%S")} agds[{self._structure_id}]'
        out_dir_path = os.path.join(self._out_path, out_dir_day, out_dir_structure)
        
        Path(out_dir_path).mkdir(parents=True, exist_ok=True)
        return out_dir_path
