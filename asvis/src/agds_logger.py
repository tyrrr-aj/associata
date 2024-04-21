import parse
import graph
import rabbitmq
import threading
import datetime
import os
from pathlib import Path


class AgdsLogger:
    def __init__(self, connection, structure_id, out_path):
        self._connection = connection
        self._out_path = out_path
        self._structure_id = structure_id
        
        self._time_origin = None
        self._seq_origin = None
        self._last_known_timestamp = None

        self._observed_graph = graph.Graph('AGDS')
        self._node_groups = {}

        self._finishing_lock = threading.Lock()

        self._channel = rabbitmq.Channel(self._connection, structure_id)


    def start(self):
        print(f'AGDS Logger for {self._structure_id} started')
        self._started = True
        self._channel.respond(b'"vis_setup_done"')
        threading.Thread(target=self._channel.listen, args=(self.on_log_message,)).start()


    def finish(self, timestamp=None):
        self._finishing_lock.acquire()
        
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

            self._channel.respond(b'"vis_stopped"')
            self._channel.close()

            self._started = False
        
        self._finishing_lock.release()


    def on_log_message(self, message):
        [timestamp_info, event_type, event_info] = message.split('#')
        
        if event_type == 'node_group_creation':
            info_format = '{{{},{},{}}}'
            [ng_id, ng_name, _ng_type] = parse.parse(info_format, event_info)

            self._node_groups[ng_id] = ng_name

            print(f'Node group {ng_name} ({ng_id}) created')
        

        else:
            timestamp = self._get_timestamp(timestamp_info)
            
            if event_type == 'node_creation':
                info_format = '{{{},{},{},{}}}'
                [n_id, n_type, n_value, n_group] = parse.parse(info_format, event_info)
                
                try:
                    float_value = float(n_value)
                    n_value = round(float_value, 3)
                except:
                    pass

                attributes = {
                    'type': n_type, 
                    'node_group': self._node_groups[n_group],
                    'value': n_value
                }
                self._observed_graph.add_node(n_id, attributes, timestamp)

                # print(f'Node {n_id} created')


            elif event_type == 'connection_formed':
                info_format = '{{{},{}}}'
                [source_node_id, dest_node_id] = parse.parse(info_format, event_info)
                
                # print(f'Connection formed: {source_node_id} <-> {dest_node_id}')
                self._observed_graph.add_connection(source_node_id, dest_node_id, timestamp)


            elif event_type == 'connection_broken':
                info_format = '{{{},{}}}'
                [source_node_id, dest_node_id] = parse.parse(info_format, event_info)

                # print(f'Connection broken: {source_node_id} <-> {dest_node_id}')                
                self._observed_graph.remove_connection(source_node_id, dest_node_id, timestamp)


            elif event_type == 'node_stimulated':
                info_format = '{{{},{}}}'
                [node_id, excitation] = parse.parse(info_format, event_info)
                
                self._observed_graph.add_node_stimulation(node_id, excitation, timestamp)


            elif event_type == 'node_poisoned':
                info_format = '{{{},{},{},{},{},{}}}'
                [node_id, acc_poison_lvl, tmp_poison_lvl, tmp_poison_multiplier, source_node, stimuli] = parse.parse(info_format, event_info)
                
                self._observed_graph.add_node_poisoning(node_id, acc_poison_lvl, tmp_poison_lvl, tmp_poison_multiplier, source_node, stimuli, timestamp)


            elif event_type == 'node_killed':
                info_format = '{{{}}}'
                [node_id] = parse.parse(info_format, event_info)

                self._observed_graph.add_node_killing(node_id, timestamp)


            elif event_type == 'stimuli_propagated':
                ...


            elif event_type == 'experiment_end':
                self.finish(timestamp)


            else:
                print(f'Unknown event type: {event_type}')



    def _get_timestamp(self, timestamp_info):
        if (self._time_origin is None or self._seq_origin is None):
            self._set_time_origin(timestamp_info)
            return float(0.0)
        
        (time, seq) = self._parse_timestamp_info(timestamp_info)
        rel_time = time - self._time_origin
        rel_seq = seq - self._seq_origin

        timestamp = rel_time + rel_seq / 10**9  # TODO: may fail for too long experiments
        self._last_known_timestamp = timestamp

        return timestamp


    def _set_time_origin(self, timestamp_info):
        (time, seq) = self._parse_timestamp_info(timestamp_info)
        self._time_origin = time
        self._seq_origin = seq

    def _parse_timestamp_info(self, timestamp_info):
        timestamp_format = '{{{},{}}}'
        [time, seq] = [int(n) for n in parse.parse(timestamp_format, timestamp_info)]
        return (time, seq)