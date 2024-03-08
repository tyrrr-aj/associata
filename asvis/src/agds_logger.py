import parse
import graph
import rabbitmq
import threading
import datetime
import os


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

        self._channel = rabbitmq.Channel(self._connection, structure_id)


    def start(self):
        print(f'AGDS Logger for {self._structure_id} started')
        self._channel.respond(b'"vis_setup_done"')
        threading.Thread(target=self._channel.listen, args=(self.on_log_message,)).start()


    def finish(self, timestamp=None):
        if timestamp is None:
            if self._last_known_timestamp is not None:
                timestamp = self._last_known_timestamp + 1.0
            else:
                timestamp = self._time_origin + 1.0
        self._observed_graph.close_all_time_ranges(timestamp)
        

        out_file = f'agds[{self._structure_id}]_{datetime.datetime.now().strftime("%Y-%m-%d#%H_%M_%S")}.gexf'
        out_file_path = os.path.join(self._out_path, out_file)
        self._observed_graph.export(out_file_path)

        self._channel.respond(b'"vis_stopped"')
        self._channel.close()


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
                
                attributes = {
                    'type': n_type, 
                    'node_group': self._node_groups[n_group],
                    'value': n_value
                }
                self._observed_graph.add_node(n_id, attributes, timestamp)

                print(f'Node {n_id} created')


            elif event_type == 'connection_formed':
                info_format = '{{{},{}}}'
                [source_node_id, dest_node_id] = parse.parse(info_format, event_info)
                
                self._observed_graph.add_connection(source_node_id, dest_node_id, timestamp)


            elif event_type == 'connection_broken':
                info_format = '{{{},{}}}'
                [source_node_id, dest_node_id] = parse.parse(info_format, event_info)
                
                self._observed_graph.remove_connection(source_node_id, dest_node_id, timestamp)


            elif event_type == 'node_stimulated':
                info_format = '{{{},{}}}'
                [node_id, excitation] = parse.parse(info_format, event_info)
                
                self._observed_graph.add_node_stimulation(node_id, excitation, timestamp)


            elif event_type == 'node_poisoned':
                info_format = '{{{},{},{}}}'
                [node_id, poison_lvl, _deadly_dose] = parse.parse(info_format, event_info)
                
                self._observed_graph.add_node_poisoning(node_id, poison_lvl, timestamp)


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

        #timestamp = float(f'{rel_time}.{rel_seq}')     # rel_seq used like that breaks chronology (seq=9 > seq=10)
        timestamp = float(rel_time)
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