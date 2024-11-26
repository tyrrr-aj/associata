import networkx as nx
import os



class Graph:
    def __init__(self, structure_type):
        self.graph = nx.MultiGraph(mode='dynamic', structure_type=structure_type)
        
        self._node_group_modes = {} # {(experiment_step, stimulation_name): {node_group_name: mode}}
        self.node_excitations = {}  # {(experiment_step, stimulation_name): {node_id: {depth: excitation}}}
        self.edge_stimulations = {} # {(experiment_step, stimulation_name): {(source_node, dest_node): {depth: stimulus}}}


    def add_node(self, node_id, node_group, node_value, attributes, experiment_step):
        self.graph.add_node(
            node_id, 
            node_group=node_group,
            start=experiment_step, 
            value=node_value,
            label=f'{node_group}: {node_value}',
            acc_poison_lvl=self._init_timeframed_attr(experiment_step, value=0.0),
            **attributes
        )


    def add_connection(self, source, dest, experiment_step):
        if self.graph.has_edge(source, dest) and any(attributes['start'] == experiment_step in attributes for attributes in self.graph.get_edge_data(source, dest).values()):
            return
        
        self.graph.add_edge(
            source, 
            dest, 
            key=experiment_step,
            start=experiment_step
        )


    def remove_connection(self, source_node, dest_node, experiment_step):
        for edge_key, edge_attributes in self.graph.get_edge_data(source_node, dest_node).items():
            if 'end' not in edge_attributes:
                self.graph[source_node][dest_node][edge_key]['end'] = experiment_step
                break


    def add_node_poisoning(self, node_id, new_acc_poison_level, experiment_step):
        self._add_timeframed_node_attribute_value(self.graph, node_id, 'acc_poison_lvl', new_acc_poison_level, experiment_step)


    def add_node_killing(self, node_id, experiment_step):
        self.graph.nodes[node_id]['end'] = experiment_step
        for source_node, dest_node in self.graph.edges(node_id):
            self.remove_connection(source_node, dest_node, experiment_step)


    def add_node_group_modes(self, node_group_modes, experiment_step, stimulation_name):
        self._node_group_modes[(experiment_step, stimulation_name)] = node_group_modes


    def add_node_stimulation(self, stimulated_node_id, source_node_id, new_excitation, stimulus, experiment_step, stimulation_name, depth):
        stimulations = self.node_excitations.get((experiment_step, stimulation_name), {})
        node_stimulations = stimulations.get(stimulated_node_id, {})
        old_excitation = node_stimulations.get(depth, 0.0)

        node_stimulations[depth] = new_excitation if new_excitation > old_excitation else old_excitation
        stimulations[stimulated_node_id] = node_stimulations
        self.node_excitations[(experiment_step, stimulation_name)] = stimulations

        if source_node_id in self.graph.nodes:  # source is not a node group
            self.add_connection_stimulation(source_node_id, stimulated_node_id, stimulus, experiment_step, stimulation_name, depth)


    def add_connection_stimulation(self, source_node_id, dest_node_id, stimulus, experiment_step, stimulation_name, depth):
        stimulations = self.edge_stimulations.get((experiment_step, stimulation_name), {})
        edge_stimulations = stimulations.get((source_node_id, dest_node_id), {})
        old_stimulus = edge_stimulations.get(depth, 0.0)

        edge_stimulations[depth] = old_stimulus + stimulus
        stimulations[(source_node_id, dest_node_id)] = edge_stimulations
        self.edge_stimulations[(experiment_step, stimulation_name)] = stimulations


    def close_all_time_ranges(self, experiment_step):
        for node_id in self.graph:
            self._close_node_attribute_time_range(self.graph, node_id, 'acc_poison_lvl', experiment_step)


    def export_topology(self, output_dir, last_known_experiment_step):
        self.close_all_time_ranges(last_known_experiment_step)
        output_path = self._get_output_path(output_dir, 'topology')
        nx.write_gexf(self.graph, output_path)


    def export_stimulation(self, output_dir, experiment_step, stimulation_name):
        if (experiment_step, stimulation_name) not in self.node_excitations:
            print(f'No stimulations for step {experiment_step} and name {stimulation_name}')
            print(f'Available stimulations: {list(self.node_excitations.keys())}')
            return

        edge_subgraph = self.graph.edge_subgraph(self._get_edges_existing_at_step(experiment_step - 1)) # -1 because nodes and edges are added at the end of the step, after all stimulations, and changes resulting from poisoning should not affect visualization for particular stepfor particular step

        graph_snapshot = nx.Graph()
        graph_snapshot.add_nodes_from(edge_subgraph.nodes(data=True))
        graph_snapshot.add_edges_from(edge_subgraph.edges(data=True))  

        max_depth = max([depth for _, node_excitations in self.node_excitations[(experiment_step, stimulation_name)].items() for depth in node_excitations]) + 1

        # node group modes
        node_group_modes = self._node_group_modes.get((experiment_step, stimulation_name), {})
        nx.set_node_attributes(graph_snapshot, {node_id: node_group_modes.get(graph_snapshot.nodes[node_id]['node_group'], 'unknown') for node_id in graph_snapshot.nodes}, 'ng_mode')

        # accumulated poison levels
        nx.set_node_attributes(graph_snapshot, {node_id: self._get_last_attr_value(node_id, 'acc_poison_lvl', experiment_step) for node_id in graph_snapshot.nodes}, 'acc_poison_lvl')

        # topology and basic excitation
        for node_id in graph_snapshot.nodes:
            if 'start' in graph_snapshot.nodes[node_id]:
                del graph_snapshot.nodes[node_id]['start']
            if 'end' in graph_snapshot.nodes[node_id]:
                del graph_snapshot.nodes[node_id]['end']
            if 'excitation' not in graph_snapshot.nodes[node_id]:
                graph_snapshot.nodes[node_id]['excitation'] = self._init_timeframed_attr(0, value=0.0)

        for source_node, dest_node in graph_snapshot.edges:
            if 'start' in graph_snapshot.edges[source_node, dest_node]:
                del graph_snapshot.edges[source_node, dest_node]['start']
            if 'end' in graph_snapshot.edges[source_node, dest_node]:
                del graph_snapshot.edges[source_node, dest_node]['end']

        # node excitations
        for node_id, node_excitations in self.node_excitations.get((experiment_step, stimulation_name), {}).items():
            for depth, excitation in node_excitations.items():
                self._add_timeframed_node_attribute_value(graph_snapshot, node_id, 'excitation', excitation, depth)
            
        for node_id in graph_snapshot.nodes:
            self._close_node_attribute_time_range(graph_snapshot, node_id, 'excitation', max_depth)

        # edge stimulations
        nx.set_edge_attributes(graph_snapshot, {e: self._init_timeframed_attr(0, value=0.0) for e in graph_snapshot.edges}, 'stimulus')

        for (source_node, dest_node), edge_stimulations in self.edge_stimulations.get((experiment_step, stimulation_name), {}).items():
            for depth in range(max_depth):
                stimulus = edge_stimulations.get(depth, 0.0)
                self._add_timestamped_edge_attribute_value(graph_snapshot, source_node, dest_node, 'stimulus', stimulus, depth)

        for source_node, dest_node in graph_snapshot.edges:
            self._close_edge_attribute_time_range(graph_snapshot, source_node, dest_node, 'stimulus', max_depth)

        # export
        output_path = self._get_output_path(output_dir, f'stimulation({stimulation_name})_step{experiment_step}')
        try:
            nx.write_gexf(graph_snapshot, output_path)
        except Exception as e:
            print(f'Error while exporting graph snapshot: {e}')


    def _add_timeframed_node_attribute_value(self, graph, node_id, attribute, value, start):
        if attribute in graph.nodes[node_id]:
            earlier_values = [v for v in graph.nodes[node_id][attribute] if v[1] < start]
            later_values = [v for v in graph.nodes[node_id][attribute] if v[1] > start]

            if len(earlier_values) > 0:
                if earlier_values[-1][0] == value:
                    return
                else:
                    earlier_values[-1] = (*earlier_values[-1][:2], start)

            if len(later_values) > 0:
                if later_values[0][0] == value:
                    later_values[0] = (value, start, *later_values[0][2:])
                new_entry = (value, start, later_values[0][1])
            else:
                new_entry = (value, start)

            new_values = earlier_values + [new_entry] + later_values

        else:
            new_values = [(value, start)]
        
        nx.set_node_attributes(graph, {node_id: new_values}, attribute)


    def _add_timestamped_edge_attribute_value(self, graph, source, dest, attribute, value, timestamp):
        current_values = graph.edges[source, dest][attribute] if attribute in graph.edges[source, dest] else []
        
        if len(current_values) > 0 and current_values[-1][1] == timestamp:
            current_values[-1] = (value, timestamp)
        elif len(current_values) > 0 and current_values[-1][0] == value:
            current_values[-1] = (value, current_values[-1][1], timestamp+1)
        else:
            if (len(current_values) > 0 and len(current_values[-1]) == 2):
                current_values[-1] = (*current_values[-1], timestamp)

            new_entry = (value, timestamp, timestamp+1)
            current_values += [new_entry]

        nx.set_edge_attributes(graph, {(source, dest): current_values}, attribute)


    def _close_node_attribute_time_range(self, graph, node_id, attribute, closing_time):
        node = graph.nodes[node_id]
        last_entry = node[attribute][-1] if attribute in node else (0.0, 0) # TODO: assumes attribute of type float
        if len(last_entry) == 2:
            preserved_entries = node[attribute][:-1] if attribute in node else []
            node[attribute] = preserved_entries + [(*last_entry, closing_time)]


    def _close_edge_attribute_time_range(self, graph, source, dest, attribute, closing_time):
        edge = graph.edges[source, dest]
        last_entry = edge[attribute][-1] if attribute in edge else (0.0, 0)
        if len(last_entry) == 2:
            edge[attribute][-1] = (*last_entry, closing_time)
            # nx.set_edge_attributes(graph, {(source, dest): edge[attribute][:-1] + [(*last_entry, closing_time)]}, attribute)


    def _init_timeframed_attr(self, start_time, end_time=None, value=0.0):
        return [(value, start_time)] if end_time is None else [(value, start_time, end_time)]
    

    def _get_last_attr_value(self, node_id, attr_name, at_timestamp=None):
        attr_entries = self.graph.nodes[node_id][attr_name]
        
        if at_timestamp is None or at_timestamp < 0:
            return attr_entries[-1][0]
        else:
            return [e for e in attr_entries if e[1] <= at_timestamp and (len(e) == 2 or e[2] >= at_timestamp)][-1][0]


    def _get_edges_existing_at_step(self, experiment_step):
        return [edge for edge in self.graph.edges 
                if experiment_step >= self.graph.edges[edge]['start'] 
                and (experiment_step <= self.graph.edges[edge]['end'] if 'end' in self.graph.edges[edge] else True)]


    def _get_output_path(self, output_dir, output_name):
        return os.path.join(output_dir, f'{output_name}.gexf')
