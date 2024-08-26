import networkx as nx


class Graph:
    def __init__(self, structure_type):
        self.graph = nx.Graph(mode='dynamic', structure_type=structure_type)
        
        self._edge_activation_time_ms = 5

    def add_node(self, node_id, attributes, creation_time):
        self.graph.add_node(
            node_id, 
            start=creation_time, 
            excitation=self._init_timeframed_attr(creation_time), 
            acc_poison_lvl=self._init_timeframed_attr(creation_time),
            tmp_poison_lvl=self._init_timeframed_attr(creation_time),
            tmp_poison_multiplier=self._init_timeframed_attr(creation_time),
            **attributes
        )

    def add_connection(self, source, dest, creation_time):
        self.graph.add_edge(
            source, 
            dest, 
            start=creation_time, 
            poison_passed=self._init_timeframed_attr(creation_time, end_time=creation_time + self._edge_activation_time_ms)
        )

    def remove_connection(self, source_node, dest_node, removal_time):
        self.graph.edges[source_node, dest_node]['end'] = removal_time

    def add_node_stimulation(self, node_id, excitation, timestamp):
        self._close_attribute_time_range(node_id, 'excitation', timestamp)      # first range is opened at node creation time
        self._add_timeframed_node_attribute_value(node_id, 'excitation', excitation, timestamp)

    def add_node_poisoning(self, node_id, acc_poison_lvl, tmp_poison_lvl, stimulus, source_node, timestamp):
        if acc_poison_lvl != self._get_last_attr_value(node_id, 'acc_poison_lvl'):
            self._close_attribute_time_range(node_id, 'acc_poison_lvl', timestamp)      # first range is opened at node creation time
            self._add_timeframed_node_attribute_value(node_id, 'acc_poison_lvl', acc_poison_lvl, timestamp)

        if tmp_poison_lvl != self._get_last_attr_value(node_id, 'tmp_poison_lvl'):
            self._close_attribute_time_range(node_id, 'tmp_poison_lvl', timestamp)
            self._add_timeframed_node_attribute_value(node_id, 'tmp_poison_lvl', tmp_poison_lvl, timestamp)
        
        self._add_timeframed_edge_attribute_value(source_node, node_id, 'poison_passed', stimulus, timestamp)


    def add_node_killing(self, node_id, timestamp):
        self.graph.nodes[node_id]['end'] = timestamp

    def add_connection_stimulation(self, connection_stimulation):
        ...

    def close_all_time_ranges(self, timestamp):
        for node_id in self.graph:
            self._close_attribute_time_range(node_id, 'excitation', timestamp)
            self._close_attribute_time_range(node_id, 'acc_poison_lvl', timestamp)
            self._close_attribute_time_range(node_id, 'tmp_poison_lvl', timestamp)
            self._close_attribute_time_range(node_id, 'tmp_poison_multiplier', timestamp)

    def export(self, path):
        nx.write_gexf(self.graph, path)

    def _add_timeframed_node_attribute_value(self, node_id, attribute, value, start, end=None):
        current_values = self.graph.nodes[node_id][attribute] if attribute in self.graph.nodes[node_id] else []
        new_entry = (value, start, end) if end is not None else (value, start)

        nx.set_node_attributes(self.graph, {node_id: current_values + [new_entry]}, attribute)

    def _add_timeframed_edge_attribute_value(self, source, dest, attribute, value, start, end=None):
        current_values = self.graph.edges[source, dest][attribute] if attribute in self.graph.edges[source, dest] else []
        new_entry = (value, start, end) if end is not None else (value, start, start + self._edge_activation_time_ms)

        nx.set_edge_attributes(self.graph, {(source, dest): current_values + [new_entry]}, attribute)

    def _close_attribute_time_range(self, node_id, attribute, closing_time):
        node = self.graph.nodes[node_id]
        last_entry = node[attribute][-1]
        if len(last_entry) == 2:
            node[attribute][-1] = (*last_entry, closing_time)

    def _init_timeframed_attr(self, start_time, end_time=None, value=0.0):
        return [(value, start_time)] if end_time is None else [(value, start_time, end_time)]
    
    def _get_last_attr_value(self, node_id, attr_name):
        return self.graph.nodes[node_id][attr_name][-1][0]
