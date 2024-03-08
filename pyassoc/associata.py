from unittest import result
import rabbitmq
import subprocess
import atexit
import uuid
import parse



#########################################################
##################### Module state ######################
#########################################################

_initializing = False
_module_initialized = False

_aas_proc = None
_asvis_proc = None

_aas_log = None
_asvis_log = None

_rabbitmq_connection = None
_ctrl_channel: rabbitmq.Channel


#########################################################
####################### Protocol ########################
#########################################################

stop_cmd = 'stop.'



#########################################################
######################### API ###########################
#########################################################

structure_creation_timeout_seconds = 3


class AAS:
    def __init__(self, structure_type):
        global _module_initialized, _rabbitmq_connection, _ctrl_channel

        if not _module_initialized:
            raise RuntimeError('ERROR: associata module is not initialized. Call associata.init() first.')

        self.id = str(uuid.uuid4())
        self._channel = rabbitmq.Channel(_rabbitmq_connection, self.id)


        backend_structure_created = False
        vis_structure_created = False

        def on_message_callback(body):
            nonlocal backend_structure_created, vis_structure_created

            if body == b'"structure_created"':
                backend_structure_created = True
                return True
            elif body == b'"vis_setup_done"':
                vis_structure_created = True
                return True
            return False

        cmd_body = f'{{new_structure,{structure_type},"{self.id}"}}.'

        _ctrl_channel.send_vis(cmd_body)
        self._channel.receive_vis(structure_creation_timeout_seconds, on_message_callback)
        
        if not vis_structure_created:
            self._channel.close()
            raise RuntimeError('ERROR: AAS visualization setup failed')


        _ctrl_channel.send_backend(cmd_body)
        self._channel.receive_backend(structure_creation_timeout_seconds, on_message_callback)

        if not backend_structure_created:
            self._channel.close()
            raise RuntimeError('ERROR: AAS structure creation failed')
        

    def stop(self):
        structure_stopped = False
        vis_stopped = False

        def on_message_callback(body):
            nonlocal structure_stopped, vis_stopped

            print(f'Got message: {body}')

            if body == b'"structure_stopped"':
                structure_stopped = True
                return True
            elif body == b'"vis_stopped"':
                vis_stopped = True
                return True
            return False

        self._channel.send_backend(stop_cmd)
        self._channel.send_vis(stop_cmd)

        self._channel.receive_backend(structure_creation_timeout_seconds, on_message_callback)
        self._channel.receive_vis(structure_creation_timeout_seconds, on_message_callback)
        
        self._channel.close()

        if not structure_stopped:
            print('WARNING: AAS structure stopping failed')
        
        if not vis_stopped:
            print('WARNING: AAS visualization stopping for structure failed')
        
        
    def _dict_to_erl_map_repr(self, d):
        return '#{' + ', '.join([f'"{k}" => {v}' for k, v in d.items()]) + '}'
    


class AGDS(AAS):
    def __init__(self):
        super().__init__('agds')
        self._stimulated = False
        
        self._inference_timeout_sec = 5
        self._poisoning_timeout_sec = 5
        self._query_timeout_sec = 5
        
        self._query_response = None

    def add_numerical_vng(self, name, epsilon):
        self._channel.send_backend(f'{{add_vng,"{name}",numerical,{epsilon}}}.')

    def add_categorical_vng(self, name):
        self._channel.send_backend(f'{{add_vng,"{name}",categorical}}.')

    def add_observation(self, vng_values):
        self._channel.send_backend(f'{{add_observation,{self._dict_to_erl_map_repr(vng_values)}}}.')
    
    def infere(self, inference_setup, max_depth):
        self.reset_excitation()
        self._stimulated = True
        self._channel.send_backend(f'{{infere,{inference_setup.get_formatted_entries()},{max_depth}}}.')
        self._channel.receive_backend(self._inference_timeout_sec, lambda message: message == b'"inference_finished"')

    def poison(self, inference_setup, max_depth, deadly_dose):
        self.reset_excitation()
        self._stimulated = True
        self._channel.send_backend(f'{{poison,{inference_setup.get_formatted_entries()},{max_depth},{deadly_dose}}}.')
        self._channel.receive_backend(self._poisoning_timeout_sec, lambda message: message == b'"poisoning_finished"')

    def reset_excitation(self):
        if self._stimulated:
            self._stimulated = False
            self._channel.send_backend(f'reset_excitation.')
            # wait for completion?

    def get_excitations_for_vng(self, vng_name):
        return self._query_backend(f'{{get_excitation,vng,"{vng_name}"}}.', self._parse_excitation_response)
    
    def get_excitations_for_ong(self):
        return self._query_backend('{get_excitation,ong}.', self._parse_excitation_response)

    def get_vn_neighbours(self, vng_name, vn_value):
        return self._query_backend(f'{{get_neighbours,vn,"{vng_name}",{vn_value}}}.', self._parse_neighbours_response)
    
    def get_on_neighbours(self, on_index):
        return self._query_backend(f'{{get_neighbours,on,{on_index}}}.', self._parse_neighbours_response)
        

    def _query_backend(self, query, callback):
        self._channel.send_backend(query)
        self._channel.receive_backend(self._query_timeout_sec, callback)
        return self._query_response



    def _parse_excitation_response(self, message):
        return self._parse_response(message, '{{excitations,[{exc_values}]}}', 
                                    lambda exc: {r[0].strip(): float(r[1].strip()) for r in parse.findall('{{{},{}}}', exc.named['exc_values'])})
    
    def _parse_neighbours_response(self, message):
        return self._parse_response(message, '{{neighbours,{neighbours}}}', 
                                    lambda neighs: [('vn', vn.named['vng_name'], vn.named['repr_value']) for vn in parse.findall('{{vn,"{vng_name}",{repr_value}}}', neighs.named['neighbours'])]
                                    + [('on', int(on.named['on_index'])) for on in parse.findall('{{on,{on_index}}}', neighs.named['neighbours'])])
    
    
    def _parse_response(self, message, pattern, transformation):
        print(f'Got message: {message}')

        if result := parse.parse(pattern, message.decode()):
            self._query_response = transformation(result)
        else:
            self._query_response = None

        return True



class InferenceSetup:
    def __init__(self):
        self.stimulated_vns = {}
        self.stimulated_ons = {}
        self.vngs_stimulated_with_repr_values = []

    def stimulate_vn(self, vng_name, value, stimuli=1.0):
        self.stimulated_vns[(vng_name, value)] = stimuli
        
    def stimulate_on(self, on_index, stimuli=1.0):
        self.stimulated_ons[on_index] = stimuli

    def stimulate_vng_with_repr_values(self, vng_name):
        self.vngs_stimulated_with_repr_values.append(vng_name)

    def get_formatted_entries(self):
        vn_entries = [f'{{vn, "{vng}", {value}}} => {stimuli}' for (vng, value), stimuli in self.stimulated_vns.items()]
        on_entries = [f'{{on, {index}}} => {stimuli}' for index, stimuli in self.stimulated_ons.items()]
        vng_repr_entries = [f'{{vng, "{vng}"}} => repr_value' for vng in self.vngs_stimulated_with_repr_values]

        return f'#{{{", ".join(vn_entries + on_entries + vng_repr_entries)}}}'



def init():
    global _initializing, _module_initialized
    
    if not _module_initialized:

        _initializing = True

        _setup_rabbitmq()
        _start_backend()

        if _initializing:
            _initializing = False
            _module_initialized = True


def stop():
    global _initializing, _module_initialized, _rabbitmq_connection
    
    if _initializing or _module_initialized:
        _stop_backend()
        rabbitmq.disconnect(_rabbitmq_connection)

        _close_output_files()

        _initializing = False
        _module_initialized = False



#########################################################
####################### RabbitMQ ########################
#########################################################


def _setup_rabbitmq():
    global _rabbitmq_connection, _ctrl_channel

    _rabbitmq_connection = rabbitmq.connect()
    _ctrl_channel = rabbitmq.Channel(_rabbitmq_connection, 'ctrl')



#########################################################
##################### Subprocesses ######################
#########################################################

backend_start_timeout_seconds = 5
vis_start_timeout_seconds = 5

backend_stop_timeout_seconds = 2
vis_stop_timeout_seconds = 2


def _start_backend():
    # start main aas erlang process and visualization handler
    # both programs assume that necessary RabbitMQ topology has already been created

    global _aas_proc, _aas_log, _asvis_proc, _asvis_log

    try:
        _asvis_proc, _asvis_log = _start_subprocess([R'C:\Users\adams\Doktorat\aasociata\asvis\asvis.cmd', R'C:\Users\adams\Doktorat\RL\experiments'], vis_start_timeout_seconds, 'vis')
        _aas_proc, _aas_log = _start_subprocess([R'C:\Users\adams\Doktorat\aasociata\aas-engine\_build\default\rel\aas\bin\aas.cmd', 'foreground'], backend_start_timeout_seconds, 'backend')
    except RuntimeError as e:
        print(f'ERROR: Failed to start backend: {e}')
        stop()


def _stop_backend():
    global _ctrl_channel, _aas_proc, _asvis_proc

    _ctrl_channel.send_backend(stop_cmd)
    _ctrl_channel.send_vis(stop_cmd)

    if _aas_proc is not None:
        _stop_subprocess(_aas_proc, backend_stop_timeout_seconds, 'backend')
        _aas_proc = None

    if _asvis_proc is not None:
        _stop_subprocess(_asvis_proc, vis_stop_timeout_seconds, 'vis')
        _asvis_proc = None


def _start_subprocess(cmd, timeout_seconds, name):
    """Starts a subprocess and waits for it to report readiness within specified timeout."""
    # cmd should be passed as list, eg. ['erl', '-noshell', 'aas']

    global _ctrl_channel

    output = open(f'output-{name}.log', 'w')

    subproc = subprocess.Popen(cmd, stdout=output, stderr=output, shell=True, text=True)

    def on_message_callback(body):
        print(f'Got message: {body}')
        if is_started_message(body):
            print(f'Subprocess "{name}" started successfully')
            return True
        return False

    def on_timeout_callback():
        print(f'ERROR: subprocess "{name}" did not report readiness within timeout ({timeout_seconds}s)')
        subproc.terminate()
        raise RuntimeError(f'Subprocess "{name}" did not start within timeout')
    
    def is_started_message(body):
        return body == b'"started"'

    if name == 'backend':   # TODO: should be done in some self-respecting way...
        _ctrl_channel.receive_backend(timeout_seconds, on_message_callback, on_timeout_callback, is_started_message)
    elif name == 'vis':
        _ctrl_channel.receive_vis(timeout_seconds, on_message_callback, on_timeout_callback, is_started_message)
    else:
        raise ValueError(f'Unknown subprocess name: {name}')
            
    return subproc, output


def _stop_subprocess(subproc, timeout_seconds, subprocess_name=None):
    try:
        subproc.wait(timeout_seconds)
        print(f'Subprocess "{subprocess_name}" stopped successfully')
    except subprocess.TimeoutExpired:
        print(f'WARNING: Subprocess "{subprocess_name}" did not stop within timeout ({timeout_seconds}s)')
        subproc.terminate()


def _close_output_files():
    if _aas_log is not None:
        _aas_log.close()
    if _asvis_log is not None:
        _asvis_log.close()


def _print_subprocess_output(subproc, name):
    stdout, stderr = subproc.communicate()
    print(f'Subprocess "{name}" output:')
    print(stdout.decode())
    print(f'Subprocess "{name}" error messages:')
    print(stderr.decode())


#########################################################
###################### Internals ########################
#########################################################
        
atexit.register(stop)
