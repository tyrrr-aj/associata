from unittest import result
import pyrlang_channel
from term import Atom
import subprocess
import atexit
import uuid
import parse
import os
import asyncio



#########################################################
##################### Module state ######################
#########################################################

_initializing = False
_module_initialized = False

_aas_proc = None
_asvis_proc = None

_aas_log = None
_asvis_log = None

_aas_log_err = None
_asvis_log_err = None

_connection = None
_ctrl_channel: pyrlang_channel.Channel

_next_id = 0


#########################################################
####################### Topology ########################
#########################################################

client_node_name = 'associata@Beast'
backend_node_name = 'aas@Beast'
vis_node_name = 'aas_vis@Beast'

ctrl_channel_name = 'ctrl'

cookie = 'aas_cookie'


#########################################################
######################### API ###########################
#########################################################

structure_creation_timeout_seconds = 3


async def init():
    global _initializing, _module_initialized
    
    if not _module_initialized:

        _initializing = True

        _setup_ipc()
        await _start_backend()

        if _initializing:
            _initializing = False
            _module_initialized = True


async def stop():
    global _initializing, _module_initialized, _connection
    
    if _initializing or _module_initialized:
        await _stop_backend()

        _ctrl_channel.close()
        pyrlang_channel.disconnect(_connection)

        _close_output_files()

        _initializing = False
        _module_initialized = False


async def create_agds():
    global _module_initialized

    if not _module_initialized:
        raise RuntimeError('ERROR: associata module is not initialized. Call associata.init() first.')

    new_agds_id = _create_id('agds')
    new_agds_channel = pyrlang_channel.Channel(_connection, new_agds_id)

    new_agds = AGDS(new_agds_id, new_agds_channel)

    await _setup_ipc_for_structure(new_agds)
    return new_agds


async def _setup_ipc_for_structure(new_structure):
    global _ctrl_channel

    cmd_body = (Atom('new_structure'), Atom(new_structure.structure_type), new_structure.id)

    await _ctrl_channel.send_vis_async(cmd_body)

    vis_response = await new_structure._channel.receive_async(structure_creation_timeout_seconds)
    if (vis_response != 'vis_setup_done'):
        raise RuntimeError('ERROR: AAS visualization setup failed')
    

    await _ctrl_channel.send_backend_async(cmd_body)
    
    backend_response = await new_structure._channel.receive_async(structure_creation_timeout_seconds)
    if (backend_response != 'structure_created'):
        raise RuntimeError('ERROR: AAS structure creation failed')


def _create_id(structure_type):
    global _next_id
    id = f'{structure_type}{_next_id}'
    _next_id += 1
    return id


class AAS:
    def __init__(self, structure_type, id, channel):        
        self.structure_type = structure_type
        self.id = id
        self._channel = channel
        

    async def stop(self):
        await self._channel.send_backend_async(Atom('stop'))
        backend_res = await self._channel.receive_async(structure_creation_timeout_seconds)

        await self._channel.send_vis_async(Atom('stop'))
        vis_res = await self._channel.receive_async(structure_creation_timeout_seconds)
        
        self._channel.close()

        if backend_res != 'structure_stopped':
            print('WARNING: AAS structure stopping failed')
        
        if vis_res != 'vis_stopped':
            print('WARNING: AAS visualization stopping for structure failed')


class AGDS(AAS):
    def __init__(self, id, channel):
        super().__init__('agds', id, channel)
        self._stimulated = False
        
        self._inference_timeout_sec = 5
        self._poisoning_timeout_sec = 5
        self._query_timeout_sec = 5

    async def add_numerical_vng(self, name, epsilon, is_action=False):
        await self._channel.send_backend_async((
            Atom('add_vng'), 
            name, 
            Atom('numerical'), 
            float(epsilon), 
            is_action))

    async def add_categorical_vng(self, name, is_action=False):
        await self._channel.send_backend_async((
            Atom('add_vng'), 
            name, 
            Atom('categorical'), 
            is_action))

    async def add_observation(self, vng_values):
        add_observation_cmd = (Atom('add_observation'), vng_values)
        # print(f'Adding observation: {add_observation_cmd}')
        await self._channel.send_backend_async(add_observation_cmd)
    
    async def infere(self, inference_setup, max_depth):
        # await self.reset_excitation()
        self._stimulated = True
        # print(f'sending inference: {inference_setup.get_entries()}')
        await self._channel.send_backend_async((Atom('infere'), inference_setup.get_entries(), max_depth))
        # print('waiting for inference response')
        await self._channel.receive_async(self._inference_timeout_sec)    # 'inference_finished'
        # print(f'inference finished')

    async def poison(self, inference_setup, max_depth, deadly_dose, min_acc_dose):
        # await self.reset_excitation()
        self._stimulated = True
        # print(f'sending poison: {inference_setup.get_entries()}')
        await self._channel.send_backend_async((
            Atom('poison'), 
            inference_setup.get_entries(), 
            max_depth, 
            float(deadly_dose), 
            float(min_acc_dose)))
        # print('waiting for poisoning response')
        await self._channel.receive_async(self._poisoning_timeout_sec)    # 'poisoning_finished'
        # print(f'poisoning finished')

    async def reset_excitation(self):
        if self._stimulated:
            self._stimulated = False
            await self._channel.send_backend_async(Atom('reset_excitation'))
            await self._channel.receive_async(self._query_timeout_sec)    # excitation_reset_finished

    async def get_excitations_for_vng(self, vng_name):
        # print(f'getting excitations for vng: {vng_name}')
        exc = await self._query_backend((Atom('get_excitation'), Atom('vng'), vng_name), self._parse_excitation_response)
        # print(f'got excitations for vng {vng_name}: {exc}')
        return exc


    async def get_excitations_for_ong(self):
        # print('getting excitations for ong')
        exc = await self._query_backend((Atom('get_excitation'), Atom('ong')), self._parse_excitation_response)
        # print(f'got excitations for ong: {exc}')
        return exc

    async def get_vn_neighbours(self, vng_name, vn_value):
        # print(f'getting neighbours for vn: {vng_name}={vn_value}')
        neighs = await self._query_backend((Atom('get_neighbours'), Atom('vn'), vng_name, float(vn_value)), self._parse_neighbours_response)
        # print(f'got neighbours for vn {vng_name}={vn_value}: {neighs}')
        return neighs
    
    async def get_on_neighbours(self, on_index):
        # print(f'getting neighbours for on: {on_index}')
        neighs = await self._query_backend((Atom('get_neighbours'), Atom('on'), on_index), self._parse_neighbours_response)
        # print(f'got neighbours for on {on_index}: {neighs}')
        return neighs
        

    async def _query_backend(self, query, parse_response_fn):
        await self._channel.send_backend_async(query)
        response = await self._channel.receive_async(self._query_timeout_sec)
        return parse_response_fn(response)


    def _parse_excitation_response(self, message):
        match message:
            case (Atom('excitations'), exc_values):
                return exc_values
            case _:
                return None
    
    def _parse_neighbours_response(self, message):
        match message:
            case (Atom('neighbours'), neighbours):
                return neighbours
            case _:
                return None



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

    def get_entries(self):
        vn_entries = {(Atom('vn'), vng, float(value)) : stimuli for (vng, value), stimuli in self.stimulated_vns.items()}
        on_entries = {(Atom('on'), index) : stimuli for index, stimuli in self.stimulated_ons.items()}
        vng_repr_entries = {(Atom('vng'), vng) : Atom('repr_value') for vng in self.vngs_stimulated_with_repr_values}

        return vn_entries | on_entries | vng_repr_entries
    


#########################################################
#################### Communication ######################
#########################################################


def _setup_ipc():
    global _connection, _ctrl_channel

    _connection = pyrlang_channel.connect(client_node_name, backend_node_name, vis_node_name, cookie)
    _ctrl_channel = pyrlang_channel.Channel(_connection, ctrl_channel_name)



#########################################################
##################### Subprocesses ######################
#########################################################

backend_start_timeout_seconds = 3
vis_start_timeout_seconds = 3

backend_stop_timeout_seconds = 3
vis_stop_timeout_seconds = 3


async def _start_backend():
    # start main aas erlang process and visualization handler

    global _aas_proc, _aas_log, _asvis_proc, _asvis_log, _aas_log_err, _asvis_log_err

    try:
        _asvis_proc, _asvis_log, _asvis_log_err = await _start_subprocess(
            # [R'C:\Users\adams\Doktorat\aasociata\asvis\asvis.cmd', R'C:\Users\adams\Doktorat\aasociata\pyassoc\experiments', client_node_name, vis_node_name, cookie], 
            [R'python', R'C:\Users\adams\Doktorat\aasociata\asvis\src\asvis.py', R'C:\Users\adams\Doktorat\aasociata\pyassoc\experiments', client_node_name, vis_node_name, cookie], 
            vis_start_timeout_seconds, 
            'vis')
        
        _aas_proc, _aas_log, _aas_log_err = await _start_subprocess(
            [R'C:\Users\adams\Doktorat\aasociata\aas-engine\_build\default\rel\aas\bin\aas.cmd', 'foreground'], 
            backend_start_timeout_seconds, 
            'backend')
        
    except RuntimeError as e:
        print(f'ERROR: Failed to start backend: {e}')
        await stop()


async def _stop_backend():
    global _ctrl_channel, _aas_proc, _asvis_proc

    await _ctrl_channel.send_backend_async(Atom('stop'))
    await _ctrl_channel.send_vis_async(Atom('stop'))

    if _aas_proc is not None:
        _stop_subprocess(_aas_proc, backend_stop_timeout_seconds, 'backend')
        _aas_proc = None

    if _asvis_proc is not None:
        _stop_subprocess(_asvis_proc, vis_stop_timeout_seconds, 'vis')
        _asvis_proc = None


async def _start_subprocess(cmd, timeout_seconds, name):
    """Starts a subprocess and waits for it to report readiness within specified timeout."""
    # cmd should be passed as list, eg. ['erl', '-noshell', 'aas']

    global _ctrl_channel

    env = os.environ.copy()
    env["PYTHONUNBUFFERED"] = "1"

    output = open(f'output-{name}.log', 'w')
    output_err = open(f'output-{name}-err.log', 'w')
    subproc = subprocess.Popen(cmd, stdout=output, stderr=output_err, shell=True, text=True, env=env)

    if (await _ctrl_channel.receive_async(timeout_seconds) != Atom('started')):
        subproc.terminate()
        raise RuntimeError(f'Subprocess "{name}" did not start within timeout')
    
    return subproc, output, output_err


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
    if _aas_log_err is not None:
        _aas_log_err.close()
    if _asvis_log is not None:
        _asvis_log.close()
    if _asvis_log_err is not None:
        _asvis_log_err.close()


def _print_subprocess_output(subproc, name):
    stdout, stderr = subproc.communicate()
    print(f'Subprocess "{name}" output:')
    print(stdout.decode())
    print(f'Subprocess "{name}" error messages:')
    print(stderr.decode())


#########################################################
###################### Internals ########################
#########################################################
        
# atexit.register(stop)
