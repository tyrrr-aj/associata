import asyncio
import random
import sys
import associata

n_vng = 20
n_observations = 10_000
n_inferences = 100

epsilon = 1.0
min_vn_value = 0
max_vn_value = 400

min_passed_stimulus = 0.5

special_epsilon = 100
use_categorical = False

seed = 42

min_n_cpu_cores = 1
max_n_cpu_cores = 256
inc_n_cpu_core_factor = 2


def scale_to_vn_value(value_0_to_1, as_int=False): 
    scaled_value = value_0_to_1 * (max_vn_value - min_vn_value) + min_vn_value
    return round(scaled_value) if as_int else scaled_value


def get_vng_definitions(special_epsilon=None, use_categorical=False):
    n_regular_vngs = n_vng if special_epsilon == None else n_vng - 1

    for vng_idx in range(n_regular_vngs):
        is_categorical = use_categorical and vng_idx % 2 == 0
        yield f'vng{vng_idx}', is_categorical, epsilon

    if special_epsilon != None:
        yield f'vng{n_regular_vngs}', False, special_epsilon


def get_random_data(categoricals_used):
    for _ in range(n_observations):
        yield {vng_name: scale_to_vn_value(random.random(), is_categorical) for vng_name, is_categorical, _epsilon in get_vng_definitions(use_categorical=categoricals_used)}


def get_random_queries(categoricals_used):
    node_group_modes = {vng_name: associata.NodeGroupMode.transitive for vng_name, _is_categorical, _epsilon in get_vng_definitions()}
    node_group_modes['ong'] = associata.NodeGroupMode.transitive

    for _ in range(n_inferences):
        query = associata.StimulationSetup(node_group_modes)

        for vng_name, is_categorical, _epsilon in get_vng_definitions(use_categorical=categoricals_used):
            query.stimulate_vn(vng_name, scale_to_vn_value(random.random(), is_categorical))
            
        yield query


async def build(agds, special_epsilon=None, use_categorical=False):
    for name, is_categorical, epsilon in get_vng_definitions(special_epsilon, use_categorical):
        if is_categorical:
            await agds.add_categorical_vng(name)
        else:
            await agds.add_numerical_vng(name, epsilon)

    for exp_step, data in enumerate(get_random_data(use_categorical)):
        await agds.add_observation(data, exp_step)


async def infere(agds, categoricals_used=False):
    experiment_step = n_observations
    stimulation_name = 'infere'

    for query_idx, query in enumerate(get_random_queries(categoricals_used)):
        await agds.infere(query, min_passed_stimulus, experiment_step, stimulation_name)

        if (query_idx + 1) % 10 == 0:
            print(f'\tfinished {query_idx + 1} queries')


async def main():
    # n_cores = int(sys.argv[1]) if len(sys.argv) > 1 else 16

    await associata.init()
    # await associata.set_n_cpu_cores(n_cores)

    agds = await associata.create_agds()

    random.seed(seed)

    await build(agds, special_epsilon, use_categorical)
    print('Build completed')

    n_nodes = await agds.get_structure_size()
    print(f'N_nodes: {n_nodes}')

    n_cores = min_n_cpu_cores
    while n_cores <= max_n_cpu_cores:
        print('\n' + '=' * 15)
        print(f'Inference (n_cores: {n_cores}):')
        
        await associata.set_n_cpu_cores(n_cores)
        await agds.reset_inference_time()

        await infere(agds, use_categorical)

        total_inference_time_ms = await agds.get_inference_time_ms()

        try:
            avg_inference_time_ms = total_inference_time_ms / n_inferences
            print(f'Avg inference time: {avg_inference_time_ms}ms')
        except:
            print('Warning: failed to read inference time from AGDS')

        n_cores *= inc_n_cpu_core_factor



if __name__ == '__main__':
    asyncio.run(main())

