import asyncio
import random
import sys
import associata

n_vng = 20
n_observations = 5_000
n_inferences = 100

epsilon = 1.0
min_vn_value = 0
max_vn_value = 400

min_passed_stimulus = 0.5

seed = 42


def scale_to_vn_value(value_0_to_1): 
    return value_0_to_1 * (max_vn_value - min_vn_value) + min_vn_value


def get_vng_definitions():
    for vng_idx in range(n_vng):
        yield f'vng{vng_idx}', epsilon


def get_random_data():
    for _ in range(n_observations):
        yield {vng_name: scale_to_vn_value(random.random()) for vng_name, _epsilon in get_vng_definitions()}


def get_random_queries():
    node_group_modes = {vng_name: associata.NodeGroupMode.transitive for vng_name, _epsilon in get_vng_definitions()}
    node_group_modes['ong'] = associata.NodeGroupMode.transitive

    for _ in range(n_inferences):
        query = associata.StimulationSetup(node_group_modes)

        for vng_name, _epsilon in get_vng_definitions():
            query.stimulate_vn(vng_name, scale_to_vn_value(random.random()))
            
        yield query


async def build(agds):
    for name, epsilon in get_vng_definitions():
        await agds.add_numerical_vng(name, epsilon)

    for exp_step, data in enumerate(get_random_data()):
        await agds.add_observation(data, exp_step)


async def infere(agds):
    experiment_step = n_observations
    stimulation_name = 'infere'

    for query_idx, query in enumerate(get_random_queries()):
        await agds.infere(query, min_passed_stimulus, experiment_step, stimulation_name)

        if (query_idx + 1) % 10 == 0:
            print(f'\tfinished {query_idx + 1} queries')


async def main():
    n_cores = int(sys.argv[1]) if len(sys.argv) > 1 else 16

    await associata.init()
    await associata.set_n_cpu_cores(n_cores)

    agds = await associata.create_agds()

    random.seed(seed)

    await build(agds)
    print('Build completed')

    print('Inference:')
    await infere(agds)

    total_inference_time_ms = await agds.get_inference_time_ms()
    avg_inference_time_ms = total_inference_time_ms / n_inferences

    n_nodes = await agds.get_structure_size()

    print(f'N_nodes: {n_nodes}\nAvg inference time: {avg_inference_time_ms}ms')



if __name__ == '__main__':
    asyncio.run(main())

