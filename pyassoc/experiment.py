import gymnasium as gym
import associata
from rl import *
import asyncio
import numpy as np
import matplotlib.pyplot as plt
import datetime
import os


# ############# Cart pole #############
state_space_bounds = np.array([[-2.5, -3.0, -0.25, -3.0], [2.5, 3.0, 0.25, 3.0]])
# state_space_epsilon = np.array([0.5, 0.5, 0.05, 0.5])
# state_space_epsilon = np.array([0.1, 0.1, 0.01, 0.1])
state_space_epsilon = np.array([0.5, 0.5, 0.05, 0.5])
state_space_feature_names = ['x_pos', 'x_speed', 'ang_pos', 'ang_speed']
action_space = np.array([0, 1])


# ############# Matplotlib #############
def save_reward_plot(rewards, structure_size_history, mean_width=1):
    mean_rewards = np.convolve(rewards, np.ones(mean_width) / mean_width, mode='valid') if mean_width > 1 else rewards
    mean_structure_size_history = np.convolve(structure_size_history, np.ones(mean_width) / mean_width, mode='valid') if mean_width > 1 else structure_size_history
    
    fig, ax1 = plt.subplots()
    ax2 = ax1.twinx()

    ax1.set_xlabel('Episode')

    ax1.set_ylabel('Reward', color='blue')
    ax1.tick_params(axis='y', labelcolor='blue')
    ax1.plot(mean_rewards)

    ax2.set_ylabel('Structure size', color='red')
    ax2.tick_params(axis='y', labelcolor='red')
    ax2.plot(mean_structure_size_history, color='red')

    plt.title(f'Episode rewards and structure size [mean{mean_width}]')
    day_dir = f'experiments\\{datetime.datetime.now().strftime("%Y-%m-%d")}'
    day_subdirs = [d for d in os.listdir(day_dir) if os.path.isdir(os.path.join(day_dir, d))]
    day_subdirs.sort()
    exp_dir = day_subdirs[-1]
    plt.savefig(f'{day_dir}\\{exp_dir}\\rewards[mean{mean_width}].png')


# ############# Sarsa-AGDS #############

async def run():
    await associata.init()
    await associata.set_n_cpu_cores(2)
    sarsa_agds = SarsaAGDS(state_space_feature_names, state_space_bounds, state_space_epsilon, action_space, greedey_epsilon=1.0)

    env = gym.make("CartPole-v1", render_mode="rgb_array")
    observation, info = env.reset(seed=42)

    reward = None

    # start_time = datetime.datetime.now()
    # milestones = []

    n_steps = 3000

    for step_no in range(n_steps):
        action = (await sarsa_agds.step(observation, reward))[0]
        observation, reward, terminated, truncated, info = env.step(action)

        if terminated or truncated:
            observation, info = env.reset()
            final_reward = 0 if terminated else None
            await sarsa_agds.reset_episode(final_reward=final_reward)
            reward = None

        # if step_no % 1000 == 0:
        #     milestones.append((step_no, datetime.datetime.now() - start_time))

        if True: #step_no > 1000:
            # await asyncio.sleep(3)
            # await sarsa_agds.export_stimulation(step_no, 'pick_action')
            # await sarsa_agds.export_stimulation(step_no, 'last_sa_value_search')
            # await sarsa_agds.export_stimulation(step_no, 'next_sa_value_search')
            # await sarsa_agds.export_stimulation(step_no, 'poison')
            ...

    await sarsa_agds.reset_episode(save_score=False)
    env.close()

    # milestones.append((n_steps, datetime.datetime.now() - start_time))

    # await asyncio.sleep(10)

    # await sarsa_agds.export_topology()
    # await sarsa_agds.export_stimulation(20, 'pick_action')
    # await sarsa_agds.export_stimulation(20, 'last_sa_value_search')
    # await sarsa_agds.export_stimulation(20, 'next_sa_value_search')
    # await sarsa_agds.export_stimulation(20, 'poison')

    # await asyncio.sleep(2)

    # save_reward_plot(sarsa_agds.episode_rewards, sarsa_agds.structure_size_history)
    # save_reward_plot(sarsa_agds.episode_rewards, sarsa_agds.structure_size_history, mean_width=150)
    # save_reward_plot(sarsa_agds.episode_rewards, sarsa_agds.structure_size_history, mean_width=350)
    # save_reward_plot(sarsa_agds.episode_rewards, sarsa_agds.structure_size_history, mean_width=500)

    # with open('experiments\\milestones.txt', 'a') as f:
    #     for n_steps, elapsed_time in milestones:
    #         f.write(f'{n_steps}: {int(elapsed_time)}s\n')

    await sarsa_agds.stop()
    await associata.stop()


if __name__ == "__main__":
    asyncio.run(run())
