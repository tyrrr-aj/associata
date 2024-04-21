import gymnasium as gym
import associata
from rl import *


# ############# Cart pole #############
state_space_bounds = np.array([[-2.5, -3.0, -0.25, -3.0], [2.5, 3.0, 0.25, 3.0]])
# state_space_epsilon = np.array([0.5, 0.5, 0.05, 0.5])
state_space_epsilon = np.array([0.1, 0.1, 0.01, 0.1])
state_space_feature_names = ['x_pos', 'x_speed', 'ang_pos', 'ang_speed']
action_space = np.array([0, 1])

# ############# Sarsa-AGDS #############

associata.init()
sarsa_agds = SarsaAGDS(state_space_feature_names, state_space_bounds, state_space_epsilon, action_space)

env = gym.make("CartPole-v1", render_mode="rgb_array")
observation, info = env.reset(seed=42)

reward = None

for _ in range(10):
    action = sarsa_agds.step(observation, reward)[0]
    observation, reward, terminated, truncated, info = env.step(action)

    if terminated or truncated:
        observation, info = env.reset()
        sarsa_agds.reset_episode()
        reward = None

sarsa_agds.reset_episode(False)
env.close()

sarsa_agds.stop()
associata.stop()
