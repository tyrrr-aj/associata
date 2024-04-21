
import numpy as np
import associata
from abc import ABC, abstractmethod
import time


class TD(ABC):
    def __init__(self, state_space_bounds, state_space_epsilon, action_space, alpha=0.2, gamma=1.0, greedey_epsilon=0.1):
        self._state_space_bounds = state_space_bounds
        self._state_space_epsilon = state_space_epsilon
        self._state_space_shape = np.ceil((state_space_bounds[1, :] - state_space_bounds[0, :]) / state_space_epsilon).astype('int')
        self._action_space = action_space
        
        self._init_q()

        self.greedy_epsilon = greedey_epsilon
        self.alpha = alpha
        self.gamma = gamma

        self._last_state = None
        self._last_action = None
        self._acc_reward = 0.0
        self.episode_rewards = []

        self._step_nr = 0

        self._time_origin = time.time()


    def _timestamp(self):
        return f'[{int(time.time() - self._time_origin)}s]'


    def step(self, observation, reward=None):
        print(f'\n=============== Step {self._step_nr} ===============')

        state = self._get_state(observation)

        # print(f'{self._timestamp()} State: {state}\nReward (for prevoius action): {reward}\n')

        action = self._get_action(state)

        print(f'{self._timestamp()} Picked action: {action}')

        if self._last_action is not None and reward is not None:
            self._update_q(state, action, reward)
            self._acc_reward += reward

        self._last_state = state
        self._last_action = action

        self._step_nr += 1

        return action
    

    def reset_episode(self, save_score=True):
        self._last_state = None
        self._last_action = None

        if save_score:
            self.episode_rewards.append(self._acc_reward)
        
        self._acc_reward = 0.0

    @abstractmethod
    def _get_state(self, observation):
        pass

    @abstractmethod
    def _get_action(self, state):
        pass

    @abstractmethod
    def _init_q(self):
        pass

    @abstractmethod
    def _update_q(self, next_state, next_action, reward):
        pass


class Sarsa(TD):
    def _get_state(self, observation):
        indices = np.floor((observation - self._state_space_bounds[0, :]) / self._state_space_epsilon)
        return np.clip(indices, np.zeros(len(self._state_space_shape)), self._state_space_shape - 1).astype('int')


    def _get_action(self, state):
        if np.random.random() < self.greedy_epsilon:
            # exploratory action
            return np.array([np.random.choice(self._action_space)])

        else:
            # exploiting action
            action_values = self.q[*state, :]
            max_action_value = np.max(action_values)
            max_actions = np.argwhere(action_values == max_action_value)
            if (len(max_actions) > 1):
                action_index = np.random.randint(max_actions.shape[0])
            else:
                action_index = 0
            return max_actions[action_index]
        

    def _init_q(self):
        self.q = np.zeros(tuple(self._state_space_shape) + self._action_space.shape)


    def _update_q(self, next_state, next_action, reward):
        self.q[*self._last_state, *self._last_action] += self.alpha * (reward + self.gamma * self.q[*next_state, *next_action] - self.q[*self._last_state, *self._last_action])


class QLearning(TD):
    def _get_state(self, observation):
        indices = np.floor((observation - self._state_space_bounds[0, :]) / self._state_space_epsilon)
        return np.clip(indices, np.zeros(len(self._state_space_shape)), self._state_space_shape - 1).astype('int')


    def _get_action(self, state):
        if np.random.random() < self.greedy_epsilon:
            # exploratory action
            return np.array([np.random.choice(self._action_space)])

        else:
            # exploiting action
            action_values = self.q[*state, :]
            max_action_value = np.max(action_values)
            max_actions = np.argwhere(action_values == max_action_value)
            if (len(max_actions) > 1):
                action_index = np.random.randint(max_actions.shape[0])
            else:
                action_index = 0
            return max_actions[action_index]


    def _init_q(self):
        self.q = np.zeros(tuple(self._state_space_shape) + self._action_space.shape)


    def _update_q(self, next_state, _next_action, reward):
        max_q = np.max(self.q[*next_state, :])
        self.q[*self._last_state, *self._last_action] += self.alpha * (reward + self.gamma * max_q - self.q[*self._last_state, *self._last_action])


class TD_AGDS(TD):
    @abstractmethod
    def _updated_q_value(self, last_sa_value, next_state, next_action, reward):
        pass

    def __init__(self, state_space_feature_names, state_space_bounds, state_space_epsilon, action_space, alpha=0.2, gamma=1.0, greedey_epsilon=0.1):
        self._state_space_feature_names = state_space_feature_names

        self._inference_depth = 2

        super().__init__(state_space_bounds, state_space_epsilon, action_space, alpha=alpha, gamma=gamma, greedey_epsilon=greedey_epsilon)


    def stop(self):
        self.q.stop()


    def _get_state(self, observation):
        return observation


    def _get_action(self, state):
        if np.random.random() < self.greedy_epsilon:
            # exploratory action
            return self._get_random_action()

        else:
            # exploiting action
            best_sa = self._search_for_best_action(state)

            if best_sa is None:
                return self._get_random_action()

            # TODO: handles only one-dimensional action space
            best_sa_neigh_nodes = self.q.get_on_neighbours(best_sa)
            # print(f'Best sa neigh nodes: {best_sa_neigh_nodes}')

            if best_sa_neigh_nodes == []:
                return self._get_random_action()
            
            return np.array([int(float(ef[2])) for ef in best_sa_neigh_nodes if ef[0] == 'vn' and ef[1] == 'action'])
        
        

    def _init_q(self):
        self.q = associata.AGDS()
        for f_name, f_epsilon in zip(self._state_space_feature_names, self._state_space_epsilon):
            self.q.add_numerical_vng(f_name, f_epsilon)
        self.q.add_categorical_vng('value')
        self.q.add_categorical_vng('action', is_action=True)


    def _update_q(self, next_state, next_action, reward):
        last_sa_value = self._search_for_action_value(self._last_state, self._last_action)
        updated_last_sa_value = self._updated_q_value(last_sa_value, next_state, next_action, reward)
        
        last_sa_search = self._setup_search_from_state(self._last_state)
        last_sa_search = self._setup_search_from_action(self._last_action, last_sa_search)
        self.q.poison(last_sa_search, self._inference_depth, 3.0, 1.5)     # TODO: Adjust depth and deadly dose

        # TODO: handles only one-dimensional action space
        new_observation = {vng_name: vng_value for vng_name, vng_value in zip(
                                    self._state_space_feature_names + ['value', 'action'], 
                                    self._last_state.tolist() + [updated_last_sa_value, self._last_action[0]]
                                )}
        self.q.add_observation(new_observation)



    def _infere_and_get_max_from_vng(self, vng_name, setup, depth):
        self.q.infere(setup, depth)
        excitations = self.q.get_excitations_for_vng(vng_name)

        return self._get_maximizing_key(excitations)
    

    def _infere_and_get_max_from_ong(self, setup, depth):
        self.q.infere(setup, depth)
        excitations = self.q.get_excitations_for_ong()

        return self._get_maximizing_key(excitations)

    
    def _get_maximizing_key(self, excitations):
        if excitations is not None and len(excitations) > 0:
            max_excitation = max(excitations.values())
            return np.random.choice([k for k, v in excitations.items() if v == max_excitation])
        else:
            return None


    def _setup_search_from_state(self, state, search=None):
        search = associata.InferenceSetup() if search is None else search
        for f_name, f_value in zip(self._state_space_feature_names, state):
            search.stimulate_vn(f_name, f_value)
        return search
    

    def _setup_search_from_action(self, action, search=None):
        search = associata.InferenceSetup() if search is None else search
        search.stimulate_vn('action', action[0])    # TODO: handles only one-dimensional action space
        return search
    
    def _search_for_best_action(self, state):
        best_action_search = self._setup_search_from_state(state)
        best_action_search.stimulate_vng_with_repr_values('value')

        return self._infere_and_get_max_from_ong(best_action_search, self._inference_depth)
    
    def _search_for_action_value(self, state, action):
        sa_value_search = self._setup_search_from_state(state)
        sa_value_search = self._setup_search_from_action(action, sa_value_search)
        sa_value = self._infere_and_get_max_from_vng('value', sa_value_search, self._inference_depth)

        return float(sa_value) if sa_value is not None else 0.0

    def _get_random_action(self):
        return np.array([np.random.choice(self._action_space)])


class SarsaAGDS(TD_AGDS):
    def _updated_q_value(self, last_sa_value, next_state, next_action, reward):
        next_sa_value = self._search_for_action_value(next_state, next_action)        
        return last_sa_value + self.alpha * (reward + self.gamma * next_sa_value - last_sa_value)


class QLearningAGDS(TD_AGDS):
    def _updated_q_value(self, last_sa_value, next_state, next_action, reward):
        best_next_sa = self._search_for_best_action(next_state)
        
        if best_next_sa is None:
            best_next_sa_value = 0.0
        else:
            best_next_sa_neigh_nodes = self.q.get_on_neighbours(best_next_sa)
            if best_next_sa_neigh_nodes == []:
                best_next_sa_value = 0.0
            else:
                best_next_sa_value = [float(ef[2]) for ef in best_next_sa_neigh_nodes if ef[0] == 'vn' and ef[1] == 'value'][0]

        return last_sa_value + self.alpha * (reward + self.gamma * best_next_sa_value - last_sa_value)
