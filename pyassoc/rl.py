
from matplotlib import pyplot as plt
import numpy as np
import associata
from abc import ABC, abstractmethod
import time
from plot_multidim import plot_3d_function_slice, plot_interactive_slices 


class TD(ABC):
    def __init__(self, state_space_bounds, state_space_epsilon, action_space, alpha=0.2, gamma=1.0, greedy_epsilon=0.1, state_space_feature_names=None):
        self._state_space_bounds = state_space_bounds
        self._state_space_epsilon = state_space_epsilon
        self._state_space_shape = np.ceil((state_space_bounds[1, :] - state_space_bounds[0, :]) / state_space_epsilon).astype('int')
        self._action_space = action_space

        if state_space_feature_names is not None:
            self._state_space_feature_names = np.array(state_space_feature_names)
        else:
            self._state_space_feature_names = np.array([f'state_{i}' for i in range(len(self._state_space_shape))])
        
        self._is_initialized = False

        self.greedy_epsilon = greedy_epsilon
        self.alpha = alpha
        self.gamma = gamma

        self._last_state = None
        self._last_action = None
        self._acc_reward = 0.0
        self.episode_rewards = []

        self._step_nr = 0

        self._time_origin = time.time()

        self._actions_taken_history = []


    def _timestamp(self):
        return f'[{int(time.time() - self._time_origin)}s]'


    async def step(self, observation, reward=None):
        if not self._is_initialized:
            await self._init_q()
            self._is_initialized = True

        print(f'\n=============== Step {self._step_nr} ===============')

        state = await self._get_state(observation)

        # print(f'{self._timestamp()} State: {state}\nReward (for prevoius action): {reward}\n')

        action = await self._get_action(state)
        self._actions_taken_history.append(action)

        print(f'{self._timestamp()} Picked action: {action}')

        if self._last_action is not None and reward is not None:
            await self._update_q(state, action, reward)
            self._acc_reward += reward

        self._last_state = state
        self._last_action = action

        self._step_nr += 1

        return action
    

    async def reset_episode(self, final_reward=None, save_score=True):
        if final_reward is not None and self._last_state is not None and self._last_action is not None:
            await self._set_known_q_value(self._last_state, self._last_action, final_reward)

        self._last_state = None
        self._last_action = None

        if save_score:
            self.episode_rewards.append(self._acc_reward)
        
        self._acc_reward = 0.0


    async def plot_policy(self, state_dims=(0, 1), action_dim=0, output_file_name=None):
        async def policy(x):
            state = await self._get_state(x)
            return await self._get_action(state, epsilon=0)

        fig1, ax1 = await plot_3d_function_slice(
            policy,
            self._state_space_bounds[0, :],
            self._state_space_bounds[1, :],
            self._state_space_epsilon,
            state_dims,
            action_dim,
            input_dim_names=self._state_space_feature_names,
            output_dim_name='action',
            title='Policy (3D slice)'
        )

        if output_file_name is None:
            plt.show()
        else:
            plt.savefig(output_file_name + '.png')

        fig2 = await plot_interactive_slices(
            policy,
            self._state_space_bounds[0, :], 
            self._state_space_bounds[1, :],
            self._state_space_epsilon,
            state_dims,
            action_dim,
            input_dim_names=self._state_space_feature_names,
            output_dim_name='action'
        )

        if output_file_name is None:
            plt.show()
        else:
            plt.savefig(output_file_name + '_slices.png')

    @abstractmethod
    async def _get_state(self, observation):
        pass

    @abstractmethod
    async def _get_action(self, state, epsilon=None):
        pass

    @abstractmethod
    async def _init_q(self):
        pass

    @abstractmethod
    async def _update_q(self, next_state, next_action, reward):
        pass

    @abstractmethod
    async def _set_known_q_value(self, state, reward):
        pass


class Sarsa(TD):
    def __init__(self, state_space_bounds, state_space_epsilon, action_space, alpha=0.2, gamma=1, greedy_epsilon=0.1, state_space_feature_names=None):
        self._dont_know_history = []
        self._exploratory_action_history = []
        super().__init__(state_space_bounds, state_space_epsilon, action_space, alpha, gamma, greedy_epsilon, state_space_feature_names)

    async def _get_state(self, observation):
        indices = np.floor((observation - self._state_space_bounds[0, :]) / self._state_space_epsilon)
        return np.clip(indices, np.zeros(len(self._state_space_shape)), self._state_space_shape - 1).astype('int')


    async def _get_action(self, state, epsilon=None):
        if epsilon is None:
            epsilon = self.greedy_epsilon

        if np.random.random() < epsilon:
            # exploratory action
            self._dont_know_history.append(0)
            self._exploratory_action_history.append(1)
            return np.array([np.random.choice(self._action_space)])

        else:
            # exploiting action
            self._exploratory_action_history.append(0)

            action_values = self.q[*state, :]
            max_action_value = np.max(action_values)
            max_actions = np.argwhere(action_values == max_action_value)
            if (len(max_actions) > 1):
                action_index = np.random.randint(max_actions.shape[0])

                self._dont_know_history.append(1)
            else:
                action_index = 0
                self._dont_know_history.append(0)
            return max_actions[action_index]
        

    async def _init_q(self):
        self.q = np.zeros(tuple(self._state_space_shape) + self._action_space.shape)


    async def _update_q(self, next_state, next_action, reward):
        self.q[*self._last_state, *self._last_action] += self.alpha * (reward + self.gamma * self.q[*next_state, *next_action] - self.q[*self._last_state, *self._last_action])


    async def _set_known_q_value(self, state, action, reward):
        self.q[*state, *action] = reward


class QLearning(TD):
    async def _get_state(self, observation):
        indices = np.floor((observation - self._state_space_bounds[0, :]) / self._state_space_epsilon)
        return np.clip(indices, np.zeros(len(self._state_space_shape)), self._state_space_shape - 1).astype('int')


    async def _get_action(self, state, epsilon=None):
        if epsilon is None:
            epsilon = self.greedy_epsilon

        if np.random.random() < epsilon:
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


    async def _init_q(self):
        self.q = np.zeros(tuple(self._state_space_shape) + self._action_space.shape)


    async def _update_q(self, next_state, _next_action, reward):
        max_q = np.max(self.q[*next_state, :])
        self.q[*self._last_state, *self._last_action] += self.alpha * (reward + self.gamma * max_q - self.q[*self._last_state, *self._last_action])


    async def _set_known_q_value(self, state, action, reward):
        self.q[*state, *action] = reward


class TD_AGDS(TD):
    @abstractmethod
    async def _updated_q_value(self, last_sa_value, next_state, next_action, reward):
        pass

    def __init__(
        self,
        state_space_feature_names,
        state_space_bounds,
        state_space_epsilon,
        action_space,
        alpha=0.25,
        gamma=1.0,
        greedy_epsilon=0.1,
        save_stimulations_in_step=None,
        min_passed_stimulus_vng=0.0,
        min_vn_excitation=0.0,
        min_passed_stimulus_ong=0.0,
        min_on_excitation=0.0,
        poison_min_passed_stimulus=0.0,
        poison_deadly_dose=None,
        poison_min_acc_dose=0.0,
        value_epsilon=0.01,
        min_value=0.0,
        max_value=1.0
    ):
        self.structure_size_history = []
        self._save_stimulations_in_step = save_stimulations_in_step
        self._dont_know_history = []
        self._exploratory_action_history = []

        # HYPERPARAMETERS (extracted for external modification)
        self.min_passed_stimulus_vng = min_passed_stimulus_vng
        self.min_vn_excitation = min_vn_excitation
        self.min_passed_stimulus_ong = min_passed_stimulus_ong
        self.min_on_excitation = min_on_excitation
        self.poison_min_passed_stimulus = poison_min_passed_stimulus
        self.poison_deadly_dose = poison_deadly_dose if poison_deadly_dose is not None else 1.0
        self.poison_min_acc_dose = poison_min_acc_dose
        self.value_epsilon = value_epsilon
        self.min_value = min_value
        self.max_value = max_value

        super().__init__(state_space_bounds, state_space_epsilon, action_space, alpha=alpha, gamma=gamma, greedy_epsilon=greedy_epsilon, state_space_feature_names=state_space_feature_names)


    async def stop(self):
        await self.q.stop()


    async def export_topology(self):
        await self.q.export_topology()


    async def export_stimulation(self, experiment_step, stimulation_name):
        await self.q.export_stimulation(experiment_step, stimulation_name)


    async def reset_episode(self, final_reward=None, save_score=True):
        if save_score and hasattr(self, 'q'):
            self.structure_size_history.append(await self.q.get_structure_size())
        return await super().reset_episode(final_reward, save_score)


    async def _get_state(self, observation):
        return observation


    async def _get_action(self, state, epsilon=None):
        if epsilon is None:
            epsilon = self.greedy_epsilon

        if np.random.random() < epsilon:
            # exploratory action
            self._dont_know_history.append(0)
            self._exploratory_action_history.append(1)
            return self._get_random_action()

        else:
            self._exploratory_action_history.append(0)

            # exploiting action
            best_sa = await self._search_for_best_action(state, 'pick_action')

            if best_sa is None:
                return self._get_random_action()

            # TODO: handles only one-dimensional action space
            best_sa_neigh_nodes = await self.q.get_on_neighbours(int(best_sa))
            # print(f'Best sa neigh nodes: {best_sa_neigh_nodes}')

            if best_sa_neigh_nodes == []:
                return self._get_random_action()
            
            return np.array([int(float(ef[2])) for ef in best_sa_neigh_nodes if ef[0] == 'vn' and ef[1] == 'action'])
        
        

    async def _init_q(self):
        self.q = await associata.create_agds(save_stimulations_in_step=self._save_stimulations_in_step)
        for f_name, f_epsilon, f_bounds in zip(self._state_space_feature_names, self._state_space_epsilon, self._state_space_bounds.T):
            await self.q.add_numerical_vng(str(f_name), f_epsilon, f_bounds[0], f_bounds[1])
        await self.q.add_numerical_vng('value', self.value_epsilon, self.min_value, self.max_value)
        await self.q.add_categorical_vng('action')


    async def _update_q(self, next_state, next_action, reward):
        last_sa_value = await self._search_for_action_value(self._last_state, self._last_action, 'last_sa_value_search')
        updated_last_sa_value = await self._updated_q_value(last_sa_value, next_state, next_action, reward)

        # TODO: handles only one-dimensional action space
        # TODO: handles only float values for VNGs
        new_observation = {str(vng_name): float(vng_value) for vng_name, vng_value in zip(
                                    list(self._state_space_feature_names) + ['value', 'action'], 
                                    self._last_state.tolist() + [updated_last_sa_value, self._last_action[0]]
                                )}
        
        new_on_index = await self.q.add_observation(new_observation, self._step_nr)
        await self._poison(new_on_index, self._last_action)


    async def _set_known_q_value(self, state, action, value):
        # print(f'{self._timestamp()} Setting known Q value in step {self._step_nr} for state {state} and action {action} to {value}')

        # TODO: handles only one-dimensional action space
        # TODO: handles only float values for VNGs
        observation = {str(vng_name): float(vng_value) for vng_name, vng_value in zip(
                                list(self._state_space_feature_names) + ['value', 'action'], 
                                state.tolist() + [value, action[0]]
                            )}
        
        new_on_index = await self.q.add_observation(observation, self._step_nr)
        await self._poison(new_on_index, action, 'poison_known_q_value')


    async def _search_for_action_value(self, state, action, stimulation_name):
        sa_value_search = self._setup_search_from_state(
            state,
            ong_mode=associata.NodeGroupMode.transitive,
            action_mode=associata.NodeGroupMode.responsive_exciation,
            value_mode=associata.NodeGroupMode.accumulative
        )
        sa_value_search = self._add_search_from_action(action, sa_value_search)
        sa_value = await self._infere_and_get_max_from_vng(
            'value',
            sa_value_search,
            self.min_passed_stimulus_vng,
            self.min_vn_excitation,
            stimulation_name
        )

        return float(sa_value) if sa_value is not None else 0.0


    async def _search_for_best_action(self, state, stimulation_name):
        best_action_search = self._setup_search_from_state(
            state,
            ong_mode=associata.NodeGroupMode.transitive,
            action_mode=associata.NodeGroupMode.accumulative,
            value_mode=associata.NodeGroupMode.responsive_value
        )

        return await self._infere_and_get_max_from_ong(
            best_action_search,
            self.min_passed_stimulus_ong,
            self.min_on_excitation,
            stimulation_name
        )
    

    async def _poison(self, new_on, action, name='poison'):
        poison_search = self._setup_search_from_on(
            new_on,
            ong_mode=associata.NodeGroupMode.accumulative, 
            action_mode=associata.NodeGroupMode.responsive_exciation, 
            value_mode=associata.NodeGroupMode.passive, 
            state_mode=associata.NodeGroupMode.transitive
        )
        poison_search = self._add_search_from_action(action, poison_search)

        await self.q.poison(
            poison_search,
            self.poison_min_passed_stimulus,
            self.poison_deadly_dose,
            self.poison_min_acc_dose,
            self._step_nr,
            name
        )


    def _setup_search_from_state(self, state, ong_mode, action_mode, value_mode):
        node_group_modes = {
            'ong': ong_mode,
            'value': value_mode,
            'action': action_mode,
        } | {
            str(feature_name): associata.NodeGroupMode.transitive for feature_name in self._state_space_feature_names
        }
        
        search = associata.StimulationSetup(node_group_modes)
        
        for f_name, f_value in zip(self._state_space_feature_names, state):
            search.stimulate_vn(str(f_name), f_value)

        return search
    

    def _setup_search_from_on(self, on_node, ong_mode, action_mode, value_mode, state_mode):
        node_group_modes = {
            'ong': ong_mode,
            'value': value_mode,
            'action': action_mode,
        } | {
            str(feature_name): state_mode for feature_name in self._state_space_feature_names
        }

        search = associata.StimulationSetup(node_group_modes)
        
        search.stimulate_on(on_node)

        return search
    

    def _add_search_from_action(self, action, search):
        search.stimulate_vn('action', action[0])    # TODO: handles only one-dimensional action space
        return search
    
    
    async def _infere_and_get_max_from_vng(self, vng_name, setup, min_passed_stimulus, min_vn_excitation, stimulation_name):
        await self.q.infere(setup, min_passed_stimulus, self._step_nr, stimulation_name)
        excitations = await self.q.get_excitations_for_vng(vng_name)
        important_excitations = {k: v for k, v in excitations.items() if v > min_vn_excitation}

        return self._get_maximizing_key(important_excitations)
    

    async def _infere_and_get_max_from_ong(self, setup, min_passed_stimulus, min_on_excitation, stimulation_name):
        await self.q.infere(setup, min_passed_stimulus, self._step_nr, stimulation_name)
        excitations = await self.q.get_excitations_for_ong()
        important_excitations = {k: v for k, v in excitations.items() if v > min_on_excitation}

        strongest_excited_on = self._get_maximizing_key(important_excitations)

        if len([v for v in important_excitations.values() if v == important_excitations.get(strongest_excited_on, -1)]) != 1:
            self._dont_know_history.append(1)
        else:
            self._dont_know_history.append(0)

        return strongest_excited_on

    
    def _get_maximizing_key(self, excitations):
        if excitations is not None and len(excitations) > 0:
            max_excitation = max(excitations.values())
            return np.random.choice([k for k, v in excitations.items() if v == max_excitation])
        else:
            return None


    def _get_random_action(self):
        return np.array([np.random.choice(self._action_space)])


class SarsaAGDS(TD_AGDS):
    async def _updated_q_value(self, last_sa_value, next_state, next_action, reward):
        next_sa_value = await self._search_for_action_value(next_state, next_action, 'next_sa_value_search')        
        new_q_value = last_sa_value + self.alpha * (reward + self.gamma * next_sa_value - last_sa_value)

        print("=" * 10 + f' Step {self._step_nr} Q-value update ' + "=" * 10)
        print(f"Last state={self._last_state}, last action={self._last_action}, last Q-value={last_sa_value}, reward={reward}")
        print(f"Next state={next_state}, next action={next_action}, next Q-value={next_sa_value}")
        print(f"Computation: new Q-value = {last_sa_value} + {self.alpha} * ({reward} + {self.gamma} * {next_sa_value} - {last_sa_value}) = {new_q_value}")
        print("\n")

        return new_q_value


class QLearningAGDS(TD_AGDS):
    async def _updated_q_value(self, last_sa_value, next_state, next_action, reward):
        best_next_sa = await self._search_for_best_action(next_state, 'qlearning_best_next_sa')
        
        if best_next_sa is None:
            best_next_sa_value = 0.0
        else:
            best_next_sa_neigh_nodes = await self.q.get_on_neighbours(best_next_sa)
            if best_next_sa_neigh_nodes == []:
                best_next_sa_value = 0.0
            else:
                best_next_sa_value = [float(ef[2]) for ef in best_next_sa_neigh_nodes if ef[0] == 'vn' and ef[1] == 'value'][0]

        return last_sa_value + self.alpha * (reward + self.gamma * best_next_sa_value - last_sa_value)
