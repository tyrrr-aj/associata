
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
        self.episode_step_nr = []

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
            self.episode_step_nr.append(self._step_nr)
        
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
        # self._state_history = {}
        super().__init__(state_space_bounds, state_space_epsilon, action_space, alpha, gamma, greedy_epsilon, state_space_feature_names)

    async def _get_state(self, observation):
        indices = np.floor((observation - self._state_space_bounds[0, :]) / self._state_space_epsilon)
        state = np.clip(indices, np.zeros(len(self._state_space_shape)), self._state_space_shape - 1).astype('int')
        # if tuple(state) not in self._state_history:
        #     self._state_history[tuple(state)] = len(self._state_history)
        return state


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
        # last_state_id = self._state_history[tuple(self._last_state)]
        # next_state_id = self._state_history[tuple(next_state)]
        # print("=" * 10 + f' Step {self._step_nr} Q-value update ' + "=" * 10)
        # print(f"Last state={last_state_id}, last action={self._last_action}, last Q-value={self.q[*self._last_state, *self._last_action]}, reward={reward}")
        # print(f"Next state={next_state_id}, next action={next_action}, next Q-value={self.q[*next_state, *next_action]}")
        # print(f"Computation: new Q-value = {self.q[*self._last_state, *self._last_action]} + {self.alpha} * ({reward} + {self.gamma} * {self.q[*next_state, *next_action]} - {self.q[*self._last_state, *self._last_action]}) = {self.q[*self._last_state, *self._last_action] + self.alpha * (reward + self.gamma * self.q[*next_state, *next_action] - self.q[*self._last_state, *self._last_action])}")
        # print("\n")
        self.q[*self._last_state, *self._last_action] += self.alpha * (reward + self.gamma * self.q[*next_state, *next_action] - self.q[*self._last_state, *self._last_action])


    async def _set_known_q_value(self, state, action, reward):
        # state_id = self._state_history[tuple(state)]
        # print("=" * 10 + f' Step {self._step_nr} Q-value update ' + "=" * 10)
        # print(f"Setting known Q-value for state={state_id}, action={action} to value={reward}")
        # print("\n")
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
        max_value=1.0,
        new_observations_treatment='replace', # 'replace' | 'add_and_count'
        value_selection_mode='closest_on', # 'closest_on' | 'closest_vn' | 'direct_on' | 'direct_or_closest_on'
        action_selection_mode='direct_on', # 'direct_on' | 'inference'
        vn_to_vn_weight_mode='classical_subtractive', # 'constant' | 'classical_multiplicative' | 'classical_subtractive'
        vn_to_on_weight_mode='constant' # 'constant' | 'one_over_n_on' | 'rate_of_occurance'
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
        self.new_observations_treatment = new_observations_treatment
        self.value_selection_mode = value_selection_mode
        self.action_selection_mode = action_selection_mode
        self.vn_to_vn_weight_mode = vn_to_vn_weight_mode
        self.vn_to_on_weight_mode = vn_to_on_weight_mode

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
            # exploiting action
            self._exploratory_action_history.append(0)

            if self.action_selection_mode == 'direct_on':
                return await self._pick_action_through_direct_on(state)
            elif self.action_selection_mode == 'inference':
                return await self._pick_action_through_inference(state)
            else:
                raise ValueError(f"Unknown action_selection_mode: {self.action_selection_mode}")
            

    async def _pick_action_through_direct_on(self, state):
        ons = await self.q.get_on_for_exact_vn_values(
            {str(f_name): float(f_value) for f_name, f_value in zip(self._state_space_feature_names, state.tolist())}
        )

        match ons:
            case None:
                return self._get_random_action()
            case (on1, on2):
                on1_neighs = await self.q.get_on_neighbours(on1)
                on2_neighs = await self.q.get_on_neighbours(on2)

                on1_value = np.array([float(ef[2]) for ef in on1_neighs if ef[0] == 'vn' and ef[1] == 'value'])
                on2_value = np.array([float(ef[2]) for ef in on2_neighs if ef[0] == 'vn' and ef[1] == 'value'])

                if on1_value > on2_value:
                    return np.array([int(float(ef[2])) for ef in on1_neighs if ef[0] == 'vn' and ef[1] == 'action'])
                else:
                    return np.array([int(float(ef[2])) for ef in on2_neighs if ef[0] == 'vn' and ef[1] == 'action'])
                
            case on:
                neighs = await self.q.get_on_neighbours(on)
                return np.array([int(float(ef[2])) for ef in neighs if ef[0] == 'vn' and ef[1] == 'action'])


    async def _pick_action_through_inference(self, state):
        best_sa = await self._search_for_best_action(state, 'pick_action')

        if best_sa is None:
            return self._get_random_action()

        # TODO: handles only one-dimensional action space
        best_sa_neigh_nodes = await self.q.get_on_neighbours(int(best_sa))

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

        await self._store_observation(self._last_state, self._last_action, updated_last_sa_value)


    async def _set_known_q_value(self, state, action, value):
        await self._store_observation(state, action, value)


    async def _store_observation(self, state, action, value):
        # handles only float values for VNGs
        state_repr = self._get_state_representation(state)
        action_repr = self._get_action_representation(action)
        value_repr = self._get_value_representation(value)

        if self.new_observations_treatment == 'replace':
            await self._store_observation_replacing(state_repr, action_repr, value_repr)
        elif self.new_observations_treatment == 'add_and_count':
            await self._store_observation_counting(state_repr, action_repr, value_repr)


    def _get_state_representation(self, state):
        return {str(vng_name): float(vng_value) for vng_name, vng_value in zip(
                                    list(self._state_space_feature_names), 
                                    state.tolist()
                                )}
    

    def _get_action_representation(self, action):
        return { 'action': float(action[0]) }   # TODO: handles only one-dimensional action space
    

    def _get_value_representation(self, value):
        return { 'value': float(value) }

    

    def _get_action_representation(self, action):
        return { 'action': float(action[0]) }   # TODO: handles only one-dimensional action space
    

    def _get_value_representation(self, value):
        return { 'value': float(value) }


    async def _store_observation_replacing(self, state_repr, action_repr, value_repr):
        on_for_state_action = await self.q.get_on_for_exact_vn_values(state_repr | action_repr)

        if on_for_state_action is None:
            await self.q.add_observation(state_repr | action_repr | value_repr, self._step_nr)
        else:
            await self.q.reconnect_on(on_for_state_action, state_repr | action_repr, value_repr, self._step_nr)


    async def _store_observation_counting(self, state_repr, action_repr, value_repr):
        await self.q.add_observation(state_repr | action_repr | value_repr, self._step_nr)

    async def _get_action_value_through_closest_vn(self, state, action, stimulation_name):
        sa_value_search = self._setup_search_from_state(
            state,
            ong_mode=associata.NodeGroupMode.transitive,
            action_mode=associata.NodeGroupMode.responsive_exciation,
            value_mode=associata.NodeGroupMode.accumulative
        )
        sa_value_search = self._add_search_from_action(action, sa_value_search)

        sa_value = await self._infere_and_get_weighted_avg_from_vng(
            'value',
            sa_value_search,
            self.min_passed_stimulus_vng,
            self.min_vn_excitation,
            stimulation_name
        )
        
        return float(sa_value) if sa_value is not None else None

        
    async def _get_action_value_through_direct_on(self, state, action, stimulation_name):
        sa = await self.q.get_on_for_exact_vn_values(
            {str(f_name): float(f_value) for f_name, f_value in zip(self._state_space_feature_names, state.tolist())} | 
            {'action': float(action[0])}    # TODO: handles only one-dimensional action space
        )

        if sa is None:
            return None
        else:
            sa_value_neigh_nodes = await self.q.get_on_neighbours(sa)
            sa_value = [float(ef[2]) for ef in sa_value_neigh_nodes if ef[0] == 'vn' and ef[1] == 'value'][0]
            return float(sa_value)


    async def _get_action_value_through_closest_on(self, state, action, stimulation_name):
        sa_value_search = self._setup_search_from_state(
            state,
            ong_mode=associata.NodeGroupMode.accumulative,
            action_mode=associata.NodeGroupMode.responsive_exciation,
            value_mode=associata.NodeGroupMode.passive
        )
        sa_value_search = self._add_search_from_action(action, sa_value_search)

        closest_sa = await self._infere_and_get_max_from_ong(
            sa_value_search,
            self.min_passed_stimulus_ong,
            self.min_on_excitation,
            stimulation_name
        )

        # print (f'get_assoc_action_value: closest ON for state={state}, action={action} is {closest_sa}')

        if closest_sa is None:
            return None
        else:
            closest_sa_neigh_nodes = await self.q.get_on_neighbours(closest_sa)
            sa_value = [float(ef[2]) for ef in closest_sa_neigh_nodes if ef[0] == 'vn' and ef[1] == 'value'][0]
            return float(sa_value)


    async def _search_for_action_value(self, state, action, stimulation_name):
        if self.value_selection_mode == 'closest_on':
            action_value = await self._get_action_value_through_closest_on(state, action, stimulation_name)

        elif self.value_selection_mode == 'closest_vn':
            action_value = await self._get_action_value_through_closest_vn(state, action, stimulation_name)

        elif self.value_selection_mode == 'direct_on':
            action_value = await self._get_action_value_through_direct_on(state, action, stimulation_name)
            
        elif self.value_selection_mode == 'direct_or_closest_on':
            direct_value = await self._get_action_value_through_direct_on(state, action, stimulation_name)
            if direct_value is not None:
                action_value = direct_value
            else:
                action_value = await self._get_action_value_through_closest_on(state, action, stimulation_name)
            
        return action_value if action_value is not None else 0.0


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
        search = self._setup_search(ong_mode, action_mode, value_mode, state_mode=associata.NodeGroupMode.transitive)
        search = self._setup_search(ong_mode, action_mode, value_mode, state_mode=associata.NodeGroupMode.transitive)
        
        for f_name, f_value in zip(self._state_space_feature_names, state):
            search.stimulate_vn(str(f_name), f_value)

        return search
    

    def _setup_search_from_on(self, on_node, ong_mode, action_mode, value_mode, state_mode):
        search = self._setup_search(ong_mode, action_mode, value_mode, state_mode)        
        search.stimulate_on(on_node)
        return search
    

    def _setup_search(self, ong_mode, action_mode, value_mode, state_mode):
        search = self._setup_search(ong_mode, action_mode, value_mode, state_mode)        
        search.stimulate_on(on_node)
        return search
    

    def _setup_search(self, ong_mode, action_mode, value_mode, state_mode):
        node_group_modes = {
            'ong': ong_mode,
            'value': value_mode,
            'action': action_mode,
        } | {
            str(feature_name): state_mode for feature_name in self._state_space_feature_names
        }

        search = associata.StimulationSetup(node_group_modes, self.vn_to_vn_weight_mode, self.vn_to_on_weight_mode)
        search = associata.StimulationSetup(node_group_modes, self.vn_to_vn_weight_mode, self.vn_to_on_weight_mode)
        return search

    def _add_search_from_action(self, action, search):
        search.stimulate_vn('action', action[0])    # TODO: handles only one-dimensional action space
        return search
    
    
    async def _infere_and_get_max_from_vng(self, vng_name, setup, min_passed_stimulus, min_vn_excitation, stimulation_name):
        important_excitations = await self._infere_and_get_important_excitations_from_vng(vng_name, setup, min_passed_stimulus, min_vn_excitation, stimulation_name)
        return self._get_maximizing_key(important_excitations)

    
    async def _infere_and_get_weighted_avg_from_vng(self, vng_name, setup, min_passed_stimulus, min_vn_excitation, stimulation_name):
        important_excitations = await self._infere_and_get_important_excitations_from_vng(vng_name, setup, min_passed_stimulus, min_vn_excitation, stimulation_name)

        if len(important_excitations) == 0:
            return None

        total_excitation = sum(important_excitations.values())
        weighted_avg_key = sum(k * v for k, v in important_excitations.items()) / total_excitation

        return weighted_avg_key
    

    async def _infere_and_get_important_excitations_from_vng(self, vng_name, setup, min_passed_stimulus, min_vn_excitation, stimulation_name):
        await self.q.infere(setup, min_passed_stimulus, self._step_nr, stimulation_name)
        excitations = await self.q.get_excitations_for_vng(vng_name)
        important_excitations = {k: v for k, v in excitations.items() if v > min_vn_excitation}

        return important_excitations
    

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

        # last_state_on = await self.q.get_on_for_exact_vn_values(
        #     {str(f_name): float(f_value) for f_name, f_value in zip(self._state_space_feature_names, self._last_state.tolist())} | 
        #     {'action': float(self._last_action[0])}    # TODO: handles only one-dimensional action space
        # )
        # next_state_on = await self.q.get_on_for_exact_vn_values(
        #     {str(f_name): float(f_value) for f_name, f_value in zip(self._state_space_feature_names, next_state.tolist())} | 
        #     {'action': float(next_action[0])}    # TODO: handles only one-dimensional action space  
        # )

        # print("=" * 10 + f' Step {self._step_nr} Q-value update ' + "=" * 10)
        # print(f"Last state={last_state_on}, last action={self._last_action}, last Q-value={last_sa_value}, reward={reward}")
        # print(f"Next state={next_state_on}, next action={next_action}, next Q-value={next_sa_value}")
        # print(f"Computation: new Q-value = {last_sa_value} + {self.alpha} * ({reward} + {self.gamma} * {next_sa_value} - {last_sa_value}) = {new_q_value}")
        # print("\n")

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
