
from matplotlib import pyplot as plt
import numpy as np
import associata
from abc import ABC, abstractmethod
import time
from plot_multidim import plot_3d_function_slice, plot_interactive_slices 
from enum import Enum
from mpl_toolkits.axes_grid1 import host_subplot
import mpl_toolkits.axisartist as AA


class TdLogger:
    
    def __init__(self, log_to='stdout', log_actions_picked=True, log_last_sa_value=False, log_next_sa_value=False):
        self.log_to = log_to
        self.log_actions_picked = log_actions_picked
        self.log_last_sa_value = log_last_sa_value
        self.log_next_sa_value = log_next_sa_value

        self._time_origin = time.time()

    def set_time_origin(self, time_origin):
        self._time_origin = time_origin

    def log_step(self, step_nr, action, old_last_sa_value, new_last_sa_value, next_sa_value):
        message = self._header(step_nr)

        if self.log_actions_picked:
            message = self._append_action_picked_msg(message, step_nr, action)
        if self.log_last_sa_value:
            message = self._append_old_last_sa_value_msg(message, step_nr, old_last_sa_value)
        if self.log_last_sa_value:
            message = self._append_new_last_sa_value_msg(message, step_nr, new_last_sa_value)
        if self.log_next_sa_value:
            message = self._append_next_sa_value_msg(message, step_nr, next_sa_value)

        message += '\n'

        self._log(message)

    def _header(self, step_nr):
        return f'{self._timestamp()} {"=" * 10} Step {step_nr} {"=" * 10}'
    
    def _append_action_picked_msg(self, msg, step_nr, action):
        return msg + f'\nStep {step_nr}: Picked action: {action}'
    
    def _append_old_last_sa_value_msg(self, msg, step_nr, old_last_sa_value):
        return msg + f'\nStep {step_nr}: Old last SA value: {old_last_sa_value}'
    
    def _append_new_last_sa_value_msg(self, msg, step_nr, new_last_sa_value):
        return msg + f'\nStep {step_nr}: New last SA value: {new_last_sa_value}'
    
    def _append_next_sa_value_msg(self, msg, step_nr, next_sa_value):
        return msg + f'\nStep {step_nr}: Next SA value: {next_sa_value}'


    def _log(self, message):
        if self.log_to == 'stdout':
            print(message)
        elif self.log_to is not None:
            with open(self.log_to, 'a') as f:
                f.write(message + '\n')

    def _timestamp(self):
        return f'[{int(time.time() - self._time_origin)}s]'
    

class TdHistorian:
    def __init__(self):
        self.states = []
        self.actions = []
        self.prev_step_rewards = []

        self.episode_end_step_numbers = []
        self.episode_rewards = []
        self.episode_reward = 0.0

        self.action_selection_kinds = []

        self.auxiliary_data = {}

    def record_episode_end(self, step_nr, final_reward, save_score=True):
        if save_score:
            self.episode_end_step_numbers.append(step_nr)
            self.episode_rewards.append(self.episode_reward + final_reward)

        self.episode_reward = 0.0

    def record_step(self, state, action, action_selection_kind, reward_for_prev_sa):
        self.states.append(state)
        self.actions.append(action)
        self.action_selection_kinds.append(action_selection_kind)

        if reward_for_prev_sa is not None:
            self.prev_step_rewards.append(reward_for_prev_sa)
            self.episode_reward += reward_for_prev_sa

    def record_auxiliary_data(self, key, value):
        if key not in self.auxiliary_data:
            self.auxiliary_data[key] = []
        self.auxiliary_data[key].append(value)


class ActionSelectionKind(Enum):
    EXPLORATORY = 1
    EXPLOITATIVE = 2
    EXPLOITATIVE_RANDOM = 3


class TdPlotter:
    def __init__(self, td_historian):
        self.td_historian = td_historian

    # x_labels: 'episodes' | 'steps'
    # auxilary_data: list of keys from td_historian.auxiliary_data to plot on the same graph
    def plot_episode_rewards(self, moving_average_window=1, x_labels='episodes', auxiliary_data=[]):
        fig = plt.figure()
        host = host_subplot(111, axes_class=AA.Axes)

        avg_reward = np.convolve(self.td_historian.episode_rewards, np.ones(moving_average_window) / moving_average_window, 'valid')
        x_ticks = self.td_historian.episode_end_step_numbers[moving_average_window - 1:] if x_labels == 'steps' else np.arange(moving_average_window - 1, len(self.td_historian.episode_rewards))
        title = f'Episode Reward{f" (Moving Average, window={moving_average_window}" if moving_average_window > 1 else ""})'

        axes = [host] + [host.twinx() for _ in range(len(auxiliary_data))]
        parasites = axes[1:]

        palette = plt.get_cmap('tab10')
        main_color = palette(0)
        parasites_colors = [palette(i + 1) for i in range(len(auxiliary_data))]

        host.plot(x_ticks, avg_reward, label='Episode reward', color=main_color)
        host.set_ylabel('Episode Reward')

        for i, (key, color) in enumerate(zip(auxiliary_data, parasites_colors)):
            aux_values = self.td_historian.auxiliary_data.get(key, [])
            aux_values_ma = np.convolve(aux_values, np.ones(moving_average_window) / moving_average_window, 'valid')
            
            offset = 60 * (i)
            parasites[i].axis['right'] = parasites[i].new_fixed_axis(loc='right', offset=(offset, 0))
            parasites[i].axis['right'].toggle(all=True)

            parasites[i].plot(x_ticks, aux_values_ma, label=key, color=color)
            parasites[i].set_ylabel(key)

        for i, ax in enumerate(axes):
            ax_key = 'left' if ax is host else 'right'
            ax.axis[ax_key].label.set_color(palette(i))
            ax.axis[ax_key].major_ticks.set_color(palette(i))
            ax.axis[ax_key].major_ticklabels.set_color(palette(i))

        host.set_xlabel(x_labels)
        host.set_title(title)

        plt.show()

    def plot_action_selection_kinds(self, moving_average_window=1):
        fig = plt.figure()
        host = host_subplot(111, axes_class=AA.Axes)

        action_selection_kinds = [kind.value for kind in self.td_historian.action_selection_kinds]
        action_selection_kinds_ma = np.convolve(action_selection_kinds, np.ones(moving_average_window) / moving_average_window, 'valid')
        x_ticks = np.arange(len(action_selection_kinds_ma))

        host.plot(x_ticks, action_selection_kinds_ma, label='Action Selection Kind', color='blue')
        host.set_ylabel('Action Selection Kind (1: Exploratory, 2: Exploitative, 3: Exploitative Random)')

        host.set_xlabel('Steps')
        host.set_title('Action Selection Kinds Over Time')

        plt.show()

    def plot_actions(self, moving_average_window=1):
        fig = plt.figure()
        host = host_subplot(111, axes_class=AA.Axes)

        actions = [action[0] for action in self.td_historian.actions]  # Assuming actions are arrays
        actions_ma = np.convolve(actions, np.ones(moving_average_window) / moving_average_window, 'valid')
        x_ticks = np.arange(len(actions_ma))

        host.plot(x_ticks, actions_ma, label='Actions', color='green')
        host.set_ylabel('Actions')

        host.set_xlabel('Steps')
        host.set_title('Actions Over Time')

        plt.show()


class TD(ABC):
    def __init__(self, state_space_bounds, state_space_epsilon, action_space, alpha=0.2, gamma=1.0, greedy_epsilon=0.1, state_space_feature_names=None,
                 logger=None):
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

        self._step_nr = 0

        self.logger = logger
        self.historian = TdHistorian()
        self.plotter = TdPlotter(self.historian)


    async def step(self, observation, reward=None):
        if not self._is_initialized:
            await self._init_q()
            self._is_initialized = True

        state = await self._get_state(observation)
        action, action_selection_kind = await self._get_action(state)

        if self._last_action is not None and reward is not None:
            old_last_sa_value, new_last_sa_value, next_sa_value = await self._update_q(state, action, reward)
        else:
            old_last_sa_value, new_last_sa_value, next_sa_value = None, None, None

        self._last_state = state
        self._last_action = action

        if self.logger is not None:
            self.logger.log_step(self._step_nr, action, old_last_sa_value, new_last_sa_value, next_sa_value)
        self.historian.record_step(state, action, action_selection_kind, reward)

        self._step_nr += 1
        return action
    

    async def reset_episode(self, final_reward=None, save_score=False):
        if final_reward is not None and self._last_state is not None and self._last_action is not None:
            await self._set_known_q_value(self._last_state, self._last_action, final_reward)

        self._last_state = None
        self._last_action = None

        self.historian.record_episode_end(self._step_nr, final_reward, save_score=save_score)


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
    def __init__(self, state_space_bounds, state_space_epsilon, action_space, alpha=0.2, gamma=1, greedy_epsilon=0.1, state_space_feature_names=None,
                 logger=None):
        super().__init__(state_space_bounds, state_space_epsilon, action_space, alpha, gamma, greedy_epsilon, state_space_feature_names, logger)

    async def _get_state(self, observation):
        indices = np.floor((observation - self._state_space_bounds[0, :]) / self._state_space_epsilon)
        state = np.clip(indices, np.zeros(len(self._state_space_shape)), self._state_space_shape - 1).astype('int')
        return state


    async def _get_action(self, state, epsilon=None):
        if epsilon is None:
            epsilon = self.greedy_epsilon

        if np.random.random() < epsilon:
            # exploratory action
            return np.array([np.random.choice(self._action_space)]), ActionSelectionKind.EXPLORATORY

        else:
            # exploiting action
            action_values = self.q[*state, :]
            max_action_value = np.max(action_values)
            max_actions = np.argwhere(action_values == max_action_value)
            if (len(max_actions) > 1):
                action_index = np.random.randint(max_actions.shape[0])
                selection_mode = ActionSelectionKind.EXPLOITATIVE_RANDOM
            else:
                action_index = 0
                selection_mode = ActionSelectionKind.EXPLOITATIVE
            return max_actions[action_index], selection_mode
        

    async def _init_q(self):
        self.q = np.zeros(tuple(self._state_space_shape) + self._action_space.shape)


    async def _update_q(self, next_state, next_action, reward):
        old_last_sa_value = self.q[*self._last_state, *self._last_action]
        next_sa_value = self.q[*next_state, *next_action]
        new_last_sa_value = old_last_sa_value + self.alpha * (reward + self.gamma * next_sa_value - old_last_sa_value)
        
        self.q[*self._last_state, *self._last_action] = new_last_sa_value
        return old_last_sa_value, new_last_sa_value, next_sa_value


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
            return np.array([np.random.choice(self._action_space)]), ActionSelectionKind.EXPLORATORY

        else:
            # exploiting action
            action_values = self.q[*state, :]
            max_action_value = np.max(action_values)
            max_actions = np.argwhere(action_values == max_action_value)
            if (len(max_actions) > 1):
                action_index = np.random.randint(max_actions.shape[0])
                selection_mode = ActionSelectionKind.EXPLOITATIVE_RANDOM
            else:
                action_index = 0
                selection_mode = ActionSelectionKind.EXPLOITATIVE
            return max_actions[action_index], selection_mode


    async def _init_q(self):
        self.q = np.zeros(tuple(self._state_space_shape) + self._action_space.shape)


    async def _update_q(self, next_state, _next_action, reward):
        max_q = np.max(self.q[*next_state, :])

        old_last_sa_value = self.q[*self._last_state, *self._last_action]
        next_sa_value = max_q
        new_last_sa_value = old_last_sa_value + self.alpha * (reward + self.gamma * next_sa_value - old_last_sa_value)

        self.q[*self._last_state, *self._last_action] = new_last_sa_value
        return old_last_sa_value, new_last_sa_value, next_sa_value


    async def _set_known_q_value(self, state, action, reward):
        self.q[*state, *action] = reward


class TD_AGDS(TD):
    @abstractmethod
    async def _updated_q_value(self, last_sa_value, next_state, next_action, reward):
        pass

    def __init__(
        self
        
        , state_space_feature_names
        , state_space_bounds
        , state_space_epsilon
        , action_space
        
        , alpha=0.25
        , gamma=1.0
        , greedy_epsilon=0.1

        , min_passed_stimulus_vng=0.0
        , min_vn_excitation=0.0
        , min_passed_stimulus_ong=0.0
        , min_on_excitation=0.0
        , poison_min_passed_stimulus=0.0
        , poison_deadly_dose=None
        , poison_min_acc_dose=0.0
        , value_epsilon=0.01
        , min_value=0.0
        , max_value=1.0
        
        , new_observations_treatment='replace' # 'replace' | 'add_and_count'
        , value_selection_mode='closest_on' # 'closest_on' | 'closest_vn' | 'direct_on' | 'direct_or_closest_on'
        , action_selection_mode='direct_on' # 'direct_on' | 'inference'
        , vn_to_vn_weight_mode='classical_subtractive' # 'constant' | 'classical_multiplicative' | 'classical_subtractive' | 'rate_of_occurance_subtractive'
        , vn_to_on_weight_mode='constant' # 'constant' | 'one_over_n_on' | 'rate_of_occurance'

        , save_stimulations_in_step=None
        , logger=None

    ):
        # HYPERPARAMETERS
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

        self._save_stimulations_in_step = save_stimulations_in_step

        super().__init__(state_space_bounds, state_space_epsilon, action_space, alpha=alpha, gamma=gamma, greedy_epsilon=greedy_epsilon, state_space_feature_names=state_space_feature_names, logger=logger)


    async def stop(self):
        await self.q.stop()


    async def export_topology(self):
        await self.q.export_topology()


    async def export_stimulation(self, experiment_step, stimulation_name):
        await self.q.export_stimulation(experiment_step, stimulation_name)


    async def reset_episode(self, final_reward=None, save_score=True):
        if save_score and hasattr(self, 'q'):
            self.record_auxiliary_data('structure_size', await self.q.get_structure_size())
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
            return self._get_random_action(), ActionSelectionKind.EXPLORATORY

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
        on_indices = await self.q.get_ons_for_exact_vn_values(
            {str(f_name): float(f_value) for f_name, f_value in zip(self._state_space_feature_names, state.tolist())}
        )

        match on_indices:
            case []:
                return self._get_random_action(), ActionSelectionKind.EXPLOITATIVE_RANDOM
            case [single_on]:
                neighs = await self.q.get_on_neighbours(single_on)
                return self._get_action_from_on_neighs(neighs), ActionSelectionKind.EXPLOITATIVE
            
            case [*ons]:
                all_ons_neighs = [await self.q.get_on_neighbours(on) for on in ons]
                on_values = [self._get_sa_value_from_on_neighs(on_neighs) for on_neighs in all_ons_neighs]

                best_on_local_index = np.argmax(on_values)
                best_on_neighs = all_ons_neighs[best_on_local_index]
                best_action = self._get_action_from_on_neighs(best_on_neighs)
                
                return best_action, ActionSelectionKind.EXPLOITATIVE


    async def _pick_action_through_inference(self, state):
        best_sa = await self._search_for_best_action(state, 'pick_action')

        if best_sa is None:
            return self._get_random_action()

        # TODO: handles only one-dimensional action space
        best_sa_neigh_nodes = await self.q.get_on_neighbours(int(best_sa))

        if best_sa_neigh_nodes == []:
            return self._get_random_action(), ActionSelectionKind.EXPLOITATIVE_RANDOM

        return self._get_action_from_on_neighs(best_sa_neigh_nodes), ActionSelectionKind.EXPLOITATIVE


    def _get_action_from_on_neighs(self, on_neighs):
        return np.array([int(float(vn_value)) for vn_value in self._get_on_neigh_repr_value_by_vng_name(on_neighs, 'action')])
    
    def _get_sa_value_from_on_neighs(self, on_neighs):
        return np.array([float(vn_value) for vn_value in self._get_on_neigh_repr_value_by_vng_name(on_neighs, 'value')])
    
    def _get_on_neigh_repr_value_by_vng_name(self, on_neighs, vng_name):
        return [ef[2] for ef in on_neighs if ef[0] == 'vn' and ef[1] == vng_name]


    async def _init_q(self):
        self.q = await associata.create_agds(save_stimulations_in_step=self._save_stimulations_in_step)
        for f_name, f_epsilon, f_bounds in zip(self._state_space_feature_names, self._state_space_epsilon, self._state_space_bounds.T):
            await self.q.add_numerical_vng(str(f_name), f_epsilon, f_bounds[0], f_bounds[1])
        await self.q.add_numerical_vng('value', self.value_epsilon, self.min_value, self.max_value)
        await self.q.add_categorical_vng('action')


    async def _update_q(self, next_state, next_action, reward):
        last_sa_value = await self._search_for_action_value(self._last_state, self._last_action, 'last_sa_value_search')
        updated_last_sa_value, next_sa_value = await self._updated_q_value(last_sa_value, next_state, next_action, reward)

        await self._store_observation(self._last_state, self._last_action, updated_last_sa_value)
        return last_sa_value, updated_last_sa_value, next_sa_value


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
        on_for_state_action = await self.q.get_ons_for_exact_vn_values(state_repr | action_repr)

        match on_for_state_action:
            case []:
                await self.q.add_observation(state_repr | action_repr | value_repr, self._step_nr)
            case [single_on]:
                await self.q.reconnect_on(single_on, state_repr | action_repr, value_repr, self._step_nr)
            case _:
                raise ValueError(f"Multiple ONs matched for exact VN values for state={state_repr}, action={action_repr}: {on_for_state_action}")


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

        
    async def _get_action_value_through_direct_on(self, state, action):
        sa = await self.q.get_ons_for_exact_vn_values(
            {str(f_name): float(f_value) for f_name, f_value in zip(self._state_space_feature_names, state.tolist())} | 
            {'action': float(action[0])}    # TODO: handles only one-dimensional action space
        )

        match sa:
            case []:
                return None
            case [single_sa]:
                sa_value_neigh_nodes = await self.q.get_on_neighbours(single_sa)
                sa_value = self._get_sa_value_from_on_neighs(sa_value_neigh_nodes)[0]
                return float(sa_value)
            case _:
                raise ValueError(f"Multiple ONs matched for exact VN values for state={state}, action={action}: {sa}")


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
            action_value = await self._get_action_value_through_direct_on(state, action)
            
        elif self.value_selection_mode == 'direct_or_closest_on':
            direct_value = await self._get_action_value_through_direct_on(state, action)
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

        return new_q_value, next_sa_value


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

        updated_last_sa_value = last_sa_value + self.alpha * (reward + self.gamma * best_next_sa_value - last_sa_value)

        return updated_last_sa_value, best_next_sa_value
