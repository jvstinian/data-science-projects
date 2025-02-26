'''
Created on Mar 25, 2018
Author: Justin Smith
'''
import sys
import itertools
import numpy, random, os
import tensorflow as tf
# import tensorflow.compat.v1 as tf
# tf.disable_v2_behavior()
from dqn.replay_memory import ReplayMemory, MultiAgentReplayMemory
from dqn.optimizer import Optimizer
from dqn.q_network import QNetwork


class DQN:
    
    def __init__(self, config, env, directory, callback=None, summary_writer=None, verbose=False):
        
        self.env = env
        if verbose:
            print(self.env.action_spaces[self.env.possible_agents[0]])
        self.actions = range(0, self.env.action_spaces[self.env.possible_agents[0]].n) # TODO: Improve this
        self.feedback_size = self.env.observation_spaces[self.env.possible_agents[0]].shape # NEW.  NOTE: The use of .shape might need adjustment in the future
        if len(self.feedback_size) >= 3:
            self.feedback_size = self.feedback_size[1:]
        print("feedback_size: %s" % (self.feedback_size,))
        self.callback = callback
        self.summary_writer = summary_writer
        
        self.config = config
        # self.max_agents = config['max_agents'] # new
        self.batch_size = config['batch_size']
        self.n_episode = config['num_episode']
        self.capacity = config['capacity']
        self.epsilon_decay = config['epsilon_decay']
        self.epsilon_min = config['epsilon_min']
        self.num_frames = config['num_frames']
        self.num_nullops = config['num_nullops']
        self.time_between_two_copies = config['time_between_two_copies']
        self.input_scale = config['input_scale']
        self.update_interval = config['update_interval']
        self.directory = directory

        self.verbose = verbose

        self._init_modules()
        
    def _init_modules(self):
        
        # Replay memory
        self.replay_memory = MultiAgentReplayMemory(
                self.env.possible_agents,
                history_len=self.num_frames, 
                capacity=self.capacity, 
                batch_size=self.batch_size,
                input_scale=self.input_scale
        )
        
        input_shape = self.feedback_size + (self.num_frames,)
        # Q-network
        self.q_network = QNetwork(input_shape=input_shape, n_outputs=len(self.actions), 
                                  network_type=self.config['network_type'], scope='q_network')
        # Target network
        self.target_network = QNetwork(input_shape=input_shape, n_outputs=len(self.actions), 
                                       network_type=self.config['network_type'], scope='target_network')
        # Optimizer
        self.optimizer = Optimizer(config=self.config, 
                                   feedback_size=self.feedback_size, 
                                   q_network=self.q_network, 
                                   target_network=self.target_network, 
                                   replay_memory=self.replay_memory)
        # Ops for updating target network
        # self.clone_op = self.target_network.get_clone_op(self.q_network) # TODO
        # For tensorboard
        # TODO: The following are logged explicitly below using the v2 approach
        # self.t_score = tf.placeholder(dtype=tf.float32, shape=[], name='new_score')
        # tf.summary.scalar("score", self.t_score, collections=['dqn'])
        # self.summary_op = tf.summary.merge_all('dqn')
    
    def set_summary_writer(self, summary_writer=None):
        self.summary_writer = summary_writer
        self.optimizer.set_summary_writer(summary_writer)
    
    # def choose_action(self, sess, state, epsilon_greedy):
    #     if numpy.random.binomial(1, epsilon_greedy) == 1:
    #         action = random.choice(self.actions)
    #     else:
    #         x = numpy.asarray(numpy.expand_dims(state, axis=0) / self.input_scale, dtype=numpy.float32)
    #         # print("dim state=%s, dim x=%s" % (state.shape, x.shape))
    #         action = self.q_network.get_q_action(sess, x)[0]
    #     return action
    
    def choose_action(self, state, epsilon_greedy):
        action = {}
        if numpy.random.binomial(1, epsilon_greedy) == 1:
            for agent_id in state:
                action[agent_id] = random.choice(self.actions) # self.env.action_spaces[agent_id].sample()
                if (state[agent_id][0,:,:].reshape((39,8))[18:21, 3] <= 1.0).any() and (action[agent_id] == 11):
                    print("Looks like we're randomly firing at another player.  Changing to a healing action.")
                    action[agent_id] = 13
                
        else:
            for agent_id in state:
                x = numpy.asarray(numpy.expand_dims(state[agent_id], axis=0) / self.input_scale, dtype=numpy.float32)
                # print("dim state=%s, dim x=%s" % (state.shape, x.shape))
                action[agent_id] = self.q_network.get_q_action(x)[0]
        return action
    
    def play(self, actions):
        new_state, r, termination, truncated, _ = self.env.step(actions)
        combined_termination = { 
            agent_id: (termination.get(agent_id, False) or truncated.get(agent_id, False))
            for agent_id in itertools.chain(termination.keys(), truncated.keys())
        }
        # print("play - frame keys: ", new_state.keys())
        # print("play - r keys: ", r.keys())
        # print("play - termination keys: ", combined_termination.keys())
        return r, new_state, combined_termination
        
    # def update_target_network(self, sess):
    #     sess.run(self.clone_op)
        
    def update_target_network(self):
        self.target_network.clone_op(self.q_network)

    # def train(self, sess, saver=None):
    #     num_of_trials = -1
    #     for episode in range(self.n_episode):
    #         total_reward = 0
    #         frame = self.env.reset()
    #         # frame = self.env.get_current_feedback()
    #         for _ in range(self.num_nullops):
    #             action_idx=4
    #             r, new_frame, termination = self.play(action_idx)
    #             total_reward += r
    #             self.replay_memory.add(frame, action_idx, r, termination)
    #             frame = new_frame
    #         
    #         for _ in range(self.config['T']):
    #             num_of_trials += 1
    #             epsilon_greedy = self.epsilon_min + \
    #                 max(self.epsilon_decay - num_of_trials, 0) / \
    #                 self.epsilon_decay * (1 - self.epsilon_min)
    #             if self.verbose:
    #                 print("epi {}, frame {}k: reward {}, eps {}".format(episode, 
    #                                                                     int(num_of_trials / 1000), 
    #                                                                     total_reward,
    #                                                                     epsilon_greedy))
    #             if num_of_trials % self.update_interval == 0:
    #                 self.optimizer.train_one_step(sess, num_of_trials, self.batch_size)
    #             
    #             state = self.replay_memory.phi(frame)
    #             action_idx = self.choose_action(sess, state, epsilon_greedy)
    #             r, new_frame, termination = self.play(action_idx)
    #             total_reward += r
    #             self.replay_memory.add(frame, action_idx, r, termination)
    #             frame = new_frame
    #             
    #             if num_of_trials % self.time_between_two_copies == 0:
    #                 self.update_target_network(sess)
    #                 self.save(sess, saver)
    #             
    #             if self.callback:
    #                 self.callback()
    #             if termination:
    #                 score = total_reward # self.env.get_total_reward()
    #                 summary_str = sess.run(self.summary_op, feed_dict={self.t_score: score})
    #                 self.summary_writer.add_summary(summary_str, num_of_trials)
    #                 self.summary_writer.flush()
    #                 break
    
    def sample_action_space(self):
        ret = {}
        for agent_id in self.env.agents:
            ret[agent_id] = self.env.action_spaces[agent_id].sample()
        return ret

    def train(self, saver=None):
        num_of_trials = -1
        for episode in range(self.n_episode):
            total_rewards = {agent_id: 0.0 for agent_id in self.env.possible_agents}
            total_reward = 0 # TODO: Will need to be total_rewards?
            frame, _ = self.env.reset() # TODO: frames?
            # frame = self.env.get_current_feedback()
            for _ in range(self.num_nullops):
                # NOTE: In the previous call to the step method, the environment updated the active agents after the frame was created.
                #       We filter the frame to the active agents to guarantee consistency.
                updated_frame = { agent_id: frame[agent_id] for agent_id in self.env.agents }
                actions = self.sample_action_space() # TODO: multiagent sampling
                r, new_frame, termination = self.play(actions)
                for agent_id in r:
                    total_rewards[agent_id] += r[agent_id]
                total_reward += sum(r.values())
                self.replay_memory.add(updated_frame, actions, r, termination) # TODO: support multiagent replay memory
                frame = new_frame
            
            for _ in range(self.config['T']):
                num_of_trials += 1
                epsilon_greedy = self.epsilon_min + \
                    max(self.epsilon_decay - num_of_trials, 0) / \
                    self.epsilon_decay * (1 - self.epsilon_min)
                if self.verbose:
                    print("epi {}, frame {}k: reward {}, eps {}".format(episode, 
                                                                        int(num_of_trials / 1000), 
                                                                        total_reward,
                                                                        epsilon_greedy))
                if num_of_trials % self.update_interval == 0:
                    # self.optimizer.train_one_step(sess, num_of_trials, self.batch_size) # TODO
                    self.optimizer.train_one_step(num_of_trials, self.batch_size)
                
                updated_frame = { agent_id: frame[agent_id] for agent_id in self.env.agents }
                state = self.replay_memory.phi(updated_frame)
                # NOTE: In the step method, the environment updates the active agents after the frame is created
                #       but before the step returns.  Before moving to the next step call, we filter the frame
                #       to the active agents
                # action_idx = self.choose_action(sess, state, epsilon_greedy) # TODO
                action = self.choose_action(state, epsilon_greedy)
                r, new_frame, termination = self.play(action)
                for agent_id in r:
                    total_rewards[agent_id] += r[agent_id]
                total_reward += sum(r.values())
                # print("frame keys: ", updated_frame.keys())
                # print("action keys: ", action.keys())
                # print("r keys: ", r.keys())
                # print("termination keys: ", termination.keys())
                self.replay_memory.add(updated_frame, action, r, termination)
                frame = new_frame
                
                # Perhaps added (num_of_trials > 0)
                if num_of_trials % self.time_between_two_copies == 0:
                    # self.update_target_network(sess) # TODO
                    # self.save(saver) # TODO: For now we're using the checkpoint saver functionality, should we consider using this save approach?
                    self.checkpoint_save()
                    self.update_target_network()
                
                if self.callback:
                    self.callback()
                if all(termination.values()):
                    score = total_reward # self.env.get_total_reward()
                    # summary_str = sess.run(self.summary_op, feed_dict={self.t_score: score}) # TODO
                    # self.summary_writer.add_summary(summary_str, num_of_trials) # TODO
                    # self.summary_writer.flush()
                    with self.summary_writer.as_default():
                      tf.summary.scalar("t_score", score, step=num_of_trials)
                      self.summary_writer.flush()
                    break
            self.replay_memory.append_to_main()
    
    # TODO: Where is the following used?
    def evaluate(self):
        
        for episode in range(self.n_episode):
            total_rewards = {agent_id: 0.0 for agent_id in self.env.possible_agents}
            total_reward = 0
            frame, _ = self.env.reset()
            # self.env.get_current_feedback()
            for _ in range(self.num_nullops):
                updated_frame = { agent_id: frame[agent_id] for agent_id in self.env.agents }
                actions = self.sample_action_space() # TODO: multiagent sampling
                ## action_idx=self.env.action_space.sample()
                r, new_frame, termination = self.play(actions)
                for agent_id in r:
                    total_rewards[agent_id] += r[agent_id]
                # r, new_frame, termination = self.play(action_idx) # 2024-12-18 NOTE: Changed this from ...(action=action_idx)
                # total_reward += r
                total_reward += sum(r.values())
                # self.replay_memory.add(frame, 0, r, termination)
                self.replay_memory.add(updated_frame, actions, r, termination) # TODO: support multiagent replay memory
                frame = new_frame
            
            for _ in range(self.config['T']):
                if self.verbose:
                    print("episode {}, total reward {}".format(episode, 
                                                            total_reward))
                
                # NOTE: In the step method, the environment updates the active agents after the frame is created
                #       but before the step returns.  Before moving to the next step call, we filter the frame
                #       to the active agents
                updated_frame = { agent_id: frame[agent_id] for agent_id in self.env.agents }
                state = self.replay_memory.phi(updated_frame)
                # state = self.replay_memory.phi(frame)
                action = self.choose_action(state, self.epsilon_min)
                # action = self.choose_action(sess, state, self.epsilon_min)     
                r, new_frame, termination = self.play(action)
                for agent_id in r:
                    total_rewards[agent_id] += r[agent_id]
                # r, new_frame, termination = self.play(action)
                total_reward += sum(r.values())
                # total_reward += r
                self.replay_memory.add(updated_frame, action, r, termination)
                # self.replay_memory.add(frame, action, r, termination)
                frame = new_frame

                if self.callback:
                    self.callback()
                if all(termination.values()):
                    break
    
    # def save(self, sess, saver, model_name='model.ckpt'):
    def save(self, saver, model_name='model.ckpt'):
        # if saver:
        #     try:
        #         checkpoint_path = os.path.join(self.directory, model_name)
        #         saver.save(sess, checkpoint_path)
        #     except:
        #         pass
        if saver:
            try:
                checkpoint_path = os.path.join(self.directory, model_name)
                # TODO: Is this the correct use of tf.saved_model.save?
                saver(self.target_network, checkpoint_path)
            except:
                raise RuntimeError("Caught exception trying to save model.")

    def checkpoint_save(self, model_name='model.ckpt'):
        # TODO: The following variable should possibly be defined as a member variable of the class
        checkpoint = tf.train.Checkpoint(target_network=self.target_network)
        checkpoint_prefix = os.path.join(self.directory, model_name)
        save_path = checkpoint.save(file_prefix=checkpoint_prefix)
        print(f"Saved to checkpoint at {save_path}")
    
    # TODO
    # def load(self, sess, saver, model_name='model.ckpt'):
    #     if saver:
    #         try:
    #             checkpoint_path = os.path.join(self.directory, model_name)
    #             saver.restore(sess, checkpoint_path)
    #         except:
    #             pass
    # TODO
    # TODO: Probably moving away from the following for now
    def load(self, loader, model_name='model.ckpt'):
        try:
            checkpoint_path = os.path.join(self.directory, model_name)
            # TODO: The following is not correct
            self.target_network = loader(checkpoint_path)
            print("Printing loaded model: ")
            print(self.target_network)
        except:
            raise RuntimeError("Caught exception trying to restore model.")

    def checkpoint_restore(self, model_name='model.ckpt'):
        # TODO: The following variable should possibly be defined as a member variable of the class
        # checkpoint = tf.train.Checkpoint(target_network=self.target_network)
        checkpoint = tf.train.Checkpoint(target_network=self.target_network)
        # checkpoint_prefix = os.path.join(self.directory, model_name)
        print("checkpoint path: ", tf.train.latest_checkpoint(self.directory))
        status = checkpoint.restore(tf.train.latest_checkpoint(self.directory))

        # TODO: I have to make a call so the variables are created
        # print("Evaluating restored model on dummy variable: ", self.target_network.call( tf.constant(1.0, shape=(1, 2, 273, 1)) ))
        # print("Evaluating q_network on dummy variable: ", self.q_network.call( tf.constant(1.0, shape=(1, 2, 273, 1)) ))
        print("Evaluating restored model on dummy variable: ", self.target_network.call( tf.constant(1.0, shape=(1, 2, 312, 1)) ))
        print("Evaluating q_network on dummy variable: ", self.q_network.call( tf.constant(1.0, shape=(1, 2, 312, 1)) ))

        print("target network scope: ", self.target_network.scope)
        # You only see variables if the calls are made above
        print("q_network variables: ", self.q_network.variables)
        print("target_network variables: ", self.target_network.variables)
        # new_vars = {v.name.replace(self.target_network.scope, ''): v for v in self.target_network.variables}
        # for v in self.q_network.variables:
        #     v.assign(new_vars[v.name.replace(self.q_network.scope, '')], read_value=True)
        self.q_network.clone_op(self.target_network)
        print("updated q_network example: ", self.q_network.trainable_variables[0])

        return status
    
