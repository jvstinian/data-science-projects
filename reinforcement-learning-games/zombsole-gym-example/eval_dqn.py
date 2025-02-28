#!/usr/bin/env python
'''
Created on Mar 28, 2018

@author: ywz
'''
import os
import argparse
import gym
from dqn.q_learning import DQN
from dqn.config import DEMO, DEMO_CNN, ZOMBSOLE_MLP
from zombsole.gym_env import ZombsoleGymEnv, ZombsoleGymEnvDiscreteAction
from gym.envs.registration import registry, register
import tensorflow.compat.v1 as tf
tf.disable_v2_behavior()


def main():
    parser = argparse.ArgumentParser(description=None)
    parser.add_argument('-g', '--game', default='demo', type=str, help='Game')
    parser.add_argument('-d', '--device', default='cpu', type=str, help='Device')
    parser.add_argument('-m', '--model-version', default='v1', type=str, help='Model version: user-specified model version for experiment tracking')
    args = parser.parse_args()
    
    rom = args.game
    model_version = args.model_version
    if rom == 'zombsole_mlp':
        # pulling this forward from the next branch
        game = gym.make('jvstinian/Zombsole-v0', map_name="easy_exit", rules_name="safehouse", render_mode="human")
        conf = ZOMBSOLE_MLP
    elif rom == 'zombpyg_mlp':
        import zombpyg.gym_env # to register the demo gym environment
        # NOTE: We make some changes relative to the training environment
        # The following has more zombies than the environment used to train
        # TODO: Need to track environment settings
        game = gym.make('jvstinian/Zombpyg-v0', map_id="tiny_space_v1", rules_id="safehouse", initial_zombies=10, minimum_zombies=10, enable_rendering=True)
        conf = ZOMBPYG_MLP
        # TODO: Perhaps make num_episode a command-line argument?
        conf['num_episode'] = 2 # 10
    elif rom == 'zombpyg_withplayers_mlp':
        import zombpyg.gym_env # to register the demo gym environment
        game = gym.make('jvstinian/Zombpyg-v0', map_id="easy_exit", rules_id="survival", initial_zombies=10, minimum_zombies=10, player_specs="terminator:random:5", enable_rendering=True)
        conf = ZOMBPYG_MLP
        conf['num_episode'] = 2 # 10
    elif rom == 'zombsole_surroundings_mlp':
        import zombsole.gym_env # to register the demo gym environment
        game = gym.make('jvstinian/Zombsole-SurroundingsView-v0', map_name="easy_exit_v2", rules_name="safehouse", render_mode="human", initial_zombies=4)
        conf = ZOMBSOLE_MLP
        conf['num_episode'] = 5
        conf['epsilon_min'] = 0.005
        # conf['T'] = 50
    elif rom == 'demo_mlp':
        import prlp_demo.gym_env # to register the demo gym environment
        game = gym.make('prlp/Demo-v0')
        conf = DEMO
        conf['num_episode'] = 2
    else:
        game = gym.make('jvstinian/Zombsole-v0')
        conf = ZOMBSOLE_MLP

    model_dir = os.path.join(conf['log_dir'], rom, model_version)
    device = '/{}:0'.format(args.device)
    with tf.device(device):
        dqn = DQN(conf, game, model_dir, callback=game.render, verbose=True)
    
    with tf.Session(config=tf.ConfigProto(allow_soft_placement=True)) as sess:
        summary_writer = tf.summary.FileWriter(str(model_dir), sess.graph)
        dqn.set_summary_writer(summary_writer)
        sess.run(tf.global_variables_initializer())
        saver = tf.train.Saver()
        dqn.load(sess, saver)
        dqn.evaluate(sess)
        

if __name__ == "__main__":
    main()
