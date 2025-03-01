#!/usr/bin/env python
'''
Created on Mar 28, 2018

@author: ywz
'''
import os
import argparse
import gym
from dqn.q_learning import DQN
from dqn.config import (
    ZombsoleMLPConfig, ZombsoleSurroundingsMLPConfig, ZombsoleCNNConfig,
    ZombpygMLPConfig, ZombpygWithPlayersMLPConfig,
    DemoConfig, CartpoleConfig,
)
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
    config = None
    if rom == 'zombsole_mlp':
        config = ZombsoleMLPConfig
        env_config = config['environment']
        env_config.update(config.get('eval_overrides', {}).get('environment', {}))
        game = gym.make('jvstinian/Zombsole-v0', **env_config)
    elif rom == 'zombpyg_mlp':
        import zombpyg.gym_env # to register the demo gym environment
        config = ZombpygMLPConfig
        env_config = config['environment']
        env_config.update(config.get('eval_overrides', {}).get('environment', {}))
        game = gym.make('jvstinian/Zombpyg-v0', **env_config)
    elif rom == 'zombpyg_withplayers_mlp':
        import zombpyg.gym_env # to register the demo gym environment
        config = ZombpygWithPlayersMLPConfig
        env_config = config['environment']
        env_config.update(config.get('eval_overrides', {}).get('environment', {}))
        game = gym.make('jvstinian/Zombpyg-v0', **env_config)
    elif rom == 'zombsole_surroundings_mlp':
        import zombsole.gym_env # to register the demo gym environment
        config = ZombsoleSurroundingsMLPConfig
        env_config = config['environment']
        env_config.update(config.get('eval_overrides', {}).get('environment', {}))
        game = gym.make('jvstinian/Zombsole-SurroundingsView-v0', **env_config)
    elif rom == 'demo_mlp':
        import prlp_demo.gym_env # to register the demo gym environment
        config = DemoConfig
        game = gym.make('prlp/Demo-v0')
    else:
        config = DemoConfig
        game = gym.make('prlp/Demo-v0')

    # Set up model configuration with overrides for eval
    conf = config['model']
    conf.update(config.get('eval_overrides', {}).get('model', {}))

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
