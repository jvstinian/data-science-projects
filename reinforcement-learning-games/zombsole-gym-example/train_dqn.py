#!/usr/bin/env python
'''
Created: 2021-07-20
Author: Justin Smith (jvstinian@gmail.com)
'''
import os
import sys
import argparse
import gymnasium as gym
from gymnasium.envs.registration import registry, register
import tensorflow as tf
# import json
from zombsole.gym_env import ZombsoleGymEnv, ZombsoleGymEnvDiscreteAction
from envs.cartpole import CartPoleObservationWrapper
from dqn.q_learning import DQN
from dqn.config import (
    ZombsoleMLPConfig, ZombsoleSurroundingsMLPConfig, ZombsoleCNNConfig,
    ZombpygMLPConfig, ZombpygWithPlayersMLPConfig,
    DemoConfig, CartpoleConfig,
)
import zombpyg.gym_env # to register the zombpyg gym environment
import prlp_demo.gym_env # to register the demo gym environment

# TODO: Is this registration needed or is it handled in the import?
# register(
#     id='Zombsole-v0', 
#     # entry_point='zombsole.gym_env:ZombsoleGymEnv', 
#     entry_point='zombsole.gym_env:ZombsoleGymEnvDiscreteAction', 
#     max_episode_steps=1000,
#     kwargs={
#         'rules_name': 'extermination',
#         'player_names': [],
#         'map_name': 'bridge',
#         'agent_id': 0,
#         'initial_zombies': 5,
#         'minimum_zombies': 0,
#         'debug': False
#     }
# )

# Remove if exists and recreate directory
def truncate_dir(path):
    if tf.io.gfile.exists(path):
        tf.io.gfile.rmtree(path)
    tf.io.gfile.makedirs(path)
    return path


def main():
    parser = argparse.ArgumentParser(description=None)
    parser.add_argument('-c', '--config', default='zombsole_mlp', 
                        type=str, help='Game Configuration')
    parser.add_argument('-d', '--device', default='cpu', type=str, help='Device: cpu, gpu')
    parser.add_argument('-m', '--model-version', default='v1', type=str, help='Model version: user-specified model version for experiment tracking')
    parser.add_argument('-r', '--render', action='store_true', help='Use rendering callback')
    args = parser.parse_args()

    configid = args.config
    model_version = args.model_version
    if model_version == "":
        print("ERROR: Model version must be a non-empty string", file=sys.stderr)
        sys.exit(1)

    config = None
    if configid == 'zombsole_mlp':
        config = ZombsoleMLPConfig
        env_config = config['environment']
        game = gym.make('jvstinian/Zombsole-v0', **env_config)
    elif configid == 'zombpyg_mlp':
        config = ZombpygMLPConfig
        env_config = config['environment']
        game = gym.make('jvstinian/Zombpyg-v0', **env_config)
    elif configid == 'zombpyg_withplayers_mlp':
        config = ZombpygWithPlayersMLPConfig
        env_config = config['environment']
        game = gym.make('jvstinian/Zombpyg-v0', **env_config)
    elif configid == 'zombsole_surroundings_mlp':
        import zombsole.gym_env # to register the zombsole gym environment
        config = ZombsoleSurroundingsMLPConfig
        env_config = config['environment']
        game = gym.make('jvstinian/Zombsole-SurroundingsView-v0', **env_config)
    elif configid == 'zombsole_surroundings_cnn':
        import zombsole.gym_env # to register the zombsole gym environment
        config = ZombsoleCNNConfig
        env_config = config['environment']
        game = gym.make('jvstinian/Zombsole-SurroundingsView-v0', **env_config)
    elif configid == 'demo_mlp':
        config = DemoConfig
        game = gym.make('prlp/Demo-v0')
    elif configid == 'cartpole_mlp':
        config = CartpoleConfig
        game = CartPoleObservationWrapper()
    else:
        config = DemoConfig
        game = gym.make('prlp/Demo-v0')

    # TODO: From the merge conflict
    # log_dir = os.path.join(conf['log_dir'], '{}/train'.format(args.config))
    # if not tf.io.gfile.exists(log_dir):
    #     tf.io.gfile.makedirs(log_dir)
    # model_dir = os.path.join(conf['log_dir'], args.config)
    
    conf = config['model']
    model_dir = os.path.join(conf['log_dir'], args.config, model_version)
    log_dir = os.path.join(model_dir, 'train')
    if not tf.gfile.Exists(log_dir):
        tf.gfile.MakeDirs(log_dir)
    
    with open(os.path.join(model_dir, "run_config.json"), "w") as run_config_file:
        json.dump(config, run_config_file, indent=4)

    device = '/{}:0'.format(args.device)
    lcallback = game.render if args.render else None
    device = '/{}:0'.format(args.device)
    with tf.device(device):
        dqn = DQN(conf, game, model_dir, callback=lcallback, verbose=True)

    # TODO
    # with tf.Session(config=tf.ConfigProto(allow_soft_placement=True)) as sess:
    #     saver = tf.train.Saver()
    #     writer = tf.summary.FileWriter(delete_dir(log_dir), sess.graph_def)
    #     dqn.set_summary_writer(summary_writer=writer)
    #     
    #     sess.run(tf.global_variables_initializer())
    #     if configid == "zombpyg_withplayers_mlp": # TODO: Figure out how to train agents when there are other players or agents
    #         dqn.load(sess, saver)
    #     dqn.train(sess, saver)
    # NOTE: The following probably wasn't the way to go
    # def saver(model):
    #     tf.saved_model.save(model, model_dir)
    saver = tf.saved_model.save
    writer = tf.summary.create_file_writer(truncate_dir(log_dir))
    dqn.set_summary_writer(summary_writer=writer)
    dqn.train(saver)

    # TODO: From the merge conflict
    # with tf.Session(config=tf.ConfigProto(allow_soft_placement=True)) as sess:
    #     saver = tf.train.Saver()
    #     writer = tf.summary.FileWriter(truncate_dir(log_dir), sess.graph_def)
    #     dqn.set_summary_writer(summary_writer=writer)
    #     
    #     sess.run(tf.global_variables_initializer())
    #     if tf.train.latest_checkpoint(model_dir) is not None:
    #         dqn.load(sess, saver)
    #     # if configid == "zombpyg_withplayers_mlp":
    #     #     dqn.load(sess, saver)
    #     # elif configid == "zombpyg_mlp":
    #     #     dqn.load(sess, saver)
    #     dqn.train(sess, saver)
        

if __name__ == "__main__":
    main()

