#!/usr/bin/env python
'''
Created: 2021-07-20
Author: Justin Smith (jvstinian@gmail.com)
'''
import os
import argparse
import tensorflow.compat.v1 as tf
tf.disable_v2_behavior()
import gym
from gym.envs.registration import registry, register
from zombsole.gym_env import ZombsoleGymEnv, ZombsoleGymEnvDiscreteAction
from zombsole.renderer import OpencvRenderer
from dqn.q_learning import DQN
from dqn.config import DEMO, DEMO_CNN, ZOMBSOLE_MLP


register(
    id='Zombsole-v0', 
    entry_point='zombsole.gym_env:ZombsoleGymEnvDiscreteAction', 
    max_episode_steps=1000,
    kwargs={
        'rules_name': 'extermination',
        'player_names': [],
        'map_name': 'bridge',
        'agent_id': 0,
        'initial_zombies': 5,
        'minimum_zombies': 0,
        'debug': False
    }
)

# Remove if exists and recreate directory
def truncate_dir(path):
    if tf.gfile.Exists(path):
        tf.gfile.DeleteRecursively(path)
    tf.gfile.MakeDirs(path)
    return path

def main():
    parser = argparse.ArgumentParser(description=None)
    parser.add_argument('-c', '--config', default='zombsole_mlp', 
                        type=str, help='Game Configuration')
    parser.add_argument('-d', '--device', default='cpu', type=str, help='Device: cpu, gpu')
    parser.add_argument('-r', '--render', default=False, type=bool, help='Use rendering callback')
    args = parser.parse_args()

    configid = args.config
    if configid == 'zombsole_mlp':
        game = gym.make('Zombsole-v0', map_name="easy_exit", rules_name="safehouse", renderer=OpencvRenderer(50, 25))
        conf = ZOMBSOLE_MLP
    else:
        game = gym.make('Zombsole-v0')
        conf = ZOMBSOLE_MLP

    log_dir = os.path.join(conf['log_dir'], '{}/train'.format(args.config))
    if not tf.gfile.Exists(log_dir):
        tf.gfile.MakeDirs(log_dir)
    model_dir = os.path.join(conf['log_dir'], args.config)
    
    device = '/{}:0'.format(args.device)
    lcallback = game.render if args.render else None
    with tf.device(device):
        dqn = DQN(conf, game, model_dir, callback=lcallback, verbose=True)
    
    with tf.Session(config=tf.ConfigProto(allow_soft_placement=True)) as sess:
        saver = tf.train.Saver()
        writer = tf.summary.FileWriter(truncate_dir(log_dir), sess.graph_def)
        dqn.set_summary_writer(summary_writer=writer)
        
        sess.run(tf.global_variables_initializer())
        dqn.train(sess, saver)
        

if __name__ == "__main__":
    main()

