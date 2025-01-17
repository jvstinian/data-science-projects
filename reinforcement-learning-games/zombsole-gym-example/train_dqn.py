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
import gym.envs
from gym.envs.registration import registry, register
from zombsole.gym_env import ZombsoleGymEnv, ZombsoleGymEnvDiscreteAction
from zombsole.renderer import OpencvRenderer
from envs.cartpole import CartPoleObservationWrapper
from dqn.q_learning import DQN
from dqn.config import DEMO, DEMO_CNN, ZOMBSOLE_MLP, ZOMBSOLE_CNN
import zombpyg.gym_env # to register the zombpyg gym environment
import prlp_demo.gym_env # to register the demo gym environment


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
    import gym
    parser = argparse.ArgumentParser(description=None)
    parser.add_argument('-c', '--config', default='zombsole_mlp', 
                        type=str, help='Game Configuration')
    parser.add_argument('-d', '--device', default='cpu', type=str, help='Device: cpu, gpu')
    parser.add_argument('-r', '--render', action='store_true', help='Use rendering callback')
    args = parser.parse_args()

    configid = args.config
    if configid == 'zombsole_mlp':
        game = gym.make('Zombsole-v0', map_name="easy_exit", rules_name="safehouse", renderer=OpencvRenderer(50, 25), initial_zombies=5)
        conf = ZOMBSOLE_MLP
    elif configid == 'zombpyg_mlp':
        # game = gym.make('zombpyg/Zombpyg-v0', map_id="easy_exit", rules_id="survival", initial_zombies=30, minimum_zombies=10, enable_rendering=True) # "log_debug/"
        # game = gym.make('zombpyg/Zombpyg-v0', map_id="open_room", rules_id="survival", initial_zombies=15, minimum_zombies=5, enable_rendering=True) # "log/"
        # game = gym.make('zombpyg/Zombpyg-v0', map_id="easy_exit", rules_id="survival", initial_zombies=40, minimum_zombies=20, player_specs="terminator:random:4", enable_rendering=True)
        game = gym.make('zombpyg/Zombpyg-v0', map_id="easy_exit", rules_id="survival", initial_zombies=40, minimum_zombies=20, enable_rendering=True)
        conf = ZOMBSOLE_MLP
        conf['input_scale'] = 2
        conf['T'] = 4000
        conf['num_episode'] = 300
        # conf['learning_rate'] = 0.1e-2 # 0.5e-2 worked for 10 zombies
        # conf['gamma'] = 0.7
        conf['learning_rate'] = 0.5e-2 # 0.1e-2 works when for easy_exit with no players # 0.5e-2 for open_room
        conf['update_interval'] = 50
        conf['batch_size'] = 1600
        conf['epsilon_decay'] = 100000
    elif configid == 'zombpyg_withplayers_mlp':
        game = gym.make('zombpyg/Zombpyg-v0', map_id="easy_exit", rules_id="survival", initial_zombies=100, minimum_zombies=50, player_specs="terminator:random:5", enable_rendering=True)
        # game = gym.make('zombpyg/Zombpyg-v0', map_id="easy_exit", rules_id="survival", initial_zombies=100, minimum_zombies=50, enable_rendering=True)
        conf = ZOMBSOLE_MLP
        conf['input_scale'] = 2
        conf['T'] = 4000
        conf['num_episode'] = 250 # 300
        # conf['learning_rate'] = 0.1e-2 # 0.5e-2 worked for 10 zombies
        # conf['gamma'] = 0.7
        conf['learning_rate'] = 0.1e-2 # 0.1e-2 works when for easy_exit with no players # 0.5e-2 for open_room
        conf['update_interval'] = 50
        conf['batch_size'] = 1600
        conf['epsilon_decay'] = 10000 # 100000
    elif configid == 'zombsole_surroundings_mlp':
        import zombsole.gym_env # to register the zombsole gym environment
        game = gym.make('jvstinian/Zombsole-SurroundingsView-v0', map_name="easy_exit_v2", rules_name="safehouse", renderer=OpencvRenderer(50, 25), initial_zombies=4)
        conf = ZOMBSOLE_MLP
        conf['num_episode'] = 2000
        conf['epsilon_min'] = 0.005 # testing
        conf['epsilon_decay'] = 50000
        conf['gamma'] = 0.9
    elif configid == 'zombsole_surroundings_cnn':
        import zombsole.gym_env # to register the zombsole gym environment
        game = gym.make('jvstinian/Zombsole-SurroundingsView-v0', map_name="easy_exit", rules_name="safehouse", renderer=OpencvRenderer(50, 25), initial_zombies=8)
        conf = ZOMBSOLE_CNN
    elif configid == 'demo_mlp':
        game = gym.make('prlp/Demo-v0')
        conf = DEMO
    elif configid == 'cartpole_mlp':
        game = CartPoleObservationWrapper()
        conf = DEMO
    else:
        game = gym.make('prlp/Demo-v0')
        conf = DEMO

    log_dir = os.path.join(conf['log_dir'], '{}/train'.format(args.config))
    if not tf.gfile.Exists(log_dir):
        tf.gfile.MakeDirs(log_dir)
    model_dir = os.path.join(conf['log_dir'], args.config)
    
    device = '/{}:0'.format(args.device)
    lcallback = game.render if args.render else None
    with tf.device(device):
        dqn = DQN(conf, game, model_dir, callback=lcallback, verbose=True)
    
    with tf.Session(config=tf.ConfigProto(allow_soft_placement=True)) as sess:
        # saver = tf.train.Saver()
        writer = tf.summary.FileWriter(truncate_dir(log_dir), sess.graph_def)
        dqn.set_summary_writer(summary_writer=writer)
        
        sess.run(tf.global_variables_initializer())
        saver = tf.train.Saver()
        if configid == "zombpyg_withplayers_mlp": # TODO
            dqn.load(sess, saver)
        dqn.train(sess, saver)
        

if __name__ == "__main__":
    main()

