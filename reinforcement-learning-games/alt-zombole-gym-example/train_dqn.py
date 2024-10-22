'''
Created: 2021-07-20
Author: Justin Smith (jvstinian@gmail.com)
'''
import os
import argparse
# import tensorflow as tf
# from config import DEMO
# from task import Task
# from dpg import DPG
from dqn.q_learning import DQN
from dqn.config import DEMO, DEMO_CNN, ZOMBSOLE_MLP
# from dqn.environment import new_atari_game, new_demo
from dqn.environment import new_zombpyg, new_demo
import gym
from zombsole.gym_env import ZombsoleGymEnv
from gym.envs.registration import registry, register
import tensorflow.compat.v1 as tf
tf.disable_v2_behavior()

register(
    id='Zombsole-v0', 
    entry_point='zombsole.gym_env:ZombsoleGymEnv', 
    max_episode_steps=1000,
    kwargs={
        'rules_name': 'extermination',
        # 'player_names': ['sniper', 'sniper', 'sniper', 'terminator'],
        'player_names': [],
        'map_name': 'bridge',
        'agent_id': 'agent00',
        'initial_zombies': 10,
        'minimum_zombies': 0,
        'debug': False
    }
)
# register(
#     id='Zombpyg-v0', 
#     entry_point='zombpyg.gym_env:ZombpygGymEnv', 
#     max_episode_steps=300*50,
#     kwargs={
#     }
# )

def delete_dir(path):
    if tf.gfile.Exists(path):
        tf.gfile.DeleteRecursively(path)
    tf.gfile.MakeDirs(path)
    return path


def main():
    parser = argparse.ArgumentParser(description=None)
    parser.add_argument('-c', '--config', default='zombsole_mlp', 
                        type=str, help='Game Configuration')
    parser.add_argument('-d', '--device', default='cpu', type=str, help='Device: cpu, gpu')
    args = parser.parse_args()

    configid = args.config
    if configid == 'zombsole_mlp':
        game = gym.make('Zombsole-v0')
        conf = ZOMBSOLE_MLP
    elif configid == 'zombpyg_mlp':
        game_qwargs = {
            "initial_zombies": 8, 
            "minimum_zombies": 1, 
            "rules_id": "safehouse",
            "map_id": "open_room",
            "verbose": True,
        }
        game = gym.make('zombpyg.gym_env:zombpyg/Zombpyg-v0', **game_qwargs)
        # conf = ZOMBSOLE_MLP
        conf = DEMO
    elif configid == 'demo_mlp':
        game = new_demo()
        conf = DEMO
    else:
        game = gym.make('Zombsole-v0')
        conf = ZOMBSOLE_MLP

    # task = Task(args.task)
    log_dir = os.path.join(conf['log_dir'], '{}/train'.format(args.config))
    if not tf.gfile.Exists(log_dir):
        tf.gfile.MakeDirs(log_dir)
    model_dir = os.path.join(conf['log_dir'], args.config)
    
    device = '/{}:0'.format(args.device)
    with tf.device(device):
        dqn = DQN(conf, game, model_dir, callback=game.render, verbose=True) # TODO
        # dqn = DQN(conf, game, model_dir, callback=None, verbose=True)
    
    with tf.Session(config=tf.ConfigProto(allow_soft_placement=True)) as sess:
        saver = tf.train.Saver()
        writer = tf.summary.FileWriter(delete_dir(log_dir), sess.graph_def)
        dqn.set_summary_writer(summary_writer=writer)
        
        sess.run(tf.global_variables_initializer())
        dqn.train(sess, saver)
        

if __name__ == "__main__":
    main()

