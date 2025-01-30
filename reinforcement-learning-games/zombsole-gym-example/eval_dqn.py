'''
Created on Mar 28, 2018

@author: ywz
'''
import os
import argparse
#import tensorflow as tf
import gym
from dqn.q_learning import DQN
from dqn.config import DEMO, DEMO_CNN, ZOMBSOLE_MLP
from zombsole.gym_env import ZombsoleGymEnv, ZombsoleGymEnvDiscreteAction
from zombsole.renderer import OpencvRenderer
from gym.envs.registration import registry, register
# from dqn.environment import new_atari_game, new_demo, new_zombpyg
import tensorflow.compat.v1 as tf
tf.disable_v2_behavior()

# TODO: This needs to be dried up
register(
    id='Zombsole-v0', 
    # entry_point='zombsole.gym_env:ZombsoleGymEnv', 
    entry_point='zombsole.gym_env:ZombsoleGymEnvDiscreteAction', 
    max_episode_steps=1000,
    kwargs={
        'rules_name': 'extermination',
        # 'player_names': ['sniper', 'sniper', 'sniper', 'terminator'],
        'player_names': [],
        'map_name': 'bridge',
        'agent_id': 0, # 'agent00',
        'initial_zombies': 5,
        'minimum_zombies': 0,
        'debug': False
    }
)

def main():
    
    parser = argparse.ArgumentParser(description=None)
    parser.add_argument('-g', '--game', default='demo', type=str, help='Game')
    parser.add_argument('-d', '--device', default='cpu', type=str, help='Device')
    args = parser.parse_args()
    
    rom = args.game
    # TODO: Come back to this later
    # if rom == 'demo':
    #     game = new_demo()
    #     conf = DEMO
    # elif rom == 'zombpyg_mlp':
    #     game_qwargs = {
    #         "initial_zombies": 8, 
    #         "minimum_zombies": 1, 
    #         "rules_id": "safehouse",
    #         "map_id": "open_room",
    #         "verbose": True,
    #     }
    #     game = gym.make('zombpyg.gym_env:zombpyg/Zombpyg-v0', **game_qwargs)
    #     # conf = ZOMBSOLE_MLP
    #     conf = DEMO
    # else:
    #     game = gym.make('Zombsole-v0')
    #     conf = ZOMBSOLE_MLP
    if rom == 'zombsole_mlp':
        # game = gym.make('Zombsole-v0')
        # pulling this forward from the next branch
        game = gym.make('Zombsole-v0', map_name="easy_exit", rules_name="safehouse", renderer=OpencvRenderer(50, 25))
        conf = ZOMBSOLE_MLP
    elif rom == 'zombpyg_mlp':
        import zombpyg.gym_env # to register the demo gym environment
        # NOTE: We make some changes relative to the training environment
        # The following has more zombies that the environment used to train
        # TODO: Need to track environment settings
        game = gym.make('zombpyg/Zombpyg-v0', map_id="easy_exit", rules_id="extermination", initial_zombies=50, minimum_zombies=30, enable_rendering=True)
        # game = gym.make('zombpyg/Zombpyg-v0', map_id="open_room", rules_id="extermination", initial_zombies=15, minimum_zombies=0, enable_rendering=True)
        # game = gym.make('zombpyg/Zombpyg-v0', map_id="open_room", rules_id="extermination", initial_zombies=15, minimum_zombies=0, enable_rendering=True)
        # game = gym.make('zombpyg/Zombpyg-v0', map_id="open_room", rules_id="survival", initial_zombies=25, minimum_zombies=5, enable_rendering=True)
        # TODO: Need zombpyg specific model configuration.
        conf = ZOMBSOLE_MLP
        conf['input_scale'] = 2
        conf['T'] = 4000
        # TODO: Perhaps make num_episode a command-line argument?
        conf['num_episode'] = 2 # 10
        conf['epsilon_min'] = 0.1 # this is the default, not needed
        # conf['log_dir'] = "log_debug/"
        # conf['log_dir'] = "log/"
        # conf['learning_rate'] = 0.5e-2
        # conf['epsilon_decay'] = 100000
    elif rom == 'zombpyg_withplayers_mlp':
        import zombpyg.gym_env # to register the demo gym environment
        # game = gym.make('zombpyg/Zombpyg-v0', map_id="easy_exit", rules_id="survival", initial_zombies=50, minimum_zombies=25, player_specs="terminator:random:2", enable_rendering=True)
        game = gym.make('zombpyg/Zombpyg-v0', map_id="easy_exit", rules_id="survival", initial_zombies=10, minimum_zombies=10, player_specs="terminator:random:5", enable_rendering=True)
        conf = ZOMBSOLE_MLP
        conf['input_scale'] = 2
        conf['T'] = 4000
        conf['num_episode'] = 2 # 10
        conf['epsilon_min'] = 0.1 # this is the default, not needed
    elif rom == 'zombsole_surroundings_mlp':
        import zombsole.gym_env # to register the demo gym environment
        game = gym.make('jvstinian/Zombsole-SurroundingsView-v0', map_name="easy_exit_v2", rules_name="safehouse", renderer=OpencvRenderer(50, 25), initial_zombies=4)
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
        game = gym.make('Zombsole-v0')
        conf = ZOMBSOLE_MLP

    model_dir = os.path.join(conf['log_dir'], rom)
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
