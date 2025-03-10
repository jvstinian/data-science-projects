'''
Created on Mar 28, 2018

@author: ywz
'''
import os
import argparse
import gym
from dqn.q_learning import DQN
from dqn.config import ZOMBPYG_MLP
from gym.envs.registration import registry, register
import tensorflow as tf
from zombpyg.multiagent_env import MultiagentZombpygEnv


def main():
    
    parser = argparse.ArgumentParser(description=None)
    parser.add_argument('-g', '--game', default='demo', type=str, help='Game')
    parser.add_argument('-d', '--device', default='cpu', type=str, help='Device')
    args = parser.parse_args()
    
    rom = args.game
    if rom == 'zombpyg_mlp':
        world_config={
            "tag": "SingleMap",
            "parameters": {
                "map_id": "tiny_space_v1",
                "w": 640, 
                "h": 480,
                "initial_zombies": 20,
                "minimum_zombies": 10,
            }
        }
        game = MultiagentZombpygEnv(
            world_config=world_config,
            rules_id="safehouse", 
            # map_id="elevator", 
            # initial_zombies=50, minimum_zombies=5, 
            agent_ids = [str(i) for i in range(3)],
            agent_weapons="rifle",
            player_specs="",
            enable_rendering=True,
            friendly_fire_guard=True
        )
        conf = ZOMBPYG_MLP
        conf['num_episode'] = 2
        conf['T'] = 6000
        # conf['T'] = 500
    elif rom == '1pzombpyg_mlp':
        # import zombpyg.gym_env # to register the demo gym environment
        world_config = {
            "tag": "RandomMap",
            "parameters": [
                {
                    "weight": 0.60,
                    "map_builder": {
                        "tag": "SingleMap",
                        "parameters": {
                            "map_id": "tiny_space_v1",
                            "w": 640, 
                            "h": 480,
                            "initial_zombies": 20,
                            "minimum_zombies": 10,
                        }
                    }
                },
                {
                    "weight": 0.20,
                    "map_builder": {
                        "tag": "SingleMap",
                        "parameters": {
                            "map_id": "elevator",
                            "w": 640, 
                            "h": 480,
                            "initial_zombies": 40,
                            "minimum_zombies": 20,
                        }
                    }
                },
                {
                    "weight": 0.20,
                    "map_builder": {
                        "tag": "SingleMap",
                        "parameters": {
                            "map_id": "catacombs",
                            "w": 640, 
                            "h": 480,
                            "initial_zombies": 40,
                            "minimum_zombies": 20,
                        }
                    }
                }
            ]
        }
        game = MultiagentZombpygEnv(
            world_config,
            # map_id="easy_exit",
            # initial_zombies=20, minimum_zombies=0, 
            rules_id="safehouse", 
            agent_ids = [str(i) for i in range(1)],
            agent_weapons="rifle",
            player_specs="",
            enable_rendering=True
        )
        conf = ZOMBPYG_MLP
        conf['num_episode'] = 5
    else:
        raise ValueError(f"config {rom} not supported")

    model_dir = os.path.join(conf['log_dir'], rom)
    device = '/{}:0'.format(args.device)
    with tf.device(device):
        dqn = DQN(conf, game, model_dir, callback=game.render, verbose=True)
    
    # TODO: Initially was using the tf.saved_model.load,
    #       but this format appears to be oriented to serving predictions,
    #       and didn't seem to provide the same interaction we had with v1.
    #       We try the checkpoint approach now, but due to the new API, 
    #       we refactoring the saving and restoring methods.
    # loader = tf.saved_model.load
    writer = tf.summary.create_file_writer(model_dir)
    dqn.set_summary_writer(summary_writer=writer)
    # dqn.load(loader)
    status = dqn.checkpoint_restore()
    # print("Evaluating restored model: ", dqn.target_network.call( tf.constant(1.0, shape=(1, 2, 273, 1)) ))
    dqn.evaluate()
    print(status.assert_consumed())  # Optional sanity checks.


if __name__ == "__main__":
    main()
