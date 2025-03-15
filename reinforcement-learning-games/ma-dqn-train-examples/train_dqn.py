#!/usr/bin/env python
'''
Created: 2021-07-20
Author: Justin Smith (jvstinian@gmail.com)
'''
import os
import argparse
import gym
from gym.envs.registration import registry, register
import tensorflow as tf
from dqn.q_learning import DQN
from dqn.config import ZOMBPYG_MLP
from zombpyg.multiagent_env import MultiagentZombpygEnv


# Remove if exists and recreate directory
def truncate_dir(path):
    if tf.io.gfile.exists(path):
        tf.io.gfile.rmtree(path)
    tf.io.gfile.makedirs(path)
    return path


def main():
    parser = argparse.ArgumentParser(description=None)
    parser.add_argument('-c', '--config', default='zombpyg_mlp', 
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

    if configid == 'zombpyg_mlp':
        world_config={
            "tag": "SingleMap",
            "parameters": {
                "map_id": "tiny_space_v1",
                "w": 640, 
                "h": 480,
                "initial_zombies": 20,
                "minimum_zombies": 15,
            }
        }
        game = MultiagentZombpygEnv(
            world_config=world_config,
            rules_id="safehouse",
            # map_id="elevator",
            # initial_zombies=50,
            # minimum_zombies=15,
            agent_ids = [str(i) for i in range(3)],
            agent_weapons="rifle",
            player_specs="",
            enable_rendering=True,
            friendly_fire_guard=True
        )
        conf = ZOMBPYG_MLP
    elif configid == '1pzombpyg_mlp':
        #import zombpyg.gym_env # to register the demo gym environment
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
            world_config=world_config,
            rules_id="safehouse",
            agent_ids = [str(i) for i in range(1)],
            agent_weapons="rifle",
            player_specs="",
            enable_rendering=True
        )
        conf = ZOMBPYG_MLP
    else:
        raise ValueError(f"config {configid} not supported")
        
    model_dir = os.path.join(conf['log_dir'], args.config, model_version)
    log_dir = os.path.join(model_dir, 'train')
    if not tf.io.gfile.exists(log_dir):
        tf.io.gfile.makedirs(log_dir)
    
    lcallback = game.render if args.render else None
    device = '/{}:0'.format(args.device)
    with tf.device(device):
        dqn = DQN(conf, game, model_dir, callback=lcallback, verbose=True)

    saver = tf.saved_model.save
    writer = tf.summary.create_file_writer(truncate_dir(log_dir))
    dqn.set_summary_writer(summary_writer=writer)
    if tf.train.latest_checkpoint(model_dir) is not None:
        _ = dqn.checkpoint_restore()
    dqn.train(saver)


if __name__ == "__main__":
    main()

