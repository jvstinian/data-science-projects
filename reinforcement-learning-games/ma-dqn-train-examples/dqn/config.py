"""
author: Justin Smith (jvstinian@gmail.com)
"""


ZOMBPYG_MLP = {
    'network_type': 'mlp',
    'gamma': 0.7,
    'batch_size': 1600,
    'num_episode': 300,
    'capacity': 100000,
    'epsilon_decay': 100000,
    'epsilon_min': 0.1,
    'num_frames': 2,
    'num_nullops': 3,
    'time_between_two_copies': 1000,
    'input_scale': 2.0,
    'update_interval': 50,
    'T': 6000,
    'learning_rate': 0.1e-2,
    'optimizer': 'momentum',
    'rho': 0.9, 
    'rmsprop_epsilon': 1e-6,
    
    'log_dir': 'log_madqn/'
}

