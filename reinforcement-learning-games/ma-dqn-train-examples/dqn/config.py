"""
author: Justin Smith (jvstinian@gmail.com)
"""


ZOMBPYG_MLP = {
    # 'max_agents': 4, # TODO: Is something like this needed?
    'network_type': 'mlp',
    'gamma': 0.7,
    'batch_size': 1600,
    'num_episode': 400,
    'capacity': 100000,
    'epsilon_decay': 100000,
    'epsilon_min': 0.1,
    'num_frames': 2,
    'num_nullops': 3,
    'time_between_two_copies': 1000,
    'input_scale': 2.0,
    'update_interval': 50,
    'T': 1490,
    
    'learning_rate': 0.5e-2, # tried as low as 0.1e-2 for single-agent
    'optimizer': 'momentum',
    'rho': 0.9, 
    'rmsprop_epsilon': 1e-6,
    
    'log_dir': 'log_madqn/'
}

