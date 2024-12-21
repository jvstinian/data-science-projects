'''
Created on Mar 25, 2018

@author: ywz

Modifications made by 
Justin Smith (jvstinian@gmail.com) in 2021
'''


DEMO = {
    'network_type': 'mlp',
    'gamma': 0.7,
    'batch_size': 32,
    'num_episode': 400,
    'capacity': 20000,
    'epsilon_decay': 100000,
    'epsilon_min': 0.1,
    'num_frames': 1,
    'num_nullops': 2,
    'time_between_two_copies': 1000,
    'input_scale': 1.0,
    'update_interval': 1,
    'T': 1000000,
    
    'learning_rate': 0.5e-2,
    'optimizer': 'momentum',
    'rho': 0.9,
    'rmsprop_epsilon': 1e-6,
    
    'log_dir': 'log/'
}

DEMO_CNN = {
    'network_type': 'cnn',
    'gamma': 0.99,
    'batch_size': 32,
    'num_episode': 500000,
    'capacity': 20000, # ATARI had 1000000
    'epsilon_decay': 1000000,
    'epsilon_min': 0.1,
    'num_frames': 4,
    'num_nullops': 5,
    'time_between_two_copies': 10000,
    'input_scale': 1.0, # ATARI had 255.0
    'update_interval': 1,
    'T': 1000000,
    
    'learning_rate': 2e-4,
    'optimizer': 'rmsprop',
    'rho': 0.99,
    'rmsprop_epsilon': 1e-6,
    
    'log_dir': 'log/'
}

ZOMBSOLE_MLP = {
    'network_type': 'mlp',
    'gamma': 0.7,
    'batch_size': 32,
    'num_episode': 1000,
    'capacity': 20000,
    'epsilon_decay': 10000, # was 100000
    'epsilon_min': 0.1,
    'num_frames': 2,
    'num_nullops': 3, # tried 10 also
    'time_between_two_copies': 1000,
    'input_scale': 1.0,
    'update_interval': 1,
    'T': 990,
    
    'learning_rate': 0.5e-2,
    'optimizer': 'momentum', # tried rmsprop
    'rho': 0.9, # tried 0.0
    'rmsprop_epsilon': 1e-6,
    
    'log_dir': 'log/'
}

