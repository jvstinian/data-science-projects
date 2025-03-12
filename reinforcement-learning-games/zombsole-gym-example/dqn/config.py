'''
Created on Mar 25, 2018

@author: ywz

Modifications made by 
Justin Smith (jvstinian@gmail.com) in 2021
'''
import copy


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

CARTPOLE = copy.deepcopy(DEMO)
CARTPOLE['epsilon_decay'] = 50000
CARTPOLE['num_episode'] = 2000
CARTPOLE['T'] = 200

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

ZOMBPYG_MLP = {
    'network_type': 'mlp',
    'gamma': 0.7,
    'batch_size': 1600,
    'num_episode': 300,
    'capacity': 20000,
    'epsilon_decay': 100000,
    'epsilon_min': 0.1,
    'num_frames': 2,
    'num_nullops': 3, # tried 10 also
    'time_between_two_copies': 1000, # also tried 100
    'input_scale': 2.0,
    'update_interval': 50,
    'T': 6000,
    'learning_rate': 0.1e-2,
    'optimizer': 'momentum', # tried rmsprop
    'rho': 0.9, # tried 0.0
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
    'time_between_two_copies': 1000, # also tried 100
    'input_scale': 2048.0,
    'update_interval': 1,
    'T': 290, # changed from 990
    'learning_rate': 0.01, # also tried 0.5e-2, 0.1
    'optimizer': 'momentum', # tried rmsprop
    'rho': 0.9, # tried 0.0
    'rmsprop_epsilon': 1e-6,
    'log_dir': 'log/'
}

ZOMBSOLE_CNN = {
    'network_type': 'cnn',
    'gamma': 0.99,
    'batch_size': 32,
    'num_episode': 10000,
    'capacity': 200000, # ATARI had 1000000
    'epsilon_decay': 100000,
    'epsilon_min': 0.1,
    'num_frames': 4,
    'num_nullops': 5,
    'time_between_two_copies': 500,
    'input_scale': 1.0,
    'update_interval': 1,
    'T': 290,
    
    'learning_rate': 2e-4,
    'optimizer': 'rmsprop',
    'rho': 0.99,
    'rmsprop_epsilon': 1e-6,
    
    'log_dir': 'log/'
}

ZOMBSOLE_SURROUNDINGS_MLP = copy.deepcopy(ZOMBSOLE_MLP)
ZOMBSOLE_SURROUNDINGS_MLP['num_episode'] = 2000
ZOMBSOLE_SURROUNDINGS_MLP['epsilon_min'] = 0.005
ZOMBSOLE_SURROUNDINGS_MLP['epsilon_decay'] = 50000
ZOMBSOLE_SURROUNDINGS_MLP['gamma'] = 0.9

ZombsoleMLPConfig = {
    'environment': {
        'map_name': 'easy_exit', 
        'rules_name': 'safehouse', 
        'render_mode': 'human', 
        'initial_zombies': 5
    },
    'model': ZOMBSOLE_MLP
}

ZombsoleSurroundingsMLPConfig = {
    'environment': {
        'map_name': 'easy_exit_v2', 
        'rules_name': 'safehouse', 
        'render_mode': 'human', 
        'initial_zombies': 4
    },
    'model': ZOMBSOLE_SURROUNDINGS_MLP,
    'eval_overrides': {
        'model': {
            'num_episode': 5
        }
    }
}

ZombsoleCNNConfig = {
    'environment': {
        'map_name': 'easy_exit',
        'rules_name': 'safehouse', 
        'render_mode': 'human', 
        'initial_zombies': 8
    },
    'model': ZOMBSOLE_CNN
}

ZombpygMLPConfig = {
    'environment': {
        'map_id': 'tiny_space_v1',
        'rules_id': 'safehouse',
        'initial_zombies': 10,
        'minimum_zombies': 10,
        'enable_rendering': True
    },
    'model': ZOMBPYG_MLP,
    'eval_overrides': {
        'model': {
            'num_episode': 2
        }
    }
}

ZombpygWithPlayersMLPConfig = {
    'environment': {
        'map_id': 'easy_exit',
        'rules_id': 'survival',
        'initial_zombies': 100,
        'minimum_zombies': 50,
        'player_specs': 'terminator:random:5',
        'enable_rendering': True
    },
    'model': ZOMBPYG_MLP,
    'eval_overrides': {
        'model': {
            'num_episode': 2
        }
    }
}

DemoConfig = {
    'environment': None,
    'model': DEMO,
    'eval_overrides': {
        'model': {
            'num_episode': 2
        }
    }
}

CartpoleConfig = {
    'environment': None,
    'model': CARTPOLE,
    'eval_overrides': {
        'model': {
            'num_episode': 10
        }
    }
}

