#ifndef INC_RL_ALGS_EPSILON_POLICIES_H
#define INC_RL_ALGS_EPSILON_POLICIES_H

#include <reinforcementlearning/algorithms/td_config.h>

float update_epsilon(struct SARSAConfig sarsa_config, unsigned int episode);

#endif
