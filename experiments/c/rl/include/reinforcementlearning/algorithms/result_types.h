#ifndef INC_RL_ALGS_RESULT_TYPES_H
#define INC_RL_ALGS_RESULT_TYPES_H

#include <stddef.h>

struct SimulationSummary {
    size_t num_steps;
    float total_reward;
};

#endif
