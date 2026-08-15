#ifndef INC_RL_ALGS_ARRAY_OPS_H
#define INC_RL_ALGS_ARRAY_OPS_H

#include <stddef.h>

size_t arg_max(float* values, size_t length);
float max_value(float* action_values, size_t action_count);

#endif
