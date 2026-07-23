#include <reinforcementlearning/algorithms/array_ops.h>
#include <float.h> /* FLT_MAX */

/* TODO: Move to dp.inc or to another header */
size_t arg_max(float* values, size_t length) {
    float max_value = -FLT_MAX;
    size_t best_idx = 0;
    size_t i;

    for (i = 0; i < length; i++) {
        if (values[i] > max_value) {
            max_value = values[i];
            best_idx = i;
        }
    }
    return best_idx;
}

float max_value(float* action_values, size_t action_count) {
    float maxval = -FLT_MAX;
    size_t a;

    for (a = 0; a < action_count; a++) {
        if (action_values[a] > maxval) {
            maxval = action_values[a];
        }
    }
    return maxval;
}
