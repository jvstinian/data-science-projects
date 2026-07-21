#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in dp.inc"
#endif

#ifndef DISCRETE_MODEL_TYPE
#error "DISCRETE_MODEL_TYPE is not defined in dp.inc"
#endif

#ifndef ACTION_TYPE
#error "ACTION_TYPE is not defined in dp.inc"
#endif

#ifndef ENVIRONMENT_ACTION_COUNT
#error "ENVIRONMENT_ACTION_COUNT is not defined in dp.inc"
#endif

#ifndef GET_TRANSITION_METHOD
#error "GET_TRANSITION_METHOD not defined in dp.inc"
#endif

#include <stdlib.h>

#define DP_FUNC_NAME(name) DP_CONCAT1(ENVIRONMENT_PREFIX, name)

#define DP_CONCAT1(prefix, name) DP_CONCAT2(prefix, name)
#define DP_CONCAT2(prefix, name) prefix##_##name

#ifdef DP_DECLS_ONLY

int DP_FUNC_NAME(iterative_policy_evaluation2) (DISCRETE_MODEL_TYPE* model, const float (*policy)[ENVIRONMENT_ACTION_COUNT], float df, float *value_array);

#else

int DP_FUNC_NAME(iterative_policy_evaluation2) (DISCRETE_MODEL_TYPE* model, const float (*policy)[ENVIRONMENT_ACTION_COUNT], float df, float *value_array) {
    unsigned int num_states = model->num_states;

    /* Set the value function to 0 */
    memset(value_array, 0, sizeof(float) * num_states);
    float theta = 1.0e-6; /* Convergence threshold */
    float local_delta = 0.0;

    float prev_value;
    float transition_value;
    float new_value;

    unsigned int s, s1;
    ACTION_TYPE a;
    struct TransitionProbabilityType trprob;

    int iteration_count = 0;

    while (1) {
        iteration_count += 1;
        /* printf("Iteration %d\n", iteration_count); */

        local_delta = 0.0;
        for (s = 0; s < num_states;  s++) {
            prev_value = value_array[s];
            new_value = 0.0;
            for (a = 0; a < ENVIRONMENT_ACTION_COUNT; a++) {
                transition_value = 0.0;
                for (s1 = 0; s1 < num_states;  s1++) {
                    trprob = GET_TRANSITION_METHOD(model, s, a, s1);
                    transition_value += trprob.probability * (trprob.reward + df * value_array[s1]);
                }
                new_value += policy[s][a] * transition_value;
            }
            value_array[s] = new_value;
            local_delta = fmaxf(local_delta, fabsf(new_value - prev_value));
        }
        if (local_delta < theta) {
            break;
        }
    }
    return iteration_count;
}

#endif

#undef ENVIRONMENT_PREFIX
#undef DISCRETE_MODEL_TYPE
#undef ACTION_TYPE
#undef ENVIRONMENT_ACTION_COUNT
#undef GET_TRANSITION_METHOD
#ifdef DP_DECLS_ONLY
#undef DP_DECLS_ONLY
#endif
