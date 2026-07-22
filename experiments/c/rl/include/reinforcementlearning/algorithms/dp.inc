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

#ifndef PRINT_POLICY_METHOD
#error "PRINT_POLICY_METHOD not defined in dp.inc"
#endif

#include <stdlib.h>

#define DP_FUNC_NAME(name) DP_CONCAT1(ENVIRONMENT_PREFIX, name)

#define DP_CONCAT1(prefix, name) DP_CONCAT2(prefix, name)
#define DP_CONCAT2(prefix, name) prefix##_##name

/* We explicitly define a macro for the iterative_deterministic_policy_evaluation method,
   as it is called in the other methods */
#define DP_ITER_DET_POL_EVAL_METHOD DP_FUNC_NAME(iterative_deterministic_policy_evaluation)

#ifdef DP_DECLS_ONLY

int DP_FUNC_NAME(iterative_policy_evaluation) (DISCRETE_MODEL_TYPE* model, const float (*policy)[ENVIRONMENT_ACTION_COUNT], float df, float *value_array);
int DP_FUNC_NAME(iterative_deterministic_policy_evaluation) (
    DISCRETE_MODEL_TYPE* model, const ACTION_TYPE* dpolicy, float df, float *value_array
);
int DP_FUNC_NAME(policy_iteration_with_init) (
    DISCRETE_MODEL_TYPE* model, float df, float *value_array, ACTION_TYPE* dpolicy
);
int DP_FUNC_NAME(policy_iteration) (
    DISCRETE_MODEL_TYPE* model, float df, int* num_iterations
);

#else

int DP_FUNC_NAME(iterative_policy_evaluation) (DISCRETE_MODEL_TYPE* model, const float (*policy)[ENVIRONMENT_ACTION_COUNT], float df, float *value_array) {
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

int DP_FUNC_NAME(iterative_deterministic_policy_evaluation) (
    DISCRETE_MODEL_TYPE* model, const ACTION_TYPE* dpolicy, float df, float *value_array
) {
    unsigned int num_states = model->num_states;

    /* We assume the value_array has been initialized prior to calling
     * the method */
    float theta = 1.0e-6; /* Convergence threshold */
    float local_delta = 0.0;

    float prev_value;
    float new_value;

    unsigned int s, s1;
    ACTION_TYPE a;
    struct TransitionProbabilityType trprob;

    int iteration_count = 0;
    
    while (1) {
        iteration_count += 1;

        local_delta = 0.0;
        for (s = 0; s < num_states;  s++) {
            prev_value = value_array[s];
            a = dpolicy[s];
            new_value = 0.0;
            for (s1 = 0; s1 < num_states;  s1++) {
                trprob = frozenlake_get_transition(model, s, a, s1);
                new_value += trprob.probability * (trprob.reward + df * value_array[s1]);
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

int DP_FUNC_NAME(policy_iteration_with_init) (
    DISCRETE_MODEL_TYPE* model, float df, float *value_array, ACTION_TYPE* dpolicy
) {
    /* Init_Value_Func : Value_Function_Type;
     * Init_Policy : Deterministic_Policy_Type
      type Action_Value_Array_Type is array (Action_Type) of Float;

      Value_Function : Value_Function_Type := Init_Value_Func;
      Policy : Deterministic_Policy_Type := Init_Policy;
     */
    unsigned int num_states = model->num_states;

    Boolean stable;

    unsigned int s, s1;
    ACTION_TYPE a;

    ACTION_TYPE prev_action;
    float action_values[ENVIRONMENT_ACTION_COUNT];

    struct TransitionProbabilityType trprob;

    int iteration_count = 0;

    while (1) {
        iteration_count += DP_ITER_DET_POL_EVAL_METHOD (model, dpolicy, df, value_array);
   
        stable = TRUE;

        for (s = 0; s < num_states; s++) {
            /* For state S, record the value of each action in Action_Values. */
            for (a = 0; a < ENVIRONMENT_ACTION_COUNT; a++) {
                action_values[a] = 0.0;
                for (s1 = 0; s1 < num_states; s1++) {
                    trprob = GET_TRANSITION_METHOD(model, s, a, s1);
                    action_values[a] += trprob.probability * (trprob.reward + df * value_array[s1]);
                }
            }
            /* Get the arg max */
            prev_action = dpolicy[s];
            dpolicy[s] = (ACTION_TYPE) arg_max(action_values, ENVIRONMENT_ACTION_COUNT);
            if (dpolicy[s] != prev_action) {
               stable = FALSE;
            }
        }
        if (stable) {
            break;
        }
    }
    return iteration_count;
}

int DP_FUNC_NAME(policy_iteration) (
    DISCRETE_MODEL_TYPE* model, float df, int* num_iterations
) {
    /* Reset seed */
    srand(time(NULL));
    
    unsigned int num_states = model->num_states;

    unsigned int s;

    float* value_array = malloc(num_states * sizeof(float));
    if (value_array == NULL) {
        fprintf(stderr, "policy_iteration: could not allocate state value array");
        return 1;
    }
    /* Initialize value function to 0 */
    memset(value_array, 0, sizeof(float) * num_states);

    ACTION_TYPE* dpolicy = malloc(num_states * sizeof(enum FrozenlakeAction));
    if (dpolicy == NULL) {
        fprintf(stderr, "policy_iteration: could not allocate deterministic policy array");
        free(value_array);
        return 2;
    }

    /* Initialize policy using uniform distribution over actions */
    for (s = 0; s < num_states; s++) {
        dpolicy[s] = frozenlake_get_random_action();
    }

    PRINT_POLICY_METHOD (dpolicy, num_states);  /* TODO: Remove after debugging */
    *num_iterations = DP_FUNC_NAME(policy_iteration_with_init) (model, df, value_array, dpolicy);
    return 0;
}
 
#endif

#undef ENVIRONMENT_PREFIX
#undef DISCRETE_MODEL_TYPE
#undef ACTION_TYPE
#undef ENVIRONMENT_ACTION_COUNT
#undef GET_TRANSITION_METHOD
#undef PRINT_POLICY_METHOD
#undef DP_ITER_DET_POL_EVAL_METHOD
#ifdef DP_DECLS_ONLY
#undef DP_DECLS_ONLY
#endif
