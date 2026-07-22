#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in td.inc"
#endif

#ifndef CONFIG_TYPE
#error "CONFIG_TYPE is not defined in tc.inc"
#endif

#ifndef ENVIRONMENT_TYPE
#error "ENVIRONMENT_TYPE is not defined in tc.inc"
#endif

#ifndef ACTION_TYPE
#error "ACTION_TYPE is not defined in tc.inc"
#endif

#ifndef OBSERVATION_TYPE
#error "OBSERVATION_TYPE is not defined in tc.inc"
#endif

#ifndef STEP_RETURN_TYPE
#error "STEP_RETURN_TYPE is not defined in tc.inc"
#endif

#ifndef ENVIRONMENT_ACTION_COUNT
#error "ENVIRONMENT_ACTION_COUNT is not defined in tc.inc"
#endif

#ifndef GET_NUM_STATES_METHOD
#error "GET_NUM_STATES_METHOD is not defined in tc.inc"
#endif

#ifndef MAKE_METHOD
#error "MAKE_METHOD is not defined in tc.inc"
#endif

#ifndef RESET_METHOD
#error "RESET_METHOD is not defined in tc.inc"
#endif

#ifndef STEP_METHOD
#error "STEP_METHOD is not defined in tc.inc"
#endif

#ifndef CLOSE_METHOD
#error "CLOSE_METHOD is not defined in tc.inc"
#endif

#ifndef GET_RANDOM_ACTION_ES_METHOD
#error "GET_RANDOM_ACTION_ES_METHOD is not defined in tc.inc"
#endif

#ifndef TO_DISCRETE_OBSERVATION_METHOD
#error "TO_DISCRETE_OBSERVATION is not defined in tc.inc"
#endif

/*
#include <reinforcementlearning/algorithms/td_config.h>
#include <assert.h>
*/

#define TD_FUNC_NAME(name) TD_CONCAT1(ENVIRONMENT_PREFIX, name)

#define TD_CONCAT1(prefix, name) TD_CONCAT2(prefix, name)
#define TD_CONCAT2(prefix, name) prefix##_##name

#ifdef TD_DECLS_ONLY

    /*
int TD_FUNC_NAME(mc_policy_evaluation) (CONFIG_TYPE config, ACTION_TYPE* dpolicy, struct MCConfig mc_config, float* svalue_func_out);
int TD_FUNC_NAME(mc_exploring_starts_evaluation) (CONFIG_TYPE config, struct MCConfig mc_config);
int TD_FUNC_NAME(mc_epsilon_soft_evaluation) (CONFIG_TYPE config, struct MCEpsilonSoftConfig mces_config);
    */

#else

int TD_FUNC_NAME(td_iterative_policy_evaluation) (CONFIG_TYPE config, struct TDConfig td_config, ACTION_TYPE* policy, float* value_function_out) {
    unsigned int num_states = GET_NUM_STATES_METHOD(config);

    /* TODO: return error if make fails */
    struct FrozenlakeEnvironment* env = frozenlake_make(config);
    if (env == NULL) {
        fprintf(stderr, "td_iterative_policy_evaluation: could not create environment");
        return 1;
    }
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    OBSERVATION_TYPE obs;
    unsigned int s, s1;
    STEP_RETURN_TYPE step_result;
    ACTION_TYPE action;
    Boolean terminated = FALSE;

    /* Reset value function to 0 */
    memset(value_function_out, 0, num_states * sizeof(float));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float* prev_value_function = malloc(num_states * sizeof(float));
    if (prev_value_function == NULL) {
        fprintf(stderr, "td_iterative_policy_evaluation: could not allocate previous state value array");
        free(env);
        return 1;
    }

    int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    unsigned int max_steps = 50;

    while (1) {
        episode_count++;
        /* TODO: Disable print statements */
        printf("Episode %u", episode_count);

        memcpy(prev_value_function, value_function_out, num_states * sizeof(float));
        local_delta = 0.0;

        obs = RESET_METHOD(env); /* Seed_Reset */
        s = obs.position_index;  /* TODO: To move to a C template we'll need a to_discrete_state method */
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            action = policy[s];
            step_result = STEP_RETURN_TYPE(env, action);
            obs = step_result.observation;
            s1 = obs.position_index;
            /* TODO: Disable print statements */
            printf("Action %d takes state %u to state %u", (int) action, s, s1);
            value_function_out[s] += td_config.alpha * (step_result.reward + td_config.gamma * value_function_out[s1] - value_function_out[s]);
            /* Update to next state */
            s = s1;
            step_index++;
            terminated = step_result.terminated || (step_index >= max_steps);
        }

        /* NOTE: We exit when the max value function change falls below a threshold.
                 This differs from the textbook algorithm. */
        for(s = 0; s < num_states; s++) {
            local_delta = fmaxf(local_delta, fabsf(value_function_out[s] - prev_value_function[s]));
        }
        if (local_delta < theta) {
            break;
        }
    }
    return episode_count;
}

#endif

#undef ENVIRONMENT_PREFIX
#undef CONFIG_TYPE
#undef ENVIRONMENT_TYPE
#undef ACTION_TYPE
#undef OBSERVATION_TYPE
#undef STEP_RETURN_TYPE
#undef ENVIRONMENT_ACTION_COUNT
#undef GET_NUM_STATES_METHOD
#undef MAKE_METHOD
#undef RESET_METHOD
#undef STEP_METHOD
#undef CLOSE_METHOD
#undef GET_RANDOM_ACTION_ES_METHOD
#undef TO_DISCRETE_OBSERVATION_METHOD
#ifdef TD_DECLS_ONLY
#undef TD_DECLS_ONLY
#endif

