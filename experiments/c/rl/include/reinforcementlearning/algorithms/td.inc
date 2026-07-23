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

#include <reinforcementlearning/algorithms/td_config.h>
#include <reinforcementlearning/algorithms/epsilon_policies.h>
#include <string.h>
#include <time.h>
#include <math.h> /* abs */
/*
#include <assert.h>
*/
/* TODO: Consider creating a math header */
/*
#if defined(__STDC__) && !defined(__STDC_VERSION__)
    float fmaxf(float x, float y) {
        return (x > y) ? x : y;
    }
    float fabsf(float x) {
        return (float) fabs((double) x);
    }
    float floorf(float arg) {
        return (float) floor((double) arg);
    }
#endif
*/

#define TD_FUNC_NAME(name) TD_CONCAT1(ENVIRONMENT_PREFIX, name)

#define TD_CONCAT1(prefix, name) TD_CONCAT2(prefix, name)
#define TD_CONCAT2(prefix, name) prefix##_##name

#ifdef TD_DECLS_ONLY

    /*
#if defined(__STDC__) && !defined(__STDC_VERSION__)
    float fmaxf(float x, float y);
    float fabsf(float x);
    float floorf(float arg);
#endif
    */

int TD_FUNC_NAME(td_iterative_policy_evaluation) (CONFIG_TYPE config, struct TDConfig td_config, ACTION_TYPE* policy, float* value_function_out);
int TD_FUNC_NAME(sarsa_on_policy) (CONFIG_TYPE config, struct SARSAConfig sarsa_config, float (*q)[ENVIRONMENT_ACTION_COUNT]);
int TD_FUNC_NAME(sarsa_off_policy) (CONFIG_TYPE config, struct SARSAConfig sarsa_config, float (*q)[ENVIRONMENT_ACTION_COUNT]);

#else

int TD_FUNC_NAME(td_iterative_policy_evaluation) (CONFIG_TYPE config, struct TDConfig td_config, ACTION_TYPE* policy, float* value_function_out) {
    unsigned int num_states = GET_NUM_STATES_METHOD(config);

    /* TODO: return error if make fails */
    ENVIRONMENT_TYPE* env = MAKE_METHOD(config);
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
        s = TO_DISCRETE_OBSERVATION_METHOD(obs);  /* TODO: To move to a C template we'll need a to_discrete_state method */
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            action = policy[s];
            step_result = STEP_METHOD(env, action);
            obs = step_result.observation;
            s1 = TO_DISCRETE_OBSERVATION_METHOD(obs);
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
    CLOSE_METHOD(env);
    return episode_count;
}

static ACTION_TYPE TD_FUNC_NAME(best_action_for_state) (float (*q)[ENVIRONMENT_ACTION_COUNT], unsigned int s) {
    return (ACTION_TYPE) arg_max(q[s], ENVIRONMENT_ACTION_COUNT);
}

static ACTION_TYPE TD_FUNC_NAME(choose_action_epsilon_greedy) (float epsilon, float (*q)[ENVIRONMENT_ACTION_COUNT], unsigned int s) {
    float u = rand_float();
    if (u < epsilon) {
        float a_rand;
        /* TODO: I believe we have methods for getting a random action assuming a uniform distribution */
        a_rand = rand_float() * ((float) ENVIRONMENT_ACTION_COUNT);
        return (ACTION_TYPE) ((int) floorf(a_rand));
    } else {
        return TD_FUNC_NAME(best_action_for_state) (q, s);
    }
}

int TD_FUNC_NAME(sarsa_on_policy) (CONFIG_TYPE config, struct SARSAConfig sarsa_config, float (*q)[ENVIRONMENT_ACTION_COUNT]) {
    unsigned int num_states = GET_NUM_STATES_METHOD(config);

    /* TODO: return error if make fails */
    ENVIRONMENT_TYPE* env = MAKE_METHOD(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    OBSERVATION_TYPE obs;
    unsigned int s, s1;
    STEP_RETURN_TYPE step_result;
    ACTION_TYPE a, a1;
    Boolean terminated = FALSE;
    /* The local delta indices */
    unsigned int s0;
    ACTION_TYPE a0;

    float epsilon;

    /* Reset value function to 0 */
    memset(q, 0, num_states * sizeof(*q));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float (*prev_action_value_func)[ENVIRONMENT_ACTION_COUNT] = malloc(num_states * sizeof(*q));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    unsigned int max_steps = 100;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) { 
        episode_count++;
        /* Put_Line("Episode " & Episode_Count'Image); */

        epsilon = update_epsilon(sarsa_config, (int) episode_count);
        printf("Episode %u, epsilon: %f", episode_count, epsilon);

        memcpy(prev_action_value_func, q, num_states * sizeof(*q));

        obs = RESET_METHOD(env); /* Seed_Reset */
        s = obs.position_index;
        a = TD_FUNC_NAME(choose_action_epsilon_greedy) (epsilon, q, s);
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            step_result = STEP_METHOD(env, a);
            obs = step_result.observation;
            s1 = obs.position_index;
            a1 = TD_FUNC_NAME(choose_action_epsilon_greedy) (epsilon, q, s1);
            printf("Action %d takes state %u to state %u and action %d in on-policy SARSA", a, s, s1, a1);
            q[s][a] += sarsa_config.alpha * (step_result.reward + sarsa_config.gamma * q[s1][a1] - q[s][a]);
            /* Update to next state */
            s = s1;
            a = a1;
            step_index++;
            terminated = step_result.terminated || (step_index >= max_steps);
        }

        /* NOTE: We exit when the max value function change falls below a threshold
         *       after episode Episodes_To_Minimum_Epsilon is reached.
         *       This differs from the textbook algorithm. */
        local_delta = 0.0;
        /* TODO: Decide whether to reintroduce something like the following condition before exiting */
        for (s0 = 0; s0 < num_states; s0++) {
            for (a0 = 0; a0 < ENVIRONMENT_ACTION_COUNT; a0++) {
                local_delta = fmaxf(local_delta, fabsf(q[s0][a0] - prev_action_value_func[s0][a0]));
            }
        }
        if ((episode_count >= sarsa_config.episodes_to_minimum_epsilon) && (local_delta < theta)) {
            break;
        }
    }
    CLOSE_METHOD(env);
    return 0;
}

int TD_FUNC_NAME(sarsa_off_policy) (CONFIG_TYPE config, struct SARSAConfig sarsa_config, float (*q)[ENVIRONMENT_ACTION_COUNT]) {
    unsigned int num_states = GET_NUM_STATES_METHOD(config);

    /* TODO: return error if make fails */
    ENVIRONMENT_TYPE* env = MAKE_METHOD(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    OBSERVATION_TYPE obs;
    unsigned int s, s1;
    STEP_RETURN_TYPE step_result;
    ACTION_TYPE a, a1;
    Boolean terminated = FALSE;
    /* The local delta indices */
    unsigned int s0;
    ACTION_TYPE a0;

    float epsilon;

    /* Reset value function to 0 */
    memset(q, 0, num_states * sizeof(*q));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float (*prev_action_value_func)[ENVIRONMENT_ACTION_COUNT] = malloc(num_states * sizeof(*q));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    unsigned int max_steps = 100;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) { 
        episode_count++;
        /* Put_Line("Episode " & Episode_Count'Image); */

        epsilon = update_epsilon(sarsa_config, (int) episode_count);
        printf("Episode %u, epsilon: %f", episode_count, epsilon);

        memcpy(prev_action_value_func, q, num_states * sizeof(*q));

        obs = RESET_METHOD(env); /* Seed_Reset */
        s = obs.position_index;
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            /* TODO: I think there's an error here.  A needs to be moved up (out of the loop) */
            a = TD_FUNC_NAME(choose_action_epsilon_greedy) (epsilon, q, s);
            step_result = STEP_METHOD(env, a);
            obs = step_result.observation;
            s1 = obs.position_index;
            a1 = TD_FUNC_NAME(best_action_for_state) (q, s1);  /* TODO: Just get best value for state */
            printf("Action %d takes state %u to state %u in off-policy SARSA", a, s, s1);
            q[s][a] += sarsa_config.alpha * (step_result.reward + sarsa_config.gamma * q[s1][a1] - q[s][a]);
            /* Update to next state */
            s = s1;
            step_index++;
            terminated = step_result.terminated || (step_index >= max_steps);
        }

        /* NOTE: We exit when the max value function change falls below a threshold
         *       after episode Episodes_To_Minimum_Epsilon is reached.
         *       This differs from the textbook algorithm. */
        local_delta = 0.0;
        /* TODO: Decide whether to reintroduce something like the following condition before exiting */
        for (s0 = 0; s0 < num_states; s0++) {
            for (a0 = 0; a0 < ENVIRONMENT_ACTION_COUNT; a0++) {
                local_delta = fmaxf(local_delta, fabsf(q[s0][a0] - prev_action_value_func[s0][a0]));
            }
        }
        if ((episode_count >= sarsa_config.episodes_to_minimum_epsilon) && (local_delta < theta)) {
            break;
        }
    }
    CLOSE_METHOD(env);
    return 0;
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
