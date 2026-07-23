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

/* TODO: Need to repurpose this for random number generation below */
#ifndef GET_RANDOM_ACTION_ES_METHOD
#error "GET_RANDOM_ACTION_ES_METHOD is not defined in tc.inc"
#endif

#ifndef TO_DISCRETE_OBSERVATION_METHOD
#error "TO_DISCRETE_OBSERVATION is not defined in tc.inc"
#endif

#include <reinforcementlearning/algorithms/et_config.h>
/*
#include <reinforcementlearning/algorithms/epsilon_policies.h>
#include <string.h>
#include <time.h>
#include <math.h> */ /* abs */

#define ET_FUNC_NAME(name) ET_CONCAT1(ENVIRONMENT_PREFIX, name)

#define ET_CONCAT1(prefix, name) ET_CONCAT2(prefix, name)
#define ET_CONCAT2(prefix, name) prefix##_##name

#ifdef ET_DECLS_ONLY

#else

/* TODO: Move to et.inc */
int ET_FUNC_NAME(et_iterative_policy_evaluation) (CONFIG_TYPE config, struct ETConfig et_config, float* dpolicy, float* svalue_func_out) {
    unsigned int num_states = GET_NUM_STATES_METHOD(config);

    /* TODO: return error if make fails */
    ENVIRONMENT_TYPE* env = MAKE_METHOD(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    float* e = malloc(num_states * sizeof(float));
    memset(e, 0, num_states * sizeof(float));

    float td_error;

    OBSERVATION_TYPE obs;
    unsigned int s, s1, s2;
    STEP_RETURN_TYPE step_result;
    ACTION_TYPE a;
    Boolean terminated = FALSE;
    /* The local delta indices
    unsigned int s0;
    ACTION_TYPE a0;
    */

    /* Reset value function to 0 */
    memset(svalue_func_out, 0, num_states * sizeof(*svalue_func_out));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float* prev_svalue_func = malloc(num_states * sizeof(float));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    unsigned int max_steps = 50;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) {
        episode_count++;
        /* TODO: Consider disabling printing */
        printf("Episode %u", episode_count);

        memcpy(prev_svalue_func, svalue_func_out, num_states * sizeof(float));

        /* TODO: From the text of the book and the book errata, E needs to be set to 0
                 at the beginning of an episode. */

        obs = RESET_METHOD(env); /* Seed_Reset */
        s = TO_DISCRETE_OBSERVATION_METHOD(obs);
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            a = dpolicy[s];
            step_result = STEP_METHOD(env, a);
            obs = step_result.observation;
            s1 = TO_DISCRETE_OBSERVATION_METHOD(obs);
            printf("Action %d takes state %u to state %u", a, s, s1);
            td_error = step_result.reward + et_config.gamma * svalue_func_out[s1] - svalue_func_out[s];
            e[s] += 1.0;
            for (s2 = 0; s2 < num_states; s2++) {
                svalue_func_out[s2] += et_config.alpha * td_error * e[s2];
                e[s2] = et_config.gamma * et_config.lambda * e[s2];
            }
            /* Update to next state */
            s = s1;
            step_index++;
            terminated = (step_result.terminated || (step_index >= max_steps));
       }
        
        local_delta = 0.0;
        /* NOTE: We exit when the max value function change falls below a threshold.
                 This differs from the textbook algorithm. */
        for (s = 0; s < num_states; s++) {
            local_delta = fmaxf(local_delta, fabsf(svalue_func_out[s] - prev_svalue_func[s]));
        }
        if (local_delta < theta) {
            break;
        }
    }
    CLOSE_METHOD(env);
    return 0;
}

/* NOTE: The following method is the same as those defined in td.inc.
 *       We allow for code duplication for now rather than creating
 *       another template header file for common methods. */
static ACTION_TYPE ET_FUNC_NAME(et_best_action_for_state) (float (*q)[ENVIRONMENT_ACTION_COUNT], unsigned int s) {
    return (ACTION_TYPE) arg_max(q[s], ENVIRONMENT_ACTION_COUNT);
}

/* NOTE: The following method is the same as those defined in td.inc.
 *       We allow for code duplication for now rather than creating
 *       another template header file for common methods. */
static ACTION_TYPE ET_FUNC_NAME(et_choose_action_epsilon_greedy) (float epsilon, float (*q)[ENVIRONMENT_ACTION_COUNT], unsigned int s) {
    float u = rand_float();
    if (u < epsilon) {
        float a_rand;
        /* TODO: I believe we have methods for getting a random action assuming a uniform distribution */
        a_rand = rand_float() * ((float) ENVIRONMENT_ACTION_COUNT);
        return (ACTION_TYPE) ((int) floorf(a_rand));
    } else {
        return ET_FUNC_NAME(et_best_action_for_state) (q, s);
    }
}

/* TODO: Move to et.inc */
int ET_FUNC_NAME(et_sarsa_on_policy) (CONFIG_TYPE config, struct ETSARSAConfig sarsa_config, float (*q)[ENVIRONMENT_ACTION_COUNT]) {
    unsigned int num_states = GET_NUM_STATES_METHOD(config);

    /* TODO: return error if make fails */
    ENVIRONMENT_TYPE* env = MAKE_METHOD(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    /* TODO: return error if malloc fails */
    float (*prev_esavalue_func)[ENVIRONMENT_ACTION_COUNT] = malloc(num_states * sizeof(*q));
    memset(prev_esavalue_func, 0, num_states * sizeof(*prev_esavalue_func));

    float (*esa)[ENVIRONMENT_ACTION_COUNT] = malloc(num_states * sizeof(*q));
    memset(esa, 0, num_states * sizeof(*esa));
    float td_error;

    OBSERVATION_TYPE obs;
    unsigned int s, s1, s2;
    STEP_RETURN_TYPE step_result;
    ACTION_TYPE a, a1, a2;
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
    float (*prev_q)[ENVIRONMENT_ACTION_COUNT] = malloc(num_states * sizeof(*q));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    /* TODO: Perhaps max_steps should be a parameter */
    unsigned int max_steps = 100;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) { 
        episode_count++;
        /* Put_Line("Episode " & Episode_Count'Image); */

        epsilon = update_epsilon(sarsa_config.td_config, (int) episode_count);
        printf("Episode %u, epsilon: %f", episode_count, epsilon);

        memcpy(prev_q, q, num_states * sizeof(*q));

        /* TODO: From the text of the book and the book errata, E needs to be set to 0
                 at the beginning of an episode. */
        obs = RESET_METHOD(env); /* Seed_Reset */
        s = TO_DISCRETE_OBSERVATION_METHOD(obs);
        a = ET_FUNC_NAME(et_choose_action_epsilon_greedy) (epsilon, q, s);
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            step_result = STEP_METHOD(env, a);
            obs = step_result.observation;
            s1 = TO_DISCRETE_OBSERVATION_METHOD(obs);
            a1 = ET_FUNC_NAME(et_choose_action_epsilon_greedy) (epsilon, q, s1);
            printf("Action %d takes state %u to state %u and action %d in on-policy SARSA", a, s, s1, a1);
            td_error = step_result.reward + sarsa_config.td_config.gamma * q[s1][a1] - q[s][a];
            esa[s][a] += 1.0;
            for(s2 = 0; s2 < num_states; s2++) {
                for(a2 = 0; a2 < ENVIRONMENT_ACTION_COUNT; a2++) {
                    q[s2][a2] += sarsa_config.td_config.alpha * td_error * esa[s2][a2];
                    esa[s2][a2] = sarsa_config.td_config.gamma * sarsa_config.lambda * esa[s2][a2];
                }
            }
            /* Update to next state */
            s = s1;
            a = a1;
            step_index++;
            terminated = (step_result.terminated || (step_index >= max_steps));
        }

        /* NOTE: We exit when the max value function change falls below a threshold
         *       after episode Episodes_To_Minimum_Epsilon is reached.
         *       This differs from the textbook algorithm. */
        local_delta = 0.0;
        /* TODO: Decide whether to reintroduce something like the following condition before exiting */
        for (s0 = 0; s0 < num_states; s0++) {
            for (a0 = 0; a0 < ENVIRONMENT_ACTION_COUNT; a0++) {
                local_delta = fmaxf(local_delta, fabsf(q[s0][a0] - prev_q[s0][a0]));
            }
        }
        if ((episode_count >= sarsa_config.td_config.episodes_to_minimum_epsilon) && (local_delta < theta)) {
            break;
        }
    }
    CLOSE_METHOD(env);
    return 0;
}

/* TODO: Move to et.inc */
int ET_FUNC_NAME(watkins_q_iteration) (CONFIG_TYPE config, struct ETSARSAConfig sarsa_config, float (*q)[ENVIRONMENT_ACTION_COUNT]) {
    unsigned int num_states = GET_NUM_STATES_METHOD(config);

    /* TODO: return error if make fails */
    ENVIRONMENT_TYPE* env = MAKE_METHOD(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    /* TODO: return error if malloc fails */
    float (*prev_savalue_func)[ENVIRONMENT_ACTION_COUNT] = malloc(num_states * sizeof(*q));
    memset(prev_savalue_func, 0, num_states * sizeof(*prev_savalue_func));

    float (*esa)[ENVIRONMENT_ACTION_COUNT] = malloc(num_states * sizeof(*q));
    memset(esa, 0, num_states * sizeof(*esa));
    float td_error;

    OBSERVATION_TYPE obs;
    unsigned int s, s1, s2;
    STEP_RETURN_TYPE step_result;
    ACTION_TYPE a, a1, a2, a_best;
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
    float (*prev_q)[ENVIRONMENT_ACTION_COUNT] = malloc(num_states * sizeof(*q));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;
    /* TODO: The following should be a config parameter */
    unsigned int max_steps = 100;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) {
        episode_count++;
        /* Put_Line("Episode " & Episode_Count'Image); */

        epsilon = update_epsilon(sarsa_config.td_config, (int) episode_count);
        printf("Episode %u, epsilon: %f", episode_count, epsilon);

        memcpy(prev_q, q, num_states * sizeof(*q));

        /* TODO: From the text of the book and the book errata, E needs to be set to 0
                 at the beginning of an episode. */
        obs = RESET_METHOD(env); /* Seed_Reset */
        s = TO_DISCRETE_OBSERVATION_METHOD(obs);
        a = ET_FUNC_NAME(et_choose_action_epsilon_greedy) (epsilon, q, s);
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            step_result = STEP_METHOD(env, a);
            obs = step_result.observation;
            s1 = TO_DISCRETE_OBSERVATION_METHOD(obs);
            a1 = ET_FUNC_NAME(et_choose_action_epsilon_greedy) (epsilon, q, s1);
            a_best = ET_FUNC_NAME(et_best_action_for_state) (q, s1);  /* TODO: Just get best value for state */
            /* TODO: Remove print statements */
            printf("Action %d takes state %u to state %u in Watkin's Q algorithm (off-policy SARSA usingn Eligibility Trace)", a, s, s1);

            td_error = step_result.reward + sarsa_config.td_config.gamma * q[s1][a_best] - q[s][a];
            esa[s][a] += 1.0;
            for(s2 = 0; s2 < num_states; s2++) {
                for(a2 = 0; a2 < ENVIRONMENT_ACTION_COUNT; a2++) {
                    q[s2][a2] += sarsa_config.td_config.alpha * td_error * esa[s2][a2];
                    /* IN PROGRESS: Is the following correct?  Based on the text, I think so. */
                    esa[s2][a2] = sarsa_config.td_config.gamma * sarsa_config.lambda * esa[s2][a2];
                    if (a1 == a_best) {
                        esa[s2][a2] = sarsa_config.td_config.gamma * sarsa_config.lambda * esa[s2][a2];
                    } else {
                        esa[s2][a2] = 0.0;
                    }
                }
            }
            /* Update to next state */
            s = s1;
            a = a1;
            step_index++;
            terminated = (step_result.terminated || (step_index >= max_steps));
        }

        /* NOTE: We exit when the max value function change falls below a threshold
         *       after episode Episodes_To_Minimum_Epsilon is reached.
         *       This differs from the textbook algorithm. */
        local_delta = 0.0;
        /* TODO: Decide whether to reintroduce something like the following condition before exiting */
        for (s0 = 0; s0 < num_states; s0++) {
            for (a0 = 0; a0 < ENVIRONMENT_ACTION_COUNT; a0++) {
                local_delta = fmaxf(local_delta, fabsf(q[s0][a0] - prev_q[s0][a0]));
            }
        }
        if ((episode_count >= sarsa_config.td_config.episodes_to_minimum_epsilon) && (local_delta < theta)) {
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
#ifdef ET_DECLS_ONLY
#undef ET_DECLS_ONLY
#endif

