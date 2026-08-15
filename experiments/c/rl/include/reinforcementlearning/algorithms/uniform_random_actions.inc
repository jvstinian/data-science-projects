#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in uniform_random_actions_c.inc"
#endif

#ifndef CONFIG_TYPE
#error "CONFIG_TYPE is not defined in uniform_random_actions_c.inc"
#endif

#ifndef ENVIRONMENT_TYPE
#error "ENVIRONMENT_TYPE is not defined in uniform_random_actions_c.inc"
#endif

#ifndef ACTION_TYPE
#error "ACTION_TYPE is not defined in uniform_random_actions_c.inc"
#endif

#ifndef OBSERVATION_TYPE
#error "OBSERVATION_TYPE is not defined in uniform_random_actions_c.inc"
#endif

#ifndef STEPRETURN_TYPE
#error "STEPRETURN_TYPE is not defined in uniform_random_actions_c.inc"
#endif

#ifndef MAKE_METHOD
#error "MAKE_METHOD is not defined in uniform_random_actions_c.inc"
#endif

#ifndef RESET_METHOD
#error "RESET_METHOD is not defined in uniform_random_actions_c.inc"
#endif

#ifndef STEP_METHOD
#error "STEP_METHOD is not defined in uniform_random_actions_c.inc"
#endif

#ifndef CLOSE_METHOD
#error "CLOSE_METHOD is not defined in uniform_random_actions_c.inc"
#endif

#ifndef RANDOM_ACTION_METHOD
#error "RANDOM_ACTION_METHOD is not defined in uniform_random_actions_c.inc"
#endif


#define URA_FUNC_NAME_CONCAT(PREFIX) PREFIX##_uniform_random_actions

#define URA_FUNC_NAME(PREFIX) URA_FUNC_NAME_CONCAT(PREFIX)

#ifdef URA_DECLS_ONLY

struct SimulationSummary URA_FUNC_NAME(ENVIRONMENT_PREFIX) (CONFIG_TYPE config, Boolean verbose);

#else

/* Template header approach */
struct SimulationSummary URA_FUNC_NAME(ENVIRONMENT_PREFIX) (CONFIG_TYPE config, Boolean verbose) {
    ENVIRONMENT_TYPE* env = MAKE_METHOD(config);
    if (env == NULL) {
        fprintf(stderr, "uniform_random_actions: failed to create environment\n");
        return (struct SimulationSummary) { 0, 0.0f };
    }

    /* Other declarations */
    OBSERVATION_TYPE obs;
    ACTION_TYPE action;
    STEPRETURN_TYPE step_output;
    size_t i;
    float total_reward = 0.0f;

    /* Set seed and initialize environment */
    srand(123);
    obs = RESET_METHOD(env);
    (void)obs; /* Suppress unused variable warning */

    i = 0;
    while (1) {
        action = RANDOM_ACTION_METHOD(env);
        step_output = STEP_METHOD(env, action);
        obs = step_output.observation;
        /* (void)obs; Suppress unused variable warning */
        total_reward += step_output.reward;
        i++;
        if (verbose) {
            /* TODO: Remove when ready
            printf("Step %lu, Action %d, Reward: %5.2f\n", i, action, step_output.reward);
            */
            printf("Step %lu, Reward: %5.2f\n", i, step_output.reward);
        }
        if (step_output.terminated) {
            if (verbose) {
                printf("Total Reward: %5.2f\n", total_reward);
            }
            break;
        }
    }
    CLOSE_METHOD(env);
    return (struct SimulationSummary) { i, total_reward };
}

#endif

#undef ENVIRONMENT_PREFIX
#undef CONFIG_TYPE
#undef ENVIRONMENT_TYPE
#undef ACTION_TYPE
#undef OBSERVATION_TYPE
#undef STEPRETURN_TYPE
#undef MAKE_METHOD
#undef RESET_METHOD
#undef STEP_METHOD
#undef CLOSE_METHOD
#undef RANDOM_ACTION_METHOD
#ifdef URA_DECLS_ONLY
#undef URA_DECLS_ONLY
#endif
