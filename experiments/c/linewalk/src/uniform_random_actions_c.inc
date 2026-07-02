#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in uniform_random_actions_c.inc"
#endif

#ifndef RANDOM_ACTION_METHOD
#error "RANDOM_ACTION_METHOD is not defined in uniform_random_actions_c.inc"
#endif

#define FUNC_NAME_CONCAT(PREFIX) PREFIX##_uniform_random_actions

#define FUNC_NAME(PREFIX) FUNC_NAME_CONCAT(PREFIX)


/* X-macro approach */
struct SimulationSummary FUNC_NAME(ENVIRONMENT_PREFIX) (CONFIG_TYPE config, Boolean verbose) {
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
            printf("Step %lu, Action %d, Reward: %5.2f\n", i, action, step_output.reward);
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

