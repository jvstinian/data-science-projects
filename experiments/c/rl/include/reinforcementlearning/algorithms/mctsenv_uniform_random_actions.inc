#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef CONFIG_TYPE
#error "CONFIG_TYPE is not defined in mctsenv_uniform_random_actions_c.inc"
#endif



#define MURA_FUNC_NAME_CONCAT(PREFIX) PREFIX##_mctsenv_uniform_random_actions

#define MURA_FUNC_NAME(PREFIX) MURA_FUNC_NAME_CONCAT(PREFIX)

#ifdef MURA_DECLS_ONLY


#include <reinforcementlearning/algorithms/result_types.h>
struct SimulationSummary MURA_FUNC_NAME(ENVIRONMENT_PREFIX) (CONFIG_TYPE config, unsigned int max_steps);


#else


/* The following macros are only needed for the implementation */
#ifndef STATE_TYPE
#error "STATE_TYPE is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef ACTION_TYPE
#error "ACTION_TYPE is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef PLAYER_TYPE
#error "PLAYER_TYPE is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef INITIAL_STATE_METHOD
#error "INITIAL_STATE_METHOD is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef STEP_METHOD
#error "STEP_METHOD is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef GET_PLAYER_METHOD
#error "GET_PLAYER_METHOD is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef RANDOM_ACTION_METHOD
#error "RANDOM_ACTION_METHOD is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef IS_TERMINAL_METHOD
#error "IS_TERMINAL_METHODis not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef REWARD_METHOD
#error "REWARD_METHOD not defined in mctsenv_uniform_random_actions_c.inc"
#endif

/* Template header approach */
struct SimulationSummary MURA_FUNC_NAME(ENVIRONMENT_PREFIX) (CONFIG_TYPE config, unsigned int max_steps) {
    STATE_TYPE state = INITIAL_STATE_METHOD(config);
    size_t i;
    ACTION_TYPE action;
    PLAYER_TYPE init_player;
    Boolean terminated = FALSE;

    srand(125);

    /* TODO: Remove print statements
    printf("\n");
    printf("Using MCTS interface, taking random actions...\n");
    */
    init_player = GET_PLAYER_METHOD(state);

    for(i = 0; i < max_steps; i++) {
        action = RANDOM_ACTION_METHOD(state);
        state = STEP_METHOD(state, action);
        if ((terminated = IS_TERMINAL_METHOD(state))) {
            break;
        }
    }

    /* TODO: Remove print statements
    if (terminated) {
        printf("Terminated after %lu steps\n", i + 1);
    } else {
        printf("Environment stopped after %u steps\n", max_steps);
    }
    printf("Reward: %f", reward(PLAYER1, state));
    */
    return (struct SimulationSummary) { i, REWARD_METHOD(init_player, state) };
}

#undef REWARD_METHOD
#undef IS_TERMINAL_METHOD
#undef RANDOM_ACTION_METHOD
#undef GET_PLAYER_METHOD
#undef STEP_METHOD
#undef INITIAL_STATE_METHOD
#undef ACTION_TYPE
#undef PLAYER_TYPE
#undef STATE_TYPE

#endif  /* MURA_DECLS_ONLY */

#undef MURA_FUNC_NAME
#undef MURA_FUNC_NAME_CONCAT
#undef CONFIG_TYPE
#undef ENVIRONMENT_PREFIX
#ifdef MURA_DECLS_ONLY
#undef MURA_DECLS_ONLY
#endif
