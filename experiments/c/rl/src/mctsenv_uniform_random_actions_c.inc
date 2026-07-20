#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef CONFIG_TYPE
#error "CONFIG_TYPE is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef STATE_TYPE
#error "STATE_TYPE is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef ACTION_TYPE
#error "ACTION_TYPE is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef STEP_METHOD
#error "STEP_METHOD is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef RANDOM_ACTION_METHOD
#error "RANDOM_ACTION_METHOD is not defined in mctsenv_uniform_random_actions_c.inc"
#endif

#ifndef IS_TERMINAL_METHOD
#error "IS_TERMINAL_METHODis not defined in mctsenv_uniform_random_actions_c.inc"
#endif


#include <stdlib.h>

#define FUNC_NAME_CONCAT(PREFIX) PREFIX##_mctsenv_uniform_random_actions

#define FUNC_NAME(PREFIX) FUNC_NAME_CONCAT(PREFIX)


/* Using an X-macro approach */
void FUNC_NAME(ENVIRONMENT_PREFIX) (const CONFIG_TYPE * config, unsigned int max_steps) {
    STATE_TYPE state = initial_state(*config);
    size_t i;
    ACTION_TYPE action;
    Boolean terminated = FALSE;

    srand(125);

    printf("\n");
    printf("Using MCTS interface, taking random actions...\n");
    for(i = 0; i < max_steps; i++) {
        action = RANDOM_ACTION_METHOD(state);
        state = STEP_METHOD(state, action);
        if ((terminated = IS_TERMINAL_METHOD(state))) {
            break;
        }
    }

    if (terminated) {
        printf("Terminated after %lu steps\n", i + 1);
    } else {
        printf("Environment stopped after %u steps\n", max_steps);
    }
    printf("Reward: %f", reward(PLAYER1, state));
}

#undef FUNC_NAME
#undef FUNC_NAME_CONCAT
