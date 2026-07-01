#include <stdlib.h>


/* Just testing out using an X-macro approach */

void take_random_action2(const CONFIG_TYPE * config, unsigned int max_steps) {
    STATE_TYPE state = initial_state(*config);
    size_t i;
    ACTION_TYPE action;
    Boolean terminated = FALSE;

    srand(125);

    printf("\n");
    printf("Taking random actions...\n");
    for(i = 0; i < max_steps; i++) {
        action = (enum Action) rand() % 2;
        state = STEP_METHOD(state, action);
        if ((terminated = is_terminal(state))) {
            break;
        }
    }

    if (terminated) {
        printf("Terminated\n");
    } else {
        printf("Environment stopped after %u steps\n", max_steps);
    }
    printf("Reward: %f", reward(PLAYER1, state));
}
