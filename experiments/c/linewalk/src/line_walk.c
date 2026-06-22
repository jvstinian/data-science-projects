#include "line_walk.h"
#include <stdio.h>
#include <stdlib.h>

LineWalkState initial_state(LineWalkConfig config) {
    unsigned short int pos = (config.N + 1) / 2; /* Start at the middle position */
    LineWalkState state = { config, ACTIVE, pos, 0 }; /* Start at the leftmost position */
    return state;
}

Boolean is_terminal (LineWalkState state) {
    if (state.kind == ACTIVE) {
        return FALSE;
    } else { 
        return TRUE;
    }
}

Player get_player(LineWalkState state) {
    /* Silence C89 unused parameter warnings */
    (void)state;

    return PLAYER1;
}

LineWalkState step(LineWalkState state, Action action) {
    LineWalkState new_state = state; /* Start with the current state */
    if (state.kind == ACTIVE) {
        switch (action) {
            case MOVE_LEFT:
                new_state.position -= 1;
                break;
            case MOVE_RIGHT:
                new_state.position += 1;
                break;
        }
        if (new_state.position < 1) {
            new_state.kind = TERMINAL;
            new_state.reward = -1;
        } else if (new_state.position > state.config.N) {
            new_state.kind = TERMINAL;
            new_state.reward = 1;
        }
    } 
    return new_state;
}

float reward(Player player, LineWalkState state) {
    /* Silence C89 unused parameter warnings */
    (void)player;

    if (state.kind == TERMINAL) {
        return (float) state.reward;
    } else {
        return 0.0f;
    }
}

/* TODO: Perhaps remove the num_actions output parameter and just return the count? */
unsigned int get_available_actions (LineWalkState state, Action *available_actions, unsigned int* num_actions) {
    unsigned int count = 0;
    if (state.kind == ACTIVE) {
        available_actions[count++] = MOVE_LEFT;
        available_actions[count++] = MOVE_RIGHT;
    }
    *num_actions = count;
    return count;
}

void print_state(LineWalkState state) {
    if (state.kind == ACTIVE) {
        printf("Current position: %u\n", state.position);
    } else {
        printf("Terminal state with reward: %d\n", state.reward);
    }
}


#define CONFIG_TYPE LineWalkConfig
#define STATE_TYPE LineWalkState
#define ACTION_TYPE enum Action
#define STEP_METHOD step
#include "random_action_c.inc"
#undef STEP_METHOD
#undef ACTION_TYPE
#undef STATE_TYPE
#undef CONFIG_TYPE

void take_random_action(const LineWalkConfig* config, unsigned int max_steps) {
    LineWalkState state = initial_state(*config);
    size_t i;
    enum Action action;
    Boolean terminated = FALSE;

    srand(123);
    
    for(i = 0; i < max_steps; i++) {
        action = (enum Action) rand() % 2;
        if (action == MOVE_LEFT) {
            printf("Moving left\n");
        } else if (action == MOVE_RIGHT) {
            printf("Moving right\n");
        }
        state = step(state, action);
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
