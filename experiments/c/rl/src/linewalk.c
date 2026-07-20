#include "reinforcementlearning/envs/linewalk.h"
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

enum LineWalkPlayer get_player(LineWalkState state) {
    /* Silence C89 unused parameter warnings */
    (void)state;

    return PLAYER1;
}

LineWalkState step(LineWalkState state, enum LineWalkAction action) {
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

float reward(enum LineWalkPlayer player, LineWalkState state) {
    /* Silence C89 unused parameter warnings */
    (void)player;

    if (state.kind == TERMINAL) {
        return (float) state.reward;
    } else {
        return 0.0f;
    }
}

/* TODO: Perhaps remove the num_actions output parameter and just return the count? */
unsigned int get_available_actions (LineWalkState state, enum LineWalkAction *available_actions, unsigned int* num_actions) {
    unsigned int count = 0;
    if (state.kind == ACTIVE) {
        available_actions[count++] = MOVE_LEFT;
        available_actions[count++] = MOVE_RIGHT;
    }
    *num_actions = count;
    return count;
}

enum LineWalkAction linewalk_mctsenv_get_random_action(LineWalkState state) {
    (void)state;
    return (enum LineWalkAction) rand() % 2;
}

void print_state(LineWalkState state) {
    if (state.kind == ACTIVE) {
        printf("Current position: %u\n", state.position);
    } else {
        printf("Terminal state with reward: %d\n", state.reward);
    }
}


struct LineWalkEnvironment {
    LineWalkConfig config;
    unsigned short int position;
};

struct LineWalkEnvironment* linewalk_make(struct LineWalkConfig config) {
    struct LineWalkEnvironment* env = malloc(sizeof(struct LineWalkEnvironment));
    if (env == NULL) {
        fprintf(stderr, "linewalk: Failed to allocate memory for LineWalkEnvironment\n");
        return NULL;
    }
    if (linewalk_init(config, env)) {
        fprintf(stderr, "linewalk: Failed to initialize LineWalkEnvironment\n");
        free(env);
        return NULL;
    }
    return env;
}

int linewalk_init(struct LineWalkConfig config, struct LineWalkEnvironment* env) {
    env->config = config;
    env->position = (config.N + 1) / 2; /* Start at the middle position */
    return 0;
};

struct LineWalkObservation linewalk_reset(struct LineWalkEnvironment* env) {
    env->position = (env->config.N + 1) / 2; /* Reset to middle position */
    return (struct LineWalkObservation) { env->position };
}

struct LineWalkStepReturn linewalk_step(struct LineWalkEnvironment* env, enum LineWalkAction action) {
    /* We reinvent the wheel rather than using the step method
     * that was defined above */
    unsigned short int new_pos = env->position;
    float reward;
    Boolean terminated;
    struct LineWalkStepReturn ret;

    if (new_pos > 0 && new_pos < (env->config.N + 1)) {
        switch (action) {
            case MOVE_LEFT:
                new_pos -= 1;
                break;
            case MOVE_RIGHT:
                new_pos += 1;
                break;
        }
    }
    if (new_pos < 1) {
        terminated = TRUE;
        reward = -1.0f;
    } else if (new_pos > env->config.N) {
        terminated = TRUE;
        reward = 1.0f;
    } else {
        terminated = FALSE;
        reward = 0.0f;
    }

    env->position = new_pos;
    ret.observation.position = new_pos;
    ret.reward = reward;
    ret.terminated = terminated;

    return ret;
}

void linewalk_deinit(struct LineWalkEnvironment* env) {
    (void)env;
    /* Nothing to do */
}

void linewalk_close(struct LineWalkEnvironment* env) {
    linewalk_deinit(env);
    free(env);
}


#define ENVIRONMENT_PREFIX linewalk
#define CONFIG_TYPE LineWalkConfig
#define STATE_TYPE LineWalkState
#define ACTION_TYPE enum LineWalkAction
#define STEP_METHOD step
#define RANDOM_ACTION_METHOD linewalk_mctsenv_get_random_action
#define IS_TERMINAL_METHOD is_terminal
#include "mctsenv_uniform_random_actions_c.inc"
#undef IS_TERMINAL_METHOD
#undef RANDOM_ACTION_METHOD 
#undef STEP_METHOD
#undef ACTION_TYPE
#undef STATE_TYPE
#undef CONFIG_TYPE
#undef ENVIRONMENT_PREFIX

enum LineWalkAction linewalk_get_random_action(struct LineWalkEnvironment* env) {
    (void)env;
    return (enum LineWalkAction) rand() % 2;
}

#define ENVIRONMENT_PREFIX linewalk
#define CONFIG_TYPE struct LineWalkConfig
#define ENVIRONMENT_TYPE struct LineWalkEnvironment
#define OBSERVATION_TYPE struct LineWalkObservation
#define STEPRETURN_TYPE struct LineWalkStepReturn
#define ACTION_TYPE enum LineWalkAction
#define MAKE_METHOD linewalk_make
#define RESET_METHOD linewalk_reset
#define RANDOM_ACTION_METHOD linewalk_get_random_action
#define STEP_METHOD linewalk_step
#define CLOSE_METHOD linewalk_close
#include "uniform_random_actions_c.inc"
#undef CLOSE_METHOD
#undef STEP_METHOD
#undef RANDOM_ACTION_METHOD
#undef RESET_METHOD
#undef MAKE_METHOD
#undef ACTION_TYPE
#undef STEPRETURN_TYPE
#undef OBSERVATION_TYPE
#undef ENVIRONMENT_TYPE
#undef CONFIG_TYPE
#undef ENVIRONMENT_PREFIX
