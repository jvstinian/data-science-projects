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

struct LineWalkStepReturn linewalk_step(struct LineWalkEnvironment* env, enum Action action) {
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

struct SimulationSummary uniform_random_actions(struct LineWalkConfig config, Boolean verbose) {
    struct LineWalkEnvironment* env = linewalk_make(config);
    if (env == NULL) {
        fprintf(stderr, "uniform_random_actions: failed to create LineWalkEnvironment\n");
        return (struct SimulationSummary) { 0, 0.0f };
    }

    /* Other declarations */
    struct LineWalkObservation obs;
    enum Action action;
    struct LineWalkStepReturn step_output;
    size_t i;
    float total_reward = 0.0f;

    /* Set seed and initialize environment */
    srand(123);
    obs = linewalk_reset(env);
    (void)obs; /* Suppress unused variable warning */

    i = 0;
    while (1) {
        action = (enum Action) rand() % 2;
        step_output = linewalk_step(env, action);
        obs = step_output.observation;
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
    linewalk_close(env);
    return (struct SimulationSummary) { i, total_reward };
}

enum Action linewalk_get_random_action(struct LineWalkEnvironment* env) {
    (void)env;
    return (enum Action) rand() % 2;
}

#define ENVIRONMENT_PREFIX linewalk
#define CONFIG_TYPE struct LineWalkConfig
#define ENVIRONMENT_TYPE struct LineWalkEnvironment
#define OBSERVATION_TYPE struct LineWalkObservation
#define STEPRETURN_TYPE struct LineWalkStepReturn
#define ACTION_TYPE enum Action
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
