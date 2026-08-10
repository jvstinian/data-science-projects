#include <reinforcementlearning/envs/linewalk.h>
#include <reinforcementlearning/random.h>
#include <stdio.h>
#include <stdlib.h>

/* TODO: LineWalk action list
#define ENVIRONMENT_PREFIX linewalk
#define ENVIRONMENT_STRUCT_PREFIX LineWalk
#define ACTION_TYPE enum LineWalkAction
#include <reinforcementlearning/action_array_template.inc>
*/

struct LineWalkActionList {
    enum LineWalkAction actions[2];
};

/* Note there's no capacity provided to the following function,
 * since the action list is always of size 2.
 * Also, this function is static, and is not available
 * for use outside this file. */
static struct LineWalkActionList* linewalk_action_list_create() {
    struct LineWalkActionList* ret = malloc(sizeof(struct LineWalkActionList));
    if (ret == NULL) {
        fprintf(stderr, "linewalk_action_list_create: failed to allocate memory for action list\n");
        return NULL;
    }
    ret->actions[0] = MOVE_LEFT;
    ret->actions[1] = MOVE_RIGHT;
    return ret;
}

size_t linewalk_action_list_length (struct LineWalkActionList* lp) {
    (void) lp; /* unused parameter */
    return 2;
}

enum LineWalkAction linewalk_action_list_get (struct LineWalkActionList* lp, size_t i) {
    return lp->actions[i];
}

void linewalk_action_list_shuffle (struct LineWalkActionList* lp) {
    /* Simple shuffle for 2 elements */
    if (rand_float() < 0.5) {
        enum LineWalkAction temp = lp->actions[0];
        lp->actions[0] = lp->actions[1];
        lp->actions[1] = temp;
    }
}
void linewalk_action_list_destroy (struct LineWalkActionList* lp) {
    free(lp);
}

LineWalkState linewalk_initial_state(LineWalkConfig config) {
    unsigned short int pos = (config.N + 1) / 2; /* Start at the middle position */
    LineWalkState state = { config, ACTIVE, pos, 0 }; /* Start at the leftmost position */
    return state;
}

Boolean linewalk_is_terminal(LineWalkState state) {
    if (state.kind == ACTIVE) {
        return FALSE;
    } else { 
        return TRUE;
    }
}

enum LineWalkPlayer linewalk_get_player(LineWalkState state) {
    /* Silence C89 unused parameter warnings */
    (void)state;

    return PLAYER1;
}

LineWalkState linewalk_act(LineWalkState state, enum LineWalkAction action) {
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

float linewalk_reward(enum LineWalkPlayer player, LineWalkState state) {
    /* Silence C89 unused parameter warnings */
    (void)player;

    if (state.kind == TERMINAL) {
        return (float) state.reward;
    } else {
        return 0.0f;
    }
}

/* TODO: Perhaps remove the num_actions output parameter and just return the count? */
unsigned int linewalk_get_available_actions (LineWalkState state, enum LineWalkAction *available_actions, unsigned int* num_actions) {
    unsigned int count = 0;
    if (state.kind == ACTIVE) {
        available_actions[count++] = MOVE_LEFT;
        available_actions[count++] = MOVE_RIGHT;
    }
    *num_actions = count;
    return count;
}

struct LineWalkActionList* linewalk_experimental_get_valid_actions(struct LineWalkState s) {
    (void) s; /* Silence C89 unused parameter warnings */
    return linewalk_action_list_create();
}

enum LineWalkAction linewalk_mctsenv_get_random_action(LineWalkState state) {
    (void)state;
    return (enum LineWalkAction) rand() % 2;
}

void linewalk_print_state(LineWalkState state) {
    if (state.kind == ACTIVE) {
        printf("Current position: %u\n", state.position);
    } else {
        printf("Terminal state with reward: %d\n", state.reward);
    }
}

#define ENVIRONMENT_PREFIX linewalk
#define CONFIG_TYPE LineWalkConfig
#define STATE_TYPE LineWalkState
#define ACTION_TYPE enum LineWalkAction
#define INITIAL_STATE_METHOD linewalk_initial_state
#define STEP_METHOD linewalk_act
#define PLAYER_TYPE enum LineWalkPlayer
#define GET_PLAYER_METHOD linewalk_get_player
#define RANDOM_ACTION_METHOD linewalk_mctsenv_get_random_action
#define IS_TERMINAL_METHOD linewalk_is_terminal
#define REWARD_METHOD linewalk_reward
#include <reinforcementlearning/algorithms/mctsenv_uniform_random_actions.inc>

static Boolean linewalk_action_eq(enum LineWalkAction a1, enum LineWalkAction a2) {
    return (a1 == a2);
}


#define ENVIRONMENT_PREFIX linewalk
#define ENVIRONMENT_STRUCT_PREFIX LineWalk
#define CONFIG_TYPE struct LineWalkConfig
#define STATE_TYPE struct LineWalkState
#define ACTION_TYPE enum LineWalkAction
#define PLAYER_TYPE enum LineWalkPlayer
#define INITIAL_STATE_METHOD linewalk_initial_state
#define STEP_METHOD linewalk_act
#define GET_PLAYER_METHOD linewalk_get_player
#define RANDOM_ACTION_METHOD linewalk_mctsenv_get_random_action
#define IS_TERMINAL_METHOD linewalk_is_terminal
#define REWARD_METHOD linewalk_reward
#define ACTION_LIST_TYPE struct LineWalkActionList
#define GET_VALID_ACTIONS_METHOD linewalk_experimental_get_valid_actions
#define ACTION_LIST_GET_METHOD linewalk_action_list_get
#define ACTION_LIST_LENGTH_METHOD linewalk_action_list_length
#define ACTION_LIST_SHUFFLE_METHOD linewalk_action_list_shuffle
#define ACTION_LIST_DESTROY_METHOD linewalk_action_list_destroy
#define ACTION_EQ_METHOD linewalk_action_eq
#include <reinforcementlearning/algorithms/uct.inc>


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
#include <reinforcementlearning/algorithms/uniform_random_actions.inc>
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
