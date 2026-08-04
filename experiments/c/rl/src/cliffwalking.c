#include <reinforcementlearning/envs/cliffwalking.h>
#include <reinforcementlearning/random.h>
#include <stdio.h>
#include <stdlib.h> /* malloc, free */
#include <string.h> /* memcpy */
#include <alloca.h>
#include <time.h> /* time */
#include <assert.h>

enum MapElement {
    START,
    GROUND,
    CLIFF,
    GOAL
};

static enum MapElement cliffwalk_map[CLIFFWALK_NUM_ROWS][CLIFFWALK_NUM_COLS] = {
    { GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND },
    { GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND },
    { GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND },
    {  START, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND, GROUND,   GOAL }
};

struct PositionType {
    unsigned int row;
    unsigned int col;
};

struct TransitionType {
    float probability;
    struct PositionType position;
    float reward;
    Boolean terminated;
};

/* The following is a partial set of fields for the
 * Transition_Type which follows */
struct PartialTransitionType {
    struct PositionType position;
    float reward;
    Boolean terminated;
};

struct CliffwalkingEnvironment {
    struct TransitionType p[CLIFFWALK_NUM_ROWS][CLIFFWALK_NUM_COLS][CLIFFWALK_ACTION_COUNT][CLIFFWALK_ACTION_COUNT];
    struct PositionType agent_position;
};
  
static int can_slip(enum CliffwalkingAction intended_action, enum CliffwalkingAction actual_action) {
    switch (intended_action) {
        case LEFT:
            return (actual_action == LEFT) || (actual_action == UP) || (actual_action == DOWN);
        case DOWN:
            return (actual_action == DOWN) || (actual_action == LEFT) || (actual_action == RIGHT);
        case RIGHT:
            return (actual_action == RIGHT) || (actual_action == DOWN) || (actual_action == UP);
        case UP: 
            return (actual_action == UP) || (actual_action == RIGHT) || (actual_action == LEFT);
        default:
            return 0;
    };
}

/* The following is identical to the version in frozenlake, except
 * the default is the lower left corner (3, 0) rather than the upper left corner.
 * Since there's only one map and one start position when Cliff Walking,
 * we could simplify this to just return the unique start position (3, 0). */
struct PositionType get_start_position(const enum MapElement map[CLIFFWALK_NUM_ROWS][CLIFFWALK_NUM_COLS]) {
    struct PositionType start_position = { 3, 0 };
    unsigned int r, c;
    /* Determine the start position
     * Unlike the Python version, we assume that there is either one start position or no start position
     * is defined, in which case we take the lower left corner as the start position.
     * The following loop finds the start position if it is provided.  The loop exits
     * as soon as the start position is found. */
    for(r = 0; r < CLIFFWALK_NUM_ROWS; r++) {
        for(c = 0; c < CLIFFWALK_NUM_COLS; c++) {
            if (map[r][c] == START) {
                start_position.row = r;
                start_position.col = c;
                return start_position;
            }
        }
    }
    return start_position;
}

/* The following was To_S in the Ada implementation. */
static unsigned int to_position_index(struct PositionType position) {
    return position.row * CLIFFWALK_NUM_COLS + position.col;
}

/* The following is similar to the version in frozenlake,
 * except that we don't need to provide the number of rows
 * and columns as inputs as they are constants in Cliff Walking. */
struct PositionType position_inc(struct PositionType position, enum CliffwalkingAction action) {
    struct PositionType new_position = position;
    switch (action) {
        case LEFT:
            if (new_position.col > 0) {
                new_position.col--;
            }
            break;
        case DOWN:
            if (new_position.row < CLIFFWALK_NUM_ROWS - 1) {
                new_position.row++;
            }
            break;
        case RIGHT:
            if (new_position.col < CLIFFWALK_NUM_COLS - 1) {
                new_position.col++;
            }
            break;
        case UP:
            if (new_position.row > 0) {
                new_position.row--;
            }
            break;
    }
    return new_position;
}

/* The following is adapted from frozenlake with the changes noted in the comments. */
struct PartialTransitionType update_probability_matrix(
        const enum MapElement map[CLIFFWALK_NUM_ROWS][CLIFFWALK_NUM_COLS],
        struct PositionType position,
        enum CliffwalkingAction action
) {
    struct PositionType new_position = position_inc(position, action);
    enum MapElement new_me = map[new_position.row][new_position.col];
    /* Terminated differs from the approach in frozenlake.  If the agent falls off the cliff,
     * the agent is sent back to the start with a reward of -100 for the step rather than
     * terminating the episode.
     * We only terminate if the agent reaches the goal. */
    Boolean terminated = (new_me == GOAL);
    float reward = -1.0;  /* Reward of -1 unless the agent falls off the cliff */

    if (new_me == CLIFF) {
        /* Set reward to -100 when the agent falls off the cliff, and
         * send the agent back to the start position. */
         reward = -100.0;
         new_position = get_start_position(map);
    }
    return (struct PartialTransitionType) {new_position, reward, terminated};
}
 
static int cliffwalking_init(struct CliffwalkingConfig config, struct CliffwalkingEnvironment* env) {
    struct TransitionType p[CLIFFWALK_NUM_ROWS][CLIFFWALK_NUM_COLS][CLIFFWALK_ACTION_COUNT][CLIFFWALK_ACTION_COUNT];

    unsigned int r, c;
    enum CliffwalkingAction ai, aa;
    struct PositionType start_position = get_start_position(cliffwalk_map);
    struct PositionType temp_cpos;
    struct PartialTransitionType temp_partial_transition;
    float temp_probability;

    for (r = 0; r < CLIFFWALK_NUM_ROWS; r++) {
        for (c = 0; c < CLIFFWALK_NUM_COLS; c++) {
            switch (cliffwalk_map[r][c]) {
                case GOAL:
                case CLIFF:
                    /* We handle the case where the Agent is already at the goal or 
                     * somehow permanently fell off the cliff.
                     * This case should not occur in practice */
                    for (ai = LEFT; ai <= UP; ai++) {
                        for (aa = LEFT; aa <= UP; aa++) {
                            if (ai == aa) {
                                p[r][c][ai][aa] = (struct TransitionType) { 1.0, { r, c }, 0.0, TRUE };
                            } else {
                                p[r][c][ai][aa] = (struct TransitionType) { 0.0, { r, c }, 0.0, TRUE };
                            }
                        }
                    }
                    break;
                case START:
                case GROUND:
                    for (ai = LEFT; ai <= UP; ai++) {
                        for (aa = LEFT; aa <= UP; aa++) {
                            temp_cpos = (struct PositionType) { r, c };
                            temp_partial_transition = update_probability_matrix(cliffwalk_map, temp_cpos, aa);
                            if (config.is_slippery) {
                                if (can_slip(ai, aa)) {
                                    temp_probability = 1.0 / 3.0;
                                } else {
                                    temp_probability = 0.0;
                                }
                            } else {
                                if (ai == aa) {
                                    temp_probability = 1.0;
                                } else {
                                    temp_probability = 0.0;
                                }
                            }
                            p[r][c][ai][aa] = (struct TransitionType) {
                                temp_probability,
                                temp_partial_transition.position,
                                temp_partial_transition.reward,
                                temp_partial_transition.terminated
                            };
                        }
                    }
                    break;
            }
        }
    }
    memcpy(env->p, p, sizeof(p));
    env->agent_position = start_position;
    return 0;
}
   
struct CliffwalkingEnvironment* cliffwalking_make(struct CliffwalkingConfig config) {
    struct CliffwalkingEnvironment* env = malloc(sizeof(struct CliffwalkingEnvironment));
    if (env == NULL) {
        fprintf(stderr, "cliffwalking_make: Failed to allocate memory for CliffwalkingEnvironment\n");
        return NULL;
    }
    if (cliffwalking_init(config, env)) {
        fprintf(stderr, "cliffwalking_make: Failed to initialize CliffwalkingEnvironment\n");
        free(env);
        return NULL;
    }
    return env;
}


struct CliffwalkingObservation cliffwalking_reset(struct CliffwalkingEnvironment* env) {
    struct CliffwalkingObservation result;
    /* Determine how we want to reset the random number generator. */
    srand(time(NULL));
    env->agent_position = get_start_position(cliffwalk_map);
    result.position_index = to_position_index(env->agent_position);
    return result;
}

struct CliffwalkingStepReturn cliffwalking_step(struct CliffwalkingEnvironment* env, enum CliffwalkingAction action) {
    float cumprob = 0.0f;
    float u = rand_float();
    unsigned int r, c;
    enum CliffwalkingAction actual_action;
    struct CliffwalkingObservation obs;
    struct TransitionType transition;

    r = env->agent_position.row;
    c = env->agent_position.col;

    /* Obtain the actual action for the transition based on the
     * current position, the action taken by the Agent, and
     * the random number generated. */
    for(actual_action = LEFT; actual_action <= UP; actual_action++) {
        cumprob += env->p[r][c][action][actual_action].probability;
        if (u <= cumprob) {
            break;
        }
    }
    assert(actual_action <= UP);

    transition = env->p[r][c][action][actual_action];
    
    /* Update the Agent's position based on the transition */
    env->agent_position = transition.position;
    obs.position_index = to_position_index(transition.position);
    return (struct CliffwalkingStepReturn) { obs, transition.reward, transition.terminated };
}

void cliffwalking_render_text(const struct CliffwalkingEnvironment* env) {
    unsigned int r, c;
    for (r = 0; r < CLIFFWALK_NUM_ROWS; r++) {
        for (c = 0; c < CLIFFWALK_NUM_COLS; c++) {
            /* Put a leading space for all columns except the first to match
             * the formatting of the Python version. */
            if (c != 0) {
               printf(" ");
            }

            if ((env->agent_position.row == r) && (env->agent_position.col == c)) {
                printf("A");
            } else {
                switch (cliffwalk_map[r][c]) {
                    case START:
                        printf("o");
                        break;
                    case GROUND:
                        printf("o");
                        break;
                    case CLIFF:
                        printf("C");
                        break;
                    case GOAL:
                        printf("T");
                        break;
                }
            }

            /* Put a trailing space for all columns except the last to match
             * the formatting of the Python version. */
            if (c != (CLIFFWALK_NUM_COLS - 1)) {
               printf(" ");
            }
        }
        printf("\n");
    }
}

void cliffwalking_close(struct CliffwalkingEnvironment* env) {
    /* No deinit is needed */
    free(env);
}

struct CliffwalkingDPModel {
    unsigned int num_states;
    unsigned int num_actions;
    struct TransitionProbability transition_probabilities[CLIFFWALK_NUM_STATES][CLIFFWALK_ACTION_COUNT][CLIFFWALK_NUM_STATES];
};

struct LocalExpectedRewardType {
    float probability_weighted_reward;
    float total_probability;
};

static int cliffwalking_model_init(struct CliffwalkingConfig config, struct CliffwalkingDPModel* model){
    struct CliffwalkingEnvironment env;
    size_t arr_size;
    unsigned int r, c;
    unsigned int prev_dpos, next_dpos;
    enum CliffwalkingAction a, a_act;
    struct LocalExpectedRewardType* temp_expected_rewards;
    float temp_probability, temp_reward;

    model->num_states = CLIFFWALK_NUM_STATES;
    model->num_actions = CLIFFWALK_ACTION_COUNT;

    temp_expected_rewards = alloca(model->num_states * sizeof(struct LocalExpectedRewardType));
    if (temp_expected_rewards == NULL) {
        fprintf(stderr, "cliffwalking_model_init: error creating local expected rewards with alloca");
        return 1;
    }

    arr_size = model->num_states * model->num_states * model->num_actions;
    /* Assign 0 throughout */
    memset(model->transition_probabilities, 0, arr_size * sizeof(struct TransitionProbability));

    if (cliffwalking_init(config, &env)) {
        fprintf(stderr, "cliffwalking_model_init: could not initialize transition probabilities");
        /* No cliffwalking_model_deinit call needed */
        return 1;
    }
    for (r = 0; r < CLIFFWALK_NUM_ROWS; r++) {
        for (c = 0; c < CLIFFWALK_NUM_COLS; c++) {
            prev_dpos = to_position_index((struct PositionType) { r, c });
            for(a = LEFT; a <= UP; a++) {
               /* When the cliff is slippery, an action can lead to state transitions
                * with different probabilities.
                * This can be seen when in a corner cell of the map, in which case
                * an action that would take you off the board (if there was no slipping)
                * will result in arriving at the same cell 2/3 of the time.
                * To obtain the correct values, we calculate the conditional expectation for
                * the state transitions.
                * This should also generalize if we were to consider state transitions with
                * non-uniform probabilities. */
                /* Reset temp_expected_rewards for each action */
                memset(temp_expected_rewards, 0, model->num_states * sizeof(struct LocalExpectedRewardType));

                for(a_act = LEFT; a_act <= UP; a_act++) {
                    next_dpos = to_position_index(env.p[r][c][a][a_act].position);
                    temp_probability = env.p[r][c][a][a_act].probability;
                    temp_reward = env.p[r][c][a][a_act].reward;
                    temp_expected_rewards[next_dpos].probability_weighted_reward += temp_probability * temp_reward;
                    temp_expected_rewards[next_dpos].total_probability += temp_probability;
                }
                /* Now that we've processed the possible transitions and their probabilities for a given action,
                 * we calculate the discrete transition probabilities and conditional rewards */
                for(next_dpos = 0; next_dpos < model->num_states; next_dpos++) {
                    if (temp_expected_rewards[next_dpos].total_probability > 0.0f) {
                        model->transition_probabilities[prev_dpos][a][next_dpos].probability = temp_expected_rewards[next_dpos].total_probability;
                        model->transition_probabilities[prev_dpos][a][next_dpos].reward = temp_expected_rewards[next_dpos].probability_weighted_reward / temp_expected_rewards[next_dpos].total_probability;
                    }
                }
            }
        }
    }
    /* No cliffwalking_deinit(&env) call needed */
    return 0;
}

struct CliffwalkingDPModel* cliffwalking_dpmodel_new(struct CliffwalkingConfig config) {
    struct CliffwalkingDPModel* model = malloc(sizeof(struct CliffwalkingDPModel));
    if (model == NULL) {
        fprintf(stderr, "cliffwalking_dpmodel_new: Failed to allocate memory for DP model\n");
        return NULL;
    }

    if (cliffwalking_model_init(config, model)) {
        fprintf(stderr, "cliffwalking_dpmodel_new: Failed to initialize DP model\n");
        free(model);
        return NULL;
    }
    return model;
}

void cliffwalking_dpmodel_free(struct CliffwalkingDPModel* model) {
    /* No deinit needed */
    free(model);
}

struct TransitionProbability cliffwalking_get_transition(const struct CliffwalkingDPModel* model, unsigned int s, enum CliffwalkingAction action, unsigned int next_s) {
    return model->transition_probabilities[s][action][next_s];
}
