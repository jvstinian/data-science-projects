#include "frozenlake.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h> /* memset */
#include <alloca.h>
#include <time.h> /* For setting the RNG */

float rand_float() {
    return (float)rand() / (float)RAND_MAX;
};

enum ActionType get_random_action() {
    return (enum ActionType) rand() % ACTION_COUNT;
};

struct LocalMap {
    unsigned int rows;
    unsigned int cols;
    const enum MapElement* map;
};

static enum MapElement map4x4[16] = {
    START, FROZEN, FROZEN, FROZEN,
   FROZEN,   HOLE, FROZEN,   HOLE,
   FROZEN, FROZEN, FROZEN,   HOLE,
     HOLE, FROZEN, FROZEN,   GOAL
};

static enum MapElement map8x8[64] = {
    START, FROZEN, FROZEN, FROZEN, FROZEN, FROZEN, FROZEN, FROZEN,
   FROZEN, FROZEN, FROZEN, FROZEN, FROZEN, FROZEN, FROZEN, FROZEN,
   FROZEN, FROZEN, FROZEN,   HOLE, FROZEN, FROZEN, FROZEN, FROZEN,
   FROZEN, FROZEN, FROZEN, FROZEN, FROZEN,   HOLE, FROZEN, FROZEN,
   FROZEN, FROZEN, FROZEN,   HOLE, FROZEN, FROZEN, FROZEN, FROZEN,
   FROZEN,   HOLE,   HOLE, FROZEN, FROZEN, FROZEN,   HOLE, FROZEN,
   FROZEN,   HOLE, FROZEN, FROZEN,   HOLE, FROZEN,   HOLE, FROZEN,
   FROZEN, FROZEN, FROZEN,   HOLE, FROZEN, FROZEN, FROZEN,   GOAL
};

struct LocalMap get_map(enum MapType map_name) {
    unsigned int rows;
    unsigned int cols;
    const enum MapElement* map_ptr = map4x4;
    switch (map_name) {
        case MAP_4X4:
            rows = cols = 4;
            map_ptr = map4x4;
            break;
        case MAP_8X8: 
            rows = cols = 8;
            map_ptr = map8x8;
            break;
        default:
            rows = cols = 4;
            map_ptr = map4x4;
            break;
    };
    return (struct LocalMap) { rows, cols, map_ptr };
}
  
struct PositionType get_start_position(unsigned int rows, unsigned int cols, const enum MapElement* map) {
    struct PositionType start_position = { 0, 0 };
    unsigned int r, c, i;
    /* Determine the start position
     * Unlike the Python version, we assume that there is either one start position or no start position
     * is defined, in which case we take the top left corner as the start position.
     * The following loop finds the start position if it is provided.  The loop exits
     * as soon as the start position is found. */
    for(r = 0; r < rows; r++) {
        for(c = 0; c < cols; c++) {
            i = r * cols + c;
            if (map[i] == START) {
                start_position.row = r;
                start_position.col = c;
                return start_position;
            }
        }
    }
    return start_position;
}


struct PositionType position_inc(unsigned int rows, unsigned int cols, struct PositionType position, enum ActionType action) {
    assert((rows > 0) && (cols > 0));
    unsigned int i = position.row;
    unsigned int j = position.col;
    switch (action) {
        case LEFT:
            if (j > 0) {
                j--;
            }
            break;
        case DOWN:
            if (i < rows - 1) {
                i++;
            }
            break;
        case RIGHT:
            if (j < cols - 1) {
                j++;
            }
            break;
        case UP:
            if (i > 0) {
                i--;
            }
            break;
    }
    return (struct PositionType) {i, j};
}

int can_slip(enum ActionType intended_action, enum ActionType actual_action) {
    switch (intended_action) {
        case LEFT:
            return (actual_action == LEFT) || (actual_action == UP) || (actual_action = DOWN);
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

int make(struct EnvironmentConfig config, struct EnvironmentState* state) {
    unsigned int r, c, i;
    struct PositionType new_position;
    enum MapElement new_me;
    enum ActionType ai, aa;
    float temp_probability;
    float reward;
    Boolean terminated;
    struct LocalMap map = get_map(config.map_name);
    ActionTransitionType* p = malloc(map.rows * map.cols * sizeof(ActionTransitionType));
    if (p == NULL) {
        fprintf(stderr, "make: could not allocate transition types");
        return 1;
    }

    struct PositionType start_position = get_start_position(map.rows, map.cols, map.map);

    for (r = 0; r < map.rows; r++ ){
        for (c = 0; c < map.cols; c++ ){
            i = r * map.cols + c;
            switch (map.map[i]) {
                case HOLE:
                case GOAL:
                   /* We handle the case where the Agent is already at the goal or in a hole
                    * This case should not occur in practice */
                   for (ai = LEFT; ai <= UP; ai++) {
                       for (aa = LEFT; aa <= UP; aa++) {
                           if (ai == aa) {
                               p[i][ai][aa] = (struct TransitionType) { 1.0, { r, c }, 0.0, TRUE };
                           } else {
                               p[i][ai][aa] = (struct TransitionType) { 0.0, { r, c }, 0.0, TRUE };
                           }
                       }
                   }
                   break;
                case START:
                case FROZEN:
                   for (ai = LEFT; ai <= UP; ai++) {
                       for (aa = LEFT; aa <= UP; aa++) {
                           new_position = position_inc(map.rows, map.cols, (struct PositionType) { r, c }, aa);
                           new_me = map.map[new_position.row * map.cols + new_position.col];
                           terminated = (Boolean) ( (new_me == HOLE) || (new_me == GOAL) );
                           reward = (float) (new_me == GOAL);
                           temp_probability = 0.0;
                           if (config.slippery && can_slip(ai, aa)) {
                                temp_probability = 1.0 / 3.0;
                            } else if (!config.slippery && (ai == aa)) {
                                /* Note it might not be necessary to specify !config.slippery here
                                 * but we keep it for readability of the cases */
                                temp_probability = 1.0;
                            }
                           p[i][ai][aa] = (struct TransitionType) { temp_probability, new_position, reward, terminated };
                       }
                   }
                   break;
                }
            }
        }
      
    state->rows = map.rows;
    state->cols = map.cols;
    state->map = map.map;
    state->p = p;
    state->agent_position = start_position;
    return 0;
}

void close(struct EnvironmentState state) {
    free(state.p);
}
      
struct ObservationType reset(struct EnvironmentState* env) {
    struct ObservationType result;
    srand(time(NULL));
    env->agent_position = get_start_position(env->rows, env->cols, env->map);
    result.position_index = env->agent_position.row * env->cols + env->agent_position.col;
    return result;
}

void render_text(struct EnvironmentState env) {
    unsigned int rows = env.rows, cols = env.cols;
    unsigned int r, c, i;
    for(r = 0; r < rows; r++) {
        for(c = 0; c < cols; c++) {
            if((env.agent_position.row == r) && (env.agent_position.col == c)) {
                printf("%c", 'A');
            } else {
                i = r * cols + c;
                switch (env.map[i]) {
                    case START:
                        printf("%c", 'S');
                        break;
                    case FROZEN:
                        printf("%c", 'F');
                        break;
                    case HOLE:
                        printf("%c", 'H');
                        break;
                    case GOAL:
                        printf("%c", 'G');
                        break;
                }
            }
        }
        printf("\n");
    }
}
   
struct StepReturnType step(struct EnvironmentState* env, enum ActionType action) {
    float cumprob = 0.0f;
    float u = rand_float();
    unsigned int i = env->agent_position.row * env->cols + env->agent_position.col;
    enum ActionType actual_action;
    struct ObservationType obs;
    struct TransitionType transition;

    for(actual_action = LEFT; actual_action <= UP; actual_action++) {
        cumprob = env->p[i][action][actual_action].probability + cumprob;
        if (u <= cumprob) {
            break;
        }
    }
    assert(actual_action <= UP);

    transition = env->p[i][action][actual_action];
    
    env->agent_position = transition.position;
    obs.position_index = transition.position.row * env->cols + transition.position.col;
    return (struct StepReturnType) { obs, transition.reward, transition.terminated };
}

/*
package body Frozen_Lake is
   -- Note the following is different from the Python implementation as internally we
   -- track the Agent's position using 1-based indexing.
   -- To keep the observations consistent with the Python implementation,
   -- we convert the position to a 0-based index.
   function To_S(Map: Map_Array; Position: Position_Type) return Natural is
      Row : Positive := Position.Row;
      Col : Positive := Position.Col;
      Num_Col : Positive := Map'Length(2);
   begin
      return (Row - 1) * Num_Col + (Col - 1);
   end To_S;


    -- def reset(
    --     self,
    --     *,
    --     seed: Optional[int] = None,
    --     options: Optional[dict] = None,
    -- ):
    --     super().reset(seed=seed)
    --     self.s = categorical_sample(self.initial_state_distrib, self.np_random)
    --     self.lastaction = None
    --     return int(self.s), {"prob": 1}

   
    -- def step(self, a):
    --     transitions = self.P[self.s][a]
    --     i = categorical_sample([t[0] for t in transitions], self.np_random)
    --     p, s, r, t = transitions[i]
    --     self.s = s
    --     self.lastaction = a

    --     if self.render_mode == "human":
    --         self.render()
    --     return (int(s), r, t, False, {"prob": p})
*/

struct DiscreteModelType {
    unsigned int num_states;
    struct TransitionProbabilityType* transition_probabilities;
};
      
struct LocalExpectedRewardType {
    float probability_weighted_reward;
    float total_probability;
};


struct TransitionProbabilityType frozenlake_get_transition(const struct DiscreteModelType* model, unsigned int s, enum ActionType action, unsigned int next_s) {
    size_t idx = s * ACTION_COUNT * model->num_states + action * model->num_states + next_s;
    return model->transition_probabilities[idx];
}

static unsigned int to_position_index(unsigned int cols, struct PositionType position) {
    return position.row * cols + position.col;
}

int frozenlake_model_create(struct EnvironmentConfig config, struct DiscreteModelType* model){
    struct EnvironmentState env;
    size_t arr_size;
    unsigned int prev_dpos, next_dpos;
    unsigned int model_idx;
    enum ActionType a, a_act;
    struct LocalExpectedRewardType* temp_expected_rewards;
    float temp_probability, temp_reward;

    switch (config.map_name) {
        case MAP_4X4:
            model->num_states = 16;
            break;
        case MAP_8X8:
            model->num_states = 64;
            break;
    };

    temp_expected_rewards = alloca(model->num_states * sizeof(struct LocalExpectedRewardType));
    if (temp_expected_rewards == NULL) {
        fprintf(stderr, "frozenlake_get_model: error creating local expected rewards with alloca");
        return 1;
    }

    arr_size = model->num_states * model->num_states * ACTION_COUNT;
    model->transition_probabilities = malloc(arr_size * sizeof(struct TransitionType));
    if (model->transition_probabilities == NULL) {
        fprintf(stderr, "frozenlake_get_model: could not creaate list for transition data");
        return 1;
    }

    /* Assign 0 throughout */
    memset(model->transition_probabilities, 0, arr_size * sizeof(struct TransitionType));
    if (make(config, &env)) {
        fprintf(stderr, "frozenlake_get_model: could not initialize environment");
        frozenlake_model_destroy(model);
        return 1;
    }
    for(prev_dpos=0; prev_dpos < model->num_states; prev_dpos++) {
        for(a = LEFT; a <= UP; a++) {
           /* When the frozen lake is slippery, an action can lead to state transitions
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
                next_dpos = to_position_index(env.cols, env.p[prev_dpos][a][a_act].position);
                temp_probability = env.p[prev_dpos][a][a_act].probability;
                temp_reward = env.p[prev_dpos][a][a_act].reward;
                temp_expected_rewards[next_dpos].probability_weighted_reward += temp_probability * temp_reward;
                temp_expected_rewards[next_dpos].total_probability += temp_probability;
           }
           /* Now that we've processed the possible transitions and their probabilities for a given action,
            * we calculate the discrete transition probabilities and conditional rewards */
           for(next_dpos = 0; next_dpos < model->num_states; next_dpos++) {
                if (temp_expected_rewards[next_dpos].total_probability > 0.0f) {
                    model_idx = prev_dpos * ACTION_COUNT * model->num_states + a * model->num_states + next_dpos;
                    model->transition_probabilities[model_idx].probability = temp_expected_rewards[next_dpos].total_probability;
                    model->transition_probabilities[model_idx].reward = temp_expected_rewards[next_dpos].probability_weighted_reward / temp_expected_rewards[next_dpos].total_probability;
                }
           }
        }
    }
    close(env);
    return 0;
}

void frozenlake_model_destroy(const struct DiscreteModelType* model){
    free(model->transition_probabilities);
}

int main() {
    struct EnvironmentConfig config = { MAP_4X4, FALSE };
    struct EnvironmentState state;
    struct StepReturnType step_return;
    enum ActionType action;

    printf("Is slippery: %d\n", config.slippery);

    if(make(config, &state)) {
        fprintf(stderr, "Error creating environment state");
        return 1;
    }
    struct ObservationType obs = reset(&state);
    printf("Position index: %u\n", obs.position_index);
    render_text(state);

    step_return = step(&state, DOWN);
    obs = step_return.observation;
    printf("Position index: %u\n", obs.position_index);
    printf("reward: %f, terminated: %d\n", step_return.reward, step_return.terminated);
    render_text(state);

    step_return = step(&state, DOWN);
    step_return = step(&state, RIGHT);
    step_return = step(&state, DOWN);
    step_return = step(&state, RIGHT);
    step_return = step(&state, RIGHT);
    obs = step_return.observation;
    printf("Position index: %u\n", obs.position_index);
    printf("reward: %f, terminated: %d\n", step_return.reward, step_return.terminated);
    render_text(state);

    printf("Restarting environment, taking random actions...\n");
    unsigned int step_count = 0;
    obs = reset(&state);
    while (1) {
        step_count++;
        action = get_random_action();
        step_return = step(&state, action);
        obs = step_return.observation;
        if (step_return.terminated) {
            printf("Scenario terminated at step %u and position %u with reward %f\n", step_count, obs.position_index, step_return.reward);
            break;
        }
    }
    close(state);

    /* Changing to a slippery map */
    config.slippery = TRUE;
    struct DiscreteModelType model;
    struct TransitionProbabilityType transition_info;
    if (frozenlake_model_create(config, &model)) {
        return 1;
    }
    transition_info = frozenlake_get_transition(&model, 0, LEFT, 0);
    printf("Transition info for going from state 0 to 0 with action LEFT: probability = %f, reward = %f",
            transition_info.probability, transition_info.reward
          );
    frozenlake_model_destroy(&model);
    return 0;
}
