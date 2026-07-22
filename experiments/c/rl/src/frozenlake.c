#include "reinforcementlearning/envs/frozenlake.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h> /* memset */
#include <alloca.h>
#include <time.h> /* For setting the RNG */
#include <float.h> /* FLT_MAX */
#include <math.h> /* abs */

#if defined(__STDC__) && !defined(__STDC_VERSION__)
    float fmaxf(float x, float y) {
        return (x > y) ? x : y;
    }
    float fabsf(float x) {
        return (float) fabs((double) x);
    }
    float floorf(float arg) {
        return (float) floor((double) arg);
    }
#endif

float rand_float() {
    return (float)rand() / (float)RAND_MAX;
};

enum FrozenlakeAction frozenlake_get_random_action() {
    return (enum FrozenlakeAction) rand() % FROZENLAKE_ACTION_COUNT;
};

enum MapElement {
    START,
    FROZEN,
    HOLE,
    GOAL
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

struct LocalMap get_map(enum FrozenlakeMapType map_name) {
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

typedef struct TransitionType ActionTransitionType[FROZENLAKE_ACTION_COUNT][FROZENLAKE_ACTION_COUNT];

struct FrozenlakeEnvironment {
    unsigned int rows;
    unsigned int cols;
    const enum MapElement* map;
    ActionTransitionType* p;
    struct PositionType agent_position;
};
  
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


struct PositionType position_inc(unsigned int rows, unsigned int cols, struct PositionType position, enum FrozenlakeAction action) {
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

int can_slip(enum FrozenlakeAction intended_action, enum FrozenlakeAction actual_action) {
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

struct FrozenlakeEnvironment* frozenlake_make(struct FrozenlakeConfig config) {
    struct FrozenlakeEnvironment* env = malloc(sizeof(struct FrozenlakeEnvironment));
    if (env == NULL) {
        fprintf(stderr, "frozenlake_make: Failed to allocate memory for FrozenlakeEnvironment\n");
        return NULL;
    }
    if (frozenlake_init(config, env)) {
        fprintf(stderr, "frozenlake_make: Failed to initialize FrozenlakeEnvironment\n");
        free(env);
        return NULL;
    }
    return env;
}

int frozenlake_init(struct FrozenlakeConfig config, struct FrozenlakeEnvironment* state) {
    unsigned int r, c, i;
    struct PositionType new_position;
    enum MapElement new_me;
    enum FrozenlakeAction ai, aa;
    float temp_probability;
    float reward;
    Boolean terminated;
    struct LocalMap map = get_map(config.map_name);
    ActionTransitionType* p = malloc(map.rows * map.cols * sizeof(ActionTransitionType));
    if (p == NULL) {
        fprintf(stderr, "frozenlake_init: could not allocate transition types");
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

void frozenlake_deinit(struct FrozenlakeEnvironment* env) {
    free(env->p);
}

void frozenlake_close(struct FrozenlakeEnvironment* env) {
    frozenlake_deinit(env);
    free(env);
}

struct FrozenlakeObservation frozenlake_reset(struct FrozenlakeEnvironment* env) {
    struct FrozenlakeObservation result;
    srand(time(NULL));
    env->agent_position = get_start_position(env->rows, env->cols, env->map);
    result.position_index = env->agent_position.row * env->cols + env->agent_position.col;
    return result;
}

void frozenlake_render_text(struct FrozenlakeEnvironment env) {
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
   
struct FrozenlakeStepReturn frozenlake_step(struct FrozenlakeEnvironment* env, enum FrozenlakeAction action) {
    float cumprob = 0.0f;
    float u = rand_float();
    unsigned int i = env->agent_position.row * env->cols + env->agent_position.col;
    enum FrozenlakeAction actual_action;
    struct FrozenlakeObservation obs;
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
    return (struct FrozenlakeStepReturn) { obs, transition.reward, transition.terminated };
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


struct TransitionProbabilityType frozenlake_get_transition(const struct DiscreteModelType* model, unsigned int s, enum FrozenlakeAction action, unsigned int next_s) {
    size_t idx = s * FROZENLAKE_ACTION_COUNT * model->num_states + action * model->num_states + next_s;
    return model->transition_probabilities[idx];
}

static unsigned int to_position_index(unsigned int cols, struct PositionType position) {
    return position.row * cols + position.col;
}

int frozenlake_model_create(struct FrozenlakeConfig config, struct DiscreteModelType* model){
    struct FrozenlakeEnvironment env;
    size_t arr_size;
    unsigned int prev_dpos, next_dpos;
    unsigned int model_idx;
    enum FrozenlakeAction a, a_act;
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

    arr_size = model->num_states * model->num_states * FROZENLAKE_ACTION_COUNT;
    model->transition_probabilities = malloc(arr_size * sizeof(struct TransitionType));
    if (model->transition_probabilities == NULL) {
        fprintf(stderr, "frozenlake_get_model: could not creaate list for transition data");
        return 1;
    }

    /* Assign 0 throughout */
    memset(model->transition_probabilities, 0, arr_size * sizeof(struct TransitionType));
    if (frozenlake_init(config, &env)) {
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
                    model_idx = prev_dpos * FROZENLAKE_ACTION_COUNT * model->num_states + a * model->num_states + next_dpos;
                    model->transition_probabilities[model_idx].probability = temp_expected_rewards[next_dpos].total_probability;
                    model->transition_probabilities[model_idx].reward = temp_expected_rewards[next_dpos].probability_weighted_reward / temp_expected_rewards[next_dpos].total_probability;
                }
           }
        }
    }
    frozenlake_close(&env);
    return 0;
}

void frozenlake_model_destroy(const struct DiscreteModelType* model){
    free(model->transition_probabilities);
}

/* TODO: policy should probably be coerced to have const values */
/* TODO: Complete move to dp.inc.  I've checked and the following can be replaced. */
int iterative_policy_evaluation(struct DiscreteModelType* model, const float (*policy)[FROZENLAKE_ACTION_COUNT], float df, float *value_array) {
    unsigned int num_states = model->num_states;

    /* Set the value function to 0 */
    memset(value_array, 0, sizeof(float) * num_states);
    float theta = 1.0e-6; /* Convergence threshold */
    float local_delta = 0.0;

    float prev_value;
    float transition_value;
    float new_value;

    unsigned int s, s1;
    enum FrozenlakeAction a;
    struct TransitionProbabilityType trprob;

    int iteration_count = 0;

    while (1) {
        iteration_count += 1;
        /* printf("Iteration %d\n", iteration_count); */

        local_delta = 0.0;
        for (s = 0; s < num_states;  s++) {
            prev_value = value_array[s];
            new_value = 0.0;
            for (a = 0; a < FROZENLAKE_ACTION_COUNT; a++) {
                transition_value = 0.0;
                for (s1 = 0; s1 < num_states;  s1++) {
                    trprob = frozenlake_get_transition(model, s, a, s1);
                    transition_value += trprob.probability * (trprob.reward + df * value_array[s1]);
                }
                new_value += policy[s][a] * transition_value;
            }
            value_array[s] = new_value;
            local_delta = fmaxf(local_delta, fabsf(new_value - prev_value));
        }
        if (local_delta < theta) {
            break;
        }
    }
    return iteration_count;
}

/* TODO: Move to dp.inc */
int iterative_deterministic_policy_evaluation(
    struct DiscreteModelType* model, enum FrozenlakeAction* dpolicy, float df, float *value_array
) {
    unsigned int num_states = model->num_states;

    /* We assume the value_array has been initialized prior to calling
     * the method */
    float theta = 1.0e-6; /* Convergence threshold */
    float local_delta = 0.0;

    float prev_value;
    float new_value;

    unsigned int s, s1;
    enum FrozenlakeAction a;
    struct TransitionProbabilityType trprob;

    int iteration_count = 0;
    
    while (1) {
        iteration_count += 1;
        /* printf("Iteration %d\n", iteration_count); */

        local_delta = 0.0;
        for (s = 0; s < num_states;  s++) {
            prev_value = value_array[s];
            a = dpolicy[s];
            new_value = 0.0;
            for (s1 = 0; s1 < num_states;  s1++) {
                trprob = frozenlake_get_transition(model, s, a, s1);
                new_value += trprob.probability * (trprob.reward + df * value_array[s1]);
            }
            value_array[s] = new_value;
            local_delta = fmaxf(local_delta, fabsf(new_value - prev_value));
        }
        if (local_delta < theta) {
            break;
        }
    }
    return iteration_count;
}
 
static void print_policy(const enum FrozenlakeAction* dpolicy, unsigned int num_states) {
    unsigned int s;
    enum FrozenlakeAction a;

    const char* action_names[FROZENLAKE_ACTION_COUNT] = { 
        "LEFT", "DOWN", "RIGHT", "UP"
    };

    printf("Policy: \n");
    for (s = 0; s < num_states; s++) {
        a = dpolicy[s];
        printf("%d: %s\n", s, action_names[a]);
    }
}

/* TODO: Move to dp.inc or to another header */
size_t arg_max(float* values, size_t length) {
    float max_value = -FLT_MAX;
    size_t best_idx = 0;
    size_t i;

    for (i = 0; i < length; i++) {
        if (values[i] > max_value) {
            max_value = values[i];
            best_idx = i;
        }
    }
    return best_idx;
}

/* TODO: Consider changing the name to "_given_init" */
/* TODO: Move to dp.inc */
int policy_iteration_with_init(
    struct DiscreteModelType* model, float df, float *value_array, enum FrozenlakeAction* dpolicy
) {
    /* Init_Value_Func : Value_Function_Type;
     * Init_Policy : Deterministic_Policy_Type
      type Action_Value_Array_Type is array (Action_Type) of Float;

      Value_Function : Value_Function_Type := Init_Value_Func;
      Policy : Deterministic_Policy_Type := Init_Policy;
     */
    unsigned int num_states = model->num_states;

    Boolean stable;

    unsigned int s, s1;
    enum FrozenlakeAction a;

    enum FrozenlakeAction prev_action;
    float action_values[FROZENLAKE_ACTION_COUNT];

    struct TransitionProbabilityType trprob;

    int iteration_count = 0;

    while (1) {
        iteration_count += iterative_deterministic_policy_evaluation(model, dpolicy, df, value_array);
   
        stable = TRUE;

        for (s = 0; s < num_states; s++) {
            /* For state S, record the value of each action in Action_Values. */
            for (a = 0; a < FROZENLAKE_ACTION_COUNT; a++) {
                action_values[a] = 0.0;
                for (s1 = 0; s1 < num_states; s1++) {
                    trprob = frozenlake_get_transition(model, s, a, s1);
                    action_values[a] += trprob.probability * (trprob.reward + df * value_array[s1]);
                }
            }
            /* Get the arg max */
            prev_action = dpolicy[s];
            dpolicy[s] = (enum FrozenlakeAction) arg_max(action_values, FROZENLAKE_ACTION_COUNT);
            if (dpolicy[s] != prev_action) {
               stable = FALSE;
            }
        }
        if (stable) {
            break;
        }
    }
    return iteration_count;
}

/* TODO: We need to pass value_array and dpolicy */
/* TODO: Move to dp.inc */
int policy_iteration(
    struct DiscreteModelType* model, float df, int* num_iterations
) {
    /* Reset seed */
    srand(time(NULL));
    
    unsigned int num_states = model->num_states;

    unsigned int s;

    float* value_array = malloc(num_states * sizeof(float));
    if (value_array == NULL) {
        fprintf(stderr, "policy_iteration: could not allocate state value array");
        return 1;
    }
    /* Initialize value function to 0 */
    memset(value_array, 0, sizeof(float) * num_states);

    enum FrozenlakeAction* dpolicy = malloc(num_states * sizeof(enum FrozenlakeAction));
    if (dpolicy == NULL) {
        fprintf(stderr, "policy_iteration: could not allocate deterministic policy array");
        free(value_array);
        return 2;
    }

    /* Initialize policy using uniform distribution over actions */
    for (s = 0; s < num_states; s++) {
        dpolicy[s] = frozenlake_get_random_action();
    }

    print_policy(dpolicy, num_states);  /* TODO: Remove after debugging */
    *num_iterations = policy_iteration_with_init(model, df, value_array, dpolicy);
    return 0;
}


static float max_value(float* action_values) {
    float maxval = -FLT_MAX;
    enum FrozenlakeAction a;

    for (a = 0; a < FROZENLAKE_ACTION_COUNT; a++) {
        if (action_values[a] > maxval) {
            maxval = action_values[a];
        }
    }
    return maxval;
}
   
/* TODO: Move to dp.inc */
static void get_action_values_for_state(struct DiscreteModelType* model, unsigned int s, const float* value_function, float df, float* action_values_out) {
    float new_value;
    unsigned int s1;
    enum FrozenlakeAction a;
    struct TransitionProbabilityType trprob;

    unsigned int num_states = model->num_states;

    for (a = 0; a < FROZENLAKE_ACTION_COUNT; a++) {
        new_value = 0.0;
        for (s1 = 0; s1 < num_states; s1++) {
            trprob = frozenlake_get_transition(model, s, a, s1);
            new_value += trprob.probability * (trprob.reward + df * value_function[s1]);
        }
        action_values_out[a] = new_value;
    }
}

/* TODO: Perhaps return number of iterations? */
/* TODO: Move to dp.inc */
static void value_max_action_update(struct DiscreteModelType* model, float df, float* value_function_out) {
    /* Value_Function: Value_Function_Type := (others => 0.0); */
    unsigned int num_states = model->num_states;

    /* Overwrite value function with 0s */
    memset(value_function_out, 0, num_states * sizeof(float));

    unsigned int s;

    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    float prev_value;
    float new_value;
    float next_values[FROZENLAKE_ACTION_COUNT];

    int iteration_count = 0;

    while (1) {
        iteration_count++;
        printf("Iteration %d", iteration_count); /* TODO: Remove */

        local_delta = 0.0;
        for (s = 0; s < num_states; s++) {
            prev_value = value_function_out[s];
            get_action_values_for_state(model, s, value_function_out, df, next_values);
            new_value = max_value(next_values);
            value_function_out[s] = new_value;
            local_delta = fmaxf(local_delta, fabsf(new_value - prev_value));
        }
        if (local_delta < theta) {
            break;
        }
    }
}
 
/* TODO: Move to dp.inc */
int value_iteration(struct DiscreteModelType* model, float df, enum FrozenlakeAction* dpolicy_out) {
    unsigned int num_states = model->num_states;
    unsigned int s;

    for (s=0; s < num_states; s++) {
        /* Initialize to random policy */
        dpolicy_out[s] = frozenlake_get_random_action();
    }
    
    /* Local values */
    /* TODO: Consider returning value as well */
    float* value_function = malloc(num_states * sizeof(float));
    if (value_function == NULL) {
        fprintf(stderr, "value_iteration: error allocating state value array");
        return 1;
    }

    float action_values[FROZENLAKE_ACTION_COUNT];

    print_policy(dpolicy_out, num_states);  /* TODO: Remove after debugging */
    value_max_action_update(model, df, value_function);

    for (s=0; s < num_states; s++) {
        get_action_values_for_state(model, s, value_function, df, action_values);
        dpolicy_out[s] = (enum FrozenlakeAction) arg_max(action_values, FROZENLAKE_ACTION_COUNT);
    }
    return 0;
}

/* TODO: Move to dp.inc */
int frozenlake_value_iteration_example(struct FrozenlakeConfig config, float df) {
    struct DiscreteModelType model;

    if (frozenlake_model_create(config, &model)) {
        return 1;
    }

    unsigned int num_states = model.num_states;

    enum FrozenlakeAction* optimal_dpolicy_out = malloc(num_states * sizeof(enum FrozenlakeAction));
    if (optimal_dpolicy_out == NULL) {
        fprintf(stderr, "frozenlake_value_iteration_example: could not allocate deterministic policy");
        frozenlake_model_destroy(&model);
        return 2;
    }

    /* Reset the seed */
    srand(time(NULL));

    if (value_iteration(&model, df, optimal_dpolicy_out)) {
        fprintf(stderr, "frozenlake_value_iteration_example: error running value_iteration");
        free(optimal_dpolicy_out);
        frozenlake_model_destroy(&model);
        return 3;
    }

    print_policy(optimal_dpolicy_out, num_states);

    free(optimal_dpolicy_out);
    frozenlake_model_destroy(&model);
    return 0;
}

int frozenlake_example_main() {
    struct FrozenlakeConfig config = { MAP_4X4, FALSE };
    struct FrozenlakeEnvironment state;
    struct FrozenlakeStepReturn step_return;
    enum FrozenlakeAction action;

    printf("Is slippery: %d\n", config.slippery);

    if(frozenlake_init(config, &state)) {
        fprintf(stderr, "Error creating environment state");
        return 1;
    }
    struct FrozenlakeObservation obs = frozenlake_reset(&state);
    printf("Position index: %u\n", obs.position_index);
    frozenlake_render_text(state);

    step_return = frozenlake_step(&state, DOWN);
    obs = step_return.observation;
    printf("Position index: %u\n", obs.position_index);
    printf("reward: %f, terminated: %d\n", step_return.reward, step_return.terminated);
    frozenlake_render_text(state);

    step_return = frozenlake_step(&state, DOWN);
    step_return = frozenlake_step(&state, RIGHT);
    step_return = frozenlake_step(&state, DOWN);
    step_return = frozenlake_step(&state, RIGHT);
    step_return = frozenlake_step(&state, RIGHT);
    obs = step_return.observation;
    printf("Position index: %u\n", obs.position_index);
    printf("reward: %f, terminated: %d\n", step_return.reward, step_return.terminated);
    frozenlake_render_text(state);

    printf("Restarting environment, taking random actions...\n");
    unsigned int step_count = 0;
    obs = frozenlake_reset(&state);
    while (1) {
        step_count++;
        action = frozenlake_get_random_action();
        step_return = frozenlake_step(&state, action);
        obs = step_return.observation;
        if (step_return.terminated) {
            printf("Scenario terminated at step %u and position %u with reward %f\n", step_count, obs.position_index, step_return.reward);
            break;
        }
    }
    frozenlake_close(&state);

    /* Changing to a slippery map */
    config.slippery = FALSE;
    struct DiscreteModelType model;
    struct TransitionProbabilityType transition_info;
    if (frozenlake_model_create(config, &model)) {
        return 1;
    }
    transition_info = frozenlake_get_transition(&model, 0, LEFT, 0);
    printf("Transition info for going from state 0 to 0 with action LEFT: probability = %f, reward = %f",
            transition_info.probability, transition_info.reward
          );

    unsigned int s;
    int iterations;
    float stoch_policy[16][FROZENLAKE_ACTION_COUNT];
    float value_array[16];
    for (s = 0; s < 16; s++) {
        stoch_policy[s][LEFT] = 0.25;
        stoch_policy[s][DOWN] = 0.25;
        stoch_policy[s][RIGHT] = 0.25;
        stoch_policy[s][UP] = 0.25;
    }
    /*
    stoch_policy[14][RIGHT] = 1.0;
    stoch_policy[14][UP] = 0.0;
    stoch_policy[14][DOWN] = 0.0;
    stoch_policy[14][LEFT] = 0.0;
    */
    iterations = iterative_policy_evaluation(&model, stoch_policy, 0.9, value_array);
    printf("Estimated value function using %d iterations\n", iterations);
    for (s = 0; s < 16; s++) {
        printf("Value function for state %u: %f\n", s, value_array[s]);
    }

    frozenlake_model_destroy(&model);

    printf("Value Iteration: \n");
    if (frozenlake_value_iteration_example(config, 0.9)) {
        return 1;
    }
    return 0;
}

#define ENVIRONMENT_PREFIX frozenlake
#define DISCRETE_MODEL_TYPE struct DiscreteModelType
#define ACTION_TYPE enum FrozenlakeAction
#define ENVIRONMENT_ACTION_COUNT FROZENLAKE_ACTION_COUNT 
#define GET_TRANSITION_METHOD frozenlake_get_transition
#include <reinforcementlearning/algorithms/dp.inc>
 
struct TDConfig {
    float alpha;
    float gamma;
};

/* TODO: Move to td.inc */
int td_iterative_policy_evaluation(struct FrozenlakeConfig config, struct TDConfig td_config, enum FrozenlakeAction* policy, float* value_function_out) {
    /* TODO: return error if make fails */
    struct FrozenlakeEnvironment* env = frozenlake_make(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    struct FrozenlakeObservation obs;
    unsigned int s, s1;
    struct FrozenlakeStepReturn step_result;
    enum FrozenlakeAction action;
    Boolean terminated = FALSE;

    unsigned int num_states = env->rows * env->cols;

    /* Reset value function to 0 */
    memset(value_function_out, 0, num_states * sizeof(float));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float* prev_value_function = malloc(num_states * sizeof(float));

    int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    unsigned int max_steps = 50;

    while (1) {
        episode_count++;
        printf("Episode %u", episode_count);

        memcpy(prev_value_function, value_function_out, num_states * sizeof(float));
        local_delta = 0.0;

        obs = frozenlake_reset(env); /* Seed_Reset */
        s = obs.position_index;  /* TODO: To move to a C template we'll need a to_discrete_state method */
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            action = policy[s];
            step_result = frozenlake_step(env, action);
            obs = step_result.observation;
            s1 = obs.position_index;
            printf("Action %d takes state %u to state %u", (int) action, s, s1);
            value_function_out[s] += td_config.alpha * (step_result.reward + td_config.gamma * value_function_out[s1] - value_function_out[s]);
            /* Update to next state */
            s = s1;
            step_index++;
            terminated = step_result.terminated || (step_index >= max_steps);
        }

        /* NOTE: We exit when the max value function change falls below a threshold.
                 This differs from the textbook algorithm. */
        for(s = 0; s < num_states; s++) {
            local_delta = fmaxf(local_delta, fabsf(value_function_out[s] - prev_value_function[s]));
        }
        if (local_delta < theta) {
            break;
        }
    }
    return episode_count;
}
   
struct SARSAConfig {
    float alpha;
    float gamma;
    float initial_epsilon;
    float minimum_epsilon;
    unsigned int episodes_to_minimum_epsilon;
};

/* TODO: Move to td.inc */
static enum FrozenlakeAction best_action_for_state(float (*q)[FROZENLAKE_ACTION_COUNT], unsigned int s) {
    return (enum FrozenlakeAction) arg_max(q[s], FROZENLAKE_ACTION_COUNT);
}

/* TODO: Move to td.inc */
static enum FrozenlakeAction choose_action_epsilon_greedy(float epsilon, float (*q)[FROZENLAKE_ACTION_COUNT], unsigned int s) {
    float u = rand_float();
    if (u < epsilon) {
        float a_rand;
        a_rand = rand_float() * ((float) FROZENLAKE_ACTION_COUNT);
        return (enum FrozenlakeAction) ((int) floorf(a_rand));
    } else {
        return best_action_for_state(q, s);
    }
}

/* TODO: The epsilon trajectory in the following needs improvement */
/* TODO: Move to td.inc */
static float update_epsilon(struct SARSAConfig sarsa_config, unsigned int episode) {
    float init_eps = sarsa_config.initial_epsilon;
    float min_eps = sarsa_config.minimum_epsilon;
    float episode_to_min_eps = (float) sarsa_config.episodes_to_minimum_epsilon;
    float k = episode_to_min_eps * min_eps / init_eps;

    return fmaxf(min_eps, k * init_eps / ((float) episode));
}

/* TODO: Move to td.inc */
int sarsa_on_policy(struct FrozenlakeConfig config, struct SARSAConfig sarsa_config, float (*q)[FROZENLAKE_ACTION_COUNT]) {
    /* TODO: return error if make fails */
    struct FrozenlakeEnvironment* env = frozenlake_make(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    struct FrozenlakeObservation obs;
    unsigned int s, s1;
    struct FrozenlakeStepReturn step_result;
    enum FrozenlakeAction a, a1;
    Boolean terminated = FALSE;
    /* The local delta indices */
    unsigned int s0;
    enum FrozenlakeAction a0;

    float epsilon;

    unsigned int num_states = env->rows * env->cols;

    /* Reset value function to 0 */
    memset(q, 0, num_states * sizeof(*q));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float (*prev_action_value_func)[FROZENLAKE_ACTION_COUNT] = malloc(num_states * sizeof(*q));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    unsigned int max_steps = 100;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) { 
        episode_count++;
        /* Put_Line("Episode " & Episode_Count'Image); */

        epsilon = update_epsilon(sarsa_config, (int) episode_count);
        printf("Episode %u, epsilon: %f", episode_count, epsilon);

        memcpy(prev_action_value_func, q, num_states * sizeof(*q));

        obs = frozenlake_reset(env); /* Seed_Reset */
        s = obs.position_index;
        a = choose_action_epsilon_greedy(epsilon, q, s);
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            step_result = frozenlake_step(env, a);
            obs = step_result.observation;
            s1 = obs.position_index;
            a1 = choose_action_epsilon_greedy(epsilon, q, s1);
            printf("Action %d takes state %u to state %u and action %d in on-policy SARSA", a, s, s1, a1);
            q[s][a] += sarsa_config.alpha * (step_result.reward + sarsa_config.gamma * q[s1][a1] - q[s][a]);
            /* Update to next state */
            s = s1;
            a = a1;
            step_index++;
            terminated = step_result.terminated || (step_index >= max_steps);
        }

        /* NOTE: We exit when the max value function change falls below a threshold
         *       after episode Episodes_To_Minimum_Epsilon is reached.
         *       This differs from the textbook algorithm. */
        local_delta = 0.0;
        /* TODO: Decide whether to reintroduce something like the following condition before exiting */
        for (s0 = 0; s0 < num_states; s0++) {
            for (a0 = 0; a0 < FROZENLAKE_ACTION_COUNT; a0++) {
                local_delta = fmaxf(local_delta, fabsf(q[s0][a0] - prev_action_value_func[s0][a0]));
            }
        }
        if ((episode_count >= sarsa_config.episodes_to_minimum_epsilon) && (local_delta < theta)) {
            break;
        }
    }
    return 0;
}

/* TODO: Move to td.inc */
int sarsa_off_policy(struct FrozenlakeConfig config, struct SARSAConfig sarsa_config, float (*q)[FROZENLAKE_ACTION_COUNT]) {
    /* TODO: return error if make fails */
    struct FrozenlakeEnvironment* env = frozenlake_make(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    struct FrozenlakeObservation obs;
    unsigned int s, s1;
    struct FrozenlakeStepReturn step_result;
    enum FrozenlakeAction a, a1;
    Boolean terminated = FALSE;
    /* The local delta indices */
    unsigned int s0;
    enum FrozenlakeAction a0;

    float epsilon;

    unsigned int num_states = env->rows * env->cols;

    /* Reset value function to 0 */
    memset(q, 0, num_states * sizeof(*q));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float (*prev_action_value_func)[FROZENLAKE_ACTION_COUNT] = malloc(num_states * sizeof(*q));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    unsigned int max_steps = 100;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) { 
        episode_count++;
        /* Put_Line("Episode " & Episode_Count'Image); */

        epsilon = update_epsilon(sarsa_config, (int) episode_count);
        printf("Episode %u, epsilon: %f", episode_count, epsilon);

        memcpy(prev_action_value_func, q, num_states * sizeof(*q));

        obs = frozenlake_reset(env); /* Seed_Reset */
        s = obs.position_index;
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            /* TODO: I think there's an error here.  A needs to be moved up (out of the loop) */
            a = choose_action_epsilon_greedy(epsilon, q, s);
            step_result = frozenlake_step(env, a);
            obs = step_result.observation;
            s1 = obs.position_index;
            a1 = best_action_for_state(q, s1);  /* TODO: Just get best value for state */
            printf("Action %d takes state %u to state %u in off-policy SARSA", a, s, s1);
            q[s][a] += sarsa_config.alpha * (step_result.reward + sarsa_config.gamma * q[s1][a1] - q[s][a]);
            /* Update to next state */
            s = s1;
            step_index++;
            terminated = step_result.terminated || (step_index >= max_steps);
        }

        /* NOTE: We exit when the max value function change falls below a threshold
         *       after episode Episodes_To_Minimum_Epsilon is reached.
         *       This differs from the textbook algorithm. */
        local_delta = 0.0;
        /* TODO: Decide whether to reintroduce something like the following condition before exiting */
        for (s0 = 0; s0 < num_states; s0++) {
            for (a0 = 0; a0 < FROZENLAKE_ACTION_COUNT; a0++) {
                local_delta = fmaxf(local_delta, fabsf(q[s0][a0] - prev_action_value_func[s0][a0]));
            }
        }
        if ((episode_count >= sarsa_config.episodes_to_minimum_epsilon) && (local_delta < theta)) {
            break;
        }
    }
    return 0;
}

struct ETConfig {
    float alpha;
    float gamma;
    float lambda;
};

/* TODO: Move to et.inc */
int et_iterative_policy_evaluation(struct FrozenlakeConfig config, struct ETConfig et_config, float* dpolicy, float* svalue_func_out) {
    /* TODO: return error if make fails */
    struct FrozenlakeEnvironment* env = frozenlake_make(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    unsigned int num_states = env->rows * env->cols;

    float* e = malloc(num_states * sizeof(float));
    memset(e, 0, num_states * sizeof(float));

    float td_error;

    struct FrozenlakeObservation obs;
    unsigned int s, s1, s2;
    struct FrozenlakeStepReturn step_result;
    enum FrozenlakeAction a;
    Boolean terminated = FALSE;
    /* The local delta indices
    unsigned int s0;
    enum FrozenlakeAction a0;
    */

    /* Reset value function to 0 */
    memset(svalue_func_out, 0, num_states * sizeof(*svalue_func_out));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float* prev_svalue_func = malloc(num_states * sizeof(float));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    unsigned int max_steps = 50;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) {
        episode_count++;
        printf("Episode %u", episode_count);

        memcpy(prev_svalue_func, svalue_func_out, num_states * sizeof(float));

        /* TODO: From the text of the book and the book errata, E needs to be set to 0
                 at the beginning of an episode. */

        obs = frozenlake_reset(env); /* Seed_Reset */
        s = obs.position_index;
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            a = dpolicy[s];
            step_result = frozenlake_step(env, a);
            obs = step_result.observation;
            s1 = obs.position_index;
            printf("Action %d takes state %u to state %u", a, s, s1);
            td_error = step_result.reward + et_config.gamma * svalue_func_out[s1] - svalue_func_out[s];
            e[s] += 1.0;
            for (s2 = 0; s2 < num_states; s2++) {
                svalue_func_out[s2] += et_config.alpha * td_error * e[s2];
                e[s2] = et_config.gamma * et_config.lambda * e[s2];
            }
            /* Update to next state */
            s = s1;
            step_index++;
            terminated = (step_result.terminated || (step_index >= max_steps));
       }
        
        local_delta = 0.0;
        /* NOTE: We exit when the max value function change falls below a threshold.
                 This differs from the textbook algorithm. */
        for (s = 0; s < num_states; s++) {
            local_delta = fmaxf(local_delta, fabsf(svalue_func_out[s] - prev_svalue_func[s]));
        }
        if (local_delta < theta) {
            break;
        }
    }
    return 0;
}

/* TODO: Note that there is a type with the same name in the TD example */
struct ETSARSAConfig {
    struct SARSAConfig td_config;
    float lambda;
};

/* TODO: Move to et.inc */
int et_sarsa_on_policy(struct FrozenlakeConfig config, struct ETSARSAConfig sarsa_config, float (*q)[FROZENLAKE_ACTION_COUNT]) {
    /* TODO: return error if make fails */
    struct FrozenlakeEnvironment* env = frozenlake_make(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    unsigned int num_states = env->rows * env->cols;

    /* TODO: return error if malloc fails */
    float (*prev_esavalue_func)[FROZENLAKE_ACTION_COUNT] = malloc(num_states * sizeof(*q));
    memset(prev_esavalue_func, 0, num_states * sizeof(*prev_esavalue_func));

    float (*esa)[FROZENLAKE_ACTION_COUNT] = malloc(num_states * sizeof(*q));
    memset(esa, 0, num_states * sizeof(*esa));
    float td_error;

    struct FrozenlakeObservation obs;
    unsigned int s, s1, s2;
    struct FrozenlakeStepReturn step_result;
    enum FrozenlakeAction a, a1, a2;
    Boolean terminated = FALSE;
    /* The local delta indices */
    unsigned int s0;
    enum FrozenlakeAction a0;

    float epsilon;

    /* Reset value function to 0 */
    memset(q, 0, num_states * sizeof(*q));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float (*prev_q)[FROZENLAKE_ACTION_COUNT] = malloc(num_states * sizeof(*q));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;

    /* NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm. */
    unsigned int max_steps = 100;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) { 
        episode_count++;
        /* Put_Line("Episode " & Episode_Count'Image); */

        epsilon = update_epsilon(sarsa_config.td_config, (int) episode_count);
        printf("Episode %u, epsilon: %f", episode_count, epsilon);

        memcpy(prev_q, q, num_states * sizeof(*q));

        /* TODO: From the text of the book and the book errata, E needs to be set to 0
                 at the beginning of an episode. */
        obs = frozenlake_reset(env); /* Seed_Reset */
        s = obs.position_index;
        a = choose_action_epsilon_greedy(epsilon, q, s);
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            step_result = frozenlake_step(env, a);
            obs = step_result.observation;
            s1 = obs.position_index;
            a1 = choose_action_epsilon_greedy(epsilon, q, s1);
            printf("Action %d takes state %u to state %u and action %d in on-policy SARSA", a, s, s1, a1);
            td_error = step_result.reward + sarsa_config.td_config.gamma * q[s1][a1] - q[s][a];
            esa[s][a] += 1.0;
            for(s2 = 0; s2 < num_states; s2++) {
                for(a2 = 0; a2 < FROZENLAKE_ACTION_COUNT; a2++) {
                    q[s2][a2] += sarsa_config.td_config.alpha * td_error * esa[s2][a2];
                    esa[s2][a2] = sarsa_config.td_config.gamma * sarsa_config.lambda * esa[s2][a2];
                }
            }
            /* Update to next state */
            s = s1;
            a = a1;
            step_index++;
            terminated = (step_result.terminated || (step_index >= max_steps));
        }

        /* NOTE: We exit when the max value function change falls below a threshold
         *       after episode Episodes_To_Minimum_Epsilon is reached.
         *       This differs from the textbook algorithm. */
        local_delta = 0.0;
        /* TODO: Decide whether to reintroduce something like the following condition before exiting */
        for (s0 = 0; s0 < num_states; s0++) {
            for (a0 = 0; a0 < FROZENLAKE_ACTION_COUNT; a0++) {
                local_delta = fmaxf(local_delta, fabsf(q[s0][a0] - prev_q[s0][a0]));
            }
        }
        if ((episode_count >= sarsa_config.td_config.episodes_to_minimum_epsilon) && (local_delta < theta)) {
            break;
        }
    }
    return 0;
}

/* TODO: Move to et.inc */
int watkins_q_iteration(struct FrozenlakeConfig config, struct ETSARSAConfig sarsa_config, float (*q)[FROZENLAKE_ACTION_COUNT]) {
    /* TODO: return error if make fails */
    struct FrozenlakeEnvironment* env = frozenlake_make(config);
    /* Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default); */

    unsigned int num_states = env->rows * env->cols;

    /* TODO: return error if malloc fails */
    float (*prev_savalue_func)[FROZENLAKE_ACTION_COUNT] = malloc(num_states * sizeof(*q));
    memset(prev_savalue_func, 0, num_states * sizeof(*prev_savalue_func));

    float (*esa)[FROZENLAKE_ACTION_COUNT] = malloc(num_states * sizeof(*q));
    memset(esa, 0, num_states * sizeof(*esa));
    float td_error;

    struct FrozenlakeObservation obs;
    unsigned int s, s1, s2;
    struct FrozenlakeStepReturn step_result;
    enum FrozenlakeAction a, a1, a2, a_best;
    Boolean terminated = FALSE;
    /* The local delta indices */
    unsigned int s0;
    enum FrozenlakeAction a0;

    float epsilon;

    /* Reset value function to 0 */
    memset(q, 0, num_states * sizeof(*q));
    float theta = 1.0e-6;  /* Convergence threshold */
    float local_delta = 0.0;

    /* TODO: return error if malloc fails */
    float (*prev_q)[FROZENLAKE_ACTION_COUNT] = malloc(num_states * sizeof(*q));

    unsigned int episode_count = 0;
    unsigned int step_index = 0;
    unsigned int max_steps = 100;

    /* TODO: Reset RNG, not needed once we reset below */
    srand(time(NULL));

    while (1) {
        episode_count++;
        /* Put_Line("Episode " & Episode_Count'Image); */

        epsilon = update_epsilon(sarsa_config.td_config, (int) episode_count);
        printf("Episode %u, epsilon: %f", episode_count, epsilon);

        memcpy(prev_q, q, num_states * sizeof(*q));

        /* TODO: From the text of the book and the book errata, E needs to be set to 0
                 at the beginning of an episode. */
        obs = frozenlake_reset(env); /* Seed_Reset */
        s = obs.position_index;
        a = choose_action_epsilon_greedy(epsilon, q, s);
        step_index = 0;
        terminated = FALSE;

        while (!terminated) {
            step_result = frozenlake_step(env, a);
            obs = step_result.observation;
            s1 = obs.position_index;
            a1 = choose_action_epsilon_greedy(epsilon, q, s1);
            a_best = best_action_for_state(q, s1);  /* TODO: Just get best value for state */
            printf("Action %d takes state %u to state %u in Watkin's Q algorithm (off-policy SARSA usingn Eligibility Trace)", a, s, s1);

            td_error = step_result.reward + sarsa_config.td_config.gamma * q[s1][a_best] - q[s][a];
            esa[s][a] += 1.0;
            for(s2 = 0; s2 < num_states; s2++) {
                for(a2 = 0; a2 < FROZENLAKE_ACTION_COUNT; a2++) {
                    q[s2][a2] += sarsa_config.td_config.alpha * td_error * esa[s2][a2];
                    /* IN PROGRESS: Is the following correct?  Based on the text, I think so. */
                    esa[s2][a2] = sarsa_config.td_config.gamma * sarsa_config.lambda * esa[s2][a2];
                    if (a1 == a_best) {
                        esa[s2][a2] = sarsa_config.td_config.gamma * sarsa_config.lambda * esa[s2][a2];
                    } else {
                        esa[s2][a2] = 0.0;
                    }
                }
            }
            /* Update to next state */
            s = s1;
            a = a1;
            step_index++;
            terminated = (step_result.terminated || (step_index >= max_steps));
        }

        /* NOTE: We exit when the max value function change falls below a threshold
         *       after episode Episodes_To_Minimum_Epsilon is reached.
         *       This differs from the textbook algorithm. */
        local_delta = 0.0;
        /* TODO: Decide whether to reintroduce something like the following condition before exiting */
        for (s0 = 0; s0 < num_states; s0++) {
            for (a0 = 0; a0 < FROZENLAKE_ACTION_COUNT; a0++) {
                local_delta = fmaxf(local_delta, fabsf(q[s0][a0] - prev_q[s0][a0]));
            }
        }
        if ((episode_count >= sarsa_config.td_config.episodes_to_minimum_epsilon) && (local_delta < theta)) {
            break;
        }
    }
    return 0;
}

unsigned int frozenlake_get_num_states(struct FrozenlakeConfig config) {
    switch (config.map_name) {
        case MAP_4X4:
            return 16;
        case MAP_8X8: 
            return 64;
        default:
            return 16;
    };
}

unsigned int frozenlake_to_discrete_observation(struct FrozenlakeObservation obs) {
    return obs.position_index;
}

#define ENVIRONMENT_PREFIX frozenlake
#define CONFIG_TYPE struct FrozenlakeConfig
#define ENVIRONMENT_TYPE struct FrozenlakeEnvironment
#define OBSERVATION_TYPE struct FrozenlakeObservation
#define STEP_RETURN_TYPE struct FrozenlakeStepReturn
#define ACTION_TYPE enum FrozenlakeAction
#define ENVIRONMENT_ACTION_COUNT FROZENLAKE_ACTION_COUNT
#define GET_NUM_STATES_METHOD frozenlake_get_num_states
#define MAKE_METHOD frozenlake_make
#define RESET_METHOD frozenlake_reset
#define STEP_METHOD frozenlake_step
#define CLOSE_METHOD frozenlake_close
#define GET_RANDOM_ACTION_ES_METHOD frozenlake_get_random_action
#define TO_DISCRETE_OBSERVATION_METHOD frozenlake_to_discrete_observation
#include <reinforcementlearning/algorithms/mc.inc>

