#include "frozenlake.h"
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
#endif

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

int iterative_policy_evaluation(struct DiscreteModelType* model, float (*policy)[ACTION_COUNT], float df, float *value_array) {
    unsigned int num_states = model->num_states;

    /* Set the value function to 0 */
    memset(value_array, 0, sizeof(float) * num_states);
    float theta = 1.0e-6; /* Convergence threshold */
    float local_delta = 0.0;

    float prev_value;
    float transition_value;
    float new_value;

    unsigned int s, s1;
    enum ActionType a;
    struct TransitionProbabilityType trprob;

    int iteration_count = 0;

    while (1) {
        iteration_count += 1;
        /* printf("Iteration %d\n", iteration_count); */

        local_delta = 0.0;
        for (s = 0; s < num_states;  s++) {
            prev_value = value_array[s];
            new_value = 0.0;
            for (a = 0; a < ACTION_COUNT; a++) {
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

int iterative_deterministic_policy_evaluation(
    struct DiscreteModelType* model, enum ActionType* dpolicy, float df, float *value_array
) {
    unsigned int num_states = model->num_states;

    /* We assume the value_array has been initialized prior to calling
     * the method */
    float theta = 1.0e-6; /* Convergence threshold */
    float local_delta = 0.0;

    float prev_value;
    float new_value;

    unsigned int s, s1;
    enum ActionType a;
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
   
static void print_policy(const enum ActionType* dpolicy, unsigned int num_states) {
    unsigned int s;
    enum ActionType a;

    const char* action_names[ACTION_COUNT] = { 
        "LEFT", "DOWN", "RIGHT", "UP"
    };

    printf("Policy: \n");
    for (s = 0; s < num_states; s++) {
        a = dpolicy[s];
        printf("%d: %s\n", s, action_names[a]);
    }
}

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
int policy_iteration_with_init(
    struct DiscreteModelType* model, float df, float *value_array, enum ActionType* dpolicy
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
    enum ActionType a;

    enum ActionType prev_action;
    float action_values[ACTION_COUNT];

    struct TransitionProbabilityType trprob;

    int iteration_count = 0;

    while (1) {
        iteration_count += iterative_deterministic_policy_evaluation(model, dpolicy, df, value_array);
   
        stable = TRUE;

        for (s = 0; s < num_states; s++) {
            /* For state S, record the value of each action in Action_Values. */
            for (a = 0; a < ACTION_COUNT; a++) {
                action_values[a] = 0.0;
                for (s1 = 0; s1 < num_states; s1++) {
                    trprob = frozenlake_get_transition(model, s, a, s1);
                    action_values[a] += trprob.probability * (trprob.reward + df * value_array[s1]);
                }
            }
            /* Get the arg max */
            prev_action = dpolicy[s];
            dpolicy[s] = (enum ActionType) arg_max(action_values, ACTION_COUNT);
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

    enum ActionType* dpolicy = malloc(num_states * sizeof(enum ActionType));
    if (dpolicy == NULL) {
        fprintf(stderr, "policy_iteration: could not allocate deterministic policy array");
        free(value_array);
        return 2;
    }

    /* Initialize policy using uniform distribution over actions */
    for (s = 0; s < num_states; s++) {
        dpolicy[s] = get_random_action();
    }

    print_policy(dpolicy, num_states);  /* TODO: Remove after debugging */
    *num_iterations = policy_iteration_with_init(model, df, value_array, dpolicy);
    return 0;
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
    float stoch_policy[16][ACTION_COUNT];
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


static float max_value(float* action_values) {
    float maxval = -FLT_MAX;
    enum ActionType a;

    for (a = 0; a < ACTION_COUNT; a++) {
        if (action_values[a] > maxval) {
            maxval = action_values[a];
        }
    }
    return maxval;
}
   
static void get_action_values_for_state(struct DiscreteModelType* model, unsigned int s, const float* value_function, float df, float* action_values_out) {
    float new_value;
    unsigned int s1;
    enum ActionType a;
    struct TransitionProbabilityType trprob;

    unsigned int num_states = model->num_states;

    for (a = 0; a < ACTION_COUNT; a++) {
        new_value = 0.0;
        for (s1 = 0; s1 < num_states; s1++) {
            trprob = frozenlake_get_transition(model, s, a, s1);
            new_value += trprob.probability * (trprob.reward + df * value_function[s1]);
        }
        action_values_out[a] = new_value;
    }
}

/* TODO: Perhaps return number of iterations? */
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
    float next_values[ACTION_COUNT];

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
 
int value_iteration(struct DiscreteModelType* model, float df, enum ActionType* dpolicy_out) {
    unsigned int num_states = model->num_states;
    unsigned int s;

    for (s=0; s < num_states; s++) {
        /* Initialize to random policy */
        dpolicy_out[s] = get_random_action();
    }
    
    /* Local values */
    /* TODO: Consider returning value as well */
    float* value_function = malloc(num_states * sizeof(float));
    if (value_function == NULL) {
        fprintf(stderr, "value_iteration: error allocating state value array");
        return 1;
    }

    float action_values[ACTION_COUNT];

    print_policy(dpolicy_out, num_states);  /* TODO: Remove after debugging */
    value_max_action_update(model, df, value_function);

    for (s=0; s < num_states; s++) {
        get_action_values_for_state(model, s, value_function, df, action_values);
        dpolicy_out[s] = (enum ActionType) arg_max(action_values, ACTION_COUNT);
    }
    return 0;
}

int frozenlake_value_iteration_example(struct EnvironmentConfig config, float df) {
    struct DiscreteModelType model;

    if (frozenlake_model_create(config, &model)) {
        return 1;
    }

    unsigned int num_states = model.num_states;

    enum ActionType* optimal_dpolicy_out = malloc(num_states * sizeof(enum ActionType));
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
