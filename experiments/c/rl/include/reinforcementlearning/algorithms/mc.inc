#ifndef ENVIRONMENT_PREFIX
#error "ENVIRONMENT_PREFIX must be defined in mc.inc"
#endif

#ifndef CONFIG_TYPE
#error "CONFIG_TYPE is not defined in mc.inc"
#endif

#ifndef ENVIRONMENT_TYPE
#error "ENVIRONMENT_TYPE is not defined in mc.inc"
#endif

#ifndef ACTION_TYPE
#error "ACTION_TYPE is not defined in mc.inc"
#endif

#ifndef OBSERVATION_TYPE
#error "OBSERVATION_TYPE is not defined in mc.inc"
#endif

#ifndef STEP_RETURN_TYPE
#error "STEP_RETURN_TYPE is not defined in mc.inc"
#endif

#ifndef ENVIRONMENT_ACTION_COUNT
#error "ENVIRONMENT_ACTION_COUNT is not defined in mc.inc"
#endif

#ifndef GET_NUM_STATES_METHOD
#error "GET_NUM_STATES_METHOD is not defined in mc.inc"
#endif

#ifndef MAKE_METHOD
#error "MAKE_METHOD is not defined in mc.inc"
#endif

#ifndef RESET_METHOD
#error "RESET_METHOD is not defined in mc.inc"
#endif

#ifndef STEP_METHOD
#error "STEP_METHOD is not defined in mc.inc"
#endif

#ifndef TO_DISCRETE_OBSERVATION_METHOD
#error "TO_DISCRETE_OBSERVATION is not defined in mc.inc"
#endif

#include <reinforcementlearning/algorithms/mc_config.h>

#define MC_FUNC_NAME(name) MC_CONCAT1(ENVIRONMENT_PREFIX, name)

#define MC_CONCAT1(prefix, name) MC_CONCAT2(prefix, name)
#define MC_CONCAT2(prefix, name) prefix##_##name

#ifdef MC_DECLS_ONLY

int MC_FUNC_NAME(mc_policy_evaluation) (CONFIG_TYPE config, ACTION_TYPE* dpolicy, struct MCConfig mc_config, float* svalue_func_out);

#else

struct RewardTracker {
    float total_reward;
    int count;
};

/*
type State_Reward_Tracker_Type is array(Discrete_Observation_Type) of Reward_Tracker_Type;
*/

struct SARType {
    unsigned int state; /* Discrete_Observation_Type */
    ACTION_TYPE action;
    float reward;
};

/*
type SAR_Array_Type is array (1 .. Max_Episode_Steps) of SAR_Type;
SAR_Array : SAR_Array_Type;
*/

/*
   type Policy_Type is array (Discrete_Observation_Type) of Action_Type;
   type Value_Function_Type is array(Discrete_Observation_Type) of Float;

   type State_Action_Value_Type is array (Discrete_Observation_Type, Action_Type) of Float;
   type Evaluation_Results_Type is record
      Q: State_Action_Value_Type;
      Policy : Policy_Type;
   end record;
      
   type MC_Epsilon_Soft_Config_Type is record
      Num_Episodes : Integer;
      -- Visit_Type : MC_Visit_Type;
      Discount_Factor : Float := 1.0;
      Epsilon : Float;
   end record;

   function MC_Policy_Evaluation(Env_Config : Config_Type; Policy : Policy_Type; MC_Config : MC_Config_Type) return Value_Function_Type;
   function MC_Exploring_Starts_Evaluation(Env_Config : Config_Type; MC_Config : MC_Config_Type) return Evaluation_Results_Type;
   function MC_Epsilon_Soft_Evaluation(Env_Config : Config_Type; MC_Config : MC_Epsilon_Soft_Config_Type) return Evaluation_Results_Type;
private
   type Mixed_Policy_Type is array (Discrete_Observation_Type, Action_Type) of Float;
end RL.Algorithms.MC;
*/

/*
      type State_Reward_Tracker_Type is array(Discrete_Observation_Type) of Reward_Tracker_Type;
*/

static int every_visit_reward_update(struct MCConfig mc_config, struct RewardTracker* v, /*unsigned int num_states, */ const struct SARType* sar_array_input, unsigned int step_count) {
    /* NOTE: Length of sar_array is max_episode_steps */
    if (step_count == 0) return 0;

    struct SARType* sar_array = malloc(mc_config.max_episode_steps * sizeof(struct SARType));
    if (sar_array == NULL) {
        fprintf(stderr, "every_visit_reward_update: unable to allocate array");
        return 1;
    }
    memcpy(sar_array, sar_array_input, mc_config.max_episode_steps * sizeof(struct SARType));

    unsigned int i = step_count;
    while (i > 0) {
        i--;
        if (i < (step_count - 1)) {
            sar_array[i].reward += mc_config.discount_factor * sar_array[i+1].reward;
        }
        /* Update the state reward tracker */
        v[sar_array[i].state].total_reward += sar_array[i].reward;
        v[sar_array[i].state].count        += 1;
    }
    free(sar_array);
    return 0;
}

static int first_visit_reward_update(struct MCConfig mc_config, struct RewardTracker* v, unsigned int num_states, const struct SARType* sar_array_input, unsigned int step_count) {
    if (step_count == 0) return 0;
    /* A local copy to avoid specifying "in out" for the array parameter */
    struct SARType* sar_array = malloc(mc_config.max_episode_steps * sizeof(struct SARType));
    if (sar_array == NULL) {
        fprintf(stderr, "first_visit_reward_update: unable to allocate array");
        return 1;
    }
    memcpy(sar_array, sar_array_input, mc_config.max_episode_steps * sizeof(struct SARType));

    unsigned int s;

    unsigned int* first_visit_array = malloc(num_states * sizeof(unsigned int));
    if (first_visit_array == NULL) {
        fprintf(stderr, "first_visit_reward_update: unable to allocate first visit array");
        free(sar_array);
        return 1;
    }
    /* Initialize each element of array to step_count.
       This value will indicate no visit. */
    for(s = 0; s < num_states; s++) {
        first_visit_array[s] = step_count;
    }

    unsigned int first_visit; /* Temp value for convenience */

    unsigned int i = step_count;
    while (i > 0) {
        i--;
        if (i < (step_count - 1)) {
            sar_array[i].reward += mc_config.discount_factor * sar_array[i+1].reward;
        }
        first_visit_array[sar_array[i].state] = i;
    }

    for(s = 0; s < num_states; s++) {
        if (first_visit_array[s] < step_count) {
            first_visit = first_visit_array[s];
            /* Note: We should have s == sar_array[first_visit].state */
            v[sar_array[first_visit].state].total_reward += sar_array[first_visit].reward;
            v[sar_array[first_visit].state].count        += 1;
        }
    }
    free(first_visit_array);
    free(sar_array);
    return 0;
}

int MC_FUNC_NAME(mc_policy_evaluation) (CONFIG_TYPE config, ACTION_TYPE* dpolicy, struct MCConfig mc_config, float* svalue_func_out) {
    unsigned int num_states = GET_NUM_STATES_METHOD(config);

    struct SARType* sar_array = malloc(mc_config.max_episode_steps * sizeof(struct SARType));
    if (sar_array == NULL) {
        fprintf(stderr, "mc_policy_evaluation: unable to allocate array");
        return 1;
    }

    struct RewardTracker* v = malloc(num_states * sizeof(struct RewardTracker));
    if (v == NULL) {
        fprintf(stderr, "mc_policy_evaluation: unable to allocate reward tracker array");
        free(sar_array);
        return 1;
    }
    memset(v, 0, num_states * sizeof(struct RewardTracker));

    /* Seed_Reset : Seed_Reset_Type := (Kind => Set_Default); */
    ENVIRONMENT_TYPE* env = MAKE_METHOD(config);
    OBSERVATION_TYPE obs;
    STEP_RETURN_TYPE step_output;
    unsigned int discrete_obs;
    ACTION_TYPE action;
    Boolean terminated;
    unsigned int step_count;

    unsigned int episode;
    unsigned int s;
    unsigned int i;

    for (episode = 0; episode < mc_config.num_episodes; episode++) {
        obs = RESET_METHOD(env); /* Seed_Reset */
        discrete_obs = TO_DISCRETE_OBSERVATION_METHOD(obs);
        terminated = FALSE;
        step_count = 0;
        for (i=0; i < mc_config.max_episode_steps; i++) {
            /* IN PROGRESS: might need to go i = 0 and i < max_episode_steps */
            action = dpolicy[discrete_obs];
            step_output = STEP_METHOD(env, action);
            /*
            sar_array[i].state = discrete_obs;
            sar_array[i].action = action;
            sar_array[i].reward = get_reward(step_output);
            */
            sar_array[i] = (struct SARType) { discrete_obs, action, step_output.reward };
            step_count = i + 1;

            /* Break if terminated */
            terminated = step_output.terminated;
            if (terminated) {
                break;
            }
            /* Set the next state */
            discrete_obs = TO_DISCRETE_OBSERVATION_METHOD(step_output.observation);
        }

        switch (mc_config.visit_type) {
            case FIRST_VISIT:
                first_visit_reward_update(mc_config, v, num_states, sar_array, step_count);
                break;
            case EVERY_VISIT:
               every_visit_reward_update(mc_config, v, sar_array, step_count);
                break;
        }
    }

    memset(svalue_func_out, 0, num_states*sizeof(float));
    for (s = 0; s < num_states; s++) {
        if (v[s].count > 0) {
            svalue_func_out[s] = v[s].total_reward / ((float) v[s].count);
        }
    }
    return 0;
}

#endif

#undef ENVIRONMENT_PREFIX
#undef CONFIG_TYPE
#undef ENVIRONMENT_TYPE
#undef ACTION_TYPE
#undef OBSERVATION_TYPE
#undef STEP_RETURN_TYPE
#undef ENVIRONMENT_ACTION_COUNT
#undef GET_NUM_STATES_METHOD
#undef MAKE_METHOD
#undef RESET_METHOD
#undef STEP_METHOD
#undef TO_DISCRETE_OBSERVATION_METHOD
#ifdef MC_DECLS_ONLY
#undef MC_DECLS_ONLY
#endif

/*
\/\*
type State_Action_Reward_Tracker_Type is array(Discrete_Observation_Type, Action_Type) of Reward_Tracker_Type;
State_Action_Reward_Tracker : State_Action_Reward_Tracker_Type := (others => (others => (0.0, 0)));
\*\/

void first_visit_reward_sa_update(RewardTracker (*q_tracker)[ACTION_COUNT], unsigned int num_states, SARType* sar_array_input, unsigned int step_count) {
    \/\* A local copy to avoid specifying "in out" for the array parameter \*\/
    \/\* TODO: Error handling \*\/
    \/\* TODO: Length of sar_array is max_episode_steps \*\/
    SARType* sar_array = malloc(num_states * sizeof(SARType));
    memcpy(sar_array, sar_array_input, num_states * sizeof(SARType));

    unsigned int s;

    unsigned int* first_visit_array = malloc(num_states * sizeof(unsigned int));
    \/\* Initialize each element of array to step_count.
       This value will indicate no visit. \*\/
    for(s = 0; s < num_states; s++) {
        first_visit_array[s] = step_count;
    }

    \/\* Local variables \*\/
    \/\* First visit index \*\/
    unsigned int first_visit; \/\* Temp value for convenience \*\/
    enum Action a;

    unsigned int i = step_count;
    while (i > 0) {
        i--;
        if (i < (step_count - 1)) {
            sar_array[i].reward += mc_config.discount_factory * sar_array[i+1].reward;
        }
        first_visit_array[sar_array[i].state] = i;
    }

    for(s = 0; s < num_states; s++) {
        if (first_visit_array[s] < step_count) {
            first_visit = first_visit_array[s];
            a = sar_array[first_visit].action;
            \/\* Note: We should have s == sar_array[first_visit].state \*\/
            q_tracker[s][a].total_reward += sar_array[first_visit].reward;
            q_tracker[s][a].count        += 1;
        }
    }
}

void update_policy(float (*q)[ACTION_COUNT], unsigned int num_states, float* prev_dpolicy, Boolean* state_encountered_flag, float* dpolicy_inout) {
    enum Action best_action;
    float max_value;

    unsigned int s;
    enum Action a;

    for(s = 0; s < num_states; s++) {
        \/\* Only update the policies for those states that appear in the episode \*\/
        if (state_encountered_flag[s]) {
            max_value = -FLT_MAX;
            best_action = (enum Action) 0;
            for(a = 0; a < ACTION_COUNT; a++) {
                if (q[s][a] > max_value) {
                     max_value = q[S][a];
                     best_action = a;
                }
            }
            dpolicy_inout[s] = best_action;
        }
    }
}

\/\* TODO: Should we use MC_Config_Type?  We might not want to allow the Every_Visit option. \*\/
void mc_exploring_starts_evaluation(struct Config config, struct MCConfig mc_config) {
    RewardTracker* sar_tracker[ACTION_COUNT] = malloc(num_states * ACTION_COUNT * sizeof(RewardTracker));
    memset(sar_tracker, 0, num_states * sizeof(*sar_tracker));

    SARType* sar_array = malloc(max_episode_steps * sizeof(SARType));

    \/\* NOTE: The flag for indicating whether a state was encountered in an episode
     *       doesn't impact the results.  At best it reduces the number of states
     *       for which to update action values and the policy. \*\/
    Boolean* state_encountered_flag = malloc(num_states * sizeof(Boolean));

    \/\*
    struct EvaluationResults res;
    \*\/
    enum Action* prev_dpolicy = malloc(num_states * sizeof(enum Action));
    enum Action* dpolicy = malloc(num_states * sizeof(enum Action));

    memset(dpolicy, 0, num_states * sizeof(enum Action));

    float (*q)[ACTION_COUNT] = malloc(num_states * ACTION_COUNT * sizeof(float));
    memset(dpolicy, 0, num_states * sizeof(*q));

    struct Environment env = MAKE_METHOD(config);
    \/\*
    Seed_Reset : Seed_Reset_Type := (Kind => Set_Default);
    \*\/
    struct Observation obs;
    struct StepReturn step_output;
    unsigned int discrete_obs;
    enum Action action;
    Boolean terminated;
    unsigned int last_i;

    srand(time(NULL));

    for (episode = 1; episode <= mc_config.num_episodes; episode++) {
        \/\* Exploring starts
         * Initialize Env and select a random action.
         * TODO: Random selection of env and action might need to be generalized \*\/
        memset(state_encountered_flag, 0, num_states * sizeof(Boolean));
        obs = reset(env); \/\* Seed_Reset \*\/
        action = (enum Action) get_random_action();

        \/\* Process the first action \*\/
        discrete_obs = to_discrete_observation(obs);
        step_output = step(env, action);
        \/\* TODO: Should the index be 0 in the following? \*\/
        sar_array[1] := (struct SARType) { discrete_obs, action, step_output.reward };
        state_encountered_flag[discrete_obs] = TRUE;
        \/\* TODO: Should the index be 0 in the following? \*\/
        last_i = 1;
        terminated = get_terminated(step_output);
         
        if (!terminated) {
            discrete_obs = to_discrete_observation(step_output.observation);

            \/\* Simulate as before starting from step 2 \*\/
            for (i = 2; i <= Max_Episode_Steps; i++) {
               action = dpolicy[discrete_obs];
               step_output = step(env, action);
               sar_array[i] = (struct SARType) {discrete_obs, action, step_output.reward};
               state_encountered_flag(discrete_obs) = TRUE;
               last_i = i;
               terminated = step_output.terminated;
               if (terminated) {
                   break;
               }
               discrete_obs = to_discrete_observation(step_output.observation);
            }
        }

        \/\* For first occurrent of state s and action a, record total reward \*\/
        first_visit_reward_update(sar_tracker, sar_array, last_i);

        \/\* Update action values \*\/
        for(s = 0; s < num_states; s++) {
            if (state_encountered_flag[s]) {
                for(a = 0; a < ACTION_COUNT; a++) {
                    if (sar_tracker[s][a].count > 0) { \/\* TODO \*\/
                        q[s][a] = sar_tracker[s][a].total_reward / ((float) sar_tracker[s][a].count);
                    }
                }
            }
        }
         
        \/\* Use arg max to update policy \*\/
        memcpy(prev_dpolicy, dpolicy, num_states*sizeof(enum Action));
        update_policy(q, prev_policy, state_encountered_flag, dpolicy);
        \/\* At this time we donn't consider an early exit based on policy stability \*\/
    }
    \/\* return Evaluation_Results_Type'(Q => Q, Policy => Policy); \*\/
}

\/\*
      type Action_Probability_Type is array (Action_Type) of Float;
\*\/
enum Action get_random_action(float* action_probs) {
    float u = rand_float();
    float cum_prob = 0.0;

    enum Action a = (enum Action) ACTION_COUNT - 1;
    for (a = 0; a < ACTION_COUNT; a++) {
        cum_prob += action_probs[a];
        if (u < cum_prob) {
            return a;
        }
    }
    return a;
}

enum Action get_random_action_for_state(float (*mixed_policy)[ACTION_COUNT], unsigned int s) {
    return get_random_action(mixed_policy[s]);
}

void best_policy(float (*q)[ACTION_COUNT], unsigned int num_states, enum Action* dpolicy_out) {
    enum Action best_action, a;
    float max_value;

    unsigned int s;

    for (s=0; s < num_states; s++) {
        max_value = -FLT_MAX;
        best_action = (enum Action) 0;
        for (a = 0; a < ACTION_COUNT; a++) {
            if (q[s][a] > max_value) {
                max_value = q[s][a];
                best_action = a;
            }
        }
        dpolicy_out[s] = best_action;
    }
 }

void epsilon_soft_policy_update(
    MCEpsilonSoftConfig mces_config,
    const enum Action* dpolicy,
    Boolean* state_encountered_flag,
    float (*mixed_policy_inout)[ACTION_COUNT]
) {
    enum Action best_action, a;
    float max_value;

    unsigned int s;

    float eps = mces_config.epsilon;

    for (s=0; s < num_states; s++) {
        if (state_encountered_flag[S]) {
            best_action = dpolicy[s];
            for (a = 0; a < ACTION_COUNT; a++) {
                if (a == best_action) {
                    mixed_policy_inout[s][a] = 1.0 - eps + eps / ((float) ACTION_COUNT);
                } else {
                    mixed_policy_inout[s][a] = eps / ((float) ACTION_COUNT);
                }
            }
        }
    }
}

\/\* IN PROGRESS: Should we use MC_Config_Type
 * TODO: Should we add a condition so that 1/Action_Count <= 1 - epsilon 
 *       (which is equivalent to epsilon <= 1 - 1/Action_Count). \*\/
void mc_epsilon_soft_evaluation(struct Config config, struct MCEpsilonSoftConfig mces_config) {
    \/\* TODO: Need to return Evaluation_Results_Type somehow \*\/
    RewardTracker* sar_tracker[ACTION_COUNT] = malloc(num_states * ACTION_COUNT * sizeof(RewardTracker));
    memset(sar_tracker, 0, num_states * sizeof(*sar_tracker));

    SARType* sar_array = malloc(max_episode_steps * sizeof(SARType));

    \/\* NOTE: For epsilon soft, unlike for exploring starts,
     *       the flag for indicating whether a state was encountered in an episode
     *       does have some impact when updating the action values. \*\/
    Boolean* state_encountered_flag = malloc(num_states * sizeof(Boolean));

    unsigned int s;
    enum Action a;

    \/\*
    struct EvaluationResults res;
    \*\/
    enum Action* greedy_policy = malloc(num_states * sizeof(enum Action));

    memset(greedy_policy, 0, num_states * sizeof(enum Action));

    \/\* Action_Count : Natural := Action_Type'Pos(Action_Type'Last) - Action_Type'Pos(Action_Type'First) + 1; \*\/
    \/\* TODO: Might not need prev_spolicy in this implementation \*\/
    float (*prev_spolicy)[ACTION_COUNT];
    prev_spolicy = malloc(num_states * sizeof(*prev_spolicy));

    float (*spolicy)[ACTION_COUNT];
    spolicy = malloc(num_states * sizeof(*spolicy));

    for(s = 0; s < num_states; s++) {
        for (a = 0; a < ACTION_COUNT; a++) {
            \/\* TODO: Is this what we want? \*\/
            spolicy[s][a] = 1.0 / ((float) ACTION_COUNT);
        }
    }
    
    float (*q)[ACTION_COUNT];
    q = malloc(num_states * sizeof(*q));
    memset(q, 0, num_states * sizeof(*q));
    
    struct Environment env = MAKE_METHOD(config);
    \/\*
    Seed_Reset : Seed_Reset_Type := (Kind => Set_Default);
    \*\/
    struct Observation obs;
    struct StepReturn step_output;
    unsigned int discrete_obs;
    enum Action action;
    Boolean terminated;
    unsigned int last_i;

    srand(time(NULL));

    for (episode = 1; episode <= mc_config.num_episodes; episode++) {
        \/\* Initialize Env and select a random action.
         * TODO: Random selection of env and action might need to be generalized \*\/
        memset(state_encountered_flag, 0, num_states * sizeof(Boolean));
        obs = reset(env); \/\* Seed_Reset \*\/
            
        \/\* Simulate as before starting from step 2 \*\/
        for (i = 1; i <= max_episode_steps; i++) {
            discrete_obs = to_discrete_observation(obs);
            action = get_random_action_for_state(spolicy, discrete_obs);
            step_output = step(env, action);
            sar_array[i] = (struct SARType) {discrete_obs, action, step_output.reward};
            last_i = i;
            state_encountered_flag[discrete_obs] = TRUE;
            terminated = step_output.terminated;
            if (terminated) {
                break;
            }
            \/\* Set state to next state \*\/
            discrete_obs = to_discrete_observation(step_output.observation);
        }
        \/\* For first occurrent of state s and action a, record total reward \*\/
        first_visit_reward_update(sar_tracker, sar_array, last_i);

        \/\* Update action values \*\/
        for(s = 0; s < num_states; s++) {
            \/\* TODO: Do we want to put a conditional on state_encountered_flag(S)? \*\/
            \/\* if (state_encountered_flag[s]) { ... } \*\/
            for (a = 0; a < ACTION_COUNT; a++) {
                if (sar_tracker[s][a].count > 0) {
                    q[s][a] = sar_tracker[s][a].total_reward / ((float) sar_tracker[s][a].count);
                }
            }
        }
 
        \/\* Use arg max to update policy \*\/
        best_policy(q, num_states, greedy_policy);
        epsilon_soft_policy_update(mces_config, spolicy, greedy_policy, state_encountered_flag);
         \/\* TODO: Consider what an exit for an epsilon-soft on-policy evaluation might look like. \*\/
    }
    \/\* return Evaluation_Results_Type'(Q => Q, Policy => Greedy_Policy); \*\/
}
*/
