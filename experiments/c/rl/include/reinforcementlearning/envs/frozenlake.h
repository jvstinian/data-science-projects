#include <stdio.h>
#include <reinforcementlearning/bool.h>

enum FrozenlakeMapType {
    MAP_4X4,
    MAP_8X8
};

struct FrozenlakeConfig {
  enum FrozenlakeMapType map_name;
  Boolean slippery;
};
   
enum FrozenlakeAction {
    LEFT,
    DOWN,
    RIGHT,
    UP
};

#define FROZENLAKE_ACTION_COUNT 4

struct FrozenlakeObservation {
    unsigned int position_index;
};
   
struct FrozenlakeEnvironment;

struct FrozenlakeStepReturn {
    struct FrozenlakeObservation observation;
    float reward;
    Boolean terminated;
};
  
enum FrozenlakeAction frozenlake_get_random_action();

struct FrozenlakeEnvironment* frozenlake_make(struct FrozenlakeConfig config);
int frozenlake_init(struct FrozenlakeConfig config, struct FrozenlakeEnvironment* env);
struct FrozenlakeObservation frozenlake_reset(struct FrozenlakeEnvironment* env);
struct FrozenlakeStepReturn frozenlake_step(struct FrozenlakeEnvironment* env, enum FrozenlakeAction action);
void frozenlake_deinit(struct FrozenlakeEnvironment* env);
void frozenlake_close(struct FrozenlakeEnvironment* env);
void frozenlake_render_text(struct FrozenlakeEnvironment env);

struct TransitionProbabilityType {
   float probability;
   float reward;
};

struct DiscreteModelType;

int frozenlake_model_create(struct FrozenlakeConfig config, struct DiscreteModelType* model);
void frozenlake_model_destroy(const struct DiscreteModelType* model);
struct TransitionProbabilityType frozenlake_get_transition(const struct DiscreteModelType* model, unsigned int s, enum FrozenlakeAction action, unsigned int next_s);

/* int iterative_policy_evaluation(struct DiscreteModelType* model, const float (*policy)[FROZENLAKE_ACTION_COUNT], float df, float *value_array); */
#define ENVIRONMENT_PREFIX frozenlake
#define DISCRETE_MODEL_TYPE struct DiscreteModelType
#define ACTION_TYPE enum FrozenlakeAction
#define ENVIRONMENT_ACTION_COUNT FROZENLAKE_ACTION_COUNT 
#define GET_TRANSITION_METHOD frozenlake_get_transition
#define PRINT_POLICY_METHOD print_policy
#define DP_DECLS_ONLY
#include <reinforcementlearning/algorithms/dp.inc>

int iterative_deterministic_policy_evaluation(struct DiscreteModelType* model, enum FrozenlakeAction* dpolicy, float df, float *value_array);

int value_iteration(struct DiscreteModelType* model, float df, enum FrozenlakeAction* dpolicy_out);
int frozenlake_value_iteration_example(struct FrozenlakeConfig config, float df);

int frozenlake_example_main();

unsigned int frozenlake_get_num_states(struct FrozenlakeConfig config);
unsigned int frozenlake_to_discrete_observation(struct FrozenlakeObservation obs);

#define ENVIRONMENT_PREFIX frozenlake
#define CONFIG_TYPE struct FrozenlakeConfig
#define ENVIRONMENT_TYPE struct FrozenlakeEnvironment
#define ACTION_TYPE enum FrozenlakeAction
#define OBSERVATION_TYPE struct FrozenlakeObservation
#define STEP_RETURN_TYPE struct FrozenlakeStepReturn
#define ENVIRONMENT_ACTION_COUNT FROZENLAKE_ACTION_COUNT
#define GET_NUM_STATES_METHOD frozenlake_get_num_states
#define MAKE_METHOD frozenlake_make
#define RESET_METHOD frozenlake_reset
#define STEP_METHOD frozenlake_step
#define CLOSE_METHOD frozenlake_close
#define GET_RANDOM_ACTION_ES_METHOD frozenlake_get_random_action
#define TO_DISCRETE_OBSERVATION_METHOD frozenlake_to_discrete_observation
#define MC_DECLS_ONLY
#include <reinforcementlearning/algorithms/mc.inc>

/*
package Frozen_Lake is
   type Map_Type is (Map_4x4, Map_8x8);
   type Cell_Type is (Start, Frozen, Hole, Goal);
   type Action_Type is (Left, Down, Right, Up);

   -- TODO: The following is experimental, and might not be needed in the future.
   type Map_Info_Type is record
      Map_Name: Map_Type;
      Rows: Positive;
      Cols: Positive;
   end record;
   function Get_Map_Info(Map_Name: Map_Type) return Map_Info_Type;
   -- End of experimental code.

   -- type Map_Element is private;
   type Environment_State(Rows: Positive; Cols: Positive) is limited private;

private
   type Map_Element is (S, F, H, G);
   type Map_Array is array (Positive range <>, Positive range <>) of Map_Element;


   -- The following is a partial set of fields for the
   -- Transition_Type which follows
   type Partial_Transition_Type is record
       Position : Position_Type;
       Reward : Float;
       Truncated : Boolean; -- TODO: Change variable name to Terminated
   end record;

   type Map_Transitions is array (Positive range <>, Positive range <>) of Action_Transition_Type;

   Gen: Float_Random.Generator;

   -- These functions follow similar methods in the Python implementation of FrozenLakeEnv.
   -- We make these private since they are not intended to be used directly.
   function Position_Inc(Rows : Positive; Cols : Positive; Position: Position_Type; Action: Action_Type) return Position_Type;
   function Update_Probability_Matrix(Map : Map_Array; Position: Position_Type; Action : Action_Type) return Partial_Transition_Type;
   function To_S(Map: Map_Array; Position: Position_Type) return Natural;
   function Get_Start_Position(Map: Map_Array) return Position_Type;

end Frozen_Lake;
*/
