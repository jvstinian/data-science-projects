#include <stdio.h>

typedef enum Boolean {
    FALSE,
    TRUE
} Boolean;

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

int iterative_policy_evaluation(struct DiscreteModelType* model, float (*policy)[FROZENLAKE_ACTION_COUNT], float df, float *value_array);
int iterative_deterministic_policy_evaluation(struct DiscreteModelType* model, enum FrozenlakeAction* dpolicy, float df, float *value_array);

int value_iteration(struct DiscreteModelType* model, float df, enum FrozenlakeAction* dpolicy_out);
int frozenlake_value_iteration_example(struct FrozenlakeConfig config, float df);

int main();
    
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
