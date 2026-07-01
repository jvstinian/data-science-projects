with Ada.Numerics.Float_Random;

--  Cliff walking involves crossing a gridworld from start to goal while
--  avoiding falling off a cliff.
-- 
--  ## Description
--  The game starts with the player at location (3, 0) of the 4x12 grid world
--  with the goal located at (3, 11). If the player reaches the goal the
--  episode ends.
-- 
--  A cliff runs along (3, 1..10). If the player moves to a cliff location it
--  returns to the start location.
-- 
--  The player makes moves until they reach the goal.
-- 
--  Adapted from Example 6.6 (page 132) from
--  Reinforcement Learning: An Introduction
--  by Sutton and Barto [<a href="#cliffwalk_ref">1</a>].
-- 
--  The cliff can be chosen to be slippery (disabled by default) so the player
--  may move perpendicular to the intended direction sometimes.
-- 
--  With inspiration from:
--  [https://github.com/dennybritz/reinforcement-learning/blob/master/lib/envs/cliff_walking.py](https://github.com/dennybritz/reinforcement-learning/blob/master/lib/envs/cliff_walking.py)
-- 
--  ## Action Space
--  Actions represent moving in one of four directions, namely
--  Left, Down, Right, or Up.
-- 
--  ## Observation Space
--  There are 3 x 12 + 1 possible states. The player cannot be at the cliff, nor at
--  the goal as the latter results in the end of the episode. What remains are all
--  the positions of the first 3 rows plus the bottom-left cell.
-- 
--  The observation is a value representing the player's current position as
--  current_row * ncols + current_col (where both the row and col start at 0).
-- 
--  For example, the starting position can be calculated as follows: 3 * 12 + 0 = 36.
-- 
--  The observation is returned as an `int()`.
-- 
--  ## Starting State
--  The episode starts with the player in state `36` (location (3, 0)).
-- 
--  ## Reward
--  Each time step incurs -1 reward, unless the player stepped into the cliff,
--  which incurs -100 reward.
-- 
--  ## Episode End
--  The episode terminates when the player enters state `47` (location (3, 11)).
-- 
--  ## Information
-- 
--  `step()` and `reset()` return a dict with the following keys:
--  - "p" - transition proability for the state.
-- 
--  As cliff walking is not stochastic, the transition probability returned always 1.0.
-- 
--  ## References
--  “Reinforcement Learning: An Introduction” 2020. [Online]. Available:
--  [http://www.incompleteideas.net/book/RLbook2020.pdf](http://www.incompleteideas.net/book/RLbook2020.pdf)
-- 
--  Some differences with the Python Gymnasium implementation include:
--  * the Python version returns probability info (e.g., {"prob": 1})
--    in the reset and step methods, whereas this implementation
--    omits this info
package RL.Envs.Cliffwalking is
   package Float_Random renames Ada.Numerics.Float_Random;

   type Action_Type is (Left, Down, Right, Up);

   Num_Rows : constant Positive := 4;
   Num_Cols : constant Positive := 12;

   type Environment_Config is record
      Is_Slippery : Boolean;
   end record;

   type Environment_State is limited private;

   type Observation_Type is new Natural range 0 .. (Num_Rows * Num_Cols - 1);
   
   type Step_Return_Type is record
      Observation: Observation_Type;
      Reward: Float;
      Terminated: Boolean;
   end record;
   
   function Make(config: Environment_Config) return Environment_State;
   function Reset(Env : in out Environment_State; Seed_Reset : Seed_Reset_Type) return Observation_Type;
   function Step(Env : in out Environment_State; action: Action_Type) return Step_Return_Type;
   -- Render_Text loosely follows the Python implementation, except that we use
   -- an "A" to indicate the position of the agent rather than an "x".
   procedure Render_Text(Env : Environment_State);

   -- type Discrete_State_Type is new Natural range 0 .. 63; -- Allow for 8x8 map.
   -- type Transition_Probability_Type is record
   --     Probability : Float;
   --     Reward : Float;
   -- end record;
   -- type Discrete_Model_Type is array (Discrete_State_Type, Action_Type, Discrete_State_Type) of Transition_Probability_Type;
   -- function Get_Model(config: Environment_Config) return Discrete_Model_Type;

private
   -- Map elements: S is Start, C is Cliff, G is Goal, and
   -- P s for regular ground
   type Map_Element is (S, P, C, G);  
   type Map_Array is array (1 .. Num_Rows, 1 .. Num_Cols) of Map_Element;

   type Position_Type is record
      Row: Positive;
      Col: Positive;
   end record;

   -- The following is a partial set of fields for the
   -- Transition_Type which follows
   type Partial_Transition_Type is record
       Position : Position_Type;
       Reward : Float;
       Terminated: Boolean;
   end record;

   type Transition_Type is record
       Probability : Float;
       Position : Position_Type;
       Reward : Float;
       Terminated: Boolean;
   end record;

   -- TODO: Given we have a fixed number of rows and columns, do we need Action_Transition_Type?
   -- Can we just extend the indices of Map_Transitions?
   type Action_Transition_Type is array (Action_Type, Action_Type) of Transition_Type;
   type Map_Transitions is array (1 .. Num_Rows, 1 .. Num_Cols) of Action_Transition_Type;

   type Environment_State is record
      Map: Map_Array;
      P : Map_Transitions;
      Agent_Position: Position_Type;
   end record;
   
   Gen: Float_Random.Generator;

   -- These functions follow similar methods in the Python implementation of CliffWalkingEnv,
   -- or the methods of the same name in the Frozen Lake environment with the necessary
   -- adjustments for the different rules of Cliff Walking.
   -- We make these private since they are not intended to be used directly.
   function Position_Inc(Rows : Positive; Cols : Positive; Position: Position_Type; Action: Action_Type) return Position_Type;
   function Update_Probability_Matrix(Map : Map_Array; Position: Position_Type; Action : Action_Type) return Partial_Transition_Type;
   function To_S(Map: Map_Array; Position: Position_Type) return Observation_Type;
   function Get_Start_Position(Map: Map_Array) return Position_Type;
end RL.Envs.Cliffwalking;

