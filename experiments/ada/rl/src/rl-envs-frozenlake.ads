with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Float_Random;

--  The Frozen lake environments involves crossing a Frozen(F) lake from
--  Start(S) to Goal(G) without falling into any Holes(H).
--  The agent may not always move in the intended direction due to the
--  slippery nature of the frozen lake.
-- 
--  ## Action Space
--  Actions represent moving in one of four directions, namely
--  Left, Down, Right, or Up.
-- 
--  ## Observation Space
--  The observation is a value representing the agent's current position as
--  current_row * nrows + current_col (where both the row and col start at 0).
--  The number of possible observations is dependent on the size of the map.
--  For example, the 4x4 map has 16 possible observations.
--
--  Note that in the private environment below that we actually store the
--  position in 1-based coordinates (row and column), but the public
--  observation type is the flattened 0-based position index.
-- 
--  ## Rewards
--  Reward schedule:
--    - Reach goal(G): +1
--    - Reach hole(H): 0
--    - Reach frozen(F): 0
-- 
--  ## Environment Configuration
--  There are two supported configuration options:
--    - Map name, which can be either 4x4 or 8x8, and
--    - Slippery, which can be true or false.
--   
--  Depending on map name, the maps are as follows.
--    4x4:
--         SFFF
--         FHFH
--         FFFH
--         HFFG
-- 
--    8x8:
--         SFFFFFFF
--         FFFFFFFF
--         FFFHFFFF
--         FFFFFHFF
--         FFFHFFFF
--         FHHFFFHF
--         FHFFHFHF
--         FFFHFFFG
-- 
--  Is_Slippery can be True or False. If True will move in intended direction
--  with probability of 1/3 else will move in either perpendicular direction
--  with equal probability of 1/3 in both directions.
-- 
--  For example, if action is left and is_slippery is True, then:
--    - P(move left)=1/3
--    - P(move up)=1/3
--    - P(move down)=1/3
--
-- Some differences with the Python Gymnasium implementation include:
-- * the Python version returns probability info (e.g., {"prob": 1})
--   in the reset and step methods, whereas this implementation
--   omits this info
-- * this implementation does not support randomly generated maps.
package RL.Envs.Frozenlake is
   package Float_Random renames Ada.Numerics.Float_Random;

   type Map_Type is (Map_4x4, Map_8x8);
   type Action_Type is (Left, Down, Right, Up);

   type Map_Info_Type is record
      Map_Name: Map_Type;
      Rows: Positive;
      Cols: Positive;
   end record;
   function Get_Map_Info(Map_Name: Map_Type) return Map_Info_Type;

   type Config_Type is record
      Map_Name: Map_Type;
      Is_Slippery : Boolean;
   end record;

   type Environment_Type(Rows: Positive; Cols: Positive) is limited private;

   type Observation_Type is record
      Position_Index : Natural;
   end record;
    
   type Step_Return_Type is record
      Observation: Observation_Type;
      Reward: Float;
      Terminated: Boolean;
   end record;
    
   function Make(Config: Config_Type) return Environment_Type;
   -- TODO: Add argument for seed
   function Reset(Env : in out Environment_Type) return Observation_Type;
   function Step(Env : in out Environment_Type; action: Action_Type) return Step_Return_Type;
   -- Render_Text loosely follows the Python implementation, except that we use
   -- an "A" to indicate the position of the agent rather than highlighting the
   -- cell type abbreviation in red.
   procedure Render_Text(Env : Environment_Type);

   -- TODO: We move the DP model to a child package
   -- type Discrete_State_Type is new Natural range 0 .. 63; -- Allow for 8x8 map.
   -- type Transition_Probability_Type is record
   --     Probability : Float;
   --     Reward : Float;
   -- end record;
   -- type Discrete_Model_Type is array (Discrete_State_Type, Action_Type, Discrete_State_Type) of Transition_Probability_Type;
   -- function Get_Model(Config: Config_Type) return Discrete_Model_Type;

private
   type Map_Element is (S, F, H, G);
   type Map_Array is array (Positive range <>, Positive range <>) of Map_Element;

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

   type Action_Transition_Type is array (Action_Type, Action_Type) of Transition_Type;

   type Map_Transitions is array (Positive range <>, Positive range <>) of Action_Transition_Type;

   type Environment_Type(Rows: Positive; Cols: Positive) is record
      Map: Map_Array(1 .. Rows, 1 .. Cols);
      P : Map_Transitions(1 .. Rows, 1 .. Cols);
      Agent_Position: Position_Type;
   end record;
   
   Gen: Float_Random.Generator;

   -- These functions follow similar methods in the Python implementation of FrozenLakeEnv.
   -- We make these private since they are not intended to be used directly.
   function Position_Inc(Rows, Cols : Positive; Position: Position_Type; Action: Action_Type) return Position_Type;
   function Update_Probability_Matrix(Map : Map_Array; Position: Position_Type; Action : Action_Type) return Partial_Transition_Type;
   function To_S(Map: Map_Array; Position: Position_Type) return Natural;
   function Get_Start_Position(Map: Map_Array) return Position_Type;

end RL.Envs.Frozenlake;

