with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Float_Random; -- use Ada.Numerics.Float_Random;
with Generic_Random_Functions;
with System.Pool_Local;

package Car_Rental is
   package Float_Random renames Ada.Numerics.Float_Random;
   package GRF is new Generic_Random_Functions(Real => Float);

   Lot_Size : constant Positive := 20;
   Max_Move : constant Positive := 5;

   type Action_Type is new Integer range -Max_Move .. Max_Move;

   type Environment_Config is record
      Lot_A_Request_Lambda : Float;
      Lot_A_Return_Lambda : Float;
      Lot_B_Request_Lambda : Float;
      Lot_B_Return_Lambda : Float;
      Lot_A_Init_Cars : Natural;
      Lot_B_Init_Cars : Natural;
   end record;

   Default_Config : Environment_Config := (
      Lot_A_Request_Lambda => 3.0,
      Lot_A_Return_Lambda => 3.0,
      Lot_B_Request_Lambda => 4.0,
      Lot_B_Return_Lambda => 2.0,
      Lot_A_Init_Cars => 0,
      Lot_B_Init_Cars => 0
   );

   type Environment_State is limited private;

   type Observation_Type is record
      Lot_A_Cars : Natural;
      Lot_B_Cars : Natural;
   end record;
    
   type Step_Return_Type is record
      Observation: Observation_Type;
      Reward: Float;
      Terminated: Boolean;
      Truncated: Boolean;
   end record;
    
   function Make(config: Environment_Config) return Environment_State;
   -- TODO: Add argument for seed
   function Reset(Env : in out Environment_State) return Observation_Type;
   function Step(Env : in out Environment_State; action: Action_Type) return Step_Return_Type;
   procedure Render_Text(Env : Environment_State);

   type Discrete_State_Type is new Natural range 0 .. ((Lot_Size + 1) * (Lot_Size + 1) - 1);
   type Transition_Probability_Type is record
       Probability : Float;
       Reward : Float;
   end record;
   type Discrete_Model_Type is array (Discrete_State_Type, Action_Type, Discrete_State_Type) of Transition_Probability_Type;
   type Discrete_Model_Access_Type is access Discrete_Model_Type;
   DM_Pool : System.Pool_Local.Unbounded_Reclaim_Pool;
   for Discrete_Model_Access_Type'Storage_Pool use DM_Pool;
   function Get_Model(Config: Environment_Config) return Discrete_Model_Access_Type;
   -- function Get_Model_Access(Config: Environment_Config) return Discrete_Model_Access_Type;

   type Transition_Array_Type is array (Discrete_State_Type) of Transition_Probability_Type;
   function Get_Transition_Values (Config: Environment_Config; State: Discrete_State_Type; Action: Action_Type) return Transition_Array_Type;
   function Get_Transition_Values2 (Config: Environment_Config; State: Discrete_State_Type; Action: Action_Type) return Transition_Array_Type;

private
   -- -- The following is a partial set of fields for the
   -- -- Transition_Type which follows
   -- type Partial_Transition_Type is record
   --     Position : Position_Type;
   --     Reward : Float;
   --     Truncated : Boolean; -- TODO: Change variable name to Terminated
   -- end record;

   -- type Transition_Type is record
   --     Probability : Float;
   --     Position : Position_Type;
   --     Reward : Float;
   --     Truncated : Boolean; -- TODO: Change variable name to Terminated
   -- end record;

   -- -- type Transition_Probabilities_Type is array (Natural range <>) of Transition_Type;
   -- type Action_Transition_Type is array (Action_Type, Action_Type) of Transition_Type;

   -- type Map_Transitions is array (Positive range <>, Positive range <>) of Action_Transition_Type;

   Gen: Float_Random.Generator;
   function Rando return Float;
   function Poisson is new GRF.Poisson(U => Rando);

   type Environment_State is record
      Gen: Float_Random.Generator;
      Config : Environment_Config;
      -- Lot_A_Request_Lambda : Float;
      -- Lot_A_Return_Lambda : Float;
      -- Lot_B_Request_Lambda : Float;
      -- Lot_B_Return_Lambda : Float;
      -- Lot_A_Init_Cars : Natural;
      -- Lot_B_Init_Cars : Natural;
      Lot_A_Cars : Natural;
      Lot_B_Cars : Natural;
   end record;
      
   type Cars_Per_Lot_Type is record
      Lot_A_Cars : Natural;
      Lot_B_Cars : Natural;
   end record;
      
   type Cars_After_Action_Type is record
      Cars_Per_Lot: Cars_Per_Lot_Type;
      Cars_Moved : Natural;
   end record;

   function Poisson_PMF(Lambda : Float; N : Natural) return Float;
   function Poisson_CDF(Lambda : Float; N : Natural) return Float;
   function Poisson_SF(Lambda : Float; N : Natural) return Float;  -- Survival function, i.e., 1 - CDF
   -- -- These functions follow similar methods in the Python implementation of FrozenLakeEnv.
   -- -- We make these private since they are not intended to be used directly.
   -- function Position_Inc(Rows : Positive; Cols : Positive; Position: Position_Type; Action: Action_Type) return Position_Type;
   -- function Update_Probability_Matrix(Map : Map_Array; Position: Position_Type; Action : Action_Type) return Partial_Transition_Type;
   function To_Obs(Env : Environment_State) return Natural;
   function To_Discrete_State(Cars_Per_Lot : Cars_Per_Lot_Type) return Discrete_State_Type;
   -- function Get_Start_Position(Map: Map_Array) return Position_Type;
   function From_Discrete_State (D : Discrete_State_Type) return Cars_Per_Lot_Type;
   function Calculate_Transition_Probability (Config : Environment_Config; Cars_Moved : Natural; Prev_Cars : Cars_Per_Lot_Type; Next_Cars : Cars_Per_Lot_Type) return Transition_Probability_Type;
   function Step_Cars(Cars_Count : Cars_Per_Lot_Type; Action : Action_Type) return Cars_After_Action_Type;
   function Calculate_Transition_Probability2 (Config : Environment_Config; Cars_Moved : Natural; Prev_Cars : Cars_Per_Lot_Type) return Transition_Array_Type;

end Car_Rental;

