with Ada.Numerics.Float_Random;
with Mathpaqs.Generic_Random_Functions;
with System.Pool_Local;

--  ## Description
--  The Car Rental environment involves an owner of a car rental business
--  which has two locations, which are referred to as lot A and B.  Each
--  day customers come in to rent and return cars.  A car that is returned
--  is only available for rental the next day.  At the end of each
--  day the owner must make a choice about how many cars to move between
--  the two sites with the aim being to make sure there
--  are enough cars available for rental at each site.
--  The owner makes a fixed profit for every car rented, and
--  pays a fixed amount for each car moved overnight.
--
--  The number of rental requests and returns at each site follow
--  independent Poisson processes.  The lot size at each location
--  is fixed and so there is a maximum number of cars that can
--  be made available for rental at each location.  If the number
--  of returns would result in an overflow at the lot then the
--  excess cars are returned to a lot maintained by the corporate
--  headquarters and are not subsequently available for rent.
--  
--  The number of cars that can be moved overnight is fixed as well.
--
--  The parameters and constraints referenced above are mostly
--  fixed (constant) in this implementation, i.e. we don't
--  support specifying these parameters either as generic
--  parameters or as values in the configuration type.
--  We will provide further information about the values of
--  these constants in the sections that follow.
--  The Poisson mean request and returns rates and the initial
--  number of cars in each lot can be specified though.
-- 
--  This environment is adapted from Example 4.2 (page 98) of
--  Reinforcement Learning: An Introduction (1998) by Sutton and Barto.
--
--  ## Action Space
--  Actions represent the number of cars transferred between Lot A
--  and Lot B.  The maximum number of cars that can be moved
--  is 5.  The range for the action values is -5, -4, ..., +5.
--  A positive number indicates cars were moved from Lot A to B,
--  while a negative number indicates cars were moved in the
--  opposite direction.
-- 
--  ## Observation Space
--  The observation space consists of two values, namely the number of
--  cars present on each lot at the end of the day.  These numbers
--  reflect the cars rented and returned that day.
--  For each lot, the number of cars on each lot is an integer
--  between 0 and 20 (inclusive).  If the returns would
--  cause this number to be above 20, then those cars are assumed
--  to be moved to a lot maintained by corporate headquarters
--  and are not available for rent.
--
--  For the Dynamic Programming model, we require a discrete state,
--  and so we convert the number of cars in the two lots to a
--  single value.  The approach used is the same approach used to
--  convert coordinates in other environments to a single
--  position index.  If the number of cars is given by a tuple
--  (a, b), then the corresponding discrete state value is given
--  by a * 21 + b.  TODO: Check this is accurate
--
--  ## Rewards
--  The owner makes a profit of $10 dollars for each car rented,
--  and pays $2 for each car moved.
--  Only cars available at the lot can be rented, and
--  so no profit is made for any requests in excess of the
--  number of cars available (the customers are turned away).
--
--  ## Environment Configuration
--  The Poisson mean rate for returns and requests
--  at each lot can be specified.  Also, the number of
--  cars initially available at each site can be provided.
--
--  ## References
--  Sutton, R. S., Barto, A. G. (2018).
--  Reinforcement Learning: An Introduction. The MIT Press.
--  http://incompleteideas.net/book/the-book-1st.html
package RL.Envs.Carrental is
   package Float_Random renames Ada.Numerics.Float_Random;
   package GRF is new Mathpaqs.Generic_Random_Functions(Real => Float);

   Lot_Size : constant Positive := 20;
   Max_Move : constant Positive := 5;

   type Action_Type is new Integer range -Max_Move .. Max_Move;

   type Config_Type is record
      Lot_A_Request_Lambda : Float;
      Lot_A_Return_Lambda : Float;
      Lot_B_Request_Lambda : Float;
      Lot_B_Return_Lambda : Float;
      Lot_A_Init_Cars : Natural;
      Lot_B_Init_Cars : Natural;
   end record;

   Default_Config : Config_Type := (
      Lot_A_Request_Lambda => 3.0,
      Lot_A_Return_Lambda => 3.0,
      Lot_B_Request_Lambda => 4.0,
      Lot_B_Return_Lambda => 2.0,
      Lot_A_Init_Cars => 0,
      Lot_B_Init_Cars => 0
   );

   type Environment_Type is limited private;

   type Observation_Type is record
      Lot_A_Cars : Natural;
      Lot_B_Cars : Natural;
   end record;
    
   type Step_Return_Type is record
      Observation: Observation_Type;
      Reward: Float;
      Terminated: Boolean;
   end record;
    
   function Make(Config: Config_Type) return Environment_Type;
   function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type) return Observation_Type;
   function Step(Env : in out Environment_Type; Action: Action_Type) return Step_Return_Type;
   procedure Render_Text(Env : Environment_Type);

   type Discrete_State_Type is new Natural range 0 .. ((Lot_Size + 1) * (Lot_Size + 1) - 1);
   type DP_Model_Type is array (Discrete_State_Type, Action_Type, Discrete_State_Type) of Transition_Probability_Type;
   type DP_Model_Access_Type is access DP_Model_Type;
   DM_Pool : System.Pool_Local.Unbounded_Reclaim_Pool;
   for DP_Model_Access_Type'Storage_Pool use DM_Pool;
   function Get_Model(Config: Config_Type) return DP_Model_Access_Type;

   type Transition_Array_Type is array (Discrete_State_Type) of Transition_Probability_Type;
   function Get_Transition_Values (Config: Config_Type; State: Discrete_State_Type; Action: Action_Type) return Transition_Array_Type;
   function Get_Transition_Values2 (Config: Config_Type; State: Discrete_State_Type; Action: Action_Type) return Transition_Array_Type;

private
   type Environment_Type is record
      Gen: Float_Random.Generator;
      Config : Config_Type;
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
   -- These functions follow similar methods in the Python implementation of FrozenLakeEnv.
   -- We make these private since they are not intended to be used directly.
   function To_Discrete_State(Cars_Per_Lot : Cars_Per_Lot_Type) return Discrete_State_Type;
   function From_Discrete_State (D : Discrete_State_Type) return Cars_Per_Lot_Type;
   -- The following are for calculating the transition probabilities for the
   -- dynamic programming model.
   function Calculate_Transition_Probability (Config : Config_Type; Cars_Moved : Natural; Prev_Cars : Cars_Per_Lot_Type; Next_Cars : Cars_Per_Lot_Type) return Transition_Probability_Type;
   function Step_Cars(Cars_Count : Cars_Per_Lot_Type; Action : Action_Type) return Cars_After_Action_Type;
   function Calculate_Transition_Probability2 (Config : Config_Type; Cars_Moved : Natural; Prev_Cars : Cars_Per_Lot_Type) return Transition_Array_Type;

end RL.Envs.Carrental;

