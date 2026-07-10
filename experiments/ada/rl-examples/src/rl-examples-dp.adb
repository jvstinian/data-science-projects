with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with RL; use RL;  -- Transition_Probability_Type
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Envs.Frozenlake.DP;
with RL.Algorithms.DP;

package body RL.Examples.DP is
    -- Frozenlake Iterative_Policy_Evaluation Example
    procedure Frozenlake_Policy_Evaluation_Example (Map_Name : Map_Type) is
        package Frozen_Lake_DP is new RL.Envs.Frozenlake.DP(Map_Name => Map_4x4);  -- TODO: Using Map_Name
        use Frozen_Lake_DP;
        DP_Model : DP_Model_Type := Get_Model(Config_Type'(Map_Name => Map_4x4, Is_Slippery => False));

        package DP_Algorithms is new RL.Algorithms.DP(
          State_Type => State_Type,
          Action_Type => Action_Type,
          DP_Model_Type => DP_Model_Type
        );
        use DP_Algorithms;

        Local_Random_Policy : Stochastic_Policy_Type := (others => (others => 0.25)); -- Uniform random policy
        Local_Value_Function : Value_Function_Type;
    begin
        Local_Value_Function := Iterative_Policy_Evaluation(DP_Model, Local_Random_Policy, 0.9);
        for S in State_Type loop
            Put("Value of State " & S'Image & " under uniformly random policy: ");
            Ada.Float_Text_IO.Put(Item => Local_Value_Function(S), Fore => 1, Aft => 4, Exp => 0);
            New_Line;
        end loop;
    end Frozenlake_Policy_Evaluation_Example;
    
   procedure Frozenlake_Policy_Iteration_Example (Map_Name : Map_Type; Is_Slippery : Boolean) is
      package Frozen_Lake_DP is new RL.Envs.Frozenlake.DP(Map_Name => Map_Name);
      use Frozen_Lake_DP;
      Config : Config_Type := Config_Type'(Map_Name => Map_Name, Is_Slippery => Is_Slippery);
      DP_Model : DP_Model_Type := Get_Model(Config);
   
      package Frozen_Lake_Algs is new RL.Algorithms.DP(
         State_Type => State_Type,
         Action_Type => Action_Type,
         DP_Model_Type => DP_Model_Type
      );
      use Frozen_Lake_Algs;

      Optimal_Policy : Deterministic_Policy_Type;
   begin
      Optimal_Policy := Policy_Iteration(DP_Model, 0.9);
      Print_Policy(Optimal_Policy);
   end Frozenlake_Policy_Iteration_Example;
end RL.Examples.DP;
