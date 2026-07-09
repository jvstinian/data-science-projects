with AUnit.Assertions; use AUnit.Assertions;
with RL; use RL;
with RL.Envs.Blackjack; use RL.Envs.Blackjack;
with RL.Algorithms.Random_Actions;
with Ada.Text_IO; use Ada.Text_IO;

package body Blackjack_Tests is

   overriding function Name (T : Blackjack_Test_Case) return Test_String is
     (Format ("Blackjack Tests          "));

   overriding procedure Register_Tests (T : in out Blackjack_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Blackjack_Random_Actions'Access
        , "Test Blackjack environment terminates eventually "
          & "with random actions");
   end Register_Tests;

   procedure Test_Blackjack_Random_Actions (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Config: Config_Type := Config_Type'(
         Natural_Win_Reward => SAB,
         Auto_Hit => True
      );

      function Get_Observation (Step_Return : Step_Return_Type) return Observation_Type is (Step_Return.Observation);
      function Get_Reward(Step_Return : Step_Return_Type) return Float is (Step_Return.Reward);
      function Get_Terminated_Flag(Step_Return : Step_Return_Type) return Boolean is (Step_Return.Terminated);
      -- Note in the following we set the seed for reproducibility of the test
      package Blackjack_Random_Actions is new RL.Algorithms.Random_Actions(
           Config_Type => Config_Type,
           Environment_Type => Environment_Type,
           Observation_Type => Observation_Type,
           Action_Type => Action_Type,
           Step_Return_Type => Step_Return_Type,
           Make => Make,
           Reset => Reset,
           Step => Step,
           Get_Observation => Get_Observation,
           Get_Reward => Get_Reward,
           Get_Terminated_Flag => Get_Terminated_Flag,
           Seed_Reset => Seed_Reset_Type'(Kind => Set_Seed, Seed => 123)
        );
        Sim_Summary : Blackjack_Random_Actions.Simulation_Summary;
       
    begin
        Sim_Summary := Blackjack_Random_Actions.Uniform_Random_Actions (Config, False);
        Put_Line("Number of steps taken: " & Sim_Summary.Num_Steps'Image);
        Assert (Sim_Summary.Num_Steps <= 3500, "Steps exceeded 3500");
        Assert (Sim_Summary.Total_Reward <= -Float(Sim_Summary.Num_Steps), "Reward exceeds negative of number of steps");
    end Test_Blackjack_Random_Actions;
end Blackjack_Tests;
