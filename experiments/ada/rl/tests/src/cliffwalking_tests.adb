with AUnit.Assertions; use AUnit.Assertions;
with RL; use RL;
with RL.Envs.Cliffwalking; use RL.Envs.Cliffwalking;
with RL.Algorithms.Random_Actions;
with Ada.Text_IO; use Ada.Text_IO;

package body Cliffwalking_Tests is

   overriding function Name (T : Cliffwalking_Test_Case) return Test_String is
     (Format ("Cliffwalking Tests       "));

   overriding procedure Register_Tests (T : in out Cliffwalking_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Cliffwalking_Random_Actions'Access, "Test Cliffwalking environment terminates eventually with random actions");
      Register_Routine
        (T, Test_Cliffwalking_DP_Model'Access, "Test Cliffwalking DP model transition probabilities");
   end Register_Tests;

   procedure Test_Cliffwalking_Random_Actions (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Config: Config_Type := Config_Type'(Is_Slippery => False);

      function Get_Observation (Step_Return : Step_Return_Type) return Observation_Type is (Step_Return.Observation);
      function Get_Reward(Step_Return : Step_Return_Type) return Float is (Step_Return.Reward);
      function Get_Terminated_Flag(Step_Return : Step_Return_Type) return Boolean is (Step_Return.Terminated);
      -- Note in the following we set the seed for reproducibility of the test
      package Cliffwalking_Random_Actions is new RL.Algorithms.Random_Actions(
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
        Sim_Summary : Cliffwalking_Random_Actions.Simulation_Summary;
       
    begin
        Sim_Summary := Cliffwalking_Random_Actions.Uniform_Random_Actions (Config, False);
        Put_Line("Number of steps taken: " & Sim_Summary.Num_Steps'Image);
        Assert (Sim_Summary.Num_Steps <= 3500, "Steps exceeded 3500");
        Assert (Sim_Summary.Total_Reward <= -Float(Sim_Summary.Num_Steps), "Reward exceeds negative of number of steps");
    end Test_Cliffwalking_Random_Actions;
   
    procedure Test_Cliffwalking_DP_Model (T : in out Test_Case'Class) is
        Config: Config_Type := Config_Type'(Is_Slippery => False);
        DP_Model : DP_Model_Type := Get_Model(Config);
        
        Total_Transition_Prob : Float;

        function Is_Approx_Equal(X, Y : Float; Epsilon : Float := 1.0e-6) return Boolean is
        begin
            return abs(X - Y) <= Epsilon;
        end Is_Approx_Equal;
    begin 
        for S in State_Type loop
            for A in Action_Type loop
                Total_Transition_Prob := 0.0;
                for S1 in State_Type loop
                    Total_Transition_Prob := Total_Transition_Prob + DP_Model(S, A, S1).Probability;
                end loop;
                Assert (
                    Is_Approx_Equal(Total_Transition_Prob, 1.0),
                    "Transition probabilities for State " & S'Image
                    & " and Action " & A'Image & " do not sum to 1.0"
                );
            end loop;
        end loop;
    end Test_Cliffwalking_DP_Model;
end Cliffwalking_Tests;
