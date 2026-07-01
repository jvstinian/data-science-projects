with AUnit.Assertions; use AUnit.Assertions;
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Algorithms.Random_Actions;

package body Frozenlake_Tests is

   overriding function Name (T : Frozenlake_Test_Case) return Test_String is
     (Format ("Frozenlake Tests         "));

   overriding procedure Register_Tests (T : in out Frozenlake_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Frozenlake_Random_Actions'Access, "Test Frozenlake environment terminates evantually with random actions");
   end Register_Tests;

   procedure Test_Frozenlake_Random_Actions (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Config: Environment_Config := Environment_Config'(Map_Name => Map_4x4, Slippery => False);

      function Get_Observation (Step_Return : Step_Return_Type) return Observation_Type is (Step_Return.Observation);
      function Get_Reward(Step_Return : Step_Return_Type) return Float is (Step_Return.Reward);
      function Get_Terminated_Flag(Step_Return : Step_Return_Type) return Boolean is (Step_Return.Terminated);
       package Frozenlake_Random_Actions is new RL.Algorithms.Random_Actions(
           Config_Type => Environment_Config,
           Environment_Type => Environment_State,
           Observation_Type => Observation_Type,
           Action_Type => Action_Type,
           Step_Return_Type => Step_Return_Type,
           Make => Make,
           Reset => Reset,
           Step => Step,
           Get_Observation => Get_Observation,
           Get_Reward => Get_Reward,
           Get_Terminated_Flag => Get_Terminated_Flag
        );
        Sim_Summary : Frozenlake_Random_Actions.Simulation_Summary;
       
    begin
        Sim_Summary := Frozenlake_Random_Actions.Uniform_Random_Actions (Config, False);
        Assert (Sim_Summary.Num_Steps <= 20, "Steps exceeded 20");
        Assert (Sim_Summary.Total_Reward <= 1.0, "Reward exceeds 1.0");
    end Test_Frozenlake_Random_Actions;

end Frozenlake_Tests;

