with AUnit.Assertions; use AUnit.Assertions;
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Envs.Frozenlake.DP;
with RL.Algorithms.Random_Actions;

package body Frozenlake_Tests is

   overriding function Name (T : Frozenlake_Test_Case) return Test_String is
     (Format ("Frozenlake Tests         "));

   overriding procedure Register_Tests (T : in out Frozenlake_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Frozenlake_Random_Actions'Access, "Test Frozenlake environment terminates evantually with random actions");
      Register_Routine
        (T, Test_Frozenlake_DP_Model'Access, "Test Frozenlake DP model transition probabilities for 4x4 map");
   end Register_Tests;

   procedure Test_Frozenlake_Random_Actions (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Config: Config_Type := Config_Type'(Map_Name => Map_4x4, Is_Slippery => False);

      function Get_Observation (Step_Return : Step_Return_Type) return Observation_Type is (Step_Return.Observation);
      function Get_Reward(Step_Return : Step_Return_Type) return Float is (Step_Return.Reward);
      function Get_Terminated_Flag(Step_Return : Step_Return_Type) return Boolean is (Step_Return.Terminated);
       package Frozenlake_Random_Actions is new RL.Algorithms.Random_Actions(
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
           Get_Terminated_Flag => Get_Terminated_Flag
        );
        Sim_Summary : Frozenlake_Random_Actions.Simulation_Summary;
       
    begin
        Sim_Summary := Frozenlake_Random_Actions.Uniform_Random_Actions (Config, False);
        Assert (Sim_Summary.Num_Steps <= 20, "Steps exceeded 20");
        Assert (Sim_Summary.Total_Reward <= 1.0, "Reward exceeds 1.0");
    end Test_Frozenlake_Random_Actions;
   
    procedure Test_Frozenlake_DP_Model (T : in out Test_Case'Class) is
        Config: Config_Type := Config_Type'(Map_Name => Map_4x4, Is_Slippery => False);

        package Frozen_Lake_DP is new RL.Envs.Frozenlake.DP(Map_Name => Config.Map_Name);
        use Frozen_Lake_DP;

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
    end Test_Frozenlake_DP_Model;

end Frozenlake_Tests;
