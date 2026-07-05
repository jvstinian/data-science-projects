with AUnit.Assertions; use AUnit.Assertions;
with RL; use RL;
with RL.Envs.Carrental; use RL.Envs.Carrental;
with Ada.Numerics.Discrete_Random;

package body Carrental_Tests is

   overriding function Name (T : Carrental_Test_Case) return Test_String is
     (Format ("Carrental Tests          "));

   overriding procedure Register_Tests (T : in out Carrental_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Carrental_Random_Action'Access
        , "Test Carrental environment using a random action");
      --  Register_Routine
      --    (T, Test_Carrental_DP_Model'Access
      --    , "Test Carrental DP model transition probabilities");
   end Register_Tests;

   procedure Test_Carrental_Random_Action
      (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      package Action_Random is new Ada.Numerics.Discrete_Random (
         Result_Subtype => Action_Type);
      Gen : Action_Random.Generator;

      Config : constant Config_Type := Default_Config;
      Env : Environment_Type := Make (Config);
      Seed_Reset : constant Seed_Reset_Type := Seed_Reset_Type'(
         Kind => Set_Seed, Seed => 123);

      Action : Action_Type;
      Temp_Obs : Observation_Type;
      Step_Output : Step_Return_Type;
   begin
         case Seed_Reset.Kind is
            when Set_Default => Action_Random.Reset (Gen);
            when No_Set      => null;
            when Set_Seed    => Action_Random.Reset (Gen, Seed_Reset.Seed);
         end case;

         Temp_Obs := Reset (Env, Seed_Reset);
         Action := Action_Random.Random (Gen);
         Step_Output := Step (Env, Action);
         Temp_Obs := Step_Output.Observation;

         Assert (Step_Output.Terminated = False
                , "Carrental environment should not terminate");
   end Test_Carrental_Random_Action;

    --  procedure Test_Carrental_DP_Model (T : in out Test_Case'Class) is
    --      Config: Config_Type := Config_Type'(Is_Slippery => False);
    --      DP_Model : DP_Model_Type := Get_Model(Config);
    --
    --      Total_Transition_Prob : Float;

    --      function Is_Approx_Equal(X, Y : Float; Epsilon : Float := 1.0e-6)
    --         return Boolean is
    --      begin
    --          return abs(X - Y) <= Epsilon;
    --      end Is_Approx_Equal;
    --  begin
    --      for S in State_Type loop
    --          for A in Action_Type loop
    --              Total_Transition_Prob := 0.0;
    --              for S1 in State_Type loop
    --                  Total_Transition_Prob := Total_Transition_Prob
   --                                         + DP_Model(S, A, S1).Probability;
    --              end loop;
    --              Assert (
    --                  Is_Approx_Equal(Total_Transition_Prob, 1.0),
    --                  "Transition probabilities for State " & S'Image
    --                  & " and Action " & A'Image & " do not sum to 1.0"
    --              );
    --          end loop;
    --      end loop;
    --  end Test_Carrental_DP_Model;
end Carrental_Tests;
