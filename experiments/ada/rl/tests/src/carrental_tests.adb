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
      Register_Routine
         (T, Test_Carrental_DP_Model'Access
         , "Test Carrental DP model transition probabilities");
      Register_Routine
        (T, Test_Carrental_Transition_Values'Access
        , "Test Carrental transition values using two different approaches");
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

   procedure Test_Carrental_DP_Model (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Config : constant Config_Type := Default_Config;
      DP_Model : constant DP_Model_Access_Type := Get_Model (Config);

      Total_Transition_Prob : Float;

      function Is_Approx_Equal (X, Y : Float; Epsilon : Float := 1.0e-6)
         return Boolean is
      begin
         return abs (X - Y) <= Epsilon;
      end Is_Approx_Equal;
   begin
      for S in Discrete_State_Type loop
         for A in Action_Type loop
            Total_Transition_Prob := 0.0;
            for S1 in Discrete_State_Type loop
               Total_Transition_Prob := Total_Transition_Prob
                                        + DP_Model (S, A, S1).Probability;
            end loop;
            Assert (
               Is_Approx_Equal (Total_Transition_Prob, 1.0),
               "Transition probabilities for State " & S'Image
               & " and Action " & A'Image & " do not sum to 1.0"
            );
         end loop;
      end loop;
   end Test_Carrental_DP_Model;

   procedure Test_Carrental_Transition_Values
      (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Config : constant Config_Type := Default_Config;

      Collected_Transitions : Transition_Array_Type;
      Transitions_From_State : Transition_Array_Type;

      function Is_Approx_Equal (X, Y : Float; Epsilon : Float := 1.0e-6)
         return Boolean is
      begin
         return abs (X - Y) <= Epsilon;
      end Is_Approx_Equal;
   begin
      for S in Discrete_State_Type loop
         for A in Action_Type loop
            Collected_Transitions := Collect_Transition_Values (Config, S, A);
            Transitions_From_State := Get_Transition_Values_From_State (
               Config, S, A
            );
            for S2 in Collected_Transitions'Range loop
               Assert (
                  Is_Approx_Equal (
                     Collected_Transitions (S2).Probability,
                     Transitions_From_State (S2).Probability,
                     Epsilon => 1.0e-7
                  ),
                  "Transition probabilities do not match for State "
                       & S'Image & ", Action " & A'Image & ", Next State "
                       & S2'Image
                       & ". Collected: "
                       & Float'Image (Collected_Transitions (S2).Probability)
                       & ", From State: "
                       & Float'Image (Transitions_From_State (S2).Probability)
               );
               --  Ideally We'd prefer the expected reward difference to be
               --  orders of magnitude below 0.001, but given the
               --  probability weights we do encounter differnces larger than
               --  0.0001.
               Assert (
                  Is_Approx_Equal (
                     Collected_Transitions (S2).Reward,
                     Transitions_From_State (S2).Reward,
                     Epsilon => 1.0e-3
                  ),
                  "Expected rewards do not match for State "
                       & S'Image & ", Action " & A'Image & ", Next State "
                       & S2'Image
                       & ". Collected: "
                       & Float'Image (Collected_Transitions (S2).Reward)
                       & ", From State: "
                       & Float'Image (Transitions_From_State (S2).Reward)
              );
            end loop;
         end loop;
      end loop;
   end Test_Carrental_Transition_Values;

end Carrental_Tests;
