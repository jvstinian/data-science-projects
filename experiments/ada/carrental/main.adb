with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;
with Car_Rental; use Car_Rental;
with Generic_Random_Functions;

procedure Main is
   -- package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
   -- Gen: Action_Random.Generator;
   Config : Environment_Config := Default_Config;
   Env : Environment_State := Make(Config);
   Obs: Observation_Type;
   Action: Action_Type;
   Step_Output: Step_Return_Type;
   I: Integer;
   Total_Reward: Float := 0.0;

   -- DP_Model : Discrete_Model_Type := Discrete_Model_Type'(others => (others => (others => (Probability => 0.0, Reward => 0.0))));
   DP_Model : Discrete_Model_Access_Type := Get_Model(Config);
   -- type Discrete_Model_Access_Type is access Discrete_Model_Type;
   -- DP_Model_Access : Discrete_Model_Access_Type := new Discrete_Model_Type'(others => (others => (others => (Probability => 0.0, Reward => 0.0))));
   
   Transition_Values : Transition_Array_Type;
   Prob_Check : Float := 0.0;
begin
   -- Action_Random.Reset(Gen);
   Put_Line("Initializing Car Rental environment");
   Obs:= Reset(Env);
   Render_Text(Env);
    I := 0;
    loop
       -- Action := Action_Random.Random(Gen);
       Action := 0; -- Always take action 0 for testing
       Put_Line("Calling Step with action " & Action'Image);
       Step_Output := Step(Env, Action);
       Obs := Step_Output.Observation;
       -- Put_Line("New Position Index: " & Obs.Position_Index'Image);
       Total_Reward := Total_Reward + Step_Output.Reward;
       I := I + 1;
       -- Print the reward for this step
       Put("Step" & I'Image & " Reward: ");
       Ada.Float_Text_IO.Put(Item => Step_Output.Reward, Fore => 2, Aft => 2, Exp => 0);
       New_Line;
       Render_Text(Env);
       if Step_Output.Terminated or I >= 5 then
           Put("Total Reward: ");
           Ada.Float_Text_IO.Put(Item => Total_Reward, Fore => 2, Aft => 2, Exp => 0);
           New_Line;
           Put_Line("Terminated: " & Step_Output.Terminated'Image);
           exit;
       end if;
    end loop;

   -- DP_Model_Access.all := Get_Model(Config);
    -- for S0 in Discrete_State_Type loop
    --    for A in Action_Type loop
    --       for S1 in Discrete_State_Type loop
    --          Put_Line ("Transition from state " & S0'Image & " to state " & S1'Image & " with action " & A'Image &
    --                    ": Probability = " & DP_Model(S0, A, S1).Probability'Image &
    --                    ", Reward = " & DP_Model(S0, A, S1).Reward'Image);
    --       end loop;
    --    end loop;
    -- end loop;

    Transition_Values := Get_Transition_Values(Config, 1*21 + 1, 1);
    for S2 in Discrete_State_Type loop
        Put_Line ("Transition from state 22 to state " & S2'Image & " with action 1: Probability = " &
                  Transition_Values(S2).Probability'Image &
                  ", Reward = " & Transition_Values(S2).Reward'Image);
        Prob_Check := Prob_Check + Transition_Values(S2).Probability;
    end loop;
    Put_Line("Total Probability Check: " & Prob_Check'Image);

    Put_Line ("Transition from state 22 " & " to state 440 " & " with action 1 " &
                       ": Probability = " & DP_Model(22, 1, 440).Probability'Image &
                       ", Reward = " & DP_Model(22, 1, 440).Reward'Image);
   
    Prob_Check := 0.0;
    Transition_Values := Get_Transition_Values2(Config, 1*21 + 1, 1);
    for S2 in Discrete_State_Type loop
        Put_Line ("Transition (2nd approach) from state 22 to state " & S2'Image & " with action 1: Probability = " &
                  Transition_Values(S2).Probability'Image &
                  ", Reward = " & Transition_Values(S2).Reward'Image);
        Prob_Check := Prob_Check + Transition_Values(S2).Probability;
    end loop;
    Put_Line("Total Probability Check (2nd approach): " & Prob_Check'Image);

end Main;
