with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Envs.Frozenlake.Child;

procedure Main is
    package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
    Gen: Action_Random.Generator;
    Env : Environment_State(4, 4) := Make(Environment_Config'(Map_Name => Map_4x4, Slippery => False));
    Obs: Observation_Type;
    Action: Action_Type;
    Step_Output: Step_Return_Type;
    I: Integer;
    Total_Reward: Float := 0.0;

    package Frozenlake_Child is new RL.Envs.Frozenlake.Child(Map_Info => Get_Map_Info(Map_4x4));
    DP_Model : Discrete_Model_Type := Get_Model(Environment_Config'(Map_Name => Map_4x4, Slippery => False));
begin
    Action_Random.Reset(Gen);

    -- if Get_Sutton_Barto_Reward then
    --     Put_Line("Using Sutton-Barto reward");
    -- else
    --     Put_Line("Using default reward");
    -- end if;

    Put_Line("Initializing Frozen Lake environment");
    Obs:= Reset(Env);
    Put_Line("Initial Position Index: " & Obs.Position_Index'Image);
    I := 0;
    loop
       Action := Action_Random.Random(Gen);
       Put_Line("Calling Step with action " & Action'Image);
       Step_Output := Step(Env, Action);
       Obs := Step_Output.State;
       Put_Line("New Position Index: " & Obs.Position_Index'Image);
       Total_Reward := Total_Reward + Step_Output.Reward;
       I := I + 1;
       -- Print the reward for this step
       Put("Step" & I'Image & " Reward: ");
       Ada.Float_Text_IO.Put(Item => Step_Output.Reward, Fore => 2, Aft => 2, Exp => 0);
       New_Line;
       Render_Text(Env);
       if Step_Output.Terminated then
           Put("Total Reward: ");
           Ada.Float_Text_IO.Put(Item => Total_Reward, Fore => 2, Aft => 2, Exp => 0);
           New_Line;
           Put_Line("Terminated: " & Step_Output.Terminated'Image);
           exit;
       end if;
    end loop;

    Put_Line("Frozen Lake Child Get_Map_Rows: " & Frozenlake_Child.Num_Rows'Image);
    Frozenlake_Child.Dummy_Method;
    for Current_State in Discrete_State_Type loop
        for Current_Action in Action_Type loop
            for Next_State in Discrete_State_Type loop
                declare
                    Transition : Transition_Probability_Type := DP_Model(Current_State, Current_Action, Next_State);
                begin
                    if Transition.Probability > 0.0 then
                        Put_Line("From State " & Current_State'Image & " taking Action " & Current_Action'Image &
                                 " to State " & Next_State'Image & " has transition probability " &
                                 Transition.Probability'Image & " and reward " & Transition.Reward'Image);
                    end if;
                end;
            end loop;
        end loop;
    end loop;
end Main;
