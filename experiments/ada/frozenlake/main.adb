with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;
with Frozen_Lake; use Frozen_Lake;

procedure Main is
    package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
    Gen: Action_Random.Generator;
    Env : Environment_State(4, 4) := Make(Environment_Config'(Map_Name => Map_4x4, Slippery => False));
    Obs: Observation_Type;
    Action: Action_Type;
    Step_Output: Step_Return_Type;
    I: Integer;
    Total_Reward: Float := 0.0;
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
end Main;
