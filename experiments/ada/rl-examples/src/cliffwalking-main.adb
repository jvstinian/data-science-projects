with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;
with Cliff_Walking; use Cliff_Walking;

procedure Main is
   package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
   Gen: Action_Random.Generator;
   Env : Environment_State := Make(Environment_Config'(Is_Slippery => False));
   Obs: Observation_Type;
   Action: Action_Type;
   Step_Output: Step_Return_Type;
   I: Integer;
   Total_Reward: Float := 0.0;
begin
   Put_Line("Hello, Cliff Walker!");
   Put_Line("Initializing Cliff Walking environment");
   Obs:= Reset(Env);
   Put_Line("Initial Position Index: " & Obs'Image);
   Render_Text(Env);
   I := 0;
   loop
       Action := Action_Random.Random(Gen);
       Put_Line("Calling Step with action " & Action'Image);
       Step_Output := Step(Env, Action);
       Obs := Step_Output.Observation;
       Put_Line("New Position Index: " & Obs'Image);
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
