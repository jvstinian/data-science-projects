with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
-- with Ada.Integer_Text_IO; -- use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Blackjack; use Blackjack;

procedure Main is
    package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
    Gen: Action_Random.Generator;
    Env : Environment_Type := Make(Config_Type'(Natural_Win_Reward => SAB, Auto_Hit => False));
    Obs: Observation_Type;
    Action: Action_Type;
    Step_Output: Step_Return_Type;
    I: Integer;
    Total_Reward: Float := 0.0;
begin
   Put_Line("Hello, Blackjack!");
   Action_Random.Reset(Gen);

   Put_Line("Initializing Blackjack environment");
   Obs := Reset(Env);
   Render_Text(Env);
   I := 0;
   loop
       Action := Action_Random.Random(Gen);
       Put_Line("Step" & I'Image & ": using action " & Action'Image);
       Step_Output := Step(Env, Action);
       Obs := Step_Output.Observation;
       Total_Reward := Total_Reward + Step_Output.Reward;
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
       I := I + 1;
    end loop;
end Main;

