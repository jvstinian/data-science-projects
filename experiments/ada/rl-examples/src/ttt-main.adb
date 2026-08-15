with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Float_Text_IO;
-- with Ada.Numerics.Discrete_Random;
with Tic_Tac_Toe; use Tic_Tac_Toe;

procedure Main is
   -- package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
   -- Gen: Action_Random.Generator;
   -- Env : Environment_State(4, 4) := Make(Environment_Config'(Map_Name => Map_4x4, Slippery => False));
   -- Obs: Observation_Type;
   -- Action: Action_Type;
   -- Step_Output: Step_Return_Type;
   -- I: Integer;
   -- Total_Reward: Float := 0.0;
   Board : Board_Type := (others => (others => No_Mark));
   Env : State_Type := Initial_State;
begin
   -- Action_Random.Reset(Gen);

   Put_Line("Initializing Tic Tac Toe environment");
   Print_State(Env);

   declare
      Valid_Actions : constant Valid_Actions_Type := Get_Valid_Actions(Env);
   begin
      for I in Valid_Actions'Range loop
         Put_Line("Valid Action: Row " & Valid_Actions(I).Row'Image & " Col " & Valid_Actions(I).Col'Image);
      end loop;
   end;

   -- for C in Col_Label loop
   --    Put(Character(C));
   --    Put(" position: " & Col_Label'Pos(C)'Image);
   --    New_Line;
   -- end loop;
   for C in Col_Label loop
      Put((C'Image));
      Put(" position: " & Col_Label'Pos(C)'Image);
      New_Line;
   end loop;

   Put_Line("First column label: " & Col_Label'Image(Col_Label'First));

   Env := Step(Env, Action_Type'(Row => 1, Col => B));
   -- For a draw
   -- Env := Step(Env, Action_Type'(Row => 2, Col => C));
   -- Env := Step(Env, Action_Type'(Row => 2, Col => A));
   -- Env := Step(Env, Action_Type'(Row => 0, Col => C));
   -- Env := Step(Env, Action_Type'(Row => 1, Col => C));
   -- Env := Step(Env, Action_Type'(Row => 1, Col => A));
   -- Env := Step(Env, Action_Type'(Row => 0, Col => B));
   -- Env := Step(Env, Action_Type'(Row => 2, Col => B));
   -- Env := Step(Env, Action_Type'(Row => 0, Col => A));
   -- X wins
   -- Env := Step(Env, Action_Type'(Row => 1, Col => C));
   -- Env := Step(Env, Action_Type'(Row => 0, Col => A));
   -- Env := Step(Env, Action_Type'(Row => 2, Col => C));
   -- Env := Step(Env, Action_Type'(Row => 0, Col => C));
   -- Env := Step(Env, Action_Type'(Row => 0, Col => B));
   -- Env := Step(Env, Action_Type'(Row => 2, Col => A));
   -- O wins
   Env := Step(Env, Action_Type'(Row => 2, Col => C));
   Env := Step(Env, Action_Type'(Row => 0, Col => B));
   Env := Step(Env, Action_Type'(Row => 2, Col => B));
   Env := Step(Env, Action_Type'(Row => 0, Col => C));
   Env := Step(Env, Action_Type'(Row => 2, Col => A));
   Print_State(Env);
    -- Obs:= Reset(Env);
    -- Obs:= Reset(Env);
    -- Put_Line("Initial Position Index: " & Obs.Position_Index'Image);
    -- I := 0;
    -- loop
    --    Action := Action_Random.Random(Gen);
    --    Put_Line("Calling Step with action " & Action'Image);
    --    Step_Output := Step(Env, Action);
    --    Obs := Step_Output.State;
    --    Put_Line("New Position Index: " & Obs.Position_Index'Image);
    --    Total_Reward := Total_Reward + Step_Output.Reward;
    --    I := I + 1;
    --    -- Print the reward for this step
    --    Put("Step" & I'Image & " Reward: ");
    --    Ada.Float_Text_IO.Put(Item => Step_Output.Reward, Fore => 2, Aft => 2, Exp => 0);
    --    New_Line;
    --    Render_Text(Env);
    --    if Step_Output.Terminated then
    --        Put("Total Reward: ");
    --        Ada.Float_Text_IO.Put(Item => Total_Reward, Fore => 2, Aft => 2, Exp => 0);
    --        New_Line;
    --        Put_Line("Terminated: " & Step_Output.Terminated'Image);
    --        exit;
    --    end if;
    -- end loop;
end Main;
