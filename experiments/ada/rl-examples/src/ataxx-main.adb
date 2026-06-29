with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Float_Text_IO;
-- with Ada.Numerics.Discrete_Random;
with Ataxx; use Ataxx;

procedure Main is
   -- Board : Board_Type := (others => (others => No_Mark));
   subtype Test_Player_Type is Mark range R .. K;
   Config : Config_Type := (Player_Count => Two_Player);
   Env : State_Type := Initial_State (Config);
   Valid_Actions : Valid_Actions_Type(0 .. Board_Width * Board_Width * 24 - 1);
   Num_Valid_Actions : Natural := 0;
begin
   -- Action_Random.Reset(Gen);
   Put_Line("First test player: " & Test_Player_Type'Image(Test_Player_Type'First));
   Put_Line("Last test player: " & Test_Player_Type'Image(Test_Player_Type'Last));
   -- NOTE: The following produces X rather than the desired value R
   Put_Line("Successor of test player K is " & Test_Player_Type'Image(Test_Player_Type'Succ(Test_Player_Type'Last)));

   Put_Line("Initializing Ataxx environment");
   Print_State(Env);
   declare
      Temp_Valid_Actions : Valid_Actions_Type := Get_Valid_Actions(Env);
   begin
      Num_Valid_Actions := Temp_Valid_Actions'Length;
      Put_Line("Number of valid actions: " & Natural'Image(Num_Valid_Actions));
      Valid_Actions(0 .. Num_Valid_Actions - 1) := Temp_Valid_Actions;
   end;

   for I in 0 .. (Num_Valid_Actions - 1) loop
      Put_Line("Valid Action: Source " 
         & "(" & Valid_Actions(I).Source.Row'Image & ", " & Valid_Actions(I).Source.Col'Image & ")"
         & " -> "
         & "(" & Valid_Actions(I).Target.Row'Image & ", " & Valid_Actions(I).Target.Col'Image & ")"
      );
   end loop;

   -- Put_Line("First column label: " & Col_Label'Image(Col_Label'First));

   Env := Step(Env, Action_Type'( Source => (Row => 1, Col => 1), Target => (Row => 2, Col => 2)));
   Env := Step(Env, Action_Type'( Source => (Row => 1, Col => 7), Target => (Row => 2, Col => 6)));
   Env := Step(Env, Action_Type'( Source => (Row => 2, Col => 2), Target => (Row => 1, Col => 3)));
   Env := Step(Env, Action_Type'( Source => (Row => 2, Col => 6), Target => (Row => 3, Col => 5)));
   Env := Step(Env, Action_Type'( Source => (Row => 1, Col => 3), Target => (Row => 2, Col => 5)));
   Env := Step(Env, Action_Type'( Source => (Row => 1, Col => 7), Target => (Row => 3, Col => 6)));
   Print_State(Env);
   -- -- For a draw
   -- -- Env := Step(Env, Action_Type'(Row => 2, Col => C));
   -- -- Env := Step(Env, Action_Type'(Row => 2, Col => A));
   -- -- Env := Step(Env, Action_Type'(Row => 0, Col => C));
   -- -- Env := Step(Env, Action_Type'(Row => 1, Col => C));
   -- -- Env := Step(Env, Action_Type'(Row => 1, Col => A));
   -- -- Env := Step(Env, Action_Type'(Row => 0, Col => B));
   -- -- Env := Step(Env, Action_Type'(Row => 2, Col => B));
   -- -- Env := Step(Env, Action_Type'(Row => 0, Col => A));
   -- -- X wins
   -- -- Env := Step(Env, Action_Type'(Row => 1, Col => C));
   -- -- Env := Step(Env, Action_Type'(Row => 0, Col => A));
   -- -- Env := Step(Env, Action_Type'(Row => 2, Col => C));
   -- -- Env := Step(Env, Action_Type'(Row => 0, Col => C));
   -- -- Env := Step(Env, Action_Type'(Row => 0, Col => B));
   -- -- Env := Step(Env, Action_Type'(Row => 2, Col => A));
   -- -- O wins
   -- Env := Step(Env, Action_Type'(Row => 2, Col => C));
   -- Env := Step(Env, Action_Type'(Row => 0, Col => B));
   -- Env := Step(Env, Action_Type'(Row => 2, Col => B));
   -- Env := Step(Env, Action_Type'(Row => 0, Col => C));
   -- Env := Step(Env, Action_Type'(Row => 2, Col => A));
   -- Print_State(Env);
end Main;
