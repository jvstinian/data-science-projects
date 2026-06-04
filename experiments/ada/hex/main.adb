with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Float_Text_IO;
-- with Ada.Numerics.Discrete_Random;
with Hex; use Hex;

procedure Main is
   -- Config : Config_Type := (Player_Count => Two_Player);
   Env : State_Type := Initial_State;
   Valid_Actions : Valid_Actions_Type(0 .. Board_Width * Board_Width - 1);
   Num_Valid_Actions : Natural := 0;
begin
   -- Action_Random.Reset(Gen);
   Put_Line("Initializing Hex environment");
   Put_Line("Blue labels:");
   for J in Blue_Label'Range loop
      Put_Line("  " & Blue_Label'Image(J));
   end loop;
   Put_Line("Red labels:");
   for J in Red_Label'Range loop
      Put_Line("  " & Red_Label'Image(J));
   end loop;
   Print_State(Env);
   declare
      Temp_Valid_Actions : Valid_Actions_Type := Get_Valid_Actions(Env);
   begin
      Num_Valid_Actions := Temp_Valid_Actions'Length;
      Put_Line("Number of valid actions: " & Natural'Image(Num_Valid_Actions));
      Valid_Actions(0 .. Num_Valid_Actions - 1) := Temp_Valid_Actions;
   end;

   -- for I in 0 .. (Num_Valid_Actions - 1) loop
   --    Put_Line("Valid Action: " 
   --       & "(" & Valid_Actions(I).Row'Image & ", " & Valid_Actions(I).Col & ")"
   --    );
   -- end loop;

   Env := Step(Env, Action_Type'(Row => 4, Col => 'D'));
   Env := Step(Env, Action_Type'(Row => 3, Col => 'C'));
   -- Env := Step(Env, Action_Type'(Row => 4, Col => 'D'));
   Print_State(Env);
end Main;
