with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body RL.Envs.Ataxx is
   function Initial_State(Config : Config_Type) return State_Type is
       Player_Indicators : Player_Indicator_Type := (others => False);
       Board : Board_Type := (others => (others => No_Mark));
       Scores : Game_Score_Type := (others => 0);
   begin
       case Config.Player_Count is
           when Two_Player =>
               Player_Indicators(R) := True;
               Player_Indicators(B) := True;
               Board(1, 1) := R;
               Board(Board_Width, Board_Width) := R;
               Board(1, Board_Width) := B;
               Board(Board_Width, 1) := B;
               Scores(R) := 2;
               Scores(B) := 2;
           when Four_Player =>
               Player_Indicators(R) := True;
               Player_Indicators(B) := True;
               Player_Indicators(W) := True;
               Player_Indicators(K) := True;
               Board(1, 1) := R;
               Board(Board_Width, 1) := B;
               Board(Board_Width, Board_Width) := W;
               Board(1, Board_Width) := K;
               Scores(R) := 1;
               Scores(B) := 1;
               Scores(W) := 1;
               Scores(K) := 1;
       end case;
       return State_Type'(Player_Indicators, Board, Active, Scores, R);
   end Initial_State;
    
   function Is_Terminal (State : State_Type) return Boolean is
      function Board_Full(Board : Board_Type) return Boolean is
      begin
         for I in Axis_Label loop
            for J in Axis_Label loop
               if Board(I, J) = No_Mark then
                  return False;
               end if;
            end loop;
         end loop;
         return True;
      end Board_Full;

      Players_Can_Move : Player_Indicator_Type := Can_Move(State.Board);
   begin
      -- NOTE: The Board_Full check might not be necessary, as
      --       the Players_Can_Move values should be sufficient, but
      --       we include it for completeness.
      if Board_Full(State.Board) then
         return True;
      end if;

      for P in Player_Type loop
         if State.Player_Indicators(P) and then Players_Can_Move(P) then
            -- At least one active player can move, so not terminal
            return False;  
         end if;
      end loop;

      return True;  -- No active players can move, so terminal
   end Is_Terminal;

   function Get_Player(State : State_Type) return Player_Type is 
   begin
      return (State.Current_Player);
   end Get_Player;
    
   function Step(State : State_Type; Action : Action_Type) return State_Type is
      -- Helper functions
      function Player_To_Mark(Player : Player_Type) return Mark is
      begin
         case Player is
            when R => return R;
            when B => return B;
            when W => return W;
            when K => return K;
         end case;
      end Player_To_Mark;

      function Next_Player_In_Ring(P : Player_Type) return Player_Type is
         Res : Player_Type := P;
      begin
         loop 
            if Res = Player_Type'Last then
               Res := Player_Type'First;
            else
               Res := Player_Type'Succ(Res);
            end if;

            exit when State.Player_Indicators(Res);  -- Found the next active player
         end loop;
         return Res;
      end Next_Player_In_Ring;

      -- Helper values
      Source : Cell_Indices := Action.Source;
      Target : Cell_Indices := Action.Target;
      Dist : Integer := Distance(Source, Target);
      P_Mark : constant Mark := Player_To_Mark(State.Current_Player);

      -- Local variables
      I0, I1, J0, J1 : Axis_Label;
      Overwritten_Player : Player_Type;
      Next_Player : Player_Type;
      Stop_Player : Player_Type;
      Players_Can_Move : Player_Indicator_Type := (others => False);

      -- Return value
      Res : State_Type := State;  -- Start with a copy of the current state to modify
   begin
      if Res.Board(Source.Row, Source.Col) = P_Mark and Res.Board(Target.Row, Target.Col) = No_Mark then
         Res.Board(Target.Row, Target.Col) := P_Mark;
         Res.Scores(Res.Current_Player) := Res.Scores(Res.Current_Player) + 1;

         if Dist = 2 then
            Res.Board(Source.Row, Source.Col) := No_Mark;  -- Remove piece if it's a jump
            Res.Scores(Res.Current_Player) := Res.Scores(Res.Current_Player) - 1;
         end if;

         I0 := Integer'Max(Res.Board'First(1), Target.Row - 1);
         I1 := Integer'Min(Res.Board'Last(1), Target.Row + 1);
         J0 := Integer'Max(Res.Board'First(2), Target.Col - 1);
         J1 := Integer'Min(Res.Board'Last(2), Target.Col + 1);

         for I in I0 .. I1 loop
            for J in J0 .. J1 loop
               if not (I = Target.Row and J = Target.Col) and then not (Res.Board(I, J) in P_Mark | X | No_Mark) then
                  Overwritten_Player := Player_Type'Val(Mark'Pos(Res.Board(I, J)));
                  Res.Board(I, J) := P_Mark;  -- Convert adjacent pieces to current player's mark
                  Res.Scores(Overwritten_Player) := Res.Scores(Overwritten_Player) - 1;
                  Res.Scores(Res.Current_Player) := Res.Scores(Res.Current_Player) + 1;
               end if;
            end loop;
         end loop;
      end if; -- else do nothing if the source cell doesn't match the current player

      Players_Can_Move := Can_Move(Res.Board);
      -- Determine the game status and next player
      Next_Player := Next_Player_In_Ring(Res.Current_Player);
      Stop_Player := Next_Player;
      while not Players_Can_Move(Next_Player) loop
         Next_Player := Next_Player_In_Ring(Next_Player);
         exit when Next_Player = Stop_Player;  -- We've looped through all players and none can move
      end loop;
      Res.Current_Player := Next_Player; -- Set next player
      if not Players_Can_Move(Next_Player) then
         Res.Status := Finished;  -- No players can move, so game is finished
      end if;

      return Res;
   end Step;
  
   function Reward(Player: Player_Type; State : State_Type) return Float is
   begin
      -- We return the number of occupied cells regardless of whether the
      -- game is finished.
      return Float(State.Scores(Player));
   end Reward;

   function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type is
      Player : Player_Type := State.Current_Player;
      Player_Mark : Mark := Mark'Val(Player_Type'Pos(Player));
      Max_Actions : constant Natural := State.Scores(Player) * 24;
      Valid_Actions : Valid_Actions_Type(0 .. (Max_Actions-1));
      Next_Index : Natural := 0;

      -- Temp variables for loop below
      Source : Cell_Indices;
      I0, I1, J0, J1 : Axis_Label;
   begin
      for I in Axis_Label loop
         for J in Axis_Label loop
            if State.Board(I, J) = Player_Mark then
               Source := (Row => I, Col => J);
               I0 := Integer'Max(State.Board'First(1), I - 2);
               I1 := Integer'Min(State.Board'Last(1), I + 2);
               J0 := Integer'Max(State.Board'First(2), J - 2);
               J1 := Integer'Min(State.Board'Last(2), J + 2);
               for I_Target in I0 .. I1 loop
                  for J_Target in J0 .. J1 loop
                     if not (I_Target = I and J_Target = J) and State.Board(I_Target, J_Target) = No_Mark then
                        Valid_Actions(Next_Index) := Action_Type'(
                           Source => Source,
                           Target => (Row => I_Target, Col => J_Target)
                        );
                        Next_Index := Next_Index + 1;
                     end if;
                  end loop;
               end loop;
            end if;
         end loop;
      end loop;
      if Next_Index > 0 then
         return Valid_Actions(0 .. Next_Index - 1);
      else
         return Valid_Actions(1 .. 0);
      end if;
   end Get_Valid_Actions;

   procedure Print_State (State : State_Type) is
      procedure Print_Board is
         Mark_Char : Character;
      begin
         Put(" ");
         for Col in Axis_Label loop
            Put(Col, Width => 0); -- Print column label as integer
         end loop;
         New_Line;

         for Row in Axis_Label loop
            Put(Row, Width => 0); -- Print row label as integer
            for Col in Axis_Label loop
               case State.Board(Row, Col) is
                  when R => Put('R');
                  when B => Put('B');
                  when W => Put('W');
                  when K => Put('K');
                  when X => Put('X');
                  when No_Mark => Put(' ');
               end case;
            end loop;
            New_Line;
         end loop;
      end Print_Board;
   
      procedure Print_Game_Status(State : State_Type) is
      begin
         case State.Status is
            when Active => Put("Next Player: " & State.Current_Player'Image);
            when Finished => Put("Game Over: TBD");  -- TODO: Print winner or draw status
         end case;
         New_Line;

         Put_Line("Scores: ");
         for P in Player_Type loop
            if State.Player_Indicators(P) then
               Put("   " & P'Image & ": " & State.Scores(P)'Image);
               New_Line;
            end if;
         end loop;
      end Print_Game_Status;
    begin
       Print_Board;
       Print_Game_Status(State);
    end Print_State;
    
   function Distance(From : Cell_Indices; To : Cell_Indices) return Integer is
   begin
      return Integer'Max(Abs(From.Row - To.Row), Abs(From.Col - To.Col));
   end Distance;
    
   function Can_Move(Board : Board_Type) return Player_Indicator_Type is
      function Available_Move_From(Row : Axis_Label; Col : Axis_Label) return Boolean is
         I0 : Axis_Label := Integer'Max(Board'First(1), Row - 2);
         I1 : Axis_Label := Integer'Min(Board'Last(1), Row + 2);
         J0 : Axis_Label := Integer'Max(Board'First(2), Col - 2);
         J1 : Axis_Label := Integer'Min(Board'Last(2), Col + 2);
      begin
         for I in I0 .. I1 loop
            for J in J0 .. J1 loop
               if not (I = Row and J = Col) and Board(I, J) = No_Mark then
                  return True;
               end if;
            end loop;
         end loop;
         return False;
      end Available_Move_From;

      Players_Can_Move : Player_Indicator_Type := (others => False);
      Temp_Mark : Mark;
   begin
      for I in Axis_Label loop
         for J in Axis_Label loop
            Temp_Mark := Board(I, J);
            if Temp_Mark in R | B | W | K and then Available_Move_From(I, J) then
               case Temp_Mark is
                  when R => Players_Can_Move(R) := True;
                  when B => Players_Can_Move(B) := True;
                  when W => Players_Can_Move(W) := True;
                  when K => Players_Can_Move(K) := True;
                  when others => null;
               end case;
            end if;
         end loop;
      end loop;
      return Players_Can_Move;
   end Can_Move;
end RL.Envs.Ataxx;
