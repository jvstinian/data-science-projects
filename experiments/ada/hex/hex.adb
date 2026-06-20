with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Hex is
   function Initial_State return State_Type is
      Player_Colors : Player_Color_Type := (Player1 => Red, Player2 => Blue);
      Board : Board_Type := (others => (others => No_Mark));
   begin
      return State_Type'(Player_Colors, Board, Active, Player1);
   end Initial_State;

   function Is_Terminal (State : State_Type) return Boolean is
   begin
      if State.Status /= Active then
         return True;  -- Game is already finished
      end if;
      return False;
   end Is_Terminal;

   function Get_Player(State : State_Type) return Player_Type is 
   begin
      return (State.Current_Player);
   end Get_Player;
   
   function Step(State : State_Type; Action : Action_Type) return State_Type is
      -- Helper functions
   --    function Next_Player_In_Ring(P : Player_Type) return Player_Type is
   --       Res : Player_Type := P;
   --    begin
   --       loop 
   --          if Res = Player_Type'Last then
   --             Res := Player_Type'First;
   --          else
   --             Res := Player_Type'Succ(Res);
   --          end if;

   --          exit when State.Player_Indicators(Res);  -- Found the next active player
   --       end loop;
   --       return Res;
   --    end Next_Player_In_Ring;

      -- Helper values
      Number_Of_Stones : constant Natural := Get_Number_Of_Stones(State);
      Player_Color : Stone_Color_Type;  -- We could set here but instead set below
      Stone : Mark;

   --    -- Local variables
   --    I0, I1, J0, J1 : Axis_Label;
   --    Overwritten_Player : Player_Type;
   --    Next_Player : Player_Type;
   --    Stop_Player : Player_Type;
   --    Players_Can_Move : Player_Indicator_Type := (others => False);

      -- Return value
      Res : State_Type := State;  -- Start with a copy of the current state to modify
   begin
      if Number_Of_Stones = 1 and then Res.Board(Action.Row, Action.Col) = Red then
         -- Swap colors
         Res.Player_Colors := (Player1 => Blue, Player2 => Red);
         Res.Current_Player := Player1;
      elsif Res.Board(Action.Row, Action.Col) = No_Mark then
         -- Color assigned to player
         Player_Color := Res.Player_Colors(Res.Current_Player);
         -- Determine the color of the stone to place based on the color assigned
         -- to the current player
         case Player_Color is
            when Red => Stone := Red;
            when Blue => Stone := Blue;
         end case;

         Res.Board(Action.Row, Action.Col) := Stone;  -- Place the stone

         case Res.Current_Player is
            when Player1 =>
               if Check_Win(Res.Board, Player_Color) then
                  Res.Status := Player1_Wins;
               else
                  Res.Current_Player := Player2;  -- Switch player
               end if;
            when Player2 =>
               if Check_Win(Res.Board, Player_Color) then
                  Res.Status := Player2_Wins;
               else
                  Res.Current_Player := Player1;  -- Switch player
               end if;
         end case;
      end if; -- else do nothing if there is already a stone in the target cell
      return Res;
   end Step;
  
   function Reward(Player: Player_Type; State : State_Type) return Reward_Type is
   begin
      case State.Status is
         when Player1_Wins =>
            case Player is
               when Player1 => return 1;
               when Player2 => return -1;
            end case;
         when Player2_Wins =>
            case Player is 
               when Player1 => return -1;
               when Player2 => return 1;
            end case;
         when others =>
             return 0;  -- Game is still active, so no reward
      end case;
   end Reward;
 
   function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type is
      function Get_Number_Of_Available_Moves (Number_Of_Stones : Natural) return Natural is
         Board_Size : constant Natural := Board_Width * Board_Width;
      begin
         if Number_Of_Stones <= 1 then
            return Board_Size;  -- Allow for Player 2 to swap
         else
            return Board_Size - Number_Of_Stones;
         end if;
      end Get_Number_Of_Available_Moves;

      Number_Of_Stones : constant Natural := Get_Number_Of_Stones(State);
      Number_Of_Available_Moves : constant Natural := Get_Number_Of_Available_Moves(Number_Of_Stones);

      Valid_Actions : Valid_Actions_Type(0 .. (Number_Of_Available_Moves-1));
      Next_Index : Natural := 0;
   begin
      for I in Blue_Label'Range loop
         for J in Red_Label'Range loop
            case State.Board(I, J) is
               when No_Mark =>
                  Valid_Actions(Next_Index) := Action_Type'(Row => I, Col => J);
                  Next_Index := Next_Index + 1;
               when Red =>
                  -- Only increment if there is exactly one stone on the board,
                  -- which we are now encountering at (I, J).
                  if Number_Of_Stones = 1 then
                     Valid_Actions(Next_Index) := Action_Type'(Row => I, Col => J);  -- Allow for Player 2 to swap
                     Next_Index := Next_Index + 1;
                  end if;
               when Blue => null;  -- Can't place on top of existing piece
            end case;
         end loop;
      end loop;

      if Next_Index > 0 then
         return Valid_Actions(0 .. Next_Index - 1);
      else
         return Valid_Actions(0 .. -1);
      end if;
   end Get_Valid_Actions;

   procedure Print_State (State : State_Type) is
      procedure Print_Board is
         Line_Width : constant Integer := 3 + 3 * Board_Width - 1;
         Line : String(1 .. Line_Width) := (others => ' ');
         -- Mark_Char : Character;

         -- Column for printing character
         Temp_Col : Positive;
         Temp_Row_Label_String : String(1 .. 2);
      begin
         for J in Red_Label'Range loop
            Temp_Col := 3 + 2 * (Red_Label'Pos(J) - Red_Label'Pos(Red_Label'First));
            Line(Temp_Col) := J; -- J is a character
         end loop;
         Put_Line(Line);

         for I in Blue_Label'Range loop
            Line := (others => ' ');
            Put(Temp_Row_Label_String, I);
            if Temp_Row_Label_String'Length = 1 then
               -- Put_Line("Temp label is of length 1");
               Line(2) := Temp_Row_Label_String(1);
            elsif Temp_Row_Label_String'Length = 2 then
               -- Put_Line("Temp label is of length 2");
               Line(1 .. 2) := Temp_Row_Label_String;
            end if;
            -- Put(Temp_Row_Label_String, I);
            Temp_Col := 4 + (Blue_Label'Pos(I) - Blue_Label'Pos(Blue_Label'First));
            for J in Red_Label'Range loop
               -- Put_Line("Temp column is " & Integer'Image(Temp_Col));
               case State.Board(I, J) is
                  when Red => Line(Temp_Col) := 'R';
                  when Blue => Line(Temp_Col) := 'B';
                  when No_Mark => Line(Temp_Col) := '*';
               end case;
               Temp_Col := Temp_Col + 2;  -- Increment to next column position
            end loop;
            Put_Line(Line);
         end loop;
      end Print_Board;

      procedure Print_Game_Status(State : State_Type) is
      begin
         case State.Status is
            when Active => Put("Next Player: " & State.Current_Player'Image);
            when Player1_Wins => Put("Player 1 won");
            when Player2_Wins => Put("Player 2 won");
         end case;
         New_Line;
      end Print_Game_Status;
   begin
      Print_Board;
      Print_Game_Status(State);
   end Print_State;
 
   function Get_Number_Of_Stones (State : State_Type) return Natural is
      Count : Natural := 0;
   begin
      for I in Blue_Label'Range loop
         for J in Red_Label'Range loop
            case State.Board(I, J) is
               when Red | Blue => Count := Count + 1;
               when No_Mark => null;
            end case;
         end loop;
      end loop;
      return Count;
   end Get_Number_Of_Stones;
   
   function Neighboring_Hexagons(I1 : Blue_Label; J1 : Red_Label; I2 : Blue_Label ; J2 : Red_Label) return Boolean is
      Blue_Is_Succ : Boolean := (I1 /= Blue_Label'Last) and then (I2 = Blue_Label'Succ(I1));
      Blue_Is_Prev : Boolean := (I1 /= Blue_Label'First) and then (I2 = Blue_Label'Pred(I1));
      Red_Is_Succ : Boolean := (J1 /= Red_Label'Last) and then (J2 = Red_Label'Succ(J1));
      Red_Is_Prev : Boolean := (J1 /= Red_Label'First) and then (J2 = Red_Label'Pred(J1));
   begin
      if J1 = J2 and (Blue_Is_Succ or else Blue_Is_Prev) then
         return True;
      elsif I1 = I2 and (Red_Is_Succ or else Red_Is_Prev) then
         return True;
      elsif (Blue_Is_Succ and then Red_Is_Prev) or else (Blue_Is_Prev and then Red_Is_Succ) then
         return True;
      else 
         return False;
      end if;
   end Neighboring_Hexagons;

   function Check_Win(Board : Board_Type; Stone : Stone_Color_Type) return Boolean is
   begin
      case Stone is
         when Red => return Check_Red_Win(Board);
         when Blue => return Check_Blue_Win(Board);
      end case;
   end Check_Win;
   
   function Check_Red_Win(Board : Board_Type) return Boolean is
      type Connection_Type is array (Red_Label) of Boolean;
      Reachable_Prev : Connection_Type := (others => False);
      Reachable : Connection_Type := (others => False);
   begin
      -- Initialize
      for J in Red_Label'Range loop
         if Board(Blue_Label'First, J) = Red then
            Reachable(J) := True;
         end if;
      end loop;

      -- For a hexagon (I1, J1), the neighboring hexagons with the blue
      -- label I2 where I2 is the successor of I1 are (I2, J1) and (I2, J2),
      -- where J2 is the predecessor of J1 (if it exists).
      -- Reversing this relationship, when iterating over the
      -- red labels for the next blue label, we look back to the
      -- at the connection value for the previous blue label and
      -- the same and successor red labels.
      for I in Blue_Label'Succ(Blue_Label'First) .. Blue_Label'Last loop
         Reachable_Prev := Reachable;  -- Save previous state of reachability
         for J in Red_Label'Range loop
            if Board(I, J) = Red then
               if J /= Red_Label'Last and then Reachable_Prev(Red_Label'Succ(J)) then
                  Reachable(J) := True;
               elsif Reachable_Prev(J) then
                  Reachable(J) := True;
               else
                  Reachable(J) := False;
               end if;
            else 
               Reachable(J) := False;
            end if;
         end loop;
      end loop;

      -- If any  of the hexagons for the last blue label are reachable, then Red wins
      for J in Red_Label'Range loop
         if Reachable(J) then
            return True;
         end if;
      end loop;
      return False;
   end Check_Red_Win;
   
   function Check_Blue_Win(Board : Board_Type) return Boolean is
      type Connection_Type is array (Blue_Label) of Boolean;
      Reachable_Prev : Connection_Type := (others => False);
      Reachable : Connection_Type := (others => False);
   begin
      -- Initialize
      for I in Blue_Label'Range loop
         if Board(I, Red_Label'First) = Blue then
            Reachable(I) := True;
         end if;
      end loop;

      -- For a hexagon (I1, J1), the neighboring hexagons with the red
      -- label J2 where J2 is the successor of J1 are (I1, J2) and (I2, J2),
      -- where I2 is the predecessor of I1 (if it exists).
      -- Reversing this relationship, when iterating over the
      -- blue labels for the next red label, we look back to the
      -- the connection value for the previous red label and
      -- the same and successor blue labels.
      for J in Red_Label'Succ(Red_Label'First) .. Red_Label'Last loop
         Reachable_Prev := Reachable;  -- Save previous state of reachability
         for I in Blue_Label'Range loop
            if Board(I, J) = Blue then
               if I /= Blue_Label'Last and then Reachable_Prev(Blue_Label'Succ(I)) then
                  Reachable(I) := True;
               elsif Reachable_Prev(I) then
                  Reachable(I) := True;
               else
                  Reachable(I) := False;
               end if;
            else 
               Reachable(I) := False;
            end if;
         end loop;
      end loop;

      -- If any  of the hexagons for the last red label are reachable, then Blue wins
      for I in Blue_Label'Range loop
         if Reachable(I) then
            return True;
         end if;
      end loop;
      return False;
   end Check_Blue_Win;
   
   -- function Distance(From : Cell_Indices; To : Cell_Indices) return Integer is
   -- begin
   --    return Integer'Max(Abs(From.Row - To.Row), Abs(From.Col - To.Col));
   -- end Distance;
   --  
   -- function Can_Move(Board : Board_Type) return Player_Indicator_Type is
   --    function Available_Move_From(Row : Axis_Label; Col : Axis_Label) return Boolean is
   --       I0 : Axis_Label := Integer'Max(Board'First(1), Row - 2);
   --       I1 : Axis_Label := Integer'Min(Board'Last(1), Row + 2);
   --       J0 : Axis_Label := Integer'Max(Board'First(2), Col - 2);
   --       J1 : Axis_Label := Integer'Min(Board'Last(2), Col + 2);
   --    begin
   --       for I in I0 .. I1 loop
   --          for J in J0 .. J1 loop
   --             if not (I = Row and J = Col) and Board(I, J) = No_Mark then
   --                return True;
   --             end if;
   --          end loop;
   --       end loop;
   --       return False;
   --    end Available_Move_From;

   --    Players_Can_Move : Player_Indicator_Type := (others => False);
   --    Temp_Mark : Mark;
   -- begin
   --    for I in Axis_Label loop
   --       for J in Axis_Label loop
   --          Temp_Mark := Board(I, J);
   --          if Temp_Mark in R | B | W | K and then Available_Move_From(I, J) then
   --             case Temp_Mark is
   --                when R => Players_Can_Move(R) := True;
   --                when B => Players_Can_Move(B) := True;
   --                when W => Players_Can_Move(W) := True;
   --                when K => Players_Can_Move(K) := True;
   --                when others => null;
   --             end case;
   --          end if;
   --       end loop;
   --    end loop;
   --    return Players_Can_Move;
   -- end Can_Move;
end Hex;
