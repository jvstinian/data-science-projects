with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Tic_Tac_Toe is

    function Initial_State return State_Type is
    begin 
        return State_Type'(Board => (others => (others => No_Mark)), Status => X_Move);
    end Initial_State;

    function Is_Terminal (State : State_Type) return Boolean is
    begin
       case State.Status is
          when X_Move | O_Move => return False;
          when Draw | X_Wins | O_Wins => return True;
       end case;
    end Is_Terminal;

   function Get_Player(State : State_Type) return Player_Type is
   begin
      case State.Status is
         when X_Move | X_Wins => return PlayerX;
         when O_Move | O_Wins => return PlayerO;
         when Draw => return PlayerX; -- Arbitrary choice since it's a draw, should not be used in terminal state
      end case;
   end Get_Player;

   function Check_Status_For_Action(State : State_Type; Action : Action_Type) return Game_Status_Type is
      -- Helper functions
      function Check_Row_For_Win(State : State_Type; M : Mark; R: Row_Label) return Boolean is
      begin
         for C in Col_Label loop
            if State.Board(R, C) /= M then
               return False;
            end if;
         end loop;
         return True;
      end Check_Row_For_Win;
      
      function Check_Col_For_Win(State : State_Type; M : Mark; C: Col_Label) return Boolean is
      begin
         for R in Row_Label loop
            if State.Board(R, C) /= M then
               return False;
            end if;
         end loop;
         return True;
      end Check_Col_For_Win;
      
      function Is_Move_On_Diag1(Action : Action_Type) return Boolean is
         Row_Index : Integer := Row_Label'Pos(Action.Row) - Row_Label'Pos(Row_Label'First);
         Col_Index : Integer := Col_Label'Pos(Action.Col) - Col_Label'Pos(Col_Label'First);
      begin
         return (Row_Index - Col_Index) = 0;
      end Is_Move_On_Diag1;
      
      function Is_Move_On_Diag2(Action : Action_Type) return Boolean is
         Row_Index : Integer := Row_Label'Pos(Action.Row) - Row_Label'Pos(Row_Label'First);
         Col_Index : Integer := Col_Label'Pos(Action.Col) - Col_Label'Pos(Col_Label'First);
      begin
         return (Row_Index + Col_Index) = 2;
      end Is_Move_On_Diag2;
      
      function Check_Diag1_For_Win(State : State_Type; M : Mark) return Boolean is
         Temp_R : Row_Label;
         Temp_C : Col_Label;
      begin
         for I in 0 .. 2 loop
            Temp_R := Row_Label'Val(Row_Label'Pos(Row_Label'First) + I);
            Temp_C := Col_Label'Val(Col_Label'Pos(Col_Label'First) + I);
            if State.Board(Temp_R, Temp_C) /= M then
               return False;
            end if;
         end loop;
         return True;
      end Check_Diag1_For_Win;
      
      function Check_Diag2_For_Win(State : State_Type; M : Mark) return Boolean is
         Temp_R : Row_Label;
         Temp_C : Col_Label;
      begin
         for I in 0 .. 2 loop
            Temp_R := Row_Label'Val(Row_Label'Pos(Row_Label'First) + I);
            Temp_C := Col_Label'Val(Col_Label'Pos(Col_Label'Last) - I);
            if State.Board(Temp_R, Temp_C) /= M then
               return False;
            end if;
         end loop;
         return True;
      end Check_Diag2_For_Win;
      
      function All_Moves_Exhausted(State : State_Type) return Boolean is
      begin
         for R in Row_Label loop
            for C in Col_Label loop
               if State.Board(R, C) = No_Mark then
                  return False;
               end if;
            end loop;
         end loop;
         return True;
      end All_Moves_Exhausted;
      
      Result : Game_Status_Type := State.Status; -- Default to current status
      M : Mark := State.Board(Action.Row, Action.Col);
      W : Game_Status_Type;
   begin
      case M is
         when X =>
            W := X_Wins;
         when O =>
            W := O_Wins;
         when No_Mark =>
            -- Invalid state, just return
            return Result;
      end case;

      -- M is either X or O
      if Check_Row_For_Win(State, M, Action.Row) then
         return W;
      elsif Check_Col_For_Win(State, M, Action.Col) then
         return W;
      elsif Is_Move_On_Diag1(Action) and then Check_Diag1_For_Win(State, M) then
         return W;
      elsif Is_Move_On_Diag2(Action) and then Check_Diag2_For_Win(State, M) then
         return W;
      elsif All_Moves_Exhausted(State) then
         return Draw; -- No more moves and no winner, it's a draw
      else
         return Result; -- No win, return current status
      end if;
   end Check_Status_For_Action;

   function Step(State : State_Type; Action : Action_Type) return State_Type is
      Result : State_Type := State; -- Start with the current state and modify it
   begin
      if Result.Status = X_Move then
         Result.Board(Action.Row, Action.Col) := X;
         Result.Status := O_Move; -- Next player's turn
      elsif Result.Status = O_Move then
         Result.Board(Action.Row, Action.Col) := O;
         Result.Status := X_Move; -- Next player's turn
      end if; -- else do nothing

      -- Check if the new move results in a win
      Result.Status := Check_Status_For_Action(Result, Action); 
      return Result;
   end Step;

-- pub fn tictactoe_move(state: *TicTacToeState, move: TicTacToeMove) void {
--     const orig_status: TicTacToeStatus = state.*.status;
--     // var found_blank: bool = false;
--     state.*.status = blk: {
--         var rowidx: u8 = 0;
--         while (rowidx < 3) : (rowidx += 1) {
--             const lmark: Mark = state.*.board[rowidx][0];
--             if (lmark == .none) continue;
--             if ((state.*.board[rowidx][1] == lmark) and (state.*.board[rowidx][2] == lmark)) {
--                 if (lmark == .o) {
--                     // state.*.status = .o_wins;
--                     break :blk .o_wins;
--                 } else if (lmark == .x) {
--                     // state.*.status = .x_wins;
--                     break :blk .x_wins;
--                 }
--             }
--         }
-- 
--         var colidx: u8 = 0;
--         while (colidx < 3) : (colidx += 1) {
--             const lmark: Mark = state.*.board[0][colidx];
--             if (lmark == .none) continue;
--             if ((state.*.board[1][colidx] == lmark) and (state.*.board[2][colidx] == lmark)) {
--                 if (lmark == .o) {
--                     break :blk .o_wins;
--                 } else if (lmark == .x) {
--                     break :blk .x_wins;
--                 }
--             }
--         }
-- 
--         var lmark: Mark = state.*.board[0][0];
--         if (lmark != .none) {
--             if ((state.*.board[1][1] == lmark) and (state.*.board[2][2] == lmark)) {
--                 if (lmark == .o) {
--                     break :blk .o_wins;
--                 } else if (lmark == .x) {
--                     break :blk .x_wins;
--                 }
--             }
--         }
-- 
--         lmark = state.*.board[0][2];
--         if (lmark != .none) {
--             if ((state.*.board[1][1] == lmark) and (state.*.board[2][0] == lmark)) {
--                 if (lmark == .o) {
--                     break :blk .o_wins;
--                 } else if (lmark == .x) {
--                     break :blk .x_wins;
--                 }
--             }
--         }
-- 
--         // Finally we check for blanks, and if so
--         // we continue return the original status.
--         // Otherwise we return the status indicating
--         // a draw.
--         rowidx = 0;
--         while (rowidx < 3) : (rowidx += 1) {
--             colidx = 0;
--             while (colidx < 3) : (colidx += 1) {
--                 if (state.*.board[rowidx][colidx] == .none) {
--                     break :blk orig_status;
--                 }
--             }
--         } else {
--             break :blk .draw;
--         }
--     };
-- 
--     // var found_blank: bool = false;
--     // var rowidx: u8 = 0;
--     // var colidx: u8 = 0;
--     // while (rowidx < 3) : (rowidx += 1) {
--     //     colidx = 0;
--     //     const lmark: Mark = state.*.board[rowidx][colidx];
--     //     if (lmark == .none) {
--     //         found_blank = true;
--     //         // move to next row index
--     //         continue;
--     //     }
--     //     colidx += 1;
--     //     const all_equal: bool = while (colidx < 3) : (colidx += 1) {
--     //         if (lmark != state.*.board[rowidx][colidx]) {
--     //             break false;
--     //         }
--     //     } else true;
-- 
--     //     if (all_equal) {
--     //         if (lmark == .o) {
--     //             state.*.status = .o_wins;
--     //         } else {
--     //             state.*.status = .x_wins;
--     //         }
--     //         break;
--     //     }
--     // }
-- 
--     // if ((state.*.status == .o_wins) || (state.*.status == .x_wins)) {
--     //     return;
--     // }
-- 
--     // rowidx = 0;
--     // colidx = 0;
--     // while (colidx < 3) : (colidx += 1) {
--     //     rowidx = 0;
--     //     const lmark: Mark = state.*.board[rowidx][colidx];
--     //     if (lmark == .none) {
--     //         found_blank = true;
--     //         // move to next row index
--     //         continue;
--     //     }
--     //     rowidx += 1;
--     //     const all_equal: bool = while (rowidx < 3) : (rowidx += 1) {
--     //         if (lmark != state.*.board[rowidx][colidx]) {
--     //             break false;
--     //         }
--     //     } else true;
-- 
--     //     if (all_equal) {
--     //         if (lmark == .o) {
--     //             state.*.status = .o_wins;
--     //         } else {
--     //             state.*.status = .x_wins;
--     //         }
--     //         break;
--     //     }
--     // }
-- 
--     // if ((state.*.status == .o_wins) || (state.*.status == .x_wins)) {
--     //     return;
--     // }
-- }

   function Reward(Player: Player_Type; State : State_Type) return Long_Float is
   begin
      case State.Status is
         when X_Wins =>
            if Player = PlayerX then
               return 1.0;
            else
               return -1.0;
            end if;
         when O_Wins =>
            if Player = PlayerO then
               return 1.0;
            else
               return -1.0;
            end if;
         when X_Move | O_Move | Draw =>
            return 0.0; -- Non-terminal state or a draw
      end case;
   end Reward;

   function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type is
      Result : Valid_Actions_Type := (0..8 => (Row => 0, Col => A)); -- Initialize with dummy values, will be overwritten
      I : Natural := 0;
   begin 
      for R in Row_Label loop
         for C in Col_Label loop
            if State.Board(R, C) = No_Mark then
               Result(I) := (Row => R, Col => C);
               I := I + 1;
            end if;
         end loop;
      end loop;
      
      return Result(0..I-1); -- Return only the valid portion of the array
   end Get_Valid_Actions;

   procedure Print_State (State : State_Type) is
      procedure Print_Board is
         Mark_Char : Character;
      begin
         Put(" ");
         for C in Col_Label loop
            -- Put(Character(C));
            Put(C'Image);
         end loop;
         New_Line;

         for R in Row_Label loop
            Put(Integer(R), Width => 0); -- Print row label as integer
            for C in Col_Label loop
               case State.Board(R, C) is
                  when X => Put('X');
                  when O => Put('O');
                  when No_Mark => Put(' ');
               end case;
            end loop;
            New_Line;
         end loop;
      end Print_Board;
      procedure Print_Game_Status is
      begin
       Put ("Status: ");
       case State.Status is
          when X_Move => Put_Line("X's move");
          when O_Move => Put_Line("O's move");
          when Draw => Put_Line("Game is a draw");
          when X_Wins => Put_Line("Player X wins");
          when O_Wins => Put_Line("Player O wins");
       end case;
       New_Line;
      end Print_Game_Status;
    begin
       Print_Board;
       Print_Game_Status;
    end Print_State;
end Tic_Tac_Toe;

