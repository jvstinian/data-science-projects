with Ada.Text_IO; use Ada.Text_IO;


package body Reinforcement_Learning.Environments.LineWalk is

    -- subtype Reward_Type is Integer range -1 .. 1;
    -- type Action_Type is (MoveLeft, MoveRight);
    -- type Available_Actions_Type is array (Action_Type) of Boolean;
    -- type Active_State_Type is new Integer range 1 .. N;
    -- type State_Kind_Type is (Active, Terminal);

    -- type Line_Walk_State (Kind : State_Kind_Type) is
    -- record
    --     case Kind is
    --         when Active =>
    --             State : Active_State_Type;
    --         when Terminal =>
    --             Reward : Reward_Type;
    --     end case;
    -- end record;

    function Initial_State return State_Type is
    begin 
        return State_Type'(Kind => Active, State => Active_State_Type((N + 1) / 2));
    end Initial_State;

    function Is_Terminal (State : State_Type) return Boolean is
    begin
        if State.Kind = Active then
            return False;
        else
            return True;
        end if;
    end Is_Terminal;

    function Get_Player(State : State_Type) return Player_Type is
    begin
        return Line_Walker;
    end Get_Player;

    function Step(State : State_Type; Action : Action_Type) return State_Type is
    begin
        if State.Kind = Active then
            declare
                -- New_State : Active_State_Type := State.State;
                New_State_Val : Integer := Integer(State.State);
            begin
                case Action is
                    when MoveLeft =>
                        New_State_Val := New_State_Val - 1;
                    when MoveRight =>
                        New_State_Val := New_State_Val + 1;
                end case;
                -- Put_Line ("Moved, new state value: " & Integer'Image(New_State_Val));

                if New_State_Val < 1 then
                    return State_Type'(Kind => Terminal, Reward => -1);
                elsif New_State_Val > N then
                    -- Put_Line ("Returning reward 1");
                    return State_Type'(Kind => Terminal, Reward => 1);
                else
                    return State_Type'(Kind => Active, State => Active_State_Type(New_State_Val));
                end if;
            end;
        else
            return State; -- No change if already terminal
        end if;
    end Step;

    function Reward(Player: Player_Type; State : State_Type) return Long_Float is
    begin
        if State.Kind = Terminal then
            return Long_Float(State.Reward);
        else
            return 0.0;
        end if;
    end Reward;

    function Get_Available_Actions (State : State_Type) return Available_Actions_Type is
        Result : Available_Actions_Type := (others => False);
    begin 
        if State.Kind = Active then
            Result(MoveLeft) := True;
            Result(MoveRight) := True;
        end if;
        return Result;
    end Get_Available_Actions;

    function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type is
        Result : Valid_Actions_Type := (MoveLeft, MoveRight);
    begin 
        return Result;
    end Get_Valid_Actions;

    procedure Print_State (State : State_Type) is
    begin
        if State.Kind = Active then
            Put_Line ("Current position " & Integer'Image(Integer(State.State)));
        else
            Put_Line ("Terminal state with reward: " & Integer'Image(Integer(State.Reward)));
        end if;
    end Print_State;
end Reinforcement_Learning.Environments.LineWalk;

-- initState :: Int -> Int -> LineWalkState
-- initState n pos | pos < 1   = LineWalkState n 1
--                 | pos > n   = LineWalkState n n
--                 | otherwise = LineWalkState n pos
-- 
-- applyAction :: LineWalkAction -> State LineWalkState Reward
-- applyAction MoveLeft = state (\s -> 
--     let LineWalkState n pos = s
--         newpos = pos - 1
--         reward = if newpos < 1
--                  then -1
--                  else 0
--     in (reward, LineWalkState n newpos))
-- applyAction MoveRight = state (\s -> 
--     let LineWalkState n pos = s
--         newpos = pos + 1
--         reward = if newpos > n
--                  then 1
--                  else 0
--     in (reward, LineWalkState n newpos))
-- 
-- instance MCTS.Environment LineWalkState LineWalkAction Double where
--     validActions _ = [MoveLeft, MoveRight]
--     isTerminal (LineWalkState n pos) = (pos < 1) || (pos > n)
--     act s = (flip execState s) . applyAction
--     -- act s a = execState (applyAction a) s
--     reward _ (LineWalkState n pos) =
--         if pos < 1
--         then (-1.0)
--         else if pos > n
--              then 1.0
--              else 0.0
-- 
