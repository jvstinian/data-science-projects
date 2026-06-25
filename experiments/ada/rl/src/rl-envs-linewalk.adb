with Ada.Text_IO; use Ada.Text_IO;


package body RL.Envs.LineWalk is
    function Initial_State return State_Type is
    begin 
        return State_Type'(Kind => Active, State => Active_State_Type((N + 1) / 2));
    end Initial_State;

    function Is_Terminal (State : State_Type) return Boolean is
    begin
        -- TODO: Use case
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
                New_State_Val : Integer := Integer(State.State);
            begin
                case Action is
                    when MoveLeft =>
                        New_State_Val := New_State_Val - 1;
                    when MoveRight =>
                        New_State_Val := New_State_Val + 1;
                end case;

                if New_State_Val < 1 then
                    return State_Type'(Kind => Terminal, Reward => -1);
                elsif New_State_Val > N then
                    return State_Type'(Kind => Terminal, Reward => 1);
                else
                    return State_Type'(Kind => Active, State => Active_State_Type(New_State_Val));
                end if;
            end;
        else
            return State; -- No change if already terminal
        end if;
    end Step;

    -- TODO: Probably just change to Float
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
        -- TODO: Try extended return statement
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
end RL.Envs.LineWalk;
