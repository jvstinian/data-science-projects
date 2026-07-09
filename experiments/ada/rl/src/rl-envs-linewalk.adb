with Ada.Text_IO; use Ada.Text_IO;


package body RL.Envs.LineWalk is
    function Initial_State return State_Type is
    begin 
        return State_Type'(Kind => Active, State => Active_State_Type((N + 1) / 2));
    end Initial_State;

    function Is_Terminal (State : State_Type) return Boolean is
    begin
        case State.Kind is
            when Active => return False;
            when others => return True;
        end case;
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

    function Reward(Player: Player_Type; State : State_Type) return Float is
    begin
        if State.Kind = Terminal then
            return Float(State.Reward);
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
    begin 
        return Result : Valid_Actions_Type := (MoveLeft, MoveRight);
    end Get_Valid_Actions;

    procedure Print_State (State : State_Type) is
    begin
        if State.Kind = Active then
            Put_Line ("Current position " & Integer'Image(Integer(State.State)));
        else
            Put_Line ("Terminal state with reward: " & Integer'Image(Integer(State.Reward)));
        end if;
    end Print_State;
   
   function Make(Config: Config_Type) return Environment_Type is
      pragma Unreferenced (Config);
   begin
        return Environment_Type((N + 1) / 2);
   end Make;

   function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type) return Observation_Type is
      -- Seed_Reset is not used since state transitions are deterministic
      pragma Unreferenced (Seed_Reset);
      Observation : Observation_Type;
   begin
      Env := Environment_Type((N + 1) / 2);
      Observation := Observation_Type(Env);
      return Observation;
   end Reset;

   function Step(Env : in out Environment_Type; action: Action_Type) return Step_Return_Type is
      New_State : Integer := Integer(Env);
      Ret_Obs: Observation_Type;
      Reward : Float;
      Terminated : Boolean;
   begin
      if New_State > 0 and New_State < (N + 1) then
         case Action is
            when MoveLeft =>
               New_State := New_State - 1;
            when MoveRight =>
               New_State := New_State + 1;
         end case;
      end if;

      if New_State < 1 then
         Terminated := True;
         Reward := -1.0;
      elsif New_State > N then
         Terminated := True;
         Reward := 1.0;
      else 
         Terminated := False;
         Reward := 0.0;
      end if;

      -- Update Env
      Env := Environment_Type(New_State);
      Ret_Obs := Observation_Type(Natural'Max(1, Natural'Min(N, New_State)));
      return Step_Return_Type'(
         Observation => Ret_Obs,
         Reward => Reward,
         Terminated => Terminated
      );
   end Step;

end RL.Envs.LineWalk;
