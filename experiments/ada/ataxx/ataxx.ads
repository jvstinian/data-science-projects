-- with Environment_State; use Environment_State;

package Ataxx is
    Board_Width : constant Integer := 7;

    subtype Reward_Type is Integer range 0 .. Board_Width * Board_Width;
    type Player_Type is (R, B, W, K);

    subtype Axis_Label is Integer range 1 .. Board_Width;
    type Cell_Indices is
    record
        Row : Axis_Label;
        Col : Axis_Label;
    end record;
    type Action_Type is
    record
        Source : Cell_Indices;
        Target : Cell_Indices;
    end record;

    type Valid_Actions_Type is array (Natural range <>) of Action_Type;
    type Mark is (R, B, W, K, X, No_Mark);
    type Game_Status_Type is (Active, Finished);
    type Game_Score_Type is array (Player_Type) of Reward_Type;
    type Board_Type is array (Axis_Label, Axis_Label) of Mark;

    type Player_Count_Type is (Two_Player, Four_Player);
    type Player_Indicator_Type is array (Player_Type) of Boolean;
    type Config_Type is record
        Player_Count : Player_Count_Type;
    end record;

    type State_Type is record
        Player_Indicators : Player_Indicator_Type;
        Board : Board_Type;
        Status : Game_Status_Type;
        Scores : Game_Score_Type;
        Current_Player : Player_Type;
    end record;

    function Initial_State (Config : Config_Type) return State_Type;
    function Is_Terminal (State : State_Type) return Boolean;
    function Get_Player(State : State_Type) return Player_Type;
    function Step(State : State_Type; Action : Action_Type) return State_Type;
    -- TODO: Used Long_Float for other environments, need to decide what to do here
    function Reward(Player: Player_Type; State : State_Type) return Reward_Type;
    function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type;

    procedure Print_State (State : State_Type);

private
   function Distance(From : Cell_Indices; To : Cell_Indices) return Integer;
   function Can_Move(Board : Board_Type) return Player_Indicator_Type;
end Ataxx;
