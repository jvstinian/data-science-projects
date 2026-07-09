package RL.Envs.Tic_Tac_Toe is
   type Player_Type is (PlayerX, PlayerO);

   type Row_Label is new Integer range 0 .. 2;
   -- We use a custom discrete type for column labels
   type Col_Label is (A, B, C);
   type Action_Type is
   record
       Row : Row_Label;
       Col : Col_Label;
   end record;

   type Valid_Actions_Type is array (Natural range <>) of Action_Type;
   type Mark is (X, O, No_Mark);
   type Game_Status_Type is (X_Move, O_Move, Draw, X_Wins, O_Wins);
   type Board_Type is array (Row_Label, Col_Label) of Mark;

   type State_Type is record
       Board : Board_Type;
       Status : Game_Status_Type;
   end record;

   function Initial_State return State_Type;
   function Is_Terminal (State : State_Type) return Boolean;
   function Get_Player(State : State_Type) return Player_Type;
   function Step(State : State_Type; Action : Action_Type) return State_Type;
   function Reward(Player: Player_Type; State : State_Type) return Float;
   function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type;

   procedure Print_State (State : State_Type);
private
   -- TODO: Is Reward_Type used?
   subtype Reward_Type is Integer range -1 .. 1;
   
   function Check_Status_For_Action(State : State_Type; Action : Action_Type) return Game_Status_Type;
end RL.Envs.Tic_Tac_Toe;
