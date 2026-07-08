package RL.Envs.Hex is
   Board_Width : constant Integer := 7;

   type Player_Type is (Player1, Player2);
   type Stone_Color_Type is (Red, Blue);

   subtype Blue_Label is Integer range 1 .. Board_Width;
   subtype Red_Label is Character range 'A' .. Character'Val(Character'Pos('A') + Board_Width - 1);
   type Action_Type is
   record
       Row : Blue_Label;
       Col : Red_Label;
   end record;

   type Valid_Actions_Type is array (Natural range <>) of Action_Type;
   type Mark is (Red, Blue, No_Mark);
   type Game_Status_Type is (Active, Player1_Wins, Player2_Wins);
   type Board_Type is array (Blue_Label, Red_Label) of Mark;

   type Player_Color_Type is array (Player_Type) of Stone_Color_Type;

   type State_Type is record
      Player_Colors : Player_Color_Type;
      Board : Board_Type;
      Status : Game_Status_Type;
      Current_Player : Player_Type;
   end record;

   function Initial_State return State_Type;
   function Is_Terminal (State : State_Type) return Boolean;
   function Get_Player (State : State_Type) return Player_Type;
   function Step (State : State_Type; Action : Action_Type) return State_Type;
   function Reward (Player: Player_Type; State : State_Type) return Float;
   function Get_Valid_Actions (State : State_Type) return Valid_Actions_Type;

   procedure Print_State (State : State_Type);
private
   function Get_Number_Of_Stones (State : State_Type) return Natural;
   function Check_Win (Board : Board_Type; Stone : Stone_Color_Type) return Boolean;
   function Check_Blue_Win (Board : Board_Type) return Boolean;
   function Check_Red_Win (Board : Board_Type) return Boolean;
   
   -- NOTE: The following method was not needed after all
   function Neighboring_Hexagons(I1 : Blue_Label; J1 : Red_Label; I2 : Blue_Label ; J2 : Red_Label) return Boolean;
end RL.Envs.Hex;
