-- with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
-- with Ada.Float_Text_IO; use Ada.Float_Text_IO;
-- with RL; use RL;
with RL.Envs.Blackjack; use RL.Envs.Blackjack;
-- with RL.Algorithms.MC;;
-- with Ada.Containers; use Ada.Containers;
-- with Ada.Containers.Bounded_Hashed_Maps;
-- with Ada.Numerics.Discrete_Random;
-- with Ada.Numerics.Float_Random;
with RL.Examples.MC; use RL.Examples.MC;

-- TODO: The MC_* methods have been moved to RL.Algorithms.MC 
procedure MC_Policy_Evaluation_Example is
   -- Hash Map approach
   -- function Observation_Hash (Obs : Observation_Type) return Hash_Type is
   --    Card_Value : Integer := Card_Type'Pos(Obs.Dealer_Showing_Card);
   --    Ace_Offset : Integer := Boolean'Pos(Obs.Usable_Ace);
   -- begin
   --    return Hash_Type(Obs.Player_Sum) xor Hash_Type(Card_Value) xor Hash_Type(Ace_Offset);
   -- end Observation_Hash;
   -- 
   -- package State_Value_Map_Type is new Ada.Containers.Bounded_Hashed_Maps (
   --       Key_Type => Observation_Type,
   --       Element_Type => Float, -- TODO
   --       Hash => Observation_Hash,
   --       Equivalent_Keys => "=");

   -- State_Value_Map : State_Value_Map_Type.Map(20, Hash_Type(16)); --  := State_Value_Map_Type.Empty_Map;
   Config : Config_Type := Config_Type'(Natural_Win_Reward => SAB, Auto_Hit => True);
begin
   Blackjack_Policy_Evaluation_Example (Config);
end MC_Policy_Evaluation_Example;
