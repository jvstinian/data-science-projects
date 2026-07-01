with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with RL; use RL;  -- Transition_Probability_Type
with RL.Envs.Frozenlake.DP;
with RL.Algorithms.DP;

package body RL.Examples.DP_Model is
   procedure Print_Frozenlake_DP_Model_Transitions (Map_Name : Map_Type) is
      package Frozen_Lake_DP is new RL.Envs.Frozenlake.DP(Map_Name => Map_4x4);
      use Frozen_Lake_DP;
      DP_Model : DP_Model_Type := Get_Model(Config_Type'(Map_Name => Map_4x4, Is_Slippery => False));

      package DP_Algorithms is new RL.Algorithms.DP(
         State_Type => State_Type,
         Action_Type => Action_Type,
         DP_Model_Type => DP_Model_Type
      );
      use DP_Algorithms;

      Transition : Transition_Probability_Type;
   begin
      Put_Line ("State Action Next_State Probability Reward");
      for S in State_Type loop
         for A in Action_Type loop
            for S1 in State_Type loop
                Transition := DP_Model(S, A, S1);
                 if Transition.Probability > 0.0 then
                     Put(State_Type'Pos(S), Width => 5);
                     Put(" ");
                     Put(Ada.Strings.Fixed.Tail(A'Image, 6, ' '));
                     Put(" ");
                     Put(State_Type'Pos(S1), Width => 10);
                     Put(" ");
                     Ada.Float_Text_IO.Put(Item => Transition.Probability, Fore => 6, Aft => 4, Exp => 0);
                     Put(" ");
                     Ada.Float_Text_IO.Put(Item => Transition.Reward, Fore => 1, Aft => 4, Exp => 0);
                     New_Line;
                 end if;
            end loop;
         end loop;
      end loop;
   end Print_Frozenlake_DP_Model_Transitions;
end RL.Examples.DP_Model;
