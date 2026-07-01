-- with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Float_Text_IO;
-- with RL; use RL;  -- Transition_Probability_Type
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
-- with RL.Envs.Frozenlake.DP;
-- with RL.Algorithms.DP;

package RL.Examples.DP_Model is
   procedure Print_Frozenlake_DP_Model_Transitions (Map_Name : Map_Type);
end RL.Examples.DP_Model;
