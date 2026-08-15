with Ada.Text_IO;
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Examples.DP_Model; use RL.Examples.DP_Model;
with RL.Examples.DP; use RL.Examples.DP;

procedure RL_Examples is
begin
   Ada.Text_IO.Put_Line ("RL_Examples");
   
   Ada.Text_IO.Put_Line ("Printing Frozenlake Dynamic Programming Model");
   Print_Frozenlake_DP_Model_Transitions (Map_4x4);
   
   Ada.Text_IO.Put_Line ("Frozenlake Iterative Policy Evaluation Example");
   Frozenlake_Policy_Evaluation_Example(Map_4x4);
end RL_Examples;
