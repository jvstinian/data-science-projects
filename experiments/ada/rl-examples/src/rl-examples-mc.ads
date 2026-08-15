with RL.Envs.Blackjack; use RL.Envs.Blackjack;
with RL.Algorithms.MC;

-- TODO: This is Work-In-Progress.  The code will likely not compile.
package RL.Examples.MC is
   -- Monte Carlo examples --

   function Get_Observation(Step_Return: Step_Return_Type) return Observation_Type is (Step_Return.Observation);
   function Get_Reward(Step_Return: Step_Return_Type) return Float is (Step_Return.Reward);
   function Get_Terminated(Step_Return: Step_Return_Type) return Boolean is (Step_Return.Terminated);

   Num_Discrete_States : constant Integer := 2 * 10 * 10 + 1;
   type Discrete_Observation_Type is range 0 .. Num_Discrete_States - 1;
   function To_Discrete_Observation(Obs : Observation_Type) return Discrete_Observation_Type;

   package Blackjack_MC_Algs is new RL.Algorithms.MC(
      Config_Type => Config_Type,
      Environment_Type => Environment_Type,
      Action_Type => Action_Type,
      Step_Return_Type => Step_Return_Type,
      Observation_Type => Observation_Type,
      Discrete_Observation_Type => Discrete_Observation_Type,
      Max_Episode_Steps => 21,
      Make => Make,
      Reset => Reset,
      Step => Step,
      Get_Observation => Get_Observation,
      Get_Reward => Get_Reward,
      Get_Terminated => Get_Terminated,
      To_Discrete_Observation => To_Discrete_Observation
   );
   use Blackjack_MC_Algs;

   -- Blackjack
   procedure Blackjack_Policy_Evaluation_Example (Config : Config_Type);
end RL.Examples.MC;
