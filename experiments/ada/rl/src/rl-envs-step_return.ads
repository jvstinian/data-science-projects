generic
   type Observation_Type is private;
package RL.Envs.Step_Return is

   type Step_Return_Type is record
      State: Observation_Type;
      Reward: Float;
      Terminated: Boolean;
      Done : Boolean;
   end record;

end RL.Envs.Step_Return;
