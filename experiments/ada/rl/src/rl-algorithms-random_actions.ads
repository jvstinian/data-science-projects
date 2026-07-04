generic
   type Config_Type is private;
   type Environment_Type (<>) is limited private;
   type Observation_Type is private;
   type Action_Type is (<>);
   type Step_Return_Type is private;
   
   with function Make(Config: Config_Type) return Environment_Type;
   with function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type) return Observation_Type;
   with function Step(Env : in out Environment_Type; action: Action_Type) return Step_Return_Type;
   
   with function Get_Observation (Step_Return : Step_Return_Type) return Observation_Type;
   with function Get_Reward(Step_Return : Step_Return_Type) return Float;
   with function Get_Terminated_Flag(Step_Return : Step_Return_Type) return Boolean;

   Seed_Reset : Seed_Reset_Type := (Kind => Set_Default);
package RL.Algorithms.Random_Actions is

   type Simulation_Summary is record
       Num_Steps : Natural;
       Total_Reward : Float;
   end record;

   function Uniform_Random_Actions (Config : Config_Type; Verbose : Boolean) return Simulation_Summary;

end RL.Algorithms.Random_Actions;
