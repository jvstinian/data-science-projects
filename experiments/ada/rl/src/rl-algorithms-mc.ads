
generic
   type Config_Type is (<>);
   type Environment_Type is (<>);
   type Action_Type is (<>);
   type Step_Return_Type is private;
   type Observation_Type is (<>);
   type Discrete_Observation_Type is (<>);

   Max_Episode_Steps : Positive;
   
   with function Make(Config: Config_Type) return Environment_Type;
   with function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type)
      return Observation_Type;
   with function Step(Env : in out Environment_Type; Action : Action_Type) return Step_Return_Type;
   
   with function Get_Observation(Step_Return: Step_Return_Type) return Observation_Type;
   with function Get_Reward(Step_Return: Step_Return_Type) return Float;
   with function Get_Terminated(Step_Return: Step_Return_Type) return Boolean;

   with function To_Discrete_Observation(Obs : Observation_Type) return Discrete_Observation_Type;
package RL.Algorithms.MC is
   type MC_Visit_Type is (First_Visit, Every_Visit);
   type MC_Config_Type is record
      Num_Episodes : Integer;
      Visit_Type : MC_Visit_Type;
      Discount_Factor : Float := 1.0;
   end record;

   type Policy_Type is array (Discrete_Observation_Type) of Action_Type;
   type Value_Function_Type is array(Discrete_Observation_Type) of Float;

   type Mixed_Policy_Type is array (Discrete_Observation_Type, Action_Type) of Float;
   
   type State_Action_Value_Type is array (Discrete_Observation_Type, Action_Type) of Float;
   type Evaluation_Results_Type is record
      Q: State_Action_Value_Type;
      Policy : Policy_Type;
   end record;
      
   type MC_Epsilon_Soft_Config_Type is record
      Num_Episodes : Integer;
      -- Visit_Type : MC_Visit_Type;
      Discount_Factor : Float := 1.0;
      Epsilon : Float;
   end record;

   function MC_Policy_Evaluation(Env_Config : Config_Type; Policy : Policy_Type; MC_Config : MC_Config_Type) return Value_Function_Type;
   function MC_Exploring_Starts_Evaluation(Env_Config : Config_Type; MC_Config : MC_Config_Type) return Evaluation_Results_Type;
   function MC_Epsilon_Soft_Evaluation(Env_Config : Config_Type; MC_Config : MC_Epsilon_Soft_Config_Type) return Evaluation_Results_Type;

end RL.Algorithms.MC;
