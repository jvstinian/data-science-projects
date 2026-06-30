generic
   Map_Name : Map_Type;
package RL.Envs.Frozenlake.DP is
   Map_Info : constant Map_Info_Type := Get_Map_Info(Map_Name);
   Num_Rows : constant Positive := Map_Info.Rows;
   Num_Cols : constant Positive := Map_Info.Cols;
   
   type State_Type is new Natural range 0 .. (Num_Rows * Num_Cols - 1);

   type Transition_Probability_Type is record
       Probability : Float;
       Reward : Float;
   end record;

   type DP_Model_Type is array (State_Type, Action_Type, State_Type) of Transition_Probability_Type;

   function Get_Model(Config: Environment_Config) return DP_Model_Type
      with Pre => Config.Map_Name = Map_Name;
end RL.Envs.Frozenlake.DP;
