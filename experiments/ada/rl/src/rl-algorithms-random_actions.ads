
-- generic
--    type Config_Type is private;
--    type Environment_Type is limited private;
--    type Observation_Type is private;
--    type Action_Type is (<>);
--    type Step_Return_Type is private;
-- 
--    with function Make(config: Config_Type) return Environment_Type;
--    with function Reset(Env : in out Environment_Type) return Observation_Type;
--    with function Step(Env : in out Environment_Type; action: Action_Type) return Step_Return_Type;
package Random_Actions is
   type Simulation_Summary is record
       Num_Steps : Natural;
       Total_Reward : Float;
   end record;

   function Uniform_Random_Actions (Verbose : Boolean) return Simulation_Summary;

end Random_Actions;

