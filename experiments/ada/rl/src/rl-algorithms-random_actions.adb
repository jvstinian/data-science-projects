with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;
-- with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;

package body RL.Algorithms.Random_Actions is
   function Uniform_Random_Actions (Config : Config_Type; Verbose : Boolean) return Simulation_Summary is
      package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
      Gen: Action_Random.Generator;
      Seed_Reset : Seed_Reset_Type := Seed_Reset_Type'(Kind => Set_Default);
      Env : Environment_Type := Make(Config);
      Obs: Observation_Type;
      Action: Action_Type;
      Step_Output: Step_Return_Type;
      I: Integer;
      Total_Reward: Float := 0.0;
   begin
      Action_Random.Reset(Gen);

      Obs:= Reset(Env, Seed_Reset);
      I := 0;
      loop
         Action := Action_Random.Random(Gen);
         Step_Output := Step(Env, Action);
         Obs := Get_Observation(Step_Output); -- .State;
         Total_Reward := Total_Reward + Get_Reward(Step_Output); -- .Reward;
         I := I + 1;
    
         if Verbose then
            Put("Step" & I'Image & ", Action " & Action'Image & ", ");
            Put(" Reward: ");
            Ada.Float_Text_IO.Put(Item => Get_Reward(Step_Output), Fore => 2, Aft => 2, Exp => 0);
            New_Line;
         end if;
         if Get_Terminated_Flag(Step_Output) then
            if Verbose then
               Put("Total Reward: ");
               Ada.Float_Text_IO.Put(Item => Total_Reward, Fore => 2, Aft => 2, Exp => 0);
               New_Line;
            end if;
            exit;
         end if;
      end loop;
      return Simulation_Summary'(Num_Steps => I, Total_Reward => Total_Reward);
   end Uniform_Random_Actions;

end RL.Algorithms.Random_Actions;

