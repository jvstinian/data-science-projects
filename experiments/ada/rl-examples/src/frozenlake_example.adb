with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Envs.Frozenlake.DP;
with RL.Algorithms.Random_Actions;

procedure Main is
   Config: Config_Type := Config_Type'(Map_Name => Map_4x4, Is_Slippery => False);

   -- package Frozenlake_Child is new RL.Envs.Frozenlake.Child(Map_Info => Get_Map_Info(Map_4x4));
   package Frozenlake_DP is new RL.Envs.Frozenlake.DP(Map_Name => Map_4x4);
   use Frozenlake_DP;
   DP_Model : DP_Model_Type := Get_Model(Config_Type'(Map_Name => Map_4x4, Is_Slippery => False));
   
   function Get_Observation (Step_Return : Step_Return_Type) return Observation_Type is (Step_Return.Observation);
   function Get_Reward(Step_Return : Step_Return_Type) return Float is (Step_Return.Reward);
   function Get_Terminated_Flag(Step_Return : Step_Return_Type) return Boolean is (Step_Return.Terminated);
   
   package Frozenlake_Random_Actions is new RL.Algorithms.Random_Actions(
       Config_Type => Config_Type,
       Environment_Type => Environment_Type,
       Observation_Type => Observation_Type,
       Action_Type => Action_Type,
       Step_Return_Type => Step_Return_Type,
       Make => Make,
       Reset => Reset,
       Step => Step,
       Get_Observation => Get_Observation,
       Get_Reward => Get_Reward,
       Get_Terminated_Flag => Get_Terminated_Flag
    );
    Sim_Summary : Frozenlake_Random_Actions.Simulation_Summary;

begin
   Sim_Summary := Frozenlake_Random_Actions.Uniform_Random_Actions (Config, True);
   Put_Line("Simulation summary using generic package:");
   Put_Line("  Number of steps: " & Natural'Image (Sim_Summary.Num_Steps));
   Put("  Total reward: ");
   Ada.Float_Text_IO.Put(Item => Sim_Summary.Total_Reward, Fore => 2, Aft => 2, Exp => 0);
   New_Line;

    Put_Line("Frozen Lake Child Get_Map_Rows: " & Frozenlake_DP.Num_Rows'Image);
    for Current_State in State_Type loop
        for Current_Action in Action_Type loop
            for Next_State in State_Type loop
                declare
                    Transition : Transition_Probability_Type := DP_Model(Current_State, Current_Action, Next_State);
                begin
                    if Transition.Probability > 0.0 then
                        Put_Line("From State " & Current_State'Image & " taking Action " & Current_Action'Image &
                                 " to State " & Next_State'Image & " has transition probability " &
                                 Transition.Probability'Image & " and reward " & Transition.Reward'Image);
                    end if;
                end;
            end loop;
        end loop;
    end loop;
end Main;
