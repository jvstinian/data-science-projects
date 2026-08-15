with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package body RL.Algorithms.MC is
   function MC_Policy_Evaluation(Env_Config : Config_Type; Policy : Policy_Type; MC_Config : MC_Config_Type) return Value_Function_Type is
      type Reward_Tracker_Type is record
         Total_Reward : Float;
         Count : Integer;
      end record;

      type State_Reward_Tracker_Type is array(Discrete_Observation_Type) of Reward_Tracker_Type;

      type SAR_Type is record
         State : Discrete_Observation_Type;
         Action : Action_Type;
         Reward : Float;
      end record;

      type SAR_Array_Type is array (1 .. Max_Episode_Steps) of SAR_Type;
      SAR_Array : SAR_Array_Type;

      V : State_Reward_Tracker_Type := (others => (0.0, 0));
      Seed_Reset : Seed_Reset_Type := (Kind => Set_Default);
      Env : Environment_Type := Make(Env_Config);
      Obs : Observation_Type;
      Step_Output : Step_Return_Type;
      Discrete_Obs : Discrete_Observation_Type;
      Action : Action_Type;
      Terminated : Boolean;
      Last_I : Natural;

      procedure Every_Visit_Reward_Update(V : in out State_Reward_Tracker_Type; SAR_Array_Input: SAR_Array_Type; Last_I : Natural) is
         SAR_Array : SAR_Array_Type := SAR_Array_Input; -- A local copy to avoid specifying "in out" for the array parameter
      begin
         for I in reverse 1 .. Last_I loop
            if I < Last_I then
               SAR_Array(I).Reward := SAR_Array(I).Reward + MC_Config.Discount_Factor * SAR_Array(I + 1).Reward;
            end if;
            -- Update the state reward tracker
            V(SAR_Array(I).State).Total_Reward := V(SAR_Array(I).State).Total_Reward + SAR_Array(I).Reward;
            V(SAR_Array(I).State).Count        := V(SAR_Array(I).State).Count + 1;
         end loop;
      end Every_Visit_Reward_Update;
      
      procedure First_Visit_Reward_Update(V : in out State_Reward_Tracker_Type; SAR_Array_Input : SAR_Array_Type; Last_I : Natural) is
         SAR_Array : SAR_Array_Type := SAR_Array_Input; -- A local copy to avoid specifying "in out" for the array parameter

         type First_Visit_Array_Type is array(Discrete_Observation_Type) of Natural;
         First_Visit_Array : First_Visit_Array_Type := (others => 0);

         First_Visit : Natural;  -- Temp value for convenience
      begin
         for I in reverse 1 .. Last_I loop
            if I < Last_I then
               SAR_Array(I).Reward := SAR_Array(I).Reward + MC_Config.Discount_Factor * SAR_Array(I + 1).Reward;
            end if;
            First_Visit_Array(SAR_Array(I).State) := I;
         end loop;
         for S in Discrete_Observation_Type loop
            if First_Visit_Array(S) > 0 then
               First_Visit := First_Visit_Array(S);
               V(SAR_Array(First_Visit).State).Total_Reward := V(SAR_Array(First_Visit).State).Total_Reward + SAR_Array(First_Visit).Reward;
               V(SAR_Array(First_Visit).State).Count        := V(SAR_Array(First_Visit).State).Count + 1;
            end if;
         end loop;
      end First_Visit_Reward_Update;
   begin
      for Episode in 1 .. MC_Config.Num_Episodes loop
         Obs := Reset(Env, Seed_Reset);
         Discrete_Obs := To_Discrete_Observation(Obs);
         Terminated := False;
         Last_I := 1;
         for I in 1 .. Max_Episode_Steps loop
            Action := Policy(Discrete_Obs);
            Step_Output := Step(Env, Action);
            SAR_Array(I) := (State => Discrete_Obs, Action => Action, Reward => Get_Reward(Step_Output));
            Discrete_Obs := To_Discrete_Observation(Get_Observation(Step_Output));
            Last_I := I;
            Terminated := Get_Terminated(Step_Output);
            exit when Terminated;
         end loop;

         case MC_Config.Visit_Type is
            when First_Visit =>
               First_Visit_Reward_Update(V, SAR_Array, Last_I);
            when Every_Visit =>
               Every_Visit_Reward_Update(V, SAR_Array, Last_I);
         end case;
      end loop;

      return Res : Value_Function_Type := (others => 0.0) do
         for S in Discrete_Observation_Type loop
            if V(S).Count > 0 then
               Res(S) := V(S).Total_Reward / Float(V(S).Count);
            end if;
         end loop;
      end return;
   end MC_Policy_Evaluation;
   
   -- TODO: Should we use MC_Config_Type?  We might not want to allow the Every_Visit option.
   function MC_Exploring_Starts_Evaluation(Env_Config : Config_Type; MC_Config : MC_Config_Type) return Evaluation_Results_Type is
      type Reward_Tracker_Type is record
         Total_Reward : Long_Float;  -- TODO: Using Long_Float to see what happens
         Count : Integer;
      end record;

      type State_Action_Reward_Tracker_Type is array(Discrete_Observation_Type, Action_Type) of Reward_Tracker_Type;
      State_Action_Reward_Tracker : State_Action_Reward_Tracker_Type := (others => (others => (0.0, 0)));

       type SAR_Type is record
         State : Discrete_Observation_Type;
         Action : Action_Type;
         Reward : Float;
      end record;

      type SAR_Array_Type is array (1 .. Max_Episode_Steps) of SAR_Type;
      SAR_Array : SAR_Array_Type;
   
      package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
      Action_Gen : Action_Random.Generator;

      -- NOTE: The flag for indicating whether a state was encountered in an episode
      --       doesn't impact the results.  At best it reduces the number of states
      --       for which to update action values and the policy.
      type State_Encountered_Flag_Type is array(Discrete_Observation_Type) of Boolean;
      State_Encountered_Flag : State_Encountered_Flag_Type;

      Prev_Policy : Policy_Type;
      Policy : Policy_Type := (others => Action_Type'First);
      Q : State_Action_Value_Type := (others => (others => 0.0));
      
      Env : Environment_Type := Make(Env_Config);
      Seed_Reset : Seed_Reset_Type := (Kind => Set_Default);
      Obs : Observation_Type;
      Step_Output : Step_Return_Type;
      Discrete_Obs : Discrete_Observation_Type;
      Action : Action_Type;
      Terminated : Boolean;
      Last_I : Natural;
 
      procedure First_Visit_Reward_Update(Q_Tracker: in out State_Action_Reward_Tracker_Type; SAR_Array_Input : SAR_Array_Type; Last_I : Natural) is
         -- A local copy to avoid specifying "in out" for the array parameter
         SAR_Array : SAR_Array_Type := SAR_Array_Input;

         type First_Visit_Array_Type is array(Discrete_Observation_Type) of Natural;
         First_Visit_Array : First_Visit_Array_Type := (others => 0);

         -- Local variables
         -- First visit index
         First_Visit : Natural;
         A : Action_Type;
      begin
         for I in reverse 1 .. Last_I loop
            if I < Last_I then
               SAR_Array(I).Reward := SAR_Array(I).Reward + MC_Config.Discount_Factor * SAR_Array(I + 1).Reward;
            end if;
            First_Visit_Array(SAR_Array(I).State) := I;
         end loop;
         for S in Discrete_Observation_Type loop
            if First_Visit_Array(S) > 0 then
               First_Visit := First_Visit_Array(S);
               A := SAR_Array(First_Visit).Action;
               -- NOTE: SAR_Array(First_Visit).State should equal S
               -- TODO: Using Long_Float
               Q_Tracker(S, A).Total_Reward := Q_Tracker(S, A).Total_Reward + Long_Float(SAR_Array(First_Visit).Reward);
               Q_Tracker(S, A).Count        := Q_Tracker(S, A).Count + 1;
            end if;
         end loop;
      end First_Visit_Reward_Update;

      function Update_Policy(Q : State_Action_Value_Type; Prev_Policy: Policy_Type; State_Encountered_Flag: State_Encountered_Flag_Type) return Policy_Type is
         Res : Policy_Type := Prev_Policy;  -- Copy previous policy
         Best_Action : Action_Type;
         Max_Value : Float;
      begin
         for S in Discrete_Observation_Type loop
            -- Only update the policies for those states that appear in the episode
            if State_Encountered_Flag(S) then
               Max_Value := Float'First;
               Best_Action := Action_Type'First;
               for A in Action_Type loop
                  if Q(S, A) > Max_Value then
                     Max_Value := Q(S, A);
                     Best_Action := A;
                  end if;
               end loop;
               Res(S) := Best_Action;
            end if;
         end loop;
         return Res;
      end Update_Policy;

   begin
      Action_Random.Reset(Action_Gen);
      for Episode in 1 .. MC_Config.Num_Episodes loop
         -- Exploring starts
         -- Initialize Env and select a random action.
         -- TODO: Random selection of env and action might need to be generalized
         State_Encountered_Flag := (others => False);
         Obs := Reset(Env, Seed_Reset);
         Action := Action_Random.Random(Action_Gen);

         -- Process the first action
         Discrete_Obs := To_Discrete_Observation(Obs);
         Step_Output := Step(Env, Action);
         SAR_Array(1) := (State => Discrete_Obs, Action => Action, Reward => Get_Reward(Step_Output));
         State_Encountered_Flag(Discrete_Obs) := True;
         Last_I := 1;
         Terminated := Get_Terminated(Step_Output);

         if not Terminated then
            Discrete_Obs := To_Discrete_Observation(Get_Observation(Step_Output));

            -- Simulate as before starting from step 2
            for I in 2 .. Max_Episode_Steps loop
               Action := Policy(Discrete_Obs);
               Step_Output := Step(Env, Action);
               SAR_Array(I) := (State => Discrete_Obs, Action => Action, Reward => Get_Reward(Step_Output));
               State_Encountered_Flag(Discrete_Obs) := True;
               Last_I := I;
               Terminated := Get_Terminated(Step_Output);
               exit when Terminated;
               Discrete_Obs := To_Discrete_Observation(Get_Observation(Step_Output));
            end loop;
         end if;

         -- For first occurrent of state s and action a, record total reward
         First_Visit_Reward_Update(State_Action_Reward_Tracker, SAR_Array, Last_I);

         -- Update action values
         for S in Discrete_Observation_Type loop
            if State_Encountered_Flag(S) then
               for A in Action_Type loop
                  if State_Action_Reward_Tracker(S, A).Count > 0 then -- TODO
                     -- TODO: Long_Float
                     Q(S, A) := Float(State_Action_Reward_Tracker(S, A).Total_Reward / Long_Float(State_Action_Reward_Tracker(S, A).Count));
                  end if;
               end loop;
            end if;
         end loop;
         
         -- Use arg max to update policy
         Prev_Policy := Policy;
         Policy := Update_Policy(Q, Prev_Policy, State_Encountered_Flag);
         -- At this time we donn't consider an early exit based on policy stability
      end loop;
      return Evaluation_Results_Type'(Q => Q, Policy => Policy);
   end MC_Exploring_Starts_Evaluation;
   
   -- IN PROGRESS: Should we use MC_Config_Type
   -- TODO: Should we add a condition so that 1/Action_Count <= 1 - epsilon 
   --       (which is equivalent to epsilon <= 1 - 1/Action_Count).
   function MC_Epsilon_Soft_Evaluation(Env_Config : Config_Type; MC_Config : MC_Epsilon_Soft_Config_Type) return Evaluation_Results_Type is
      type Reward_Tracker_Type is record
         Total_Reward : Long_Float;  -- TODO: Using Long_Float to see what happens
         Count : Integer;
      end record;

      type State_Action_Reward_Tracker_Type is array(Discrete_Observation_Type, Action_Type) of Reward_Tracker_Type;
      State_Action_Reward_Tracker : State_Action_Reward_Tracker_Type := (others => (others => (0.0, 0)));

       type SAR_Type is record
         State : Discrete_Observation_Type;
         Action : Action_Type;
         Reward : Float;
      end record;

      type SAR_Array_Type is array (1 .. Max_Episode_Steps) of SAR_Type;
      SAR_Array : SAR_Array_Type;
  
      package Unif_Random renames Ada.Numerics.Float_Random;
      Unif_Gen : Unif_Random.Generator;

      type Action_Probability_Type is array (Action_Type) of Float;

      function Get_Random_Action(Action_Probability: Action_Probability_Type) return Action_Type is
          U : Float := Unif_Random.Random(Unif_Gen);
          Cum_Prob : Float := 0.0;
      begin
          for A in Action_Type loop
              Cum_Prob := Cum_Prob + Action_Probability(A);
              if U < Cum_Prob then
                  return A;
              end if;
          end loop;
          return Action_Type'Last;
      end Get_Random_Action;
      
      function Get_Random_Action(Mixed_Policy: Mixed_Policy_Type; S: Discrete_Observation_Type) return Action_Type is
         Action_Probability: Action_Probability_Type;
      begin
         for A in Action_Type loop
            Action_Probability(A) := Mixed_Policy(S, A);
         end loop;
         return Get_Random_Action(Action_Probability);
      end Get_Random_Action;

      -- NOTE: For epsilon soft, unlike for exploring starts,
      --       the flag for indicating whether a state was encountered in an episode
      --       does have some impact when updating the action values.
      type State_Encountered_Flag_Type is array(Discrete_Observation_Type) of Boolean;
      State_Encountered_Flag : State_Encountered_Flag_Type;

      Greedy_Policy : Policy_Type;

      Action_Count : Natural := Action_Type'Pos(Action_Type'Last) - Action_Type'Pos(Action_Type'First) + 1;
      -- Prev_Policy : Mixed_Policy_Type;
      Mixed_Policy : Mixed_Policy_Type := (others => (others => 1.0 / Float(Action_Count)));  -- TODO: Is this what we want?
      Q : State_Action_Value_Type := (others => (others => 0.0));
      
      Env : Environment_Type := Make(Env_Config);
      Seed_Reset : Seed_Reset_Type := (Kind => Set_Default);
      Obs : Observation_Type;
      Step_Output : Step_Return_Type;
      Discrete_Obs : Discrete_Observation_Type;
      Action : Action_Type;
      Terminated : Boolean;
      Last_I : Natural;
      Stable : Boolean;
     
      -- TODO: The following is identical to the version defined above, so it might make sense
      --       to promote this function.
      procedure First_Visit_Reward_Update(Q_Tracker: in out State_Action_Reward_Tracker_Type; SAR_Array_Input : SAR_Array_Type; Last_I : Natural) is
         SAR_Array : SAR_Array_Type := SAR_Array_Input; -- A local copy to avoid specifying "in out" for the array parameter

         type First_Visit_Array_Type is array(Discrete_Observation_Type) of Natural;
         First_Visit_Array : First_Visit_Array_Type := (others => 0);

         -- Local variables
         -- First visit index
         First_Visit : Natural;
         A : Action_Type;
      begin
         for I in reverse 1 .. Last_I loop
            if I < Last_I then
               SAR_Array(I).Reward := SAR_Array(I).Reward + MC_Config.Discount_Factor * SAR_Array(I + 1).Reward;
            end if;
            First_Visit_Array(SAR_Array(I).State) := I;
         end loop;
         for S in Discrete_Observation_Type loop
            if First_Visit_Array(S) > 0 then
               First_Visit := First_Visit_Array(S);
               A := SAR_Array(First_Visit).Action;
               -- NOTE: SAR_Array(First_Visit).State should equal S
               -- TODO: Using Long_Float
               Q_Tracker(S, A).Total_Reward := Q_Tracker(S, A).Total_Reward + Long_Float(SAR_Array(First_Visit).Reward);
               Q_Tracker(S, A).Count        := Q_Tracker(S, A).Count + 1;
            end if;
         end loop;
      end First_Visit_Reward_Update;

      function Best_Policy(Q : State_Action_Value_Type) return Policy_Type is
         Res : Policy_Type;
         Best_Action : Action_Type;
         Max_Value : Float;
      begin
         for S in Discrete_Observation_Type loop
            Max_Value := Float'First;
            Best_Action := Action_Type'First;
            for A in Action_Type loop
               if Q(S, A) > Max_Value then
                  Max_Value := Q(S, A);
                  Best_Action := A;
               end if;
            end loop;
            Res(S) := Best_Action;
         end loop;
         return Res;
      end Best_Policy;

      function Epsilon_Soft_Policy_Update (MC_Config : MC_Epsilon_Soft_Config_Type; Mixed_Policy_Prev: Mixed_Policy_Type; Policy: Policy_Type; State_Encountered_Flag: State_Encountered_Flag_Type) return Mixed_Policy_Type is
         Best_Action : Action_Type;
         Mixed_Policy : Mixed_Policy_Type := Mixed_Policy_Prev;
         Eps : Float := MC_Config.Epsilon;
      begin
         for S in Discrete_Observation_Type loop
            if State_Encountered_Flag(S) then
               Best_Action := Policy(S);
               for A in Action_Type loop
                  if A = Best_Action then
                     Mixed_Policy(S, A) := 1.0 - Eps + Eps / Float(Action_Count);
                  else
                     Mixed_Policy(S, A) := Eps / Float(Action_Count);
                  end if;
               end loop;
            end if;
         end loop;
         return Mixed_Policy;
      end Epsilon_Soft_Policy_Update;
   begin
      Unif_Random.Reset(Unif_Gen);
      for Episode in 1 .. MC_Config.Num_Episodes loop
         -- Initialize Env and select a random action.
         State_Encountered_Flag := (others => False);
         Obs := Reset(Env, Seed_Reset);
         Discrete_Obs := To_Discrete_Observation(Obs);
         for I in 1 .. Max_Episode_Steps loop
            Action := Get_Random_Action(Mixed_Policy, Discrete_Obs);
            Step_Output := Step(Env, Action);
            SAR_Array(I) := (State => Discrete_Obs, Action => Action, Reward => Get_Reward(Step_Output));
            Last_I := I;
            State_Encountered_Flag(Discrete_Obs) := True;
            Terminated := Get_Terminated(Step_Output);
            exit when Terminated;
            -- Set state to next state
            Discrete_Obs := To_Discrete_Observation(Get_Observation(Step_Output));
         end loop;

         -- For first occurrent of state s and action a, record total reward
         First_Visit_Reward_Update(State_Action_Reward_Tracker, SAR_Array, Last_I);

         -- Update action values
         for S in Discrete_Observation_Type loop
            -- TODO: Do we want to put a conditional on State_Encountered_Flag(S)?
            for A in Action_Type loop
               if State_Action_Reward_Tracker(S, A).Count > 0 then
                  -- TODO: Long_Float
                  Q(S, A) := Float(State_Action_Reward_Tracker(S, A).Total_Reward / Long_Float(State_Action_Reward_Tracker(S, A).Count));
               end if;
            end loop;
         end loop;
         
         -- Use arg max to update policy
         Greedy_Policy := Best_Policy(Q);
         Mixed_Policy := Epsilon_Soft_Policy_Update (MC_Config, Mixed_Policy, Greedy_Policy, State_Encountered_Flag);
         -- TODO: Consider what an exit for an epsilon-soft on-policy evaluation might look like.
      end loop;
      return Evaluation_Results_Type'(Q => Q, Policy => Greedy_Policy);
   end MC_Epsilon_Soft_Evaluation;
end RL.Algorithms.MC;
