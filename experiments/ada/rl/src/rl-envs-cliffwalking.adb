with Ada.Text_IO; use Ada.Text_IO;
-- from gymnasium import Env, spaces
-- from gymnasium.envs.toy_text.utils import categorical_sample

package body RL.Envs.Cliffwalking is

   -- The following is similar to the version in Frozen_Lake,
   -- except that we don't need to provide the number of rows
   -- and columns as inputs as they are constants in Cliff Walking.
   function Position_Inc(Position: Position_Type; Action: Action_Type) return Position_Type is
      New_Position : Position_Type := Position;
   begin
      case Action is
         when Left => New_Position.Col := Positive'Max(New_Position.Col - 1, 1);
         when Down => New_Position.Row := Positive'Min(New_Position.Row + 1, Num_Rows);
         when Right => New_Position.Col := Positive'Min(New_Position.Col + 1, Num_Cols);
         when Up => New_Position.Row := Positive'Max(New_Position.Row - 1, 1);
      end case;
      return New_Position;
   end Position_Inc;

   -- The following is adapted from Frozen_Lake with the changes noted in the comments.
   function Update_Probability_Matrix(Map : Map_Array; Position: Position_Type; Action : Action_Type) return Partial_Transition_Type is
      New_Position : Position_Type := Position_Inc(Position, Action);
      New_Letter : Map_Element := Map(New_Position.Row, New_Position.Col);
      -- Terminated differs from the approach in Frozen_Lake.  If the agent falls off the cliff,
      -- the agent is sent back to the start with a reward of -100 for the step rather than
      -- terminating the episode.
      -- We only terminate if the agent reaches the goal.
      Terminated : Boolean := (New_Letter = G);
      Reward : Float := -1.0;  -- Reward of -1 unless the agent falls off the cliff
   begin
      if New_Letter = C then
         -- Set reward to -100 when the agent falls off the cliff, and
         -- send the agent back to the start position.
         Reward := -100.0;
         New_Position := Get_Start_Position(Map);
      end if;
      return (Position => New_Position, Reward => Reward, Terminated => Terminated);
   end Update_Probability_Matrix;
   
   -- We use the approach from Frozen_Lake, except we return an Observation_Type
   -- rather than a Natural, as the Observation_Type is a new Natural.
   -- We don't provide the Map as an input in Cliff Walking, as in the Frozen Lake
   -- environment that was used for the number of columns, which is a constant
   -- in Cliff Walking and is read directly from the package variable.
   -- Note the following is different from the Python implementation as internally we
   -- track the Agent's position using 1-based indexing.
   -- To keep the observations consistent with the Python implementation,
   -- we convert the position to a 0-based index.
   function To_S(Position: Position_Type) return Observation_Type is
      Row : Positive := Position.Row;
      Col : Positive := Position.Col;
      Position_Index : Natural := (Row - 1) * Num_Cols + (Col - 1);
   begin
      return Observation_Type(Position_Index);
   end To_S;

   -- The following is identical to the version in Frozen_Lake, except
   -- the default is the lower left corner (4, 1) rather than the upper left corner.
   -- Since there's only one map and one start position when Cliff Walking,
   -- we could simplify this to just return the unique start position (4, 1).
   -- We also note that we 1-index the map which means the lower left corner is (4, 1)
   -- rather than (3, 0) as in the Python version.
   function Get_Start_Position(Map: Map_Array) return Position_Type is
      Start_Position : Position_Type := (Row => 4, Col => 1);
   begin
      -- Determine the start position
      -- Unlike the Python version, we assume that there is either one start position or no start position
      -- is defined, in which case we take the lower left corner as the start position.
      -- The following loop finds the start position if it is provided.  The loop exits
      -- as soon as the start position is found.
      Search_Start_Position:
      for I in Map'Range(1) loop
         for J in Map'Range(2) loop
            if Map(I, J) = S then
               Start_Position := (Row => I, Col => J);
               exit Search_Start_Position;
            end if;
         end loop;
      end loop Search_Start_Position;
      return Start_Position;
   end Get_Start_Position;

   function Make(config: Config_Type) return Environment_Type is
      Map : Map_Array := Map_Array'(
               1 .. 3 => (P, P, P, P, P, P, P, P, P, P, P, P),
               4      => (S, C, C, C, C, C, C, C, C, C, C, G));

      function Can_Slip(Intended_Action : Action_Type; Actual_Action : Action_Type) return Boolean is
      begin
         case Intended_Action is
            when Left => return (Actual_Action = Left) or else (Actual_Action = Up) or else (Actual_Action = Down);
            when Down => return (Actual_Action = Down) or else (Actual_Action = Left) or else (Actual_Action = Right);
            when Right => return (Actual_Action = Right) or else (Actual_Action = Down) or else (Actual_Action = Up);
            when Up => return (Actual_Action = Up) or else (Actual_Action = Right) or else (Actual_Action = Left);
         end case;
      end Can_Slip;
      
      P : Map_Transitions;
      Start_Position : Position_Type := Get_Start_Position(Map);

      Temp_Partial_Transition : Partial_Transition_Type;
      Temp_Probability : Float;
   begin
      for I in P'Range(1) loop
         for J in P'Range(2) loop
            if Map(I, J) in G | C then
               -- We handle the case where the Agent is already at the goal or 
               -- somehow permanently fell off the cliff.
               -- This case should not occur in practice
               for A in Action_Type loop
                  for A_Act in Action_Type loop
                     if A = A_Act then
                        P(I, J, A, A_Act) := (Probability => 1.0, Position => (Row => I, Col => J), Reward => 0.0, Terminated => True);
                     else
                        P(I, J, A, A_Act) := (Probability => 0.0, Position => (Row => I, Col => J), Reward => 0.0, Terminated => True);
                     end if;
                  end loop;
               end loop;
            else
               for A in Action_Type loop
                  for A_Actual in Action_Type loop
                     Temp_Partial_Transition := Update_Probability_Matrix(Map, (Row => I, Col => J), A_Actual);
                     if Config.Is_Slippery then
                        if Can_Slip(A, A_Actual) then
                           Temp_Probability := 1.0 / 3.0;
                        else 
                           Temp_Probability := 0.0;
                        end if;
                     else
                        if A = A_Actual then
                           Temp_Probability := 1.0;
                        else 
                           Temp_Probability := 0.0;
                        end if;
                     end if;
                     P(I, J, A, A_Actual) := (
                        Probability => Temp_Probability,
                        Position => Temp_Partial_Transition.Position,
                        Reward => Temp_Partial_Transition.Reward,
                        Terminated => Temp_Partial_Transition.Terminated
                     );
                  end loop;
               end loop;
            end if;
         end loop;
      end loop;
      
      return (
         Map => Map, P => P, Agent_Position => Start_Position, Gen => <>
      );
   end Make;
   
   function Step(Env : in out Environment_Type; Action: Action_Type) return Step_Return_Type is
      -- Helper functions
      type Cumulative_Probability_Type is array (Action_Type) of Float;
      
      function Get_Cumulative_Probability(P : Map_Transitions; Position : Position_Type; Action : Action_Type) return Cumulative_Probability_Type is
         Cumulative_Probability : Cumulative_Probability_Type := (others => 0.0);

         I : Positive := Position.Row;
         J : Positive := Position.Col;

         Temp_Cumulative_Probability : Float := 0.0;
      begin
         for A_Act in Action_Type loop
            Cumulative_Probability(A_Act) := P(I, J, Action, A_Act).Probability + Temp_Cumulative_Probability;
            Temp_Cumulative_Probability := Cumulative_Probability(A_Act);
         end loop;
         return Cumulative_Probability;
      end Get_Cumulative_Probability;
      
      function Get_Random_Transition(Env: in out Environment_Type; Action : Action_Type) return Transition_Type is
         P : Map_Transitions := Env.P;
         Position : Position_Type := Env.Agent_Position;
         Cumulative_Probability : Cumulative_Probability_Type := Get_Cumulative_Probability(P, Position, Action);
         Rand : Float := Float_Random.Random(Env.Gen);
      begin
         for A_Act in Action_Type loop
            if Rand <= Cumulative_Probability(A_Act) then
               return P(Position.Row, Position.Col, Action, A_Act);
            end if;
         end loop;
         -- The following should never be reached as the cumulative probabilities should sum to 1.0
         return P(Position.Row, Position.Col, Action, Action_Type'First);
      end Get_Random_Transition;

      -- Sample to obtain the transition based on the current position and the action taken by the Agent
      Transition : Transition_Type := Get_Random_Transition(Env, Action);
   begin
      -- Update the Agent's position based on the transition
      Env.Agent_Position := Transition.Position;
      return Step_Return_Type'(
         Observation => To_S(Transition.Position),
         Reward => Transition.Reward,
         Terminated => Transition.Terminated
      );
   end Step;

   function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type) return Observation_Type is
      Result : Observation_Type;
   begin
      case Seed_Reset.Kind is
         when Set_Default => Float_Random.Reset(Env.Gen);
         when No_Set      => null;
         when Set_Seed    => Float_Random.Reset(Env.Gen, Seed_Reset.Seed);
      end case;
      Env.Agent_Position := Get_Start_Position(Env.Map);
      Result := To_S(Env.Agent_Position);
      return Result;
   end Reset;

   procedure Render_Text(Env : Environment_Type) is
   begin
      for I in Env.Map'Range(1) loop
         for J in Env.Map'Range(2) loop
            -- Put a leading space for all columns except the first to match
            -- the formatting of the Python version.
            if J /= Env.Map'First(2) then
               Put(" ");
            end if;

            if Env.Agent_Position = Position_Type'(I, J) then
               Put("A");
            else
               case Env.Map(I, J) is
                  when S => Put("o");
                  when P => Put("o");
                  when C => Put("C");
                  when G => Put("T");
               end case;
            end if;

            -- Put a trailing space for all columns except the last to match
            -- the formatting of the Python version.
            if J /= Env.Map'Last(2) then
               Put(" ");
            end if;
         end loop; -- J
         New_Line;
      end loop;
   end Render_Text;

   -- We follow the approach used for Frozenlake
   function Get_Model(Config: Config_Type) return DP_Model_Type is
      Res : DP_Model_Type := (others => (others => (others => (Probability => 0.0, Reward => 0.0))));
      Env : Environment_Type := Make(Config);
      Prev_State : State_Type;
      Next_State : State_Type;

      type Expected_Reward_Type is record
         Probability_Weighted_Reward : Float := 0.0;
         Total_Probability : Float := 0.0;
      end record;
      type Expected_Rewards_Type is array (State_Type) of Expected_Reward_Type;

      Temp_Expected_Rewards : Expected_Rewards_Type;

      Temp_Probability : Float;
      Temp_Reward : Float;
   begin
      for I in Env.P'Range(1) loop
         for J in Env.P'Range(2) loop
            Prev_State := State_Type(To_S(Position_Type'(Row => I, Col => J)));
            for A in Action_Type loop
               -- When the cliff is slippery, an action can lead to state transitions
               -- with different probabilities.
               -- This can be seen when in a corner cell of the map, in which case
               -- an action that would take you off the board (if there was no slipping)
               -- will result in arriving at the same cell 2/3 of the time.
               -- To obtain the correct values, we calculate the conditional expectation for
               -- the state transitions.
               -- This should also generalize if we were to consider state transitions with
               -- non-uniform probabilities.
               Temp_Expected_Rewards := (others => (Probability_Weighted_Reward => 0.0, Total_Probability => 0.0));

               for A_Act in Action_Type loop
                  Next_State := State_Type(To_S(Env.P(I, J, A, A_Act).Position));
                  Temp_Probability := Env.P(I, J, A, A_Act).Probability;
                  Temp_Reward := Env.P(I, J, A, A_Act).Reward;
                  Temp_Expected_Rewards(Next_State).Probability_Weighted_Reward := Temp_Expected_Rewards(Next_State).Probability_Weighted_Reward + Temp_Probability * Temp_Reward;
                  Temp_Expected_Rewards(Next_State).Total_Probability := Temp_Expected_Rewards(Next_State).Total_Probability + Temp_Probability;
               end loop;
               -- Now that we've processed the possible transitions and their probabilities for a given action,
               -- we calculate the discrete transition probabilities and conditional rewards
               for Next_State in Temp_Expected_Rewards'Range loop
                  if Temp_Expected_Rewards(Next_State).Total_Probability > 0.0 then
                     Res(Prev_State, A, Next_State) := (
                        Probability => Temp_Expected_Rewards(Next_State).Total_Probability,
                        Reward => Temp_Expected_Rewards(Next_State).Probability_Weighted_Reward / Temp_Expected_Rewards(Next_State).Total_Probability
                     );
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
      return Res;
   end Get_Model;
end RL.Envs.Cliffwalking;
