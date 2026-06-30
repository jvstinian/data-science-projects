
package body RL.Envs.Frozenlake is
   function Get_Map_Info(Map_Name: Map_Type) return Map_Info_Type is
   begin
      case Map_Name is
         when Map_4x4 => return Map_Info_Type'(Map_Name => Map_4x4, Rows => 4, Cols => 4);
         when Map_8x8 => return Map_Info_Type'(Map_Name => Map_8x8, Rows => 8, Cols => 8);
      end case;
   end Get_Map_Info;

   function Position_Inc(Rows, Cols : Positive; Position: Position_Type; Action: Action_Type) return Position_Type is
      New_Position : Position_Type := Position;
   begin
      case Action is
         when Left => New_Position.Col := Positive'Max(New_Position.Col - 1, 1);
         when Down => New_Position.Row := Positive'Min(New_Position.Row + 1, Rows);
         when Right => New_Position.Col := Positive'Min(New_Position.Col + 1, Cols);
         when Up => New_Position.Row := Positive'Max(New_Position.Row - 1, 1);
      end case;
      return New_Position;
   end Position_Inc;

   function Update_Probability_Matrix(Map : Map_Array; Position: Position_Type; Action : Action_Type) return Partial_Transition_Type is
      New_Position : Position_Type := Position_Inc(Map'Length(1), Map'Length(2), Position, Action);
      New_Letter : Map_Element := Map(New_Position.Row, New_Position.Col);
      Terminated : Boolean := (New_Letter = H) or else (New_Letter = G);
      Reward : Float := Float(Boolean'Pos(New_Letter = G)); -- Is 1.0 if New_Letter = G else 0.0
   begin
      return (Position => New_Position, Reward => Reward, Terminated => Terminated);
   end Update_Probability_Matrix;

   -- Note the following is different from the Python implementation as internally we
   -- track the Agent's position using 1-based indexing.
   -- To keep the observations consistent with the Python implementation,
   -- we convert the position to a 0-based index.
   function To_S(Map: Map_Array; Position: Position_Type) return Natural is
      Row : Positive := Position.Row;
      Col : Positive := Position.Col;
      Num_Col : Positive := Map'Length(2);
   begin
      return (Row - 1) * Num_Col + (Col - 1);
   end To_S;

   function Get_Start_Position(Map: Map_Array) return Position_Type is
      Start_Position : Position_Type := (Row => 1, Col => 1);
   begin
      -- Determine the start position
      -- Unlike the Python version, we assume that there is either one start position or no start position
      -- is defined, in which case we take the top left corner as the start position.
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

   function Make(Config: Environment_Config) return Environment_State is

      function Get_Map (Map_Name : Map_Type) return Map_Array is begin
         case Map_Name is
            when Map_4x4 => return Map_Array'(
               (S, F, F, F), (F, H, F, H),
               (F, F, F, H), (H, F, F, G));
            when Map_8x8 => return Map_Array'(
               (S, F, F, F, F, F, F, F),
               (F, F, F, F, F, F, F, F),
               (F, F, F, H, F, F, F, F),
               (F, F, F, F, F, H, F, F),
               (F, F, F, H, F, F, F, F),
               (F, H, H, F, F, F, H, F),
               (F, H, F, F, H, F, H, F),
               (F, F, F, H, F, F, F, G));
         end case;
      end Get_Map;

      function Can_Slip(Intended_Action : Action_Type; Actual_Action : Action_Type) return Boolean is
      begin
         case Intended_Action is
            when Left => return (Actual_Action = Left) or else (Actual_Action = Up) or else (Actual_Action = Down);
            when Down => return (Actual_Action = Down) or else (Actual_Action = Left) or else (Actual_Action = Right);
            when Right => return (Actual_Action = Right) or else (Actual_Action = Down) or else (Actual_Action = Up);
            when Up => return (Actual_Action = Up) or else (Actual_Action = Right) or else (Actual_Action = Left);
         end case;
      end Can_Slip;
      
      Map : Map_Array := Get_Map(Config.Map_Name);
      Rows : Positive := Map'Length(1);
      Cols : Positive := Map'Length(2);
      
      P : Map_Transitions(1 .. Rows, 1 .. Cols);
      Start_Position : Position_Type := Get_Start_Position(Map);

      Temp_Partial_Transition : Partial_Transition_Type;
      Temp_Probability : Float;
   begin
      for I in P'Range(1) loop
         for J in P'Range(2) loop
            if Map(I, J) in G | H then
               -- We handle the case where the Agent is already at the goal or in a hole
               -- This case should not occur in practice
               for A in Action_Type loop
                  for A_Act in Action_Type loop
                     if A = A_Act then
                        P(I, J)(A, A_Act) := (Probability => 1.0, Position => (Row => I, Col => J), Reward => 0.0, Terminated => True);
                     else
                        P(I, J)(A, A_Act) := (Probability => 0.0, Position => (Row => I, Col => J), Reward => 0.0, Terminated => True);
                     end if;
                  end loop;
               end loop;
            else
               -- Non-terminal states are considered here
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
                     P(I, J)(A, A_Actual) := (
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
         Rows => Rows, Cols => Cols,
         Map => Map, P => P,
         Agent_Position => Start_Position
      );
   end Make;

   function Reset(Env : in out Environment_State) return Observation_Type is
      -- TODO: No need to have a Result variable
      Result : Observation_Type;
   begin
      Float_Random.Reset(Gen);
      Env.Agent_Position := Get_Start_Position(Env.Map);
      Result := Observation_Type'(Position_Index => To_S(Env.Map, Env.Agent_Position));
      return Result;
   end Reset;
   
   function Step(Env : in out Environment_State; Action: Action_Type) return Step_Return_Type is
      -- Helper functions
      type Cumulative_Probability_Type is array (Action_Type) of Float;
      
      function Get_Cumulative_Probability(P : Map_Transitions; Position : Position_Type; Action : Action_Type) return Cumulative_Probability_Type is
         Cumulative_Probability : Cumulative_Probability_Type := (others => 0.0);

         I : Positive := Position.Row;
         J : Positive := Position.Col;

         Temp_Cumulative_Probability : Float := 0.0;
      begin
         for A_Act in Action_Type loop
            Cumulative_Probability(A_Act) := P(I, J)(Action, A_Act).Probability + Temp_Cumulative_Probability;
            Temp_Cumulative_Probability := Cumulative_Probability(A_Act);
         end loop;
         return Cumulative_Probability;
      end Get_Cumulative_Probability;
      
      function Get_Random_Transition(P : Map_Transitions; Position : Position_Type; Action : Action_Type) return Transition_Type is
         Cumulative_Probability : Cumulative_Probability_Type := Get_Cumulative_Probability(P, Position, Action);
         Rand : Float := Float_Random.Random(Gen);
      begin
         for A_Act in Action_Type loop
            if Rand <= Cumulative_Probability(A_Act) then
               return P(Position.Row, Position.Col)(Action, A_Act);
            end if;
         end loop;
         -- The following should never be reached as the cumulative probabilities should sum to 1.0
         return P(Position.Row, Position.Col)(Action, Action_Type'First);
      end Get_Random_Transition;

      -- Sample to obtain the transition based on the current position and the action taken by the Agent
      Transition : Transition_Type := Get_Random_Transition(Env.P, Env.Agent_Position, Action);

      Result : Step_Return_Type := (
         Observation => Observation_Type'(Position_Index => To_S(Env.Map, Transition.Position)),
         Reward => Transition.Reward,
         Terminated => Transition.Terminated
      );
   begin
      -- Update the Agent's position based on the transition
      Env.Agent_Position := Transition.Position;
      return Result;
   end Step;

   procedure Render_Text(Env : Environment_State) is
   begin
      for I in Env.Map'Range(1) loop
         for J in Env.Map'Range(2) loop
            if Env.Agent_Position = Position_Type'(I, J) then
               Put("A");
            else
               case Env.Map(I, J) is
                  when S => Put("S");
                  when F => Put("F");
                  when H => Put("H");
                  when G => Put("G");
               end case;
            end if;
         end loop; -- J
         New_Line;
      end loop; -- I
   end Render_Text;
   
   function Get_Model(Config: Environment_Config) return Discrete_Model_Type is
      Res : Discrete_Model_Type := (others => (others => (others => (Probability => 0.0, Reward => 0.0))));
      Env : Environment_State := Make(Config);
      Prev_State : Discrete_State_Type;
      Next_State : Discrete_State_Type;

      type Expected_Reward_Type is record
         Probability_Weighted_Reward : Float := 0.0;
         Total_Probability : Float := 0.0;
      end record;
      type Expected_Rewards_Type is array (Discrete_State_Type) of Expected_Reward_Type;

      Temp_Expected_Rewards : Expected_Rewards_Type;

      Temp_Probability : Float;
      Temp_Reward : Float;
   begin
      for I in Env.P'Range(1) loop
         for J in Env.P'Range(2) loop
            Prev_State := Discrete_State_Type(To_S(Env.Map, Position_Type'(Row => I, Col => J)));
            for A in Action_Type loop
               -- When the frozen lake is slippery, an action can lead to state transitions
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
                  Next_State := Discrete_State_Type(To_S(Env.Map, Env.P(I, J)(A, A_Act).Position));
                  Temp_Probability := Env.P(I, J)(A, A_Act).Probability;
                  Temp_Reward := Env.P(I, J)(A, A_Act).Reward;
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
end RL.Envs.Frozenlake;
