
package body Frozen_Lake is
   function Position_Inc(Rows : Positive; Cols : Positive; Position: Position_Type; Action: Action_Type) return Position_Type is
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
      return (Position => New_Position, Reward => Reward, Truncated => Terminated);
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
      -- function Get_Num_Rows(Map_Name : Map_Type) return Positive is
      --    case Map_Name is
      --       when Map_4x4 => return 4;
      --       when Map_8x8 => return 8;
      --    end case;
      -- end Get_Num_Rows;
      -- 
      -- function Get_Num_Cols(Map_Name : Map_Type) return Positive is
      --    case Map_Name is
      --       when Map_4x4 => return 4;
      --       when Map_8x8 => return 8;
      --    end case;
      -- end Get_Num_Cols;

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
      Start_Position : Position_Type := Get_Start_Position(Map); -- Position_Type := (Row => 1, Col => 1);

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
                        P(I, J)(A, A_Act) := (Probability => 1.0, Position => (Row => I, Col => J), Reward => 0.0, Truncated => True);
                     else
                        P(I, J)(A, A_Act) := (Probability => 0.0, Position => (Row => I, Col => J), Reward => 0.0, Truncated => True);
                     end if;
                  end loop;
               end loop;
            end if;

            for A in Action_Type loop
               for A_Actual in Action_Type loop
                  Temp_Partial_Transition := Update_Probability_Matrix(Map, (Row => I, Col => J), A_Actual);
                  if Config.Slippery then
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
                     Truncated => Temp_Partial_Transition.Truncated
                  );
               end loop;
            end loop;
         end loop;
      end loop;
      
      -- -- Determine the start position
      -- -- Unlike the Python version, we assume that there is either one start position or no start position
      -- -- is defined, in which case we take the top left corner as the start position.
      -- -- The following loop finds the start position if it is provided.  The loop exits
      -- -- as soon as the start position is found.
      -- Search_Start_Position:
      -- for I in Map'Range(1) loop
      --    for J in Map'Range(2) loop
      --       if Map(I, J) = S then
      --          Start_Position := (Row => I, Col => J);
      --          exit Search_Start_Position;
      --       end if;
      --    end loop;
      -- end loop Search_Start_Position;

      return (
         Rows => Rows, Cols => Cols,
         Map => Map, P => P,
         Agent_Position => Start_Position
      );
   end Make;

   function Reset(Env : in out Environment_State) return Observation_Type is
      Result : Observation_Type; --  := Observation_Type'(Position_Index => To_S(Env.Map, Env.Agent_Position));
   begin
      Float_Random.Reset(Gen);
      -- DONE: Determine the start position
      Env.Agent_Position := Get_Start_Position(Env.Map);
      Result := Observation_Type'(Position_Index => To_S(Env.Map, Env.Agent_Position));
      -- TODO: In the Python version, the info returned is {"prob": 1}
      return Result;
   end Reset;
    -- def reset(
    --     self,
    --     *,
    --     seed: Optional[int] = None,
    --     options: Optional[dict] = None,
    -- ):
    --     super().reset(seed=seed)
    --     self.s = categorical_sample(self.initial_state_distrib, self.np_random)
    --     self.lastaction = None
    --     return int(self.s), {"prob": 1}

   
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
         State => Observation_Type'(Position_Index => To_S(Env.Map, Transition.Position)),
         Reward => Transition.Reward,
         Terminated => Transition.Truncated,
         Done => False
      );
   begin
      -- Update the Agent's position based on the transition
      Env.Agent_Position := Transition.Position;
      return Result;
   end Step;
    -- def step(self, a):
    --     transitions = self.P[self.s][a]
    --     i = categorical_sample([t[0] for t in transitions], self.np_random)
    --     p, s, r, t = transitions[i]
    --     self.s = s
    --     self.lastaction = a

    --     if self.render_mode == "human":
    --         self.render()
    --     return (int(s), r, t, False, {"prob": p})
    
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
      end loop;
   end Render_Text;
end Frozen_Lake;

-- # DFS to check that it's a valid path.
-- def is_valid(board: List[List[str]], max_size: int) -> bool:
--     frontier, discovered = [], set()
--     frontier.append((0, 0))
--     while frontier:
--         r, c = frontier.pop()
--         if not (r, c) in discovered:
--             discovered.add((r, c))
--             directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]
--             for x, y in directions:
--                 r_new = r + x
--                 c_new = c + y
--                 if r_new < 0 or r_new >= max_size or c_new < 0 or c_new >= max_size:
--                     continue
--                 if board[r_new][c_new] == "G":
--                     return True
--                 if board[r_new][c_new] != "H":
--                     frontier.append((r_new, c_new))
--     return False
-- 
-- 
-- def generate_random_map(size: int = 8, p: float = 0.8) -> List[str]:
--     """Generates a random valid map (one that has a path from start to goal)
-- 
--     Args:
--         size: size of each side of the grid
--         p: probability that a tile is frozen
-- 
--     Returns:
--         A random valid map
--     """
--     valid = False
--     board = []  # initialize to make pyright happy
-- 
--     while not valid:
--         p = min(1, p)
--         board = np.random.choice(["F", "H"], (size, size), p=[p, 1 - p])
--         board[0][0] = "S"
--         board[-1][-1] = "G"
--         valid = is_valid(board, size)
--     return ["".join(x) for x in board]
-- 
-- 
