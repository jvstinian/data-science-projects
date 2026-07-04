with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;


package body RL.Envs.Carrental is
   package Math is new Ada.Numerics.Generic_Elementary_Functions(Float);

   function Poisson_PMF(Lambda : Float; N : Natural) return Float is
      -- PMF of the Poisson distribution: P(X=k) = (lambda^k * e^-lambda) / k!
      -- We can calculate this using a loop to avoid overflow issues with large factorials.
      -- For small values of N, this should be fine. For larger values, we might want to use
      -- a more stable method, such as using logarithms.
      PMF : Float := 1.0;
   begin
      for I in 1 .. N loop -- Empty product when N = 0
         PMF := PMF * (Lambda / Float(I));
      end loop;
      PMF := PMF * Float(Math.Exp(-Lambda));
      return PMF;
   end Poisson_PMF;

   function Poisson_CDF(Lambda : Float; N : Natural) return Float is
      -- CDF of the Poisson distribution: P(N <= k) = sum_{i=0}^k PMF(i)
      PMF : Float := Float(Math.Exp(-Lambda));
      CDF : Float := PMF; -- Start with PMF(0)
   begin
      for I in 1 .. N loop  -- Empty loop when N = 0
         PMF := PMF * (Lambda / Float(I)); -- PMF(i) = PMF(i-1) * (lambda / i)
         CDF := CDF + PMF;
      end loop;
      return CDF;
   end Poisson_CDF;
   
   function Poisson_SF(Lambda : Float; N : Natural) return Float is
   begin
      return 1.0 - Poisson_CDF(Lambda, N);
   end Poisson_SF;

   -- function Position_Inc(Rows : Positive; Cols : Positive; Position: Position_Type; Action: Action_Type) return Position_Type is
   --    New_Position : Position_Type := Position;
   -- begin
   --    case Action is
   --       when Left => New_Position.Col := Positive'Max(New_Position.Col - 1, 1);
   --       when Down => New_Position.Row := Positive'Min(New_Position.Row + 1, Rows);
   --       when Right => New_Position.Col := Positive'Min(New_Position.Col + 1, Cols);
   --       when Up => New_Position.Row := Positive'Max(New_Position.Row - 1, 1);
   --    end case;
   --    return New_Position;
   -- end Position_Inc;

   -- function Update_Probability_Matrix(Map : Map_Array; Position: Position_Type; Action : Action_Type) return Partial_Transition_Type is
   --    New_Position : Position_Type := Position_Inc(Map'Length(1), Map'Length(2), Position, Action);
   --    New_Letter : Map_Element := Map(New_Position.Row, New_Position.Col);
   --    Terminated : Boolean := (New_Letter = H) or else (New_Letter = G);
   --    Reward : Float := Float(Boolean'Pos(New_Letter = G)); -- Is 1.0 if New_Letter = G else 0.0
   -- begin
   --    return (Position => New_Position, Reward => Reward, Terminated => Terminated);
   -- end Update_Probability_Matrix;

   function To_Discrete_State(Cars_Per_Lot : Cars_Per_Lot_Type) return Discrete_State_Type is
   begin
      return Discrete_State_Type(Cars_Per_Lot.Lot_A_Cars * (Lot_Size + 1) + Cars_Per_Lot.Lot_B_Cars);
   end To_Discrete_State;

   -- function Get_Start_Position(Map: Map_Array) return Position_Type is
   --    Start_Position : Position_Type := (Row => 1, Col => 1);
   -- begin
   --    -- Determine the start position
   --    -- Unlike the Python version, we assume that there is either one start position or no start position
   --    -- is defined, in which case we take the top left corner as the start position.
   --    -- The following loop finds the start position if it is provided.  The loop exits
   --    -- as soon as the start position is found.
   --    Search_Start_Position:
   --    for I in Map'Range(1) loop
   --       for J in Map'Range(2) loop
   --          if Map(I, J) = S then
   --             Start_Position := (Row => I, Col => J);
   --             exit Search_Start_Position;
   --          end if;
   --       end loop;
   --    end loop Search_Start_Position;
   --    return Start_Position;
   -- end Get_Start_Position;

   function Make(Config: Config_Type) return Environment_Type is
   begin
      return Environment_Type'(
         Gen => <>,
         Config => Config,
         Lot_A_Cars => Config.Lot_A_Init_Cars,
         Lot_B_Cars => Config.Lot_B_Init_Cars
      );
   end Make;

   function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type) return Observation_Type is
      Result : Observation_Type;
   begin
      case Seed_Reset.Kind is
         when Set_Default => Float_Random.Reset(Env.Gen);
         when No_Set      => null;
         when Set_Seed    => Float_Random.Reset(Env.Gen, Seed_Reset.Seed);
      end case;
      Env.Lot_A_Cars := Env.Config.Lot_A_Init_Cars;
      Env.Lot_B_Cars := Env.Config.Lot_B_Init_Cars;
      return Observation_Type'(
         Lot_A_Cars => Env.Lot_A_Cars,
         Lot_B_Cars => Env.Lot_B_Cars
      );
   end Reset;
   --  -- def reset(
   --  --     self,
   --  --     *,
   --  --     seed: Optional[int] = None,
   --  --     options: Optional[dict] = None,
   --  -- ):
   --  --     super().reset(seed=seed)
   --  --     self.s = categorical_sample(self.initial_state_distrib, self.np_random)
   --  --     self.lastaction = None
   --  --     return int(self.s), {"prob": 1}

   function Step(Env : in out Environment_Type; Action: Action_Type) return Step_Return_Type is
      -- Helper functions
      function U_Env_Random return Float is
      begin
         return Float_Random.Random(Env.Gen);
      end U_Env_Random;

      function Poisson is new GRF.Poisson(U => U_Env_Random);

      -- Requests and returns
      Lot_A_Requests : Natural := Poisson(Env.Config.Lot_A_Request_Lambda);
      Lot_B_Requests : Natural := Poisson(Env.Config.Lot_B_Request_Lambda);
      Lot_A_Returns : Natural := Poisson(Env.Config.Lot_A_Return_Lambda);
      Lot_B_Returns : Natural := Poisson(Env.Config.Lot_B_Return_Lambda);

      -- Local variables
      Local_Cars_Moved : Natural;
      Lot_A_Rented_Cars : Natural;
      Lot_B_Rented_Cars : Natural;
      Lot_A_Updated_Cars : Natural;
      Lot_B_Updated_Cars : Natural;
   begin
      -- First update for action
      if Action > 0 then
         -- Move cars from A to B
         Local_Cars_Moved := Natural'Min(Natural(Action), Env.Lot_A_Cars);
         Lot_A_Updated_Cars := Env.Lot_A_Cars - Local_Cars_Moved;
         Lot_B_Updated_Cars := Env.Lot_B_Cars + Local_Cars_Moved;  -- TODO: Cap at 20
      else -- Action < 0
         -- Move cars from B to A
         Local_Cars_Moved := Natural'Min(Natural(-Action), Env.Lot_B_Cars);
         Lot_A_Updated_Cars := Env.Lot_A_Cars + Local_Cars_Moved;  -- TODO: Cap at 20
         Lot_B_Updated_Cars := Env.Lot_B_Cars - Local_Cars_Moved;
      end if;
      
      -- Update for the next days rental requests and returns
      -- The number of rentals are based on the currently available cars
      -- and the requested number of cars.
      Lot_A_Rented_Cars := Natural'Min(Lot_A_Updated_Cars, Lot_A_Requests);
      Lot_B_Rented_Cars := Natural'Min(Lot_B_Updated_Cars, Lot_B_Requests);

      -- Update the number of cars returned.
      Lot_A_Updated_Cars := Natural'Min(
         Env.Lot_A_Cars - Lot_A_Rented_Cars + Lot_A_Returns,
         -- Natural'Max(Env.Lot_A_Cars - Lot_A_Requests, 0) + Lot_A_Returns,
         Lot_Size
      );
      Lot_B_Updated_Cars := Natural'Min(
         Env.Lot_B_Cars - Lot_B_Rented_Cars + Lot_B_Returns,
         -- Natural'Max(Env.Lot_B_Cars - Lot_B_Requests, 0) + Lot_B_Returns,
         Lot_Size
      );

      Env.Lot_A_Cars := Lot_A_Updated_Cars;
      Env.Lot_B_Cars := Lot_B_Updated_Cars;

      return Step_Return_Type'(
         Observation => Observation_Type'(Lot_A_Cars => Env.Lot_A_Cars, Lot_B_Cars => Env.Lot_B_Cars),
         Reward => 10.0*Float(Lot_A_Rented_Cars + Lot_B_Rented_Cars) - 2.0 * Float(Local_Cars_Moved),
         Terminated => False
      );
   end Step;
   --  -- def step(self, a):
   --  --     transitions = self.P[self.s][a]
   --  --     i = categorical_sample([t[0] for t in transitions], self.np_random)
   --  --     p, s, r, t = transitions[i]
   --  --     self.s = s
   --  --     self.lastaction = a

   --  --     if self.render_mode == "human":
   --  --         self.render()
   --  --     return (int(s), r, t, False, {"prob": p})
   
   procedure Render_Text(Env : Environment_Type) is
   begin
      Put_Line("Lot A Cars: " & Env.Lot_A_Cars'Image & ", Lot B Cars: " & Env.Lot_B_Cars'Image);
   end Render_Text;

   function Calculate_Transition_Probability (
      Config : Config_Type;
      Cars_Moved : Natural;
      Prev_Cars : Cars_Per_Lot_Type;
      Next_Cars : Cars_Per_Lot_Type
   ) return Transition_Probability_Type is
      type Probability_Type is array (Natural range <>) of Float;

      -- TODO: Switch to using the following method.
      function Calculate_Lot_Request_Probabilities (Cars_Before : Natural; Cars_After: Natural; Request_Lambda : Float; Return_Lambda : Float) return Probability_Type is
         Max_Interim_Cars : Natural := Natural'Min(Cars_Before, Cars_After);
         Min_Requests : Natural := Cars_Before - Max_Interim_Cars;
         Lot_Returns : Natural;
         Prob_Returns : Float;
         Probability_Requests : Probability_Type(Min_Requests.. Cars_Before);
      begin
         for Lot_Requests in Min_Requests .. Cars_Before loop
            Lot_Returns := Cars_After - (Cars_Before - Lot_Requests);
            Prob_Returns := Poisson_PMF(Return_Lambda, Lot_Returns);
            if Cars_After >= Lot_Size then
               -- If the resulting number of cars is greater than or equal to the lot size,
               -- then returns exceeding Lot_Returns will also result in a full lot.
               Prob_Returns := Prob_Returns + Poisson_SF(Return_Lambda, Lot_Returns);
            end if;
            Probability_Requests(Lot_Requests) := Poisson_PMF(Request_Lambda, Lot_Requests) * Prob_Returns;
         end loop;
         -- For the value Cars_Before, we allow for any number of requests greater than
         -- the current number of cars available, as this results in 0 cars remaining before returns are counted.
         -- Note that we use Prob_Returns defined in the last iteration.
         Probability_Requests(Cars_Before) := Probability_Requests(Cars_Before) + Poisson_SF(Request_Lambda, Cars_Before) * Prob_Returns;
         return Probability_Requests;
      end Calculate_Lot_Request_Probabilities;

      Max_Interim_Cars_Lot_A : Natural := Natural'Min(Prev_Cars.Lot_A_Cars, Next_Cars.Lot_A_Cars);
      Max_Interim_Cars_Lot_B : Natural := Natural'Min(Prev_Cars.Lot_B_Cars, Next_Cars.Lot_B_Cars);

      Min_Requests_A : Natural := Prev_Cars.Lot_A_Cars - Max_Interim_Cars_Lot_A;
      Min_Requests_B : Natural := Prev_Cars.Lot_B_Cars - Max_Interim_Cars_Lot_B;

      Lot_A_Returns : Natural;
      Lot_B_Returns : Natural;
      Probability_Requests_Lot_A : Probability_Type(Min_Requests_A .. Prev_Cars.Lot_A_Cars);
      Probability_Requests_Lot_B : Probability_Type(Min_Requests_B .. Prev_Cars.Lot_B_Cars);
      Prob_Returns : Float;

      Temp_Prob : Float;
      Probability_Weighted_Reward : Float := 0.0;
      Total_Probability : Float := 0.0;

   begin
      -- DONE: Simplify the following using the commented-out block below
      -- DONE: The probability associated with returns needs to account for returns
      --       that would overflow the lot (i.e. result in a number of cars exceeding Lot_Size).
      --       This can only occur if Next_Cars.Lot_A_Cars = Lot_Size.
      -- if Prev_Cars.Lot_A_Cars = 0 then
      --    Probability_Requests_Lot_A(0) := 1.0 * Poisson_PMF(Config.Lot_A_Return_Lambda, Next_Cars.Lot_A_Cars);
      -- else
      --    -- Note the following could be an empty loop
      --    for Lot_A_Requests in Min_Requests_A .. (Prev_Cars.Lot_A_Cars - 1) loop
      --       Lot_A_Returns := Next_Cars.Lot_A_Cars - (Prev_Cars.Lot_A_Cars - Lot_A_Requests);
      --       Probability_Requests_Lot_A(Lot_A_Requests) := Poisson_PMF(Config.Lot_A_Request_Lambda, Lot_A_Requests) * Poisson_PMF(Config.Lot_A_Return_Lambda, Lot_A_Returns);
      --    end loop;
      --    -- For the value Prev_Cars.Lot_A_Cars, we allow for any number of requests greater than or equal
      --    -- to the value, as this results in 0 cars remaining before returns are counted.
      --    Probability_Requests_Lot_A(Prev_Cars.Lot_A_Cars) := Poisson_SF(Config.Lot_A_Request_Lambda, Prev_Cars.Lot_A_Cars - 1) * Poisson_PMF(Config.Lot_A_Return_Lambda, Next_Cars.Lot_A_Cars);
      -- end if;
      for Lot_A_Requests in Min_Requests_A .. Prev_Cars.Lot_A_Cars loop
         Lot_A_Returns := Next_Cars.Lot_A_Cars - (Prev_Cars.Lot_A_Cars - Lot_A_Requests);
         Prob_Returns := Poisson_PMF(Config.Lot_A_Return_Lambda, Lot_A_Returns);
         if Next_Cars.Lot_A_Cars >= Lot_Size then
            -- If the resulting number of cars is greater than or equal to the lot size,
            -- then returns exceeding Lot_A_Returns will also result in a full lot.
            Prob_Returns := Prob_Returns + Poisson_SF(Config.Lot_A_Return_Lambda, Lot_A_Returns);
         end if;
         Probability_Requests_Lot_A(Lot_A_Requests) := Poisson_PMF(Config.Lot_A_Request_Lambda, Lot_A_Requests) * Prob_Returns;
      end loop;
      -- For the value Prev_Cars.Lot_A_Cars, we allow for any number of requests greater than
      -- the current number of cars available, as this results in 0 cars remaining before returns are counted.
      -- Note that we use Prob_Returns defined in the last iteration.
      Probability_Requests_Lot_A(Prev_Cars.Lot_A_Cars) := Probability_Requests_Lot_A(Prev_Cars.Lot_A_Cars) + Poisson_SF(Config.Lot_A_Request_Lambda, Prev_Cars.Lot_A_Cars) * Prob_Returns;

      for Lot_B_Requests in Min_Requests_B .. Prev_Cars.Lot_B_Cars loop
         Lot_B_Returns := Next_Cars.Lot_B_Cars - (Prev_Cars.Lot_B_Cars - Lot_B_Requests);
         Prob_Returns := Poisson_PMF(Config.Lot_B_Return_Lambda, Lot_B_Returns);
         if Next_Cars.Lot_B_Cars >= Lot_Size then
            -- If the resulting number of cars is greater than or equal to the lot size,
            -- then returns exceeding Lot_B_Returns will also result in a full lot.
            Prob_Returns := Prob_Returns + Poisson_SF(Config.Lot_B_Return_Lambda, Lot_B_Returns);
         end if;
         Probability_Requests_Lot_B(Lot_B_Requests) := Poisson_PMF(Config.Lot_B_Request_Lambda, Lot_B_Requests) * Prob_Returns;
      end loop;
      -- For the value Prev_Cars.Lot_B_Cars, we allow for any number of requests greater than or equal
      -- to the value, as this results in 0 cars remaining before returns are counted.
      Probability_Requests_Lot_B(Prev_Cars.Lot_B_Cars) := Probability_Requests_Lot_B(Prev_Cars.Lot_B_Cars) + Poisson_SF(Config.Lot_B_Request_Lambda, Prev_Cars.Lot_B_Cars) * Prob_Returns;

      for Lot_A_Requests in Probability_Requests_Lot_A'Range loop
         for Lot_B_Requests in Probability_Requests_Lot_B'Range loop
            Temp_Prob := Probability_Requests_Lot_A(Lot_A_Requests) * Probability_Requests_Lot_B(Lot_B_Requests);
            Total_Probability := Total_Probability + Temp_Prob;
            Probability_Weighted_Reward := Probability_Weighted_Reward + Temp_Prob * (10.0*Float(Lot_A_Requests + Lot_B_Requests));
         end loop;
      end loop;

      return Transition_Probability_Type'(
         Probability => Total_Probability,
         Reward => (Probability_Weighted_Reward / Total_Probability) - 2.0 * Float(Cars_Moved)
      );
   end Calculate_Transition_Probability;
      
   function From_Discrete_State (D : Discrete_State_Type) return Cars_Per_Lot_Type is
   begin
      return Cars_Per_Lot_Type'(Lot_A_Cars => Natural(D) / (Lot_Size + 1), Lot_B_Cars => Natural(D) mod (Lot_Size + 1));
   end From_Discrete_State;
      
   function Step_Cars(Cars_Count : Cars_Per_Lot_Type; Action : Action_Type) return Cars_After_Action_Type is
      Cars_Moved : Natural;
      Cars_Per_Lot: Cars_Per_Lot_Type;
   begin
      -- First update for action
      if Action > 0 then
         -- Move cars from A to B
         Cars_Moved := Natural'Min(Natural(Action), Cars_Count.Lot_A_Cars);
         Cars_Per_Lot.Lot_A_Cars := Cars_Count.Lot_A_Cars - Cars_Moved;
         Cars_Per_Lot.Lot_B_Cars := Cars_Count.Lot_B_Cars + Cars_Moved;  -- TODO: Cap at 20
      else -- Action < 0
         -- Move cars from B to A
         Cars_Moved := Natural'Min(Natural(-Action), Cars_Count.Lot_B_Cars);
         Cars_Per_Lot.Lot_A_Cars := Cars_Count.Lot_A_Cars + Cars_Moved;  -- TODO: Cap at 20
         Cars_Per_Lot.Lot_B_Cars := Cars_Count.Lot_B_Cars - Cars_Moved;
      end if;
      return Cars_After_Action_Type'(
         Cars_Per_Lot => Cars_Per_Lot,
         Cars_Moved => Cars_Moved
      );
   end;

   function Get_Model(Config: Config_Type) return DP_Model_Access_Type is
      Res : DP_Model_Access_Type := new DP_Model_Type'(others => (others => (others => (Probability => 0.0, Reward => 0.0))));
      Env : Environment_Type := Make(Config);

      Cars_Count0 : Cars_Per_Lot_Type;
      Cars_After_Action : Cars_After_Action_Type;
      Cars_Count1 : Cars_Per_Lot_Type;
      Cars_Count2 : Cars_Per_Lot_Type;

      --    type Expected_Reward_Type is record
      --       Probability_Weighted_Reward : Float := 0.0;
      --       Total_Probability : Float := 0.0;
      --    end record;
      --    type Expected_Rewards_Type is array (Discrete_State_Type) of Expected_Reward_Type;
   begin
      for S0 in Discrete_State_Type loop
         Cars_Count0 := From_Discrete_State(S0);

         for A in Action_Type loop
            Cars_After_Action := Step_Cars(Cars_Count0, A);
            Cars_Count1 := Cars_After_Action.Cars_Per_Lot;

            for S2 in Discrete_State_Type loop
               Cars_Count2 := From_Discrete_State(S2);

               Res(S0, A, S2) := Calculate_Transition_Probability(Config, Cars_After_Action.Cars_Moved, Cars_Count1, Cars_Count2);
            end loop;
         end loop;
      end loop;
      return Res;
   end Get_Model;
   
   function Get_Transition_Values (Config: Config_Type; State: Discrete_State_Type; Action: Action_Type) return Transition_Array_Type is
      Res : Transition_Array_Type := (others => (Probability => 0.0, Reward => 0.0));
      
      Cars_Count0 : Cars_Per_Lot_Type;
      Cars_After_Action : Cars_After_Action_Type;
      Cars_Count1 : Cars_Per_Lot_Type;
      Cars_Count2 : Cars_Per_Lot_Type;
   begin
      Cars_Count0 := From_Discrete_State(State);
      Cars_After_Action := Step_Cars(Cars_Count0, Action);
      Cars_Count1 := Cars_After_Action.Cars_Per_Lot;
      for S2 in Discrete_State_Type loop
         Cars_Count2 := From_Discrete_State(S2);
         Res(S2) := Calculate_Transition_Probability(Config, Cars_After_Action.Cars_Moved, Cars_Count1, Cars_Count2);
      end loop;
      return Res;
   end Get_Transition_Values;

   -- TODO: The following is not currently used, but could be used as a replacement
   -- for Calculate_Transition_Probability2.
   -- Some benchmarking might be beneficial though.
   function Calculate_Transition_Probability2 (
      Config : Config_Type;
      Cars_Moved : Natural;
      Prev_Cars : Cars_Per_Lot_Type
   ) return Transition_Array_Type is
      Res : Transition_Array_Type := (others => (Probability => 0.0, Reward => 0.0));

      -- Local types
      type Expected_Reward_Type is record
            Probability_Weighted_Reward : Float := 0.0;
            Total_Probability : Float := 0.0;
      end record;
      type Expected_Rewards_Type is array (Discrete_State_Type) of Expected_Reward_Type;

      -- Local values
      Prob_Requests_Lot_A : Float;
      Prob_Returns_Lot_A : Float;
      Prob_Requests_Lot_B : Float;
      Prob_Returns_Lot_B : Float;
      Max_Lot_A_Returns : Natural;
      Max_Lot_B_Returns : Natural;

      Temp_Prob : Float;
      Temp_Reward : Float;
      Temp_Discrete_State : Discrete_State_Type;
      -- Probability_Weighted_Reward : Float := 0.0;
      -- Total_Probability : Float := 0.0;
      Interim_Rewards : Expected_Rewards_Type := (others => (Probability_Weighted_Reward => 0.0, Total_Probability => 0.0));
   begin
      for Lot_A_Requests in 0 .. Prev_Cars.Lot_A_Cars loop
         Prob_Requests_Lot_A := Poisson_PMF(Config.Lot_A_Request_Lambda, Lot_A_Requests);
         if Lot_A_Requests = Prev_Cars.Lot_A_Cars then
            -- For the value Prev_Cars.Lot_A_Cars, we allow for any number of requests greater than or equal to
            -- the current number of cars available, as this results in 0 cars remaining before returns are counted.
            -- We add the survival function value to the previous calculated value to obtain P(X >= x).
            Prob_Requests_Lot_A := Prob_Requests_Lot_A + Poisson_SF(Config.Lot_A_Request_Lambda, Prev_Cars.Lot_A_Cars);
         end if;

         Max_Lot_A_Returns := Lot_Size - (Prev_Cars.Lot_A_Cars - Lot_A_Requests);
         for Lot_A_Returns in 0 .. Max_Lot_A_Returns loop
            Prob_Returns_Lot_A := Poisson_PMF(Config.Lot_A_Return_Lambda, Lot_A_Returns);
            if Lot_A_Returns = Max_Lot_A_Returns then
               -- Similar to requests, the maximum returns corresponds to the lot being full (Lot_Size),
               -- and we add the probability for an excess number of returns (which are returned to a corporate lot)
               -- to this last value.
               Prob_Returns_Lot_A := Prob_Returns_Lot_A + Poisson_SF(Config.Lot_A_Return_Lambda, Max_Lot_A_Returns);
            end if;

            for Lot_B_Requests in 0 .. Prev_Cars.Lot_B_Cars loop
               Prob_Requests_Lot_B := Poisson_PMF(Config.Lot_B_Request_Lambda, Lot_B_Requests);
               if Lot_B_Requests = Prev_Cars.Lot_B_Cars then
                  Prob_Requests_Lot_B := Prob_Requests_Lot_B + Poisson_SF(Config.Lot_B_Request_Lambda, Prev_Cars.Lot_B_Cars);
               end if;

               Max_Lot_B_Returns := Lot_Size - (Prev_Cars.Lot_B_Cars - Lot_B_Requests);
               for Lot_B_Returns in 0 .. Max_Lot_B_Returns loop
                  Prob_Returns_Lot_B := Poisson_PMF(Config.Lot_B_Return_Lambda, Lot_B_Returns);
                  if Lot_B_Returns = Max_Lot_B_Returns then
                     Prob_Returns_Lot_B := Prob_Returns_Lot_B + Poisson_SF(Config.Lot_B_Return_Lambda, Max_Lot_B_Returns);
                  end if;

                  -- Determine the output state and the associated reward and probability
                  Temp_Prob := Prob_Requests_Lot_A * Prob_Returns_Lot_A * Prob_Requests_Lot_B * Prob_Returns_Lot_B;
                  Temp_Reward := 10.0*Float(Lot_A_Requests + Lot_B_Requests) - 2.0 * Float(Cars_Moved);
                  Temp_Discrete_State := To_Discrete_State(Cars_Per_Lot_Type'(
                     Lot_A_Cars => Prev_Cars.Lot_A_Cars - Lot_A_Requests + Lot_A_Returns,
                     Lot_B_Cars => Prev_Cars.Lot_B_Cars - Lot_B_Requests + Lot_B_Returns));
                  Interim_Rewards(Temp_Discrete_State).Probability_Weighted_Reward := Interim_Rewards(Temp_Discrete_State).Probability_Weighted_Reward + Temp_Prob * Temp_Reward;
                  Interim_Rewards(Temp_Discrete_State).Total_Probability := Interim_Rewards(Temp_Discrete_State).Total_Probability + Temp_Prob;
               end loop;
            end loop;
         end loop;
      end loop;

      for S2 in Discrete_State_Type loop
         if Interim_Rewards(S2).Total_Probability > 0.0 then
            Res(S2) := Transition_Probability_Type'(
               Probability => Interim_Rewards(S2).Total_Probability,
               Reward => Interim_Rewards(S2).Probability_Weighted_Reward / Interim_Rewards(S2).Total_Probability
            );
         end if;
      end loop;
      return Res;
   end Calculate_Transition_Probability2;
   
   function Get_Transition_Values2 (Config: Config_Type; State: Discrete_State_Type; Action: Action_Type) return Transition_Array_Type is
      -- Res : Transition_Array_Type := (others => (Probability => 0.0, Reward => 0.0));
      
      Cars_Count0 : Cars_Per_Lot_Type;
      Cars_After_Action : Cars_After_Action_Type;
      Cars_Count1 : Cars_Per_Lot_Type;
   begin
      Cars_Count0 := From_Discrete_State(State);
      Cars_After_Action := Step_Cars(Cars_Count0, Action);
      Cars_Count1 := Cars_After_Action.Cars_Per_Lot;
      return Calculate_Transition_Probability2(Config, Cars_After_Action.Cars_Moved, Cars_Count1);
   end Get_Transition_Values2;

end RL.Envs.Carrental;
