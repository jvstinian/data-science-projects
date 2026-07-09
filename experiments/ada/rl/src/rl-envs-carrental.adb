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
      return Float'Min(CDF, 1.0);
   end Poisson_CDF;
   
   function Poisson_SF(Lambda : Float; N : Natural) return Float is
   begin
      return 1.0 - Poisson_CDF(Lambda, N);
   end Poisson_SF;

   function To_Discrete_State(Cars_Per_Lot : Cars_Per_Lot_Type) return Discrete_State_Type is
   begin
      return Discrete_State_Type(Cars_Per_Lot.Lot_A_Cars * (Lot_Size + 1) + Cars_Per_Lot.Lot_B_Cars);
   end To_Discrete_State;

   function From_Discrete_State (D : Discrete_State_Type) return Cars_Per_Lot_Type is
   begin
      return Cars_Per_Lot_Type'(Lot_A_Cars => Natural(D) / (Lot_Size + 1), Lot_B_Cars => Natural(D) mod (Lot_Size + 1));
   end From_Discrete_State;
      
   function Make (Config: Config_Type) return Environment_Type is
   begin
      return Environment_Type'(
         Gen => <>,
         Config => Config,
         Lot_A_Cars => Config.Lot_A_Init_Cars,
         Lot_B_Cars => Config.Lot_B_Init_Cars
      );
   end Make;

   function Reset(Env : in out Environment_Type; Seed_Reset : Seed_Reset_Type) return Observation_Type is
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
         Lot_B_Updated_Cars := Natural'Min(Env.Lot_B_Cars + Local_Cars_Moved, Lot_Size);
      else -- Action < 0
         -- Move cars from B to A
         Local_Cars_Moved := Natural'Min(Natural(-Action), Env.Lot_B_Cars);
         Lot_A_Updated_Cars := Natural'Min(Env.Lot_A_Cars + Local_Cars_Moved, Lot_Size);
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
         Lot_Size
      );
      Lot_B_Updated_Cars := Natural'Min(
         Env.Lot_B_Cars - Lot_B_Rented_Cars + Lot_B_Returns,
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
   
   procedure Render_Text(Env : Environment_Type) is
   begin
      Put_Line("Lot A Cars: " & Env.Lot_A_Cars'Image & ", Lot B Cars: " & Env.Lot_B_Cars'Image);
   end Render_Text;

   function Calculate_Transition_Probability_Between_States (
      Config : Config_Type;
      Cars_Moved : Natural;
      Prev_Cars : Cars_Per_Lot_Type;
      Next_Cars : Cars_Per_Lot_Type
   ) return Transition_Probability_Type is
      type Probability_Type is array (Natural range <>) of Float;

      function Calculate_Lot_Request_Probabilities (
         Cars_Before : Natural; Cars_After: Natural;
         Request_Lambda : Float; Return_Lambda : Float
      ) return Probability_Type is
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
      Probability_Requests_Lot_A := Calculate_Lot_Request_Probabilities (
         Prev_Cars.Lot_A_Cars,
         Next_Cars.Lot_A_Cars,
         Config.Lot_A_Request_Lambda,
         Config.Lot_A_Return_Lambda
      );
      Probability_Requests_Lot_B := Calculate_Lot_Request_Probabilities (
         Prev_Cars.Lot_B_Cars,
         Next_Cars.Lot_B_Cars,
         Config.Lot_B_Request_Lambda,
         Config.Lot_B_Return_Lambda
      );

      for Lot_A_Requests in Probability_Requests_Lot_A'Range loop
         for Lot_B_Requests in Probability_Requests_Lot_B'Range loop
            Temp_Prob := Probability_Requests_Lot_A(Lot_A_Requests) * Probability_Requests_Lot_B(Lot_B_Requests);
            Total_Probability := Total_Probability + Temp_Prob;
            Probability_Weighted_Reward := Probability_Weighted_Reward + Temp_Prob * (10.0*Float(Lot_A_Requests + Lot_B_Requests));
         end loop;
      end loop;

      if Total_Probability > 0.0 then
         return Transition_Probability_Type'(
            Probability => Total_Probability,
            Reward => (Probability_Weighted_Reward / Total_Probability) - 2.0 * Float(Cars_Moved)
         );
      else
         -- From testing it does not appear this case is reached,
         -- but it is included for completeness.
         return Transition_Probability_Type'(
            Probability => 0.0,
            Reward => 0.0
         );
      end if;
   end Calculate_Transition_Probability_Between_States;

   function Step_Cars(Cars_Count : Cars_Per_Lot_Type; Action : Action_Type) return Cars_After_Action_Type is
      Cars_Moved : Natural;
      Cars_Per_Lot: Cars_Per_Lot_Type;
   begin
      -- First update for action
      if Action > 0 then
         -- Move cars from A to B
         Cars_Moved := Natural'Min(Natural(Action), Cars_Count.Lot_A_Cars);
         Cars_Per_Lot.Lot_A_Cars := Cars_Count.Lot_A_Cars - Cars_Moved;
         Cars_Per_Lot.Lot_B_Cars := Natural'Min(Cars_Count.Lot_B_Cars + Cars_Moved, Lot_Size);
      else -- Action < 0
         -- Move cars from B to A
         Cars_Moved := Natural'Min(Natural(-Action), Cars_Count.Lot_B_Cars);
         Cars_Per_Lot.Lot_A_Cars := Natural'Min(Cars_Count.Lot_A_Cars + Cars_Moved, Lot_Size);
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
   begin
      for S0 in Discrete_State_Type loop
         Cars_Count0 := From_Discrete_State(S0);

         for A in Action_Type loop
            Cars_After_Action := Step_Cars(Cars_Count0, A);
            Cars_Count1 := Cars_After_Action.Cars_Per_Lot;

            for S2 in Discrete_State_Type loop
               Cars_Count2 := From_Discrete_State(S2);

               Res(S0, A, S2) := Calculate_Transition_Probability_Between_States (
                  Config, Cars_After_Action.Cars_Moved, Cars_Count1, Cars_Count2
               );
            end loop;
         end loop;
      end loop;
      return Res;
   end Get_Model;
   
   -- NOTE: The following is not currently used directly, but has
   --       been added for comparing with
   --       Calculate_Transition_Probability_Between_States.
   --       The testing will be done using the collected results
   --       from Get_Transition_Values and Get_Transition_Values2,
   --       which use the two different methods.
   function Calculate_Transition_Probabilities_From_State (
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
   end Calculate_Transition_Probabilities_From_State;

   function Collect_Transition_Values (Config: Config_Type; State: Discrete_State_Type; Action: Action_Type) return Transition_Array_Type is
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
         Res(S2) := Calculate_Transition_Probability_Between_States (Config, Cars_After_Action.Cars_Moved, Cars_Count1, Cars_Count2);
      end loop;
      return Res;
   end Collect_Transition_Values;

   function Get_Transition_Values_From_State (Config: Config_Type; State: Discrete_State_Type; Action: Action_Type) return Transition_Array_Type is
      Cars_Count0 : Cars_Per_Lot_Type;
      Cars_After_Action : Cars_After_Action_Type;
      Cars_Count1 : Cars_Per_Lot_Type;
   begin
      Cars_Count0 := From_Discrete_State(State);
      Cars_After_Action := Step_Cars(Cars_Count0, Action);
      Cars_Count1 := Cars_After_Action.Cars_Per_Lot;
      return Calculate_Transition_Probabilities_From_State (Config, Cars_After_Action.Cars_Moved, Cars_Count1);
   end Get_Transition_Values_From_State;

end RL.Envs.Carrental;
