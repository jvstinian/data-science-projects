with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Envs.Frozenlake.DP;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

procedure Eligibility_Trace_Example is
    package Frozen_Lake_DP is new RL.Envs.Frozenlake.DP(Map_Name => Map_4x4);
    use Frozen_Lake_DP;
    -- TODO: Use Alt_Discrete_State_Type instead of the following
    -- type Precise_State_Type is new Integer range 0 .. (Frozen_Lake_DP.Num_Rows * Frozen_Lake_DP.Num_Cols - 1);
    -- type Precise_Model_Type is array (Precise_State_Type, Action_Type, Precise_State_Type) of Transition_Probability_Type;
    -- Precise_DP_Model : Precise_Model_Type;

    subtype Probability_Type is Float range 0.0 .. 1.0;
    type Policy_Type is array (State_Type) of Action_Type;

    type Value_Function_Type is array (State_Type) of Float;

    type Action_Value_Function_Type is array (State_Type, Action_Type) of Float;

    type ET_Config_Type is record
        Alpha : Float;
        Gamma : Float;
        Lambda : Float;
    end record;

    function ET_Iterative_Policy_Evaluation(Env_Config: Config_Type; ET_Config: ET_Config_Type; Policy : Policy_Type) return Value_Function_Type is
        Env : Environment_Type := Make (Env_Config);

        type Eligibility_Trace_Type is array (State_Type) of Float;
        E : Eligibility_Trace_Type := (others => 0.0);
        TD_Error : Float;

        Obs : Observation_Type;
        S : State_Type;
        S1 : State_Type;
        Step_Result : Step_Return_Type;
        Action : Action_Type;
        Terminated : Boolean := False;

        Value_Function : Value_Function_Type := (others => 0.0);
        Theta : Float := 1.0e-6; -- Convergence threshold
        Local_Delta : Float := 0.0;

        Prev_Value_Function : Value_Function_Type;

        Episode_Count : Integer := 0;

        Step_Index : Natural := 0;
        -- NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm.
        Max_Steps : Natural := 50;
    begin
        loop
            Episode_Count := Episode_Count + 1;
            Put_Line("Episode " & Episode_Count'Image);

            Prev_Value_Function := Value_Function;

            -- TODO: From the text of the book and the book errata, E needs to be set to 0
            --       at the beginning of an episode.
            Obs := Reset(Env);
            S := State_Type(Obs.Position_Index);
            Step_Index := 0;
            Terminated := False;

            while not Terminated loop
                Action := Policy(S);
                Step_Result := Step(Env, Action);
                Obs := Step_Result.Observation;
                S1 := State_Type(Obs.Position_Index);
                Put_Line("Action " & Action_Type'Image(Action) & " takes state " & State_Type'Image(S) & " to state " & State_Type'Image(S1));
                TD_Error := Step_Result.Reward + ET_Config.Gamma * Value_Function (S1) - Value_Function (S);
                E(S) := E(S) + 1.0;
                for S2 in State_Type loop
                    Value_Function(S2) := Value_Function(S2) + ET_Config.Alpha * TD_Error * E(S2);
                    E(S2) := ET_Config.Gamma * ET_Config.Lambda * E(S2);
                end loop;
                -- Update to next state
                S := S1;
                Step_Index := Step_Index + 1;
                Terminated := Step_Result.Terminated or else Step_Index >= Max_Steps;
            end loop;

            Local_Delta := 0.0;
            -- NOTE: We exit when the max value function change falls below a threshold.
            --       This differs from the textbook algorithm.
            for S in State_Type loop
                Local_Delta := Float'Max(Local_Delta, abs(Value_Function (S) - Prev_Value_Function (S)));
            end loop;
            exit when Local_Delta < Theta;
        end loop;
        return Value_Function;
    end ET_Iterative_Policy_Evaluation;

   -- TODO: Note that there is a type with the same name in the TD example
   type SARSA_Config_Type is record
      Alpha: Float;
      Gamma: Float;
      Lambda: Float;
      Initial_Epsilon: Float;
      Minimum_Epsilon: Float;
      Episodes_To_Minimum_Epsilon: Natural;
   end record;

   function ET_SARSA_On_Policy(Env_Config: Config_Type; SARSA_Config: SARSA_Config_Type) return Action_Value_Function_Type is
      Env : Environment_Type := Make (Env_Config);

      package Action_Unif_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
      package Float_Unif_Random renames Ada.Numerics.Float_Random;

      -- Generators
      Action_Unif_Gen : Action_Unif_Random.Generator;
      Float_Unif_Gen : Float_Unif_Random.Generator;

      -- NOTE: This function is identical to the one in the TD example
      function Best_Action_For_State(Q : Action_Value_Function_Type; S: State_Type) return Action_Type is
         Max_Value : Float := Float'First;
         Best_Action : Action_Type := Action_Type'First;
      begin
         for A in Action_Type loop
            if Q(S, A) > Max_Value then
               Max_Value := Q(S, A);
               Best_Action := A;
            end if;
         end loop;
         return Best_Action;
      end Best_Action_For_State;

      -- NOTE: This function is identical to the one in the TD example
      function Choose_Action_Epsilon_Greedy (Epsilon: Float; Q : Action_Value_Function_Type; S: State_Type) return Action_Type is
         U : Float := Float_Unif_Random.Random(Float_Unif_Gen);
      begin
         if U < Epsilon then
            return Action_Unif_Random.Random(Action_Unif_Gen);
         else
            return Best_Action_For_State(Q, S);
         end if;
      end Choose_Action_Epsilon_Greedy;

      type Eligibility_Trace_Type is array (State_Type, Action_Type) of Float;
      E : Eligibility_Trace_Type := (others => (others => 0.0));
      TD_Error : Float;

      Obs : Observation_Type;
      S : State_Type;
      S1 : State_Type;
      Step_Result : Step_Return_Type;
      A : Action_Type;
      A1 : Action_Type;
      Terminated : Boolean := False;

      Epsilon : Float;

      -- NOTE: This function is identical to the one in the TD example
      function Update_Epsilon (SARSE_Config: SARSA_Config_Type; Episode : Natural) return Float is
         Init_Eps : Float := SARSA_Config.Initial_Epsilon;
         Min_Eps : Float := SARSA_Config.Minimum_Epsilon;
         Episode_To_Min_Eps : Float := Float(SARSA_Config.Episodes_To_Minimum_Epsilon);
         K : Float := Episode_To_Min_Eps * Min_Eps / Init_Eps;
      begin
         return Float'Max(Min_Eps, K * Init_Eps / Float(Episode));
      end Update_Epsilon;

      Action_Value_Function : Action_Value_Function_Type := (others => (others => 0.0));
      Theta : Float := 1.0e-6; -- Convergence threshold
      Local_Delta : Float := 0.0;

      Prev_Action_Value_Function : Action_Value_Function_Type;

      Episode_Count : Integer := 0;
      Step_Index : Natural := 0;
      -- NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm.
      Max_Steps : Natural := 100;
   begin
      -- Generators
      Action_Unif_Random.Reset(Action_Unif_Gen);
      Float_Unif_Random.Reset(Float_Unif_Gen);

      loop
            Episode_Count := Episode_Count + 1;
            -- Put_Line("Episode " & Episode_Count'Image);

            Epsilon := Update_Epsilon(SARSA_Config, Episode_Count);
            Put_Line("Episode " & Episode_Count'Image & ", Epsilon: " & Float'Image(Epsilon));

            Prev_Action_Value_Function := Action_Value_Function;

            -- TODO: From the text of the book and the book errata, E needs to be set to 0
            --       at the beginning of an episode.
            Obs := Reset(Env);
            S := State_Type(Obs.Position_Index);
            A := Choose_Action_Epsilon_Greedy(Epsilon, Action_Value_Function, S);
            Step_Index := 0;
            Terminated := False;

            while not Terminated loop
               Step_Result := Step(Env, A);
               Obs := Step_Result.Observation;
               S1 := State_Type(Obs.Position_Index);
               A1 := Choose_Action_Epsilon_Greedy(Epsilon, Action_Value_Function, S1);
               Put_Line("Action " & Action_Type'Image(A) & " takes state " & State_Type'Image(S) & " to state " & State_Type'Image(S1) & " and action " & Action_Type'Image(A1) & " in on-policy SARSA");
               TD_Error := Step_Result.Reward + SARSA_Config.Gamma * Action_Value_Function (S1, A1) - Action_Value_Function (S, A);
               E(S, A) := E(S, A) + 1.0;
               for S2 in State_Type loop
                  for A2 in Action_Type loop
                      Action_Value_Function(S2, A2) := Action_Value_Function(S2, A2) + SARSA_Config.Alpha * TD_Error * E(S2, A2);
                      E(S2, A2) := SARSA_Config.Gamma * SARSA_Config.Lambda * E(S2, A2);
                  end loop;
               end loop;
                -- Update to next state
               S := S1;
               A := A1;
               Step_Index := Step_Index + 1;
               Terminated := Step_Result.Terminated or else Step_Index >= Max_Steps;
            end loop;

            -- NOTE: We exit when the max value function change falls below a threshold
            --       after episode Episodes_To_Minimum_Epsilon is reached.
            --       This differs from the textbook algorithm.
            Local_Delta := 0.0;
            -- TODO: Decide whether to reintroduce something like the following condition before exiting
            for S0 in State_Type loop
               for A0 in Action_Type loop
                  Local_Delta := Float'Max(Local_Delta, abs(Action_Value_Function (S0, A0) - Prev_Action_Value_Function (S0, A0)));
               end loop;
            end loop;
            exit when Episode_Count >= SARSA_Config.Episodes_To_Minimum_Epsilon and then Local_Delta < Theta;
        end loop;
        return Action_Value_Function;
    end ET_SARSA_On_Policy;

   function Watkins_Q_Iteration(Env_Config: Config_Type; SARSA_Config: SARSA_Config_Type) return Action_Value_Function_Type is
      Env : Environment_Type := Make (Env_Config);

      package Action_Unif_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
      package Float_Unif_Random renames Ada.Numerics.Float_Random;

      -- Generators
      Action_Unif_Gen : Action_Unif_Random.Generator;
      Float_Unif_Gen : Float_Unif_Random.Generator;

      -- NOTE: This function is identical to the one in the TD example
      function Best_Action_For_State(Q : Action_Value_Function_Type; S: State_Type) return Action_Type is
         Max_Value : Float := Float'First;
         Best_Action : Action_Type := Action_Type'First;
      begin
         for A in Action_Type loop
            if Q(S, A) > Max_Value then
               Max_Value := Q(S, A);
               Best_Action := A;
            end if;
         end loop;
         return Best_Action;
      end Best_Action_For_State;

      -- NOTE: This function is identical to the one in the TD example
      function Choose_Action_Epsilon_Greedy (Epsilon: Float; Q : Action_Value_Function_Type; S: State_Type) return Action_Type is
         U : Float := Float_Unif_Random.Random(Float_Unif_Gen);
      begin
         if U < Epsilon then
            return Action_Unif_Random.Random(Action_Unif_Gen);
         else
            return Best_Action_For_State(Q, S);
         end if;
      end Choose_Action_Epsilon_Greedy;

      type Eligibility_Trace_Type is array (State_Type, Action_Type) of Float;
      E : Eligibility_Trace_Type := (others => (others => 0.0));
      TD_Error : Float;

      Obs : Observation_Type;
      S : State_Type;
      S1 : State_Type;
      Step_Result : Step_Return_Type;
      A : Action_Type;
      A1 : Action_Type;
      A_Best : Action_Type;
      Terminated : Boolean := False;

      Epsilon : Float;

      -- NOTE: This function is identical to the one in the TD example
      function Update_Epsilon (SARSE_Config: SARSA_Config_Type; Episode : Natural) return Float is
         Init_Eps : Float := SARSA_Config.Initial_Epsilon;
         Min_Eps : Float := SARSA_Config.Minimum_Epsilon;
         Episode_To_Min_Eps : Float := Float(SARSA_Config.Episodes_To_Minimum_Epsilon);
         K : Float := Episode_To_Min_Eps * Min_Eps / Init_Eps;
      begin
         return Float'Max(Min_Eps, K * Init_Eps / Float(Episode));
      end Update_Epsilon;

      Action_Value_Function : Action_Value_Function_Type := (others => (others => 0.0));
      Theta : Float := 1.0e-6; -- Convergence threshold
      Local_Delta : Float := 0.0;

      Prev_Action_Value_Function : Action_Value_Function_Type;

      Episode_Count : Integer := 0;
      Step_Index : Natural := 0;
      -- NOTE: We cap the number of steps per episode.  This differs from the textbook algorithm.
      Max_Steps : Natural := 100;
   begin
      -- Generators
      Action_Unif_Random.Reset(Action_Unif_Gen);
      Float_Unif_Random.Reset(Float_Unif_Gen);

      loop
            Episode_Count := Episode_Count + 1;
            -- Put_Line("Episode " & Episode_Count'Image);

            Epsilon := Update_Epsilon(SARSA_Config, Episode_Count);
            Put_Line("Episode " & Episode_Count'Image & ", Epsilon: " & Float'Image(Epsilon));

            Prev_Action_Value_Function := Action_Value_Function;

            -- TODO: From the text of the book and the book errata, E needs to be set to 0
            --       at the beginning of an episode.
            Obs := Reset(Env);
            S := State_Type(Obs.Position_Index);
            A := Choose_Action_Epsilon_Greedy(Epsilon, Action_Value_Function, S);
            Step_Index := 0;
            Terminated := False;

            while not Terminated loop
               Step_Result := Step(Env, A);
               Obs := Step_Result.Observation;
               S1 := State_Type(Obs.Position_Index);
               A1 := Choose_Action_Epsilon_Greedy(Epsilon, Action_Value_Function, S1);
               A_Best := Best_Action_For_State(Action_Value_Function, S1);  -- TODO: Just get best value for state
               Put_Line("Action " & Action_Type'Image(A) & " takes state " & State_Type'Image(S) & " to state " & State_Type'Image(S1) & " in Watkin's Q algorithm (off-policy SARSA usingn Eligibility Trace)");
               TD_Error := Step_Result.Reward + SARSA_Config.Gamma * Action_Value_Function (S1, A_Best) - Action_Value_Function (S, A);
               E(S, A) := E(S, A) + 1.0;
               for S2 in State_Type loop
                  for A2 in Action_Type loop
                      Action_Value_Function(S2, A2) := Action_Value_Function(S2, A2) + SARSA_Config.Alpha * TD_Error * E(S2, A2);
                      -- IN PROGRESS: Is the following correct?  Based on the text, I think so.
                      if A1 = A_Best then
                         E(S2, A2) := SARSA_Config.Gamma * SARSA_Config.Lambda * E(S2, A2);
                      else 
                         E(S2, A2) := 0.0;
                      end if;
                  end loop;
               end loop;
                -- Update to next state
               S := S1;
               A := A1;
               Step_Index := Step_Index + 1;
               Terminated := Step_Result.Terminated or else Step_Index >= Max_Steps;
            end loop;

            -- NOTE: We exit when the max value function change falls below a threshold
            --       after episode Episodes_To_Minimum_Epsilon is reached.
            --       This differs from the textbook algorithm.
            Local_Delta := 0.0;
            -- TODO: Decide whether to reintroduce something like the following condition before exiting
            for S0 in State_Type loop
               for A0 in Action_Type loop
                  Local_Delta := Float'Max(Local_Delta, abs(Action_Value_Function (S0, A0) - Prev_Action_Value_Function (S0, A0)));
               end loop;
            end loop;
            exit when Episode_Count >= SARSA_Config.Episodes_To_Minimum_Epsilon and then Local_Delta < Theta;
        end loop;
        return Action_Value_Function;
    end Watkins_Q_Iteration;

    Frozen_Lake_Config : Config_Type := (Map_Name => Map_4x4, Is_Slippery => False);
    ET_Config : ET_Config_Type := (Alpha => 0.1, Gamma => 0.9, Lambda => 0.6);
    P : Policy_Type := (others => Down);
    Local_Value_Function : Value_Function_Type;

    -- SARSA
    SARSA_Config : SARSA_Config_Type := (
      Alpha => 0.1, Gamma => 0.9,
      Lambda => 0.6,
      Initial_Epsilon => 0.8, Minimum_Epsilon => 0.01,
      Episodes_To_Minimum_Epsilon => 50000
    );
    Local_Action_Value_Function : Action_Value_Function_Type;
begin
    Put_Line("Eligibility Trace iterative policy evaluation for Frozen Lake environment");
    P (8) := Right;
    P (13) := Right;
    P (14) := Right;

    Local_Value_Function := ET_Iterative_Policy_Evaluation(Frozen_Lake_Config, ET_Config, P);
    for S in State_Type loop
        -- (S, F, F, F),
        -- (F, H, F, H),
        -- (F, F, F, H),
        -- (H, F, F, G));
        Put("Value of State " & S'Image & " under policy: ");
        Ada.Float_Text_IO.Put(Item => Local_Value_Function(S), Fore => 1, Aft => 4, Exp => 0);
        New_Line;
    end loop;

    Put_Line("Running on-policy Eligibility Trace SARSA");
    Local_Action_Value_Function := ET_SARSA_On_Policy(Frozen_Lake_Config, SARSA_Config);
    for S in State_Type loop
       for A in Action_Type loop
         Put("Value of (" & S'Image & ", " & Action_Type'Image(A) & "): ");
         Ada.Float_Text_IO.Put(Item => Local_Action_Value_Function(S, A), Fore => 1, Aft => 4, Exp => 0);
         New_Line;
       end loop;
    end loop;

    Put_Line("Running Watkins Q Control Method");
    Local_Action_Value_Function := Watkins_Q_Iteration(Frozen_Lake_Config, SARSA_Config);
    for S in State_Type loop
       for A in Action_Type loop
         Put("Value of (" & S'Image & ", " & Action_Type'Image(A) & "): ");
         Ada.Float_Text_IO.Put(Item => Local_Action_Value_Function(S, A), Fore => 1, Aft => 4, Exp => 0);
         New_Line;
       end loop;
    end loop;
end Eligibility_Trace_Example;

