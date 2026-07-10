with Ada.Text_IO; use Ada.Text_IO;
with RL; use RL;  -- Transition_Probability_Type
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Envs.Frozenlake.DP;
with RL.Algorithms.DP;
with Ada.Numerics.Discrete_Random;

procedure Policy_Iteration_Example is
   package Frozen_Lake_DP is new RL.Envs.Frozenlake.DP(Map_Name => Map_4x4);
   use Frozen_Lake_DP;
   Config : Config_Type := Config_Type'(Map_Name => Map_4x4, Is_Slippery => False);
   DP_Model : DP_Model_Type := Get_Model(Config);
   
   package Frozen_Lake_Algs is new RL.Algorithms.DP(
      State_Type => State_Type,
      Action_Type => Action_Type,
      DP_Model_Type => DP_Model_Type
   );
   use Frozen_Lake_Algs;

    -- subtype Probability_Type is Float range 0.0 .. 1.0;
    -- type Policy_Type is array (State_Type) of Action_Type;

    -- type Value_Function_Type is array (State_Type) of Float;

    -- The following is already in RL.Algorithms.DP
    -- function Iterative_Policy_Evaluation(Model : DP_Model_Type; Policy : Policy_Type; Discount_Factor : Float; Value_Estimate : Value_Function_Type) return Value_Function_Type is
    --     Value_Function: Value_Function_Type := Value_Estimate;
    --     
    --     A : Action_Type;
    --     Theta : Float := 1.0e-6; -- Convergence threshold
    --     Local_Delta : Float := 0.0;

    --     Prev_Value : Float;
    --     New_Value : Float;

    --     Iteration_Count : Integer := 0;
    -- begin
    --     loop
    --         Iteration_Count := Iteration_Count + 1;
    --         Put_Line("Iteration " & Iteration_Count'Image);

    --         Local_Delta := 0.0;
    --         for S in State_Type loop
    --             Prev_Value := Value_Function(S);
    --             A := Policy(S);
    --             New_Value := 0.0;
    --             for S1 in State_Type loop
    --                 New_Value := New_Value + Model(S, A, S1).Probability * (Model(S, A, S1).Reward + Discount_Factor * Value_Function(S1));
    --             end loop;
    --             Value_Function(S) := New_Value;
    --             Local_Delta := Float'Max(Local_Delta, abs(New_Value - Prev_Value));
    --         end loop;
    --         exit when Local_Delta < Theta;
    --     end loop;
    --     return Value_Function;
    -- end Iterative_Policy_Evaluation;

    -- procedure Print_Policy(Policy : Deterministic_Policy_Type) is
    -- begin
    --     for S in State_Type loop
    --         Put_Line("Initial Policy for State " & S'Image & ": " & Policy(S)'Image);
    --     end loop;
    -- end Print_Policy;

   -- function Policy_Iteration(Model : DP_Model_Type; Discount_Factor : Float) return Deterministic_Policy_Type is
   --    package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
   --    Action_Gen: Action_Random.Generator;
   --     
   --    type Action_Value_Array_Type is array (Action_Type) of Float;

   --    function Arg_Max(Action_Values : Action_Value_Array_Type) return Action_Type is
   --       Max_Value : Float := Float'First;
   --       Best_Action : Action_Type := Action_Type'First;
   --    begin
   --       for A in Action_Type loop
   --          if Action_Values(A) > Max_Value then
   --             Max_Value := Action_Values(A);
   --             Best_Action := A;
   --          end if;
   --       end loop;
   --       return Best_Action;
   --    end Arg_Max;

   --    Value_Function : Value_Function_Type := (others => 0.0);  -- TODO: Perhaps initialize to random values?
   --    Policy : Deterministic_Policy_Type; -- := (others => Action_Random.Random(Action_Gen));  -- Initialize to random policy

   --    Stable : Boolean;
   --    Prev_Action : Action_Type;
   --    Action_Values : Action_Value_Array_Type;
   -- begin
   --    Action_Random.Reset(Action_Gen);
   --    Policy := (others => Action_Random.Random(Action_Gen));  -- Initialize to random policy
   --    Print_Policy(Policy);  -- TODO: Remove after debugging

   --      loop 
   --          Value_Function := Iterative_Policy_Evaluation(Model, Policy, Discount_Factor, Value_Function);
   --  
   --          Stable := True;
   --          for S in State_Type loop
   --              -- For state S, record the value of each action in Action_Values.
   --              for A in Action_Type loop
   --                  Action_Values(A) := 0.0;
   --                  for S1 in State_Type loop
   --                      Action_Values(A) := Action_Values(A) + Model(S, A, S1).Probability * (Model(S, A, S1).Reward + Discount_Factor * Value_Function(S1));
   --                  end loop;
   --              end loop;
   --              -- Get the arg max
   --              Prev_Action := Policy(S);
   --              Policy(S) := Arg_Max(Action_Values);
   --              if Policy(S) /= Prev_Action then
   --                  Stable := False;
   --              end if;
   --          end loop;
   --          exit when Stable;
   --      end loop;
   --      return Policy;
   --  end Policy_Iteration;

    Optimal_Policy : Deterministic_Policy_Type;
begin
    Optimal_Policy := Policy_Iteration(DP_Model, 0.9);
    Print_Policy(Optimal_Policy);
end Policy_Iteration_Example;
