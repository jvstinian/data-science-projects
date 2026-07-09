with Ada.Text_IO; use Ada.Text_IO;

package body RL.Algorithms.DP is
    function Iterative_Policy_Evaluation(Model : DP_Model_Type; Policy : Stochastic_Policy_Type; Discount_Factor : Float) return Value_Function_Type is
        Value_Function : Value_Function_Type := (others => 0.0);
        Theta : Float := 1.0e-6; -- Convergence threshold
        Local_Delta : Float := 0.0;

        Prev_Value : Float;
        Transition_Value : Float;
        New_Value : Float;

        Iteration_Count : Integer := 0;
    begin
        loop
            Iteration_Count := Iteration_Count + 1;
            Put_Line("Iteration " & Iteration_Count'Image);  -- TODO

            Local_Delta := 0.0;
            for S in State_Type loop
                Prev_Value := Value_Function(S);
                New_Value := 0.0;
                for A in Action_Type loop
                    Transition_Value := 0.0;
                    for S1 in State_Type loop
                        Transition_Value :=Transition_Value + Model(S, A, S1).Probability * (Model(S, A, S1).Reward + Discount_Factor * Value_Function(S1));
                    end loop;
                    New_Value := New_Value + Policy(S, A) * Transition_Value;
                end loop;
                Value_Function(S) := New_Value;
                Local_Delta := Float'Max(Local_Delta, abs(New_Value - Prev_Value));
            end loop;
            exit when Local_Delta < Theta;
        end loop;
        return Value_Function;
    end Iterative_Policy_Evaluation;

    function Iterative_Policy_Evaluation(Model : DP_Model_Type; Policy : Deterministic_Policy_Type; Discount_Factor : Float; Value_Estimate : Value_Function_Type) return Value_Function_Type is
        Value_Function: Value_Function_Type := Value_Estimate;
        
        A : Action_Type;
        Theta : Float := 1.0e-6; -- Convergence threshold
        Local_Delta : Float := 0.0;

        Prev_Value : Float;
        New_Value : Float;

        Iteration_Count : Integer := 0;
    begin
        loop
            Iteration_Count := Iteration_Count + 1;
            Put_Line("Iteration " & Iteration_Count'Image);

            Local_Delta := 0.0;
            for S in State_Type loop
                Prev_Value := Value_Function(S);
                A := Policy(S);
                New_Value := 0.0;
                for S1 in State_Type loop
                    New_Value := New_Value + Model(S, A, S1).Probability * (Model(S, A, S1).Reward + Discount_Factor * Value_Function(S1));
                end loop;
                Value_Function(S) := New_Value;
                Local_Delta := Float'Max(Local_Delta, abs(New_Value - Prev_Value));
            end loop;
            exit when Local_Delta < Theta;
        end loop;
        return Value_Function;
    end Iterative_Policy_Evaluation;

    procedure Print_Policy(Policy : Deterministic_Policy_Type) is
    begin
        for S in State_Type loop
            Put_Line("Initial Policy for State " & S'Image & ": " & Policy(S)'Image);
        end loop;
    end Print_Policy;

    -- function Policy_Iteration(Model : DP_Model_Type; Discount_Factor : Float) return Deterministic_Policy_Type is
    --     type Action_Value_Array_Type is array (Action_Type) of Float;

    --     function Arg_Max(Action_Values : Action_Value_Array_Type) return Action_Type is
    --         Max_Value : Float := Float'First;
    --         Best_Action : Action_Type := Action_Type'First;
    --     begin
    --         for A in Action_Type loop
    --             if Action_Values(A) > Max_Value then
    --                 Max_Value := Action_Values(A);
    --                 Best_Action := A;
    --             end if;
    --         end loop;
    --         return Best_Action;
    --     end Arg_Max;

    --     Value_Function : Value_Function_Type := (others => 0.0);  -- TODO: Perhaps initialize to random values?
    --     Policy : Deterministic_Policy_Type := (others => Action_Random.Random(Action_Gen));  -- Initialize to random policy

    --     Stable : Boolean;
    --     Prev_Action : Action_Type;
    --     Action_Values : Action_Value_Array_Type;
    -- begin
    --     Print_Policy(Policy);  -- TODO: Remove after debugging

    --     loop 
    --         Value_Function := Iterative_Policy_Evaluation(Model, Policy, Discount_Factor, Value_Function);
    -- 
    --         Stable := True;
    --         for S in State_Type loop
    --             -- For state S, record the value of each action in Action_Values.
    --             for A in Action_Type loop
    --                 Action_Values(A) := 0.0;
    --                 for S1 in State_Type loop
    --                     Action_Values(A) := Action_Values(A) + Model(S, A, S1).Probability * (Model(S, A, S1).Reward + Discount_Factor * Value_Function(S1));
    --                 end loop;
    --             end loop;
    --             -- Get the arg max
    --             Prev_Action := Policy(S);
    --             Policy(S) := Arg_Max(Action_Values);
    --             if Policy(S) /= Prev_Action then
    --                 Stable := False;
    --             end if;
    --         end loop;
    --         exit when Stable;
    --     end loop;
    --     return Policy;
    -- end Policy_Iteration;
end RL.Algorithms.DP;
