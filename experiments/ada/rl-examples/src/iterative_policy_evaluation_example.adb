with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Envs.Frozenlake.Child;

procedure Iterative_Policy_Evaluation_Example is
    DP_Model : Discrete_Model_Type := Get_Model(Environment_Config'(Map_Name => Map_4x4, Slippery => False));
    
    package Frozen_Lake_Child is new RL.Envs.Frozenlake.Child(Map_Info => Get_Map_Info(Map_4x4));
    -- TODO: Use Alt_Discrete_State_Type instead of the following
    type Precise_State_Type is new Integer range 0 .. (Frozen_Lake_Child.Num_Rows * Frozen_Lake_Child.Num_Cols - 1);
    type Precise_Model_Type is array (Precise_State_Type, Action_Type, Precise_State_Type) of Transition_Probability_Type;
    Precise_DP_Model : Precise_Model_Type;

    subtype Probability_Type is Float range 0.0 .. 1.0;
    type Policy_Type is array (Precise_State_Type, Action_Type) of Probability_Type;

    type Value_Function_Type is array (Precise_State_Type) of Float;

    function Iterative_Policy_Evaluation(Model : Precise_Model_Type; Policy : Policy_Type; Discount_Factor : Float) return Value_Function_Type is
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
            Put_Line("Iteration " & Iteration_Count'Image);

            Local_Delta := 0.0;
            for S in Precise_State_Type loop
                Prev_Value := Value_Function(S);
                New_Value := 0.0;
                for A in Action_Type loop
                    Transition_Value := 0.0;
                    for S1 in Precise_State_Type loop
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

    Local_Random_Policy : Policy_Type := (others => (others => 0.25)); -- Uniform random policy
    Local_Value_Function : Value_Function_Type;
begin
    Put_Line("Frozen Lake Child Get_Map_Rows: " & Frozen_Lake_Child.Num_Rows'Image);
    Frozen_Lake_Child.Dummy_Method;
    for Current_State in Discrete_State_Type loop
        for Current_Action in Action_Type loop
            for Next_State in Discrete_State_Type loop
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
    
    -- Initialize Precise_DP_Model 
    for S0 in Precise_State_Type loop
        for A in Action_Type loop
            for S1 in Precise_State_Type loop
                Precise_DP_Model(S0, A, S1) := DP_Model(Discrete_State_Type(S0), A, Discrete_State_Type(S1));
            end loop;
        end loop;
    end loop;

    Local_Value_Function := Iterative_Policy_Evaluation(Precise_DP_Model, Local_Random_Policy, 0.9);
    for S in Precise_State_Type loop
        -- (S, F, F, F),
        -- (F, H, F, H),
        -- (F, F, F, H),
        -- (H, F, F, G));
        Put("Value of State " & S'Image & " under random policy: ");
        Ada.Float_Text_IO.Put(Item => Local_Value_Function(S), Fore => 1, Aft => 4, Exp => 0);
        New_Line;
    end loop;

end Iterative_Policy_Evaluation_Example;
