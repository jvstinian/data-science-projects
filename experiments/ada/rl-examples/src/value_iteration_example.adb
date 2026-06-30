with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Float_Text_IO;
with RL.Envs.Frozenlake; use RL.Envs.Frozenlake;
with RL.Envs.Frozenlake.DP;
with Ada.Numerics.Discrete_Random;

procedure Value_Iteration_Example is
    package Action_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Action_Type);
    Action_Gen: Action_Random.Generator;
    
    package Frozen_Lake_DP is new RL.Envs.Frozenlake.DP(Map_Name => Map_4x4);
    use Frozen_Lake_DP;
    DP_Model : DP_Model_Type := Get_Model(Environment_Config'(Map_Name => Map_4x4, Is_Slippery => False));
    -- TODO: Use Alt_Discrete_State_Type instead of the following
    -- type Precise_State_Type is new Integer range 0 .. (Frozen_Lake_DP.Num_Rows * Frozen_Lake_DP.Num_Cols - 1);
    -- type Precise_Model_Type is array (Precise_State_Type, Action_Type, Precise_State_Type) of Transition_Probability_Type;
    -- Precise_DP_Model : Precise_Model_Type;

   subtype Probability_Type is Float range 0.0 .. 1.0;
   type Policy_Type is array (State_Type) of Action_Type;

   type Action_Value_Array_Type is array (Action_Type) of Float;
   type Value_Function_Type is array (State_Type) of Float;


   function Get_Action_Values (Model : DP_Model_Type; S : State_Type; Value_Function : Value_Function_Type; Discount_Factor : Float) return Action_Value_Array_Type is
      New_Value : Float;
   begin 
      return Action_Values : Action_Value_Array_Type do
         for A in Action_Type loop
            New_Value := 0.0;
            for S1 in State_Type loop
               New_Value := New_Value + Model(S, A, S1).Probability * (Model(S, A, S1).Reward + Discount_Factor * Value_Function(S1));
            end loop;
            Action_Values (A) := New_Value;
         end loop;
      end return;
   end Get_Action_Values;

   function Arg_Max(Action_Values : Action_Value_Array_Type) return Action_Type is
      Max_Value : Float := Float'First;
      Best_Action : Action_Type := Action_Type'First;
   begin
      for A in Action_Type loop
         if Action_Values(A) > Max_Value then
            Max_Value := Action_Values(A);
            Best_Action := A;
         end if;
      end loop;
      return Best_Action;
   end Arg_Max;
   
   function Max_Value (Action_Values : Action_Value_Array_Type) return Float is
      Max_Value : Float := Float'First;
   begin
      for A in Action_Type loop
         if Action_Values(A) > Max_Value then
            Max_Value := Action_Values(A);
         end if;
      end loop;
      return Max_Value;
   end Max_Value;

   function Value_Max_Action_Update(Model : DP_Model_Type; Discount_Factor : Float) return Value_Function_Type is
        Value_Function: Value_Function_Type := (others => 0.0);
        
        Theta : Float := 1.0e-6; -- Convergence threshold
        Local_Delta : Float := 0.0;

        Prev_Value : Float;
        New_Value : Float;
        Next_Values : Action_Value_Array_Type;

        Iteration_Count : Integer := 0;
    begin
        loop
            Iteration_Count := Iteration_Count + 1;
            Put_Line("Iteration " & Iteration_Count'Image);

            Local_Delta := 0.0;
            for S in State_Type loop
                Prev_Value := Value_Function(S);
                Next_Values := Get_Action_Values(Model, S, Value_Function, Discount_Factor);
                New_Value := Max_Value(Next_Values);
                Value_Function(S) := New_Value;
                Local_Delta := Float'Max(Local_Delta, abs(New_Value - Prev_Value));
            end loop;
            exit when Local_Delta < Theta;
        end loop;
        return Value_Function;
    end Value_Max_Action_Update;

    procedure Print_Policy(Policy : Policy_Type) is
    begin
        for S in State_Type loop
            Put_Line("Initial Policy for State " & S'Image & ": " & Policy(S)'Image);
        end loop;
    end Print_Policy;

    function Value_Iteration(Model : DP_Model_Type; Discount_Factor : Float) return Policy_Type is
        Policy : Policy_Type := (others => Action_Random.Random(Action_Gen));  -- Initialize to random policy

        -- Local values
        -- TODO: Consider returning value as well
        Value_Function : Value_Function_Type;
        Action_Values : Action_Value_Array_Type;
    begin
         Print_Policy(Policy);  -- TODO: Remove after debugging

         Value_Function := Value_Max_Action_Update(Model, Discount_Factor);

         for S in State_Type loop
            Action_Values := Get_Action_Values (Model, S, Value_Function, Discount_Factor);
            Policy(S) := Arg_Max(Action_Values);
         end loop;

         return Policy;  -- TODO: Need to return value and policy
    end Value_Iteration;

    Optimal_Policy : Policy_Type;
begin
    Action_Random.Reset(Action_Gen);

    Put_Line("Frozen Lake DP Get_Map_Rows: " & Frozen_Lake_DP.Num_Rows'Image);
    for Current_State in State_Type loop
        for Current_Action in Action_Type loop
            for Next_State in State_Type loop
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
    for S0 in State_Type loop
        for A in Action_Type loop
            for S1 in State_Type loop
                DP_Model(S0, A, S1) := DP_Model(State_Type(S0), A, State_Type(S1));
            end loop;
        end loop;
    end loop;

    Put_Line("First float: " & Float'First'Image);
    Optimal_Policy := Value_Iteration(DP_Model, 0.9);
    Print_Policy(Optimal_Policy);

end Value_Iteration_Example;
