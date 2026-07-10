with RL; use RL;  -- Transition_Probability_Type

-- TODO: This is not the final specification of this package.
--       Some DP models will use access pointers instead of
--       DP_Model_Type below, so we'll need to make some adjustments.
--       One possibility might be to try to define an access type to DP_Model_Type
--       and then simply pass the access value to the methods below.
--       Another possibility is to make DP_Model_Type a general type (limited private?)
--       and also provide a method for obtaining the transition probabilities given a
--       (state, action, state) triple.
--       The second approach would be more general and allow us to pass functions that
--       calculate the transitions on-the-fly rather than storing them as arrays or
--       access values pointing to arrays.
generic
   type State_Type is (<>);
   type Action_Type is (<>);
   -- type Transition_Probability_Type is private;  -- TODO: Remove
   type DP_Model_Type is array (State_Type, Action_Type, State_Type) of Transition_Probability_Type;
package RL.Algorithms.DP is
   subtype Probability_Type is Float range 0.0 .. 1.0;
   type Deterministic_Policy_Type is array (State_Type) of Action_Type;
   type Stochastic_Policy_Type is array (State_Type, Action_Type) of Probability_Type;

   type Value_Function_Type is array (State_Type) of Float;

   function Iterative_Policy_Evaluation(Model : DP_Model_Type; Policy : Stochastic_Policy_Type; Discount_Factor : Float) return Value_Function_Type;
   function Iterative_Policy_Evaluation(Model : DP_Model_Type; Policy : Deterministic_Policy_Type; Discount_Factor : Float; Value_Estimate : Value_Function_Type) return Value_Function_Type;
   
   procedure Print_Policy(Policy : Deterministic_Policy_Type);
   -- function Policy_Iteration(Model : DP_Model_Type; Discount_Factor : Float) return Deterministic_Policy_Type;
   function Policy_Iteration(Model : DP_Model_Type; Discount_Factor : Float) return Deterministic_Policy_Type;
end RL.Algorithms.DP;
