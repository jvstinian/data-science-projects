with Ada.Text_IO; use Ada.Text_IO;

package body RL.Envs.Frozenlake.DP is
   -- TODO: Remove
   -- function Get_Model(Config: Config_Type) return Precise_Model_Type is
   --    DP_Model : Discrete_Model_Type := Get_Model(Config);
   --    Precise_DP_Model : Precise_Model_Type;
   -- begin
   --    for S0 in State_Type loop
   --       for A in Action_Type loop
   --          for S1 in State_Type loop
   --              Precise_DP_Model(S0, A, S1) := DP_Model(Discrete_State_Type(S0), A, Discrete_State_Type(S1));
   --          end loop;
   --       end loop;
   --    end loop;
   --    return Precise_DP_Model;
   -- end Get_Model;

   function Get_Model(Config: Config_Type) return DP_Model_Type is
      Res : DP_Model_Type := (others => (others => (others => (Probability => 0.0, Reward => 0.0))));
      Env : Environment_Type := Make(Config);
      Prev_State : State_Type;
      Next_State : State_Type;

      type Expected_Reward_Type is record
         Probability_Weighted_Reward : Float := 0.0;
         Total_Probability : Float := 0.0;
      end record;
      type Expected_Rewards_Type is array (State_Type) of Expected_Reward_Type;

      Temp_Expected_Rewards : Expected_Rewards_Type;

      Temp_Probability : Float;
      Temp_Reward : Float;
   begin
      for I in Env.P'Range(1) loop
         for J in Env.P'Range(2) loop
            Prev_State := State_Type(To_S(Env.Map, Position_Type'(Row => I, Col => J)));
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
                  Next_State := State_Type(To_S(Env.Map, Env.P(I, J)(A, A_Act).Position));
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
end RL.Envs.Frozenlake.DP;
