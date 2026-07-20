with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
-- with Ada.Numerics.Discrete_Random;
-- with Ada.Numerics.Float_Random;

-- TODO: This is Work-In-Progress.  The code will likely not compile.
package body RL.Examples.MC is
   function To_Discrete_Observation(Obs : Observation_Type) return Discrete_Observation_Type is
      Dealer_Value_Index : Natural := Obs.Dealer_Showing_Card_Value - 1;
      Player_Value_Index : Natural;
      Res : Discrete_Observation_Type;
   begin
      if Obs.Player_Sum > 21 then
         return Discrete_Observation_Type'Last; -- Terminal state for player going bust
      end if;
         Player_Value_Index := Boolean'Pos(Obs.Usable_Ace) * 10 + (Obs.Player_Sum - 12); -- 10 for the range 12 .. 21
      -- if Obs.Usable_Ace then
      --    Obs.Player_Sum - 12; -- Player sums with a usable ace can be 12-21
      --    Player_Value_Index := Player_Value_Index + 20; -- Usable ace states come after non-usable ace states
      -- else 
      --    Player_Value_Index := Obs.Player_Sum - 2; -- Player sums without a usable ace can be 2-21
      -- end if;
      return Discrete_Observation_Type(Player_Value_Index * 10 + Dealer_Value_Index);
   end;

   procedure Print_Evaluation_Results(Evaluation_Results: Evaluation_Results_Type) is
   begin
      for Temp_Useable_Ace in 0 .. 1 loop
         Put_Line("Usable Ace: " & Boolean'Image(Boolean'Val(Temp_Useable_Ace)));
         Put("    ");
         for Temp_Dealer_Showing_Value in 1 .. 10 loop
            Put(Temp_Dealer_Showing_Value, Width => 8);
         end loop;
         New_Line;

         for Temp_Player_Sum in 12 .. 21 loop
            Put(Temp_Player_Sum, Width => 2);
            Put(": ");
            for Temp_Dealer_Showing_Value in 1 .. 10 loop
               declare
                  A : Action_Type := Evaluation_Results.Policy(
                     Discrete_Observation_Type(Temp_Useable_Ace * 10 * 10 + (Temp_Player_Sum - 12) * 10 + (Temp_Dealer_Showing_Value - 1))
                  );
               begin
                  case A is
                     when HIT =>
                        Put("     HIT");
                     when STICK =>
                        Put("   STICK");
                  end case;
               end;
            end loop;
            New_Line;
         end loop;
      end loop;
   end Print_Evaluation_Results;

   procedure Blackjack_Policy_Evaluation_Example (Config : Config_Type) is
      -- We use the "Auto_Hit" option which automatically hits till the player
      -- has a sum of 12 or more, so we only need to consider player sums of 12-21,
      -- or ten possible values.
      -- We also consider whether the player has a usable ace or not, which represents
      -- two possible values.
      -- The dealer cards can have a value of 1-10.
      -- So overall there are 200 = 2 * 10 * 10 possible non-terminal states.
      -- We also add a terminal state which is used when the player sum exceeds 21.

      -- Num_Discrete_States : constant Integer := 2 * 10 * 10 + 1;
      -- type Discrete_Observation_Type is range 0 .. Num_Discrete_States - 1;
      -- subtype Discrete_Observation_Type2 is Discrete_Observation_Type range 0 .. 30;
      -- function To_Discrete_Observation(Obs : Observation_Type) return Discrete_Observation_Type is
      --    Dealer_Value_Index : Natural := Obs.Dealer_Showing_Card_Value - 1;
      --    Player_Value_Index : Natural;
      --    Res : Discrete_Observation_Type;
      -- begin
      --    if Obs.Player_Sum > 21 then
      --       return Discrete_Observation_Type'Last; -- Terminal state for player going bust
      --    end if;
      --       Player_Value_Index := Boolean'Pos(Obs.Usable_Ace) * 10 + (Obs.Player_Sum - 12); -- 10 for the range 12 .. 21
      --    -- if Obs.Usable_Ace then
      --    --    Obs.Player_Sum - 12; -- Player sums with a usable ace can be 12-21
      --    --    Player_Value_Index := Player_Value_Index + 20; -- Usable ace states come after non-usable ace states
      --    -- else 
      --    --    Player_Value_Index := Obs.Player_Sum - 2; -- Player sums without a usable ace can be 2-21
      --    -- end if;
      --    return Discrete_Observation_Type(Player_Value_Index * 10 + Dealer_Value_Index);
      -- end;

      -- Env : Environment_Type := Make(Config_Type'(Natural_Win_Reward => SAB, Auto_Hit => True));

      -- Hash Map approach
      -- function Observation_Hash (Obs : Observation_Type) return Hash_Type is
      --    Card_Value : Integer := Card_Type'Pos(Obs.Dealer_Showing_Card);
      --    Ace_Offset : Integer := Boolean'Pos(Obs.Usable_Ace);
      -- begin
      --    return Hash_Type(Obs.Player_Sum) xor Hash_Type(Card_Value) xor Hash_Type(Ace_Offset);
      -- end Observation_Hash;
      -- 
      -- package State_Value_Map_Type is new Ada.Containers.Bounded_Hashed_Maps (
      --       Key_Type => Observation_Type,
      --       Element_Type => Float, -- TODO
      --       Hash => Observation_Hash,
      --       Equivalent_Keys => "=");

      -- State_Value_Map : State_Value_Map_Type.Map(20, Hash_Type(16)); --  := State_Value_Map_Type.Empty_Map;
      
      MC_PE_Config : MC_Config_Type := (Num_Episodes => 50 * 1000000, Visit_Type => First_Visit, Discount_Factor => 1.0);
      Temp_Policy : Policy_Type := (others => Stick); -- IN PROGRESS: This is a pretty bad policy, need to implement a better one
      Value_Function : Value_Function_Type;
      Stick_Level : constant Natural := 17;
     
      Evaluation_Results: Evaluation_Results_Type;
      
      MC_Epsilon_Soft_Config : MC_Epsilon_Soft_Config_Type := (Num_Episodes => 5 * 1000000, Discount_Factor => 1.0, Epsilon => 0.05);
   begin
      for Temp_Useable_Ace in 0 .. 1 loop
         for Temp_Player_Sum in 12 .. 21 loop
            for Temp_Dealer_Showing_Value in 1 .. 10 loop
               declare
                  Temp_I : Discrete_Observation_Type := Discrete_Observation_Type(Temp_Useable_Ace * 10 * 10 + (Temp_Player_Sum - 12) * 10 + (Temp_Dealer_Showing_Value - 1));
               begin
                  if Temp_Player_Sum < Stick_Level then
                     Temp_Policy(Temp_I) := Hit;
                  else
                     Temp_Policy(Temp_I) := Stick;
                  end if;
               end;
            end loop;
         end loop;
      end loop;
      Put_Line("Starting Monte Carlo Policy Evaluation");
      Put_Line("Last element of Discrete_Observation_Type: " & Discrete_Observation_Type'Image(Discrete_Observation_Type'Last));
      -- Put_Line("Last element of Discrete_Observation_Type2: " & Discrete_Observation_Type2'Image(Discrete_Observation_Type2'Last));
      -- Put_Line("Element after last element of Discrete_Observation_Type2: " & Discrete_Observation_Type2'Image(Discrete_Observation_Type2'Succ(Discrete_Observation_Type2'Last)));
      Value_Function := MC_Policy_Evaluation(Config, Temp_Policy, MC_PE_Config);
      for S in Discrete_Observation_Type loop
         Put_Line("Value for state " & Discrete_Observation_Type'Image(S) & ": " & Float'Image(Value_Function(S)));
      end loop;
      -- Put_Line(State_Value_Map.Is_Empty'Image);
      -- for I in 2 .. 21 loop
      --    State_Value_Map.Insert(
      --       Key => Observation_Type'(Player_Sum => I, Dealer_Showing_Card => Card_Type'First, Usable_Ace => False),
      --       New_Item => Float(I));
      -- end loop;
      -- for I in 2 .. 21 loop
      --    declare
      --       Value : Float;
      --    begin
      --       Value := State_Value_Map.Element(
      --          Observation_Type'(Player_Sum => I, Dealer_Showing_Card => Card_Type'First, Usable_Ace => False)
      --       );
      --       Put_Line("Value for player sum " & Integer'Image(I) & ": " & Float'Image(Value));
      --    end;
      -- end loop;
      Put_Line("Policy Evaluation Results (100 * Reward):");
      for Temp_Useable_Ace in 0 .. 1 loop
         Put_Line("Usable Ace: " & Boolean'Image(Boolean'Val(Temp_Useable_Ace)));
         Put("    ");
         for Temp_Dealer_Showing_Value in 1 .. 10 loop
            Put(Temp_Dealer_Showing_Value, Width => 8);
         end loop;
         New_Line;

         for Temp_Player_Sum in 12 .. 21 loop
            Put(Temp_Player_Sum, Width => 2);
            Put(": ");
            for Temp_Dealer_Showing_Value in 1 .. 10 loop
               Put(100.0 * Value_Function(Discrete_Observation_Type(Temp_Useable_Ace * 10 * 10 + (Temp_Player_Sum - 12) * 10 + (Temp_Dealer_Showing_Value - 1))), Fore => 5, Aft => 2, Exp => 0);
            end loop;
            New_Line;
         end loop;
      end loop;

      -- TODO: The results don't seem to match the book perfectly, even when using
      --       50 million episodes.  This implementation will need to be reviewed.
      -- Evaluation_Results_Type 
      Put_Line("MC Exploring Starts");
      Put_Line("Number of episodes: " & Integer'Image(MC_PE_Config.Num_Episodes));
      Evaluation_Results := MC_Exploring_Starts_Evaluation(Config, MC_PE_Config);
      Print_Evaluation_Results(Evaluation_Results);

      -- Put_Line("MC Epsilon Soft on-policy evaluation");
      -- Put_Line("Number of episodes: " & Integer'Image(MC_Epsilon_Soft_Config.Num_Episodes));
      -- Evaluation_Results := MC_Epsilon_Soft_Evaluation(MC_Epsilon_Soft_Config);
      -- Print_Evaluation_Results(Evaluation_Results);

   end Blackjack_Policy_Evaluation_Example;
end RL.Examples.MC;

