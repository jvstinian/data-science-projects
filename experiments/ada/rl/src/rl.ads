package RL is
   type Seed_Reset_Kind is (Set_Default, No_Set, Set_Seed);
   type Seed_Reset_Type(Kind : Seed_Reset_Kind := Set_Default) is
   record
      case Kind is
         when Set_Seed =>
            Seed : Integer;
         when others =>
            null;
      end case;
   end record;
   
   type Transition_Probability_Type is record
       Probability : Float;
       Reward : Float;
   end record;
end RL;
