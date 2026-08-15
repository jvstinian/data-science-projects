with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Blackjack_Tests is
   type Blackjack_Test_Case is new Test_Case with null record;

   overriding function Name (T : Blackjack_Test_Case) return Message_String;
   overriding procedure Register_Tests (T : in out Blackjack_Test_Case);
   procedure Test_Blackjack_Random_Actions (T : in out Test_Case'Class);
end Blackjack_Tests;


