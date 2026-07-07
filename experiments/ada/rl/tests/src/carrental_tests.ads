with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Carrental_Tests is
   type Carrental_Test_Case is new Test_Case with null record;

   overriding function Name (T : Carrental_Test_Case) return Message_String;
   overriding procedure Register_Tests (T : in out Carrental_Test_Case);
   procedure Test_Carrental_Random_Action (T : in out Test_Case'Class);
   procedure Test_Carrental_DP_Model (T : in out Test_Case'Class);
   procedure Test_Carrental_Transition_Values (T : in out Test_Case'Class);

end Carrental_Tests;
