with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Frozenlake_Tests is
   type Frozenlake_Test_Case is new Test_Case with null record;

   overriding function Name (T : Frozenlake_Test_Case) return Message_String;
   overriding procedure Register_Tests (T : in out Frozenlake_Test_Case);
   procedure Test_Frozenlake_Random_Actions (T : in out Test_Case'Class);
   procedure Test_Frozenlake_DP_Model (T : in out Test_Case'Class);

end Frozenlake_Tests;

