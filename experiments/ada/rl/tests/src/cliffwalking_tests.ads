with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Cliffwalking_Tests is
   type Cliffwalking_Test_Case is new Test_Case with null record;

   overriding function Name (T : Cliffwalking_Test_Case) return Message_String;
   overriding procedure Register_Tests (T : in out Cliffwalking_Test_Case);
   procedure Test_Cliffwalking_Random_Actions (T : in out Test_Case'Class);

end Cliffwalking_Tests;


