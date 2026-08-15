with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Linewalk_Tests is
   type Linewalk_Test_Case is new Test_Case with null record;

   overriding function Name (T : Linewalk_Test_Case) return Message_String;
   overriding procedure Register_Tests (T : in out Linewalk_Test_Case);
   procedure Test_Linewalk_Random_Actions (T : in out Test_Case'Class);
end Linewalk_Tests;
