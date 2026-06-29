with AUnit.Assertions; use AUnit.Assertions;

package body Placeholder_Tests is

   overriding function Name (T : Placeholder_Test_Case) return Test_String is
     (Format ("Placeholder Tests        "));

   overriding procedure Register_Tests (T : in out Placeholder_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Placeholder'Access, "A Simple Placeholder Test");
   end Register_Tests;

   procedure Test_Placeholder (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      Number : constant Integer := 3;
   begin
      Assert (Number = 2, "Number does not match");
   end Test_Placeholder;

end Placeholder_Tests;
