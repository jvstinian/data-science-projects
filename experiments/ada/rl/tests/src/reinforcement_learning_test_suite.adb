with Placeholder_Tests;        use Placeholder_Tests;

package body Reinforcement_Learning_Test_Suite is

   use AUnit.Test_Suites;

   Result : aliased Test_Suite;

   Placeholder_Test        : aliased Placeholder_Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Placeholder_Test'Access);
      return Result'Access;
   end Suite;

end Reinforcement_Learning_Test_Suite;
