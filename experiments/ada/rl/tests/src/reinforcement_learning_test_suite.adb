with Placeholder_Tests;        use Placeholder_Tests;
with Frozenlake_Tests;         use Frozenlake_Tests;
with Cliffwalking_Tests;       use Cliffwalking_Tests;
with Carrental_Tests;          use Carrental_Tests;

package body Reinforcement_Learning_Test_Suite is

   use AUnit.Test_Suites;

   Result : aliased Test_Suite;

   Placeholder_Test        : aliased Placeholder_Test_Case;
   Frozenlake_Test         : aliased Frozenlake_Test_Case;
   Cliffwalking_Test       : aliased Cliffwalking_Test_Case;
   Carrental_Test       : aliased Carrental_Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Placeholder_Test'Access);
      Add_Test (Result'Access, Frozenlake_Test'Access);
      Add_Test (Result'Access, Cliffwalking_Test'Access);
      Add_Test (Result'Access, Carrental_Test'Access);
      return Result'Access;
   end Suite;

end Reinforcement_Learning_Test_Suite;
