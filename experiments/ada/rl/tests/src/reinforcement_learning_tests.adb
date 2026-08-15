with Ada.Command_Line;
with Ada.Text_IO;

with AUnit.Run;
with AUnit.Reporter.Text;
with AUnit.Test_Results;

with Reinforcement_Learning_Tests_Config;
use Reinforcement_Learning_Tests_Config;
with Reinforcement_Learning_Test_Suite;
with RL;

procedure Reinforcement_Learning_Tests is
   procedure Run is new AUnit.Run.Test_Runner_With_Results
        (Reinforcement_Learning_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Results  : AUnit.Test_Results.Result;

   RL_Library_Name : constant String := "Reinforcement_Learning";
   RL_Library_Version : constant String := "0.1.0";
begin
   Ada.Text_IO.Put_Line
      ("   " & RL_Library_Name & " = " & RL_Library_Version);

   pragma Warnings (Off);
   Reporter.Set_Use_ANSI_Colors (Build_Profile = development);
   pragma Warnings (On);

   Run (Reporter, Results);

   if not Results.Successful then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;

end Reinforcement_Learning_Tests;
