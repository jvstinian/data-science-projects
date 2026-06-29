with Ada.Text_IO; use Ada.Text_IO;

package body RL.Envs.Frozenlake.Child is
    procedure Dummy_Method is
    begin
        Put_Line("This is a dummy method in the child package.");
        Put_Line("Last value of discrete state: " & Alt_Discrete_State_Type'Last'Image);
    end Dummy_Method;
end RL.Envs.Frozenlake.Child;
