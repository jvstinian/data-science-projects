with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Numerics;

generic
   Map_Info : Map_Info_Type;
package Frozen_Lake.Child is
    -- function Get_Map_Rows return Positive;
    -- function Get_Map_Cols return Positive;
    
    Num_Rows : Positive := Map_Info.Rows;
    Num_Cols : Positive := Map_Info.Cols;
    type Alt_Discrete_State_Type is new Natural range 0 .. (Num_Rows * Num_Cols - 1);
    
    procedure Dummy_Method;
end Frozen_Lake.Child;
