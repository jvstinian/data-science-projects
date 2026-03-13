procedure Main is
   -- Import the C function
   procedure C_Print (Num : Integer) with
     Import        => True,
     Convention    => C,
     External_Name => "print_from_c";
begin
   C_Print(42);
end Main;
