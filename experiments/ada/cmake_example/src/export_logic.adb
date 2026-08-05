with Ada.Text_IO; use Ada.Text_IO;

package body Export_Logic is
   procedure Ada_Message is
   begin
      Put_Line("Hello from Ada!");
   end Ada_Message;
   
   function Ada_Add(x, y: Interfaces.C.int) return Interfaces.C.int is
   begin
    return x + y;
   end Ada_Add;
end Export_Logic;

