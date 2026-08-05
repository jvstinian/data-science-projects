with Interfaces.C; use type Interfaces.C.int;

package Export_Logic is
   procedure Ada_Message;
   pragma Export (C, Ada_Message, "ada_message");

   function Ada_Add(x, y: Interfaces.C.int) return Interfaces.C.int;
   pragma Export (C, Ada_Add, "ada_add");
end Export_Logic;
