
package body Canvas is
   function To_Canvas_Array (S: Screen_Array) return Canvas_Array is
      Res: Canvas_Array;
   begin
      for J in Canvas_Array'Range(1) loop
         for I in Canvas_Array'Range(2) loop
            Res(J, I) := S((Height / 2) - J, I + (Width / 2));
         end loop;
      end loop;
      return Res;
   end To_Canvas_Array;
   
   function To_Screen_Array (C: Canvas_Array) return Screen_Array is
      Res: Screen_Array;
   begin
      for J in Screen_Array'Range(1) loop
         for I in Screen_Array'Range(2) loop
            Res(J, I) := C((Height / 2) - J, I - (Width / 2));
         end loop;
      end loop;
      return Res;
   end To_Screen_Array;
end Canvas;
