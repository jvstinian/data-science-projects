-- subtype Even_Integer is Integer with Static_Predicate => Even_Integer mod 2 = 0;
generic
   Width: Positive;
   Height: Positive;
package Canvas is
   pragma Assert(Width mod 2 = 0, "Width must be even");
   pragma Assert(Height mod 2 = 0, "Height must be even");

   type RGBA_Value is new Integer range 0 .. 255;
   type RGBA_Indices is (Red, Green, Blue, Alpha);
   type RGBA is array (RGBA_Indices) of RGBA_Value;

   type Screen_Array is array (0 .. (Height - 1), 0 .. (Width - 1)) of RGBA;
   type Canvas_Array is array ((-Height / 2) + 1 .. (Height / 2), (-Width / 2) .. ((Width / 2) - 1)) of RGBA;

   function To_Canvas_Array (S: Screen_Array) return Canvas_Array ; -- is
   --    Res: Canvas_Array;
   -- begin
   --    for J in Canvas_Array'Range(1) loop
   --       for I in Canvas_Array'Range(2) loop
   --          Res(J, I) := S((Height / 2) - J, I + (Width / 2));
   --       end loop;
   --    end loop;
   --    return Res;
   -- end To_Canvas_Array;
   
   function To_Screen_Array (C: Canvas_Array) return Screen_Array ; -- is
   --    Res: Screen_Array;
   -- begin
   --    for J in Screen_Array'Range(1) loop
   --       for I in Screen_Array'Range(2) loop
   --          Res(J, I) := C((Height / 2) - J, I - (Width / 2));
   --       end loop;
   --    end loop;
   --    return Res;
   -- end To_Canvas_Array;
end Canvas;
