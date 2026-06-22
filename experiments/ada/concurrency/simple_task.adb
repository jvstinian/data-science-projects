with Ada.Text_IO; use Ada.Text_IO;

procedure Main is -- implicitly called by the environment task
   task My_Task;

   task body My_Task is
   begin
      for I in Character range 'A' .. 'Z' loop
         Put("task: " & I);
         New_Line;
      end loop;
   end My_Task;
begin
   for I in Character range 'A' .. 'Z' loop
      Put(I);
      New_Line;
   end loop;
end Main;
