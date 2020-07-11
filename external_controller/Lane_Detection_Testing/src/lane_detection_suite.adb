pragma Ada_2012;
package body Lane_Detection_Suite is

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      pragma Compile_Time_Warning (Standard.True, "Suite unimplemented");
      return raise Program_Error with "Unimplemented function Suite";
   end Suite;

end Lane_Detection_Suite;
