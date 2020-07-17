-- @summary
-- Lane Detection controller child unit test suite body.
--
-- @author Julian Hartmer

pragma Ada_2012;
with AUnit.Test_Caller;
with Lane_Detection.Test; use Lane_Detection.Test;
package body Lane_Detection_Suite is

   package Caller is new AUnit.Test_Caller
     (Test_Fixture => Lane_Detection.Test.Test);

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create("test detect lanes", Lane_Detection.Test.test_detect_lanes'Access));

      Ret.Add_Test
        (Caller.Create("test_output_from_line_detection", Lane_Detection.Test.test_output_from_line_detection'Access));

      Ret.Add_Test
        (Caller.Create("test_output_from_curb_detection", Lane_Detection.Test.test_output_from_curb_detection'Access));

      Ret.Add_Test
        (Caller.Create("test_get_lean_from_line_color", Lane_Detection.Test.test_get_lean_from_line_color'Access));

      return Ret;
   end Suite;

end Lane_Detection_Suite;
