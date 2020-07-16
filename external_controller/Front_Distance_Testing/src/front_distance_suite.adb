-- @summary
-- Front distance test suite body.
--
-- @author Julian Hartmer
with Front_Distance.Test;
with AUnit.Test_Caller;
with Motor_Controller;

package body Front_Distance_Suite is

   package Caller is new AUnit.Test_Caller
     (Test_Fixture => Front_Distance.Test.Test);

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      --  Ret.Add_Test
      --    (Caller.Create
      --       ("Test motor values",
      --        Front_Distance.Test.check_motor_values'Access));
      Ret.Add_Test
        (Caller.Create
           ("test_calculate_output",
            Front_Distance.Test.test_calculate_output'Access));
      return Ret;
   end Suite;

end Front_Distance_Suite;
