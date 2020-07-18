-- @summary
-- Motor Controller test suite body.
--
-- @author Julian Hartmer

with Motor_Controller.Test; use Motor_Controller.Test;
with AUnit.Test_Caller;

package body Motor_Controller_Suite is

   package Caller is new AUnit.Test_Caller
     (Test_Fixture => Motor_Controller.Test.Test);

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin

      Ret.Add_Test
        (Caller.Create
           ("test_calculate_output",
            test_calculate_output'Access));

      Ret.Add_Test
        (Caller.Create
           ("test_do_state_transition",
            test_do_state_transition'Access));

      Ret.Add_Test
        (Caller.Create
           ("test_output_final_safe_state",
            test_output_final_safe_state'Access));

      Ret.Add_Test
        (Caller.Create
           ("test_output_system_error",
            test_output_system_error'Access));

      Ret.Add_Test
        (Caller.Create
           ("test_output_no_system_error",
            test_output_no_system_error'Access));

      Ret.Add_Test
        (Caller.Create
           ("test_output_front_is_clear",
            test_output_front_is_clear'Access));

      Ret.Add_Test
        (Caller.Create
           ("test_output_driving",
            test_output_driving'Access));


      return Ret;
   end Suite;

end Motor_Controller_Suite;
