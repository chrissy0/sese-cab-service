with Motor_Controller.Test;
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
           ("Test Motor Controller drive_state_output",
            Motor_Controller.Test.test_drive_states'Access));

      Ret.Add_Test
        (Caller.Create
           ("Test Motor Controller test_normal_driving_states",
            Motor_Controller.Test.test_normal_driving_states'Access));

      Ret.Add_Test
        (Caller.Create
           ("Test Motor Controller test_front_clear_states",
            Motor_Controller.Test.test_front_clear_states'Access));

      Ret.Add_Test
        (Caller.Create
           ("Test Motor Controller test_front_clear_states",
            Motor_Controller.Test.test_lean_states'Access));



      return Ret;
   end Suite;

end Motor_Controller_Suite;
