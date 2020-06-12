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
            Motor_Controller.Test.test_lane_detection_processing'Access));
      return Ret;
   end Suite;

end Motor_Controller_Suite;
