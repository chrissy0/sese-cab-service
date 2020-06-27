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
      Ret.Add_Test
        (Caller.Create
           ("Test Motor Controller drive_state_output",
            Front_Distance.Test.test_front_distance'Access));
      return Ret;
   end Suite;

end Front_Distance_Suite;
