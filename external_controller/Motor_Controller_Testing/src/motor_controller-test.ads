with AUnit;
with AUnit.Test_Fixtures;

package Motor_Controller.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure test_lane_detection_processing (T : in out Test);

end Motor_Controller.Test;
