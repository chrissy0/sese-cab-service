with AUnit;
with AUnit.Test_Fixtures;

package Lane_Detection_Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure test_lane_detection (T : in out Test);

end Lane_Detection_Test;
