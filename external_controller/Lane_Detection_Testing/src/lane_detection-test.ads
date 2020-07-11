with AUnit;
with AUnit.Test_Fixtures;

package Lane_Detection.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;
   
   procedure test_detect_lanes (T : in out Test);
   
   procedure test_output_from_line_detection (T : in out Test);
   
   procedure test_output_from_curb_detection (T : in out Test);

   procedure test_calculate_output (T : in out Test);
   
   procedure test_get_lean_from_line_color (T : in out Test);

end Lane_Detection.Test;
