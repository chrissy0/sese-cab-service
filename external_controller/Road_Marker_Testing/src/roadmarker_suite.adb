-- @summary
-- Road Marker test suite body.
--
-- @author Julian Hartmer and Chanki Hong

with Roadmarker_Functions.Test; use Roadmarker_Functions.Test;
with Roadmarker.Test; use Roadmarker.Test;
with AUnit.Test_Caller;

package body Roadmarker_Suite is

   package Caller is new AUnit.Test_Caller
     (Test_Fixture => Roadmarker_Functions.Test.Test);


   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create
           ("-------------Test Roadmarke Function's get_roadmarker-------------",
            Roadmarker_Functions.Test.test_roadmarker_functions'Access));
      Ret.Add_Test
        (Caller.Create
           ("-------------Test Roadmarke Function's on_roadmarker-------------",
            Roadmarker_Functions.Test.test_on_road_marker'Access));
      Ret.Add_Test
        (Caller.Create
           ("test_calculate_output_from_history",
            Roadmarker.Test.test_calculate_output_from_history'Access));

      Ret.Add_Test
        (Caller.Create
           ("test_calculate_output",
            Roadmarker.Test.test_calculate_output'Access));

      Ret.Add_Test
        (Caller.Create
           ("test_check_error_sensor_array",
            Roadmarker.Test.test_check_error_sensor_array'Access));

      Ret.Add_Test
        (Caller.Create
           ("test_calculate_output_was_on_hotfix_rm",
            Roadmarker.Test.test_calculate_output_was_on_hotfix_rm'Access));


      return Ret;
   end Suite;

end Roadmarker_Suite;
