-- @summary
-- Road Marker controller child unit test package specification.
--
-- @author Julian Hartmer

with AUnit;
with Roadmarker_Functions.Test; use Roadmarker_Functions.Test;

package Roadmarker.Test is

   function Name (T : Roadmarker_Functions.Test.Test) return AUnit.Message_String;

   procedure test_calculate_output_from_history (T : in out Roadmarker_Functions.Test.Test);

   procedure test_calculate_output(T : in out Roadmarker_Functions.Test.Test);

   procedure test_check_error_sensor_array(T : in out Roadmarker_Functions.Test.Test);

   procedure test_calculate_output_was_on_hotfix_rm(T : in out Roadmarker_Functions.Test.Test);

end Roadmarker.Test;
