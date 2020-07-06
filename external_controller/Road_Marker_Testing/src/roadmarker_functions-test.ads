with AUnit;
with AUnit.Test_Fixtures;

package Roadmarker_Functions.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure test_roadmarker_functions (T : in out Test);

   procedure test_on_road_marker (T : in out Test);


end Roadmarker_Functions.Test;
