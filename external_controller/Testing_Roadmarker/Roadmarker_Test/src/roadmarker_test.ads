with AUnit;
with AUnit.Test_Fixtures;

package Roadmarker_Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   function Name (T : Test) return AUnit.Message_String;

   procedure test_roadmarker (T : in out Test);

end Roadmarker_Test;
