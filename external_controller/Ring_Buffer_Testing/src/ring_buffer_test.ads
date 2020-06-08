with AUnit;
with AUnit.Test_Fixtures;

package Ring_Buffer_Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Push_Get (T : in out Test);

   procedure Test_Get_Elements (T : in out Test);

end Ring_Buffer_Test;
