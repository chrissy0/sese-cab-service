with AUnit;
with AUnit.Test_Fixtures;

package Front_Distance.Test is
   
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;
   
   procedure test_front_distance (T : in out Test);
   

end Front_Distance.Test;
