-- @summary
-- Front distance controller child test package specification.
--
-- @author Julian Hartmer
with AUnit;
with AUnit.Test_Fixtures;

package Front_Distance.Test is
   
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;
   
   procedure test_calculate_output (T : in out Test);
   
  -- procedure check_motor_values (T : in out Test);
   

end Front_Distance.Test;
