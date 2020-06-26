with AUnit;
with AUnit.Test_Fixtures;

package Motor_Controller.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure test_drive_states (T : in out Test);

   procedure test_normal_driving_states (T : in out Test);

   procedure test_front_clear_states (T : in out Test);

   procedure test_lean_states (T : in out Test);
end Motor_Controller.Test;
