pragma Ada_2012;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;
with AUnit.Test_Fixtures;
   
package Job_Executer.Test is
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;
   procedure test_Reached_expected_roadmarker_success (T : in out Test);  
   procedure test_Reached_expected_roadmarker_wrong_section (T : in out Test);  
   procedure test_register_new_cab_success (T : in out Test);
   procedure test_register_new_cab_failure (T : in out Test);
   procedure test_register_new_cab_timeout (T : in out Test);
   procedure test_register_new_cab_incomplete_JSON (T : in out Test);
   procedure test_register_new_cab_wrong_JSON (T : in out Test);
   procedure test_process_valid_section_update_section_success (T : in out Test);
   procedure test_process_valid_section_update_section_failure (T : in out Test);
   procedure test_process_valid_section_reached_no_RM (T : in out Test);
   procedure test_process_valid_section_reached_RM (T : in out Test);
   procedure test_process_valid_section_reached_RM_Wait (T : in out Test);
   procedure test_process_valid_section_reached_RM_Pickup (T : in out Test);
   procedure test_process_valid_section_reached_RM_Pickup_BE_failed (T : in out Test);
   procedure test_process_valid_section_reached_RM_Dropoff (T : in out Test);
   procedure test_process_valid_section_reached_RM_Dropoff_BE_failed (T : in out Test);
   procedure test_process_section_reached_RM (T : in out Test);
   procedure test_process_section_reached_non_existent_RM (T : in out Test);
   procedure test_process_section_no_RM (T : in out Test);
   procedure test_process_section_system_error (T : in out Test);
end Job_Executer.Test;
