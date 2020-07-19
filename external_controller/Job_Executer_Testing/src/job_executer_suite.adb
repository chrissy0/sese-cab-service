
pragma Ada_2012;
with AUnit.Test_Caller;
with Job_Executer.Test; use Job_Executer.Test;
package body Job_Executer_Suite is

   package Caller is new AUnit.Test_Caller
     (Test_Fixture => Job_Executer.Test.Test);

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      ----------------
      -- Unit Tests --
      ----------------
      ------------------------
      -- Reached Roadmarker --
      ------------------------
      Ret.Add_Test
        (Caller.Create("test_Reached_expected_roadmarker_success", 
         Job_Executer.Test.test_Reached_expected_roadmarker_success'Access));
      Ret.Add_Test
        (Caller.Create("test_Reached_expected_roadmarker_wrong_section",
         Job_Executer.Test.test_Reached_expected_roadmarker_wrong_section'Access));
      ----------------------
      -- Register new cab --
      ----------------------
      Ret.Add_Test
        (Caller.Create("test_register_new_cab_success",
         Job_Executer.Test.test_register_new_cab_success'Access));
      Ret.Add_Test
        (Caller.Create("test_register_new_cab_failure",
         Job_Executer.Test.test_register_new_cab_failure'Access));
      Ret.Add_Test
        (Caller.Create("test_register_new_cab_timeout",
         Job_Executer.Test.test_register_new_cab_timeout'Access));
      Ret.Add_Test
        (Caller.Create("test_register_new_cab_incomplete_JSON",
         Job_Executer.Test.test_register_new_cab_incomplete_JSON'Access));
      Ret.Add_Test
        (Caller.Create("test_register_new_cab_wrong_JSON",
         Job_Executer.Test.test_register_new_cab_incomplete_JSON'Access));
      ---------------------------
      -- Process valid section --
      ---------------------------
      Ret.Add_Test
        (Caller.Create("test_process_valid_section_update_section_success",
         Job_Executer.Test.test_process_valid_section_update_section_success'Access));
      Ret.Add_Test
        (Caller.Create("test_process_valid_section_update_section_failure",
         Job_Executer.Test.test_process_valid_section_update_section_failure'Access));
        Ret.Add_Test
        (Caller.Create("test_process_valid_section_reached_no_RM",
         Job_Executer.Test.test_process_valid_section_reached_no_RM'Access));
      Ret.Add_Test
        (Caller.Create("test_process_valid_section_reached_RM",
         Job_Executer.Test.test_process_valid_section_reached_RM'Access));
      Ret.Add_Test
        (Caller.Create("test_process_valid_section_reached_RM_Wait",
         Job_Executer.Test.test_process_valid_section_reached_RM_Wait'Access));
      Ret.Add_Test
        (Caller.Create("test_process_valid_section_reached_RM_Pickup",
         Job_Executer.Test.test_process_valid_section_reached_RM_Pickup'Access));
      Ret.Add_Test
        (Caller.Create("test_process_valid_section_reached_RM_Pickup_BE_failed",
         Job_Executer.Test.test_process_valid_section_reached_RM_Pickup_BE_failed'Access));
      Ret.Add_Test
        (Caller.Create("test_process_valid_section_reached_RM_Dropoff",
         Job_Executer.Test.test_process_valid_section_reached_RM_Dropoff'Access));
      Ret.Add_Test
        (Caller.Create("test_process_valid_section_reached_RM_Dropoff_BE_failed",
         Job_Executer.Test.test_process_valid_section_reached_RM_Dropoff_BE_failed'Access));
  
      ---------------------
      -- Process section --
      ---------------------
      Ret.Add_Test
        (Caller.Create("test_process_section_reached_RM",
         Job_Executer.Test.test_process_section_reached_RM'Access));
      Ret.Add_Test
        (Caller.Create("test_process_section_reached_non_existent_RM",
         Job_Executer.Test.test_process_section_reached_non_existent_RM'Access));
      Ret.Add_Test
        (Caller.Create("test_process_section_no_RM",
         Job_Executer.Test.test_process_section_no_RM'Access)); 
      Ret.Add_Test
        (Caller.Create("test_process_section_system_error",
         Job_Executer.Test.test_process_section_system_error'Access));

      return Ret;
   end Suite; 

end Job_Executer_Suite;
