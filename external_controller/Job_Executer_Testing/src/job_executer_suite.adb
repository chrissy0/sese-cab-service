
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
        (Caller.Create("test_process_section_update_section_success",
         Job_Executer.Test.test_process_section_update_section_success'Access));
      Ret.Add_Test
        (Caller.Create("test_process_section_update_section_failure",
         Job_Executer.Test.test_process_section_update_section_failure'Access));
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

      ------------------
      -- Update Route --
      ------------------
      
      Ret.Add_Test
        (Caller.Create("test_update_route_success_not_in_section",
         Job_Executer.Test.test_update_route_success_not_in_section'Access));
      Ret.Add_Test
        (Caller.Create("test_update_route_success_in_section",
         Job_Executer.Test.test_update_route_success_in_section'Access));
      Ret.Add_Test
        (Caller.Create("test_update_route_failure",
         Job_Executer.Test.test_update_route_failure'Access));
       Ret.Add_Test
        (Caller.Create("test_update_route_incomplete_JSON",
         Job_Executer.Test.test_update_route_incomplete_JSON'Access));
      Ret.Add_Test
        (Caller.Create("test_update_route_wrong_JSON",
         Job_Executer.Test.test_update_route_wrong_JSON'Access));
      Ret.Add_Test
        (Caller.Create("test_update_route_timeout",
         Job_Executer.Test.test_update_route_timeout'Access));
      Ret.Add_Test
        (Caller.Create("test_update_route_same_version",
         Job_Executer.Test.test_update_route_same_version'Access));
      ---------------------------
      -- Determine Done Signal --
      ---------------------------
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_LEFT",
         Job_Executer.Test.test_determine_done_signal_LEFT'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_RIGHT",
         Job_Executer.Test.test_determine_done_signal_RIGHT'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_UNKNOWN",
         Job_Executer.Test.test_determine_done_signal_UNKNOWN'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_WAIT",
         Job_Executer.Test.test_determine_done_signal_WAIT'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_STOP",
         Job_Executer.Test.test_determine_done_signal_STOP'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_PICKUP_instant_success",
         Job_Executer.Test.test_determine_done_signal_PICKUP_instant_success'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_double_PICKUP_instant_success",
         Job_Executer.Test.test_determine_done_signal_double_PICKUP_instant_success'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_PICKUP_failure",
         Job_Executer.Test.test_determine_done_signal_PICKUP_failure'Access));
            Ret.Add_Test
        (Caller.Create("test_determine_done_signal_PICKUP_incomplete_JSON",
         Job_Executer.Test.test_determine_done_signal_PICKUP_incomplete_JSON'Access));
            Ret.Add_Test
        (Caller.Create("test_determine_done_signal_PICKUP_wrong_JSON",
         Job_Executer.Test.test_determine_done_signal_PICKUP_wrong_JSON'Access));
            Ret.Add_Test
        (Caller.Create("test_determine_done_signal_PICKUP_timeout",
         Job_Executer.Test.test_determine_done_signal_PICKUP_timeout'Access));
            Ret.Add_Test
        (Caller.Create("test_determine_done_signal_PICKUP_late_failure",
         Job_Executer.Test.test_determine_done_signal_PICKUP_late_failure'Access));
            Ret.Add_Test
        (Caller.Create("test_determine_done_signal_PICKUP_not_completed",
         Job_Executer.Test.test_determine_done_signal_PICKUP_not_completed'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_DROPOFF_instant_success",
         Job_Executer.Test.test_determine_done_signal_DROPOFF_instant_success'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_double_DROPOFF_instant_success",
         Job_Executer.Test.test_determine_done_signal_double_DROPOFF_instant_success'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_DROPOFF_failure",
         Job_Executer.Test.test_determine_done_signal_DROPOFF_failure'Access));
       Ret.Add_Test
        (Caller.Create("test_determine_done_signal_DROPOFF_incomplete_JSON",
         Job_Executer.Test.test_determine_done_signal_DROPOFF_incomplete_JSON'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_DROPOFF_wrong_JSON",
         Job_Executer.Test.test_determine_done_signal_DROPOFF_wrong_JSON'Access));
       Ret.Add_Test
        (Caller.Create("test_determine_done_signal_DROPOFF_timeout",
         Job_Executer.Test.test_determine_done_signal_DROPOFF_timeout'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_DROPOFF_late_failure",
         Job_Executer.Test.test_determine_done_signal_PICKUP_late_failure'Access));
      Ret.Add_Test
        (Caller.Create("test_determine_done_signal_DROPOFF_not_completed",
         Job_Executer.Test.test_determine_done_signal_PICKUP_not_completed'Access));


      return Ret;
   end Suite; 

end Job_Executer_Suite;
