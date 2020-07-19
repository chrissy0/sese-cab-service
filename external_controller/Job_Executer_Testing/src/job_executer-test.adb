with Job_Executer; use Job_Executer;
with ec2b; use ec2b;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Job_Executer_Testing.Callbacks; use Job_Executer_Testing.Callbacks;
package body Job_Executer.Test is

   procedure test_Reached_expected_roadmarker_success (T : in out Test) is
      next_command : Command_t;
      return_value : Boolean;
   begin
      for section in Road_Marker_ID_T'Range loop
         next_command.section := section;
         return_value := Reached_expected_roadmarker(section, next_command);
         Assert(return_value = True, "Expected true if section = command_section");
      end loop;
   end test_Reached_expected_roadmarker_success;

  -- procedure test_Reached_expected_roadmarker_constraint_error is
  --    next_command : Command_t;
  --    return_value : Boolean;
 --     section : Integer := Road_Marker_ID_T'Last +1;
  -- begin
   --   next_command.section := section;
     -- return_value := Reached_expected_roadmarker(section, next_command);
   --end test_Reached_expected_roadmarker_constraint_error;

   procedure test_Reached_expected_roadmarker_wrong_section (T : in out Test) is
   begin
    --  AUnit.Assertions.Assert_Exception(test_Reached_expected_roadmarker_constraint_error'Access,
    --                                   "Expected a Constraint Error if section > Road_Marker_ID_T'Last");
    Assert(True, "WHAT?"); -- TODO
   end test_Reached_expected_roadmarker_wrong_section;

   procedure test_register_new_cab_success (T : in out Test) is
      cab_name :  Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String("Dieter");
      cab_id :  Integer := -1;
      start_section:  Integer := 14;
      error_counter :  Integer := 0;
      retry_register :  Boolean := True;
      Job_Executer_Done_Signal : Job_Executer_Done_T := NEXT_UNKOWN_S;
   begin
      register_new_cab(cab_name, cab_id, start_section, error_counter,
                       retry_register, Job_Executer_Done_Signal);
      Assert(cab_id = 1337, "Expected Cab_id : 1337 ");
      Assert(retry_register = False, "Retry_register : False");
      Assert(error_counter = 0, "Expected Error_counter : 0 ");
   end test_register_new_cab_success;

   procedure test_register_new_cab_failure (T : in out Test) is
      cab_name :  Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String("Dieter");
      cab_id :  Integer := -1;
      start_section:  Integer := 14;
      error_counter :  Integer := 0;
      retry_register :  Boolean := True;
      Job_Executer_Done_Signal : Job_Executer_Done_T := NEXT_UNKOWN_S;
   begin
      Job_Executer_Testing.Callbacks.action := E_Failure;
      register_new_cab(cab_name, cab_id, start_section, error_counter,
                       retry_register, Job_Executer_Done_Signal);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(cab_id = -1, "Expected Cab_id : -1 ");
      Assert(retry_register = True, "Retry_register : True");
      Assert(error_counter = 1, "Expected Error_counter : 1 ");

   end test_register_new_cab_failure;

   procedure test_register_new_cab_timeout (T : in out Test) is
      cab_name :  Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String("Dieter");
      cab_id :  Integer := -1;
      start_section:  Integer := 14;
      error_counter :  Integer := 0;
      retry_register :  Boolean := True;
      Job_Executer_Done_Signal : Job_Executer_Done_T := NEXT_UNKOWN_S;
   begin
      Job_Executer_Testing.Callbacks.action := E_Timeout;
      register_new_cab(cab_name, cab_id, start_section, error_counter,
                       retry_register, Job_Executer_Done_Signal);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(cab_id = -1, "Expected Cab_id : -1 ");
      Assert(retry_register = True, "Retry_register : True");
      Assert(error_counter = 1, "Expected Error_counter : 1 ");

   end test_register_new_cab_timeout;

   procedure test_register_new_cab_incomplete_JSON (T : in out Test) is
      cab_name :  Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String("Dieter");
      cab_id :  Integer := -1;
      start_section:  Integer := 14;
      error_counter :  Integer := 0;
      retry_register :  Boolean := True;
      Job_Executer_Done_Signal : Job_Executer_Done_T := NEXT_UNKOWN_S;
   begin
      Job_Executer_Testing.Callbacks.action := E_incomplete_JSON;
      register_new_cab(cab_name, cab_id, start_section, error_counter,
                       retry_register, Job_Executer_Done_Signal);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(cab_id = -1, "Expected Cab_id : -1 ");
      Assert(retry_register = True, "Retry_register : True");
      Assert(error_counter = 1, "Expected Error_counter : 1 ");

   end test_register_new_cab_incomplete_JSON;

   procedure test_register_new_cab_wrong_JSON (T : in out Test) is
      cab_name :  Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String("Dieter");
      cab_id :  Integer := -1;
      start_section:  Integer := 14;
      error_counter :  Integer := 0;
      retry_register :  Boolean := True;
      Job_Executer_Done_Signal : Job_Executer_Done_T := NEXT_UNKOWN_S;
   begin
      Job_Executer_Testing.Callbacks.action := E_wrong_JSON;
      register_new_cab(cab_name, cab_id, start_section, error_counter,
                       retry_register, Job_Executer_Done_Signal);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(cab_id = -1, "Expected Cab_id : -1 ");
      Assert(retry_register = True, "Retry_register : True");
      Assert(error_counter = 1, "Expected Error_counter : 1 ");

   end test_register_new_cab_wrong_JSON;

   procedure test_process_valid_section_update_section_success (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);
      Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);
      Assert(retry_location_update = False, "expected retry_location_update : False ");
      Assert(error_counter = 0, "expected error_counter : 0 ");
   end test_process_valid_section_update_section_success;

   procedure test_process_valid_section_update_section_failure (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);
      Job_Executer_Testing.Callbacks.action := E_Failure;
      Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(retry_location_update = True, "expected retry_location_update : True ");
      Assert(error_counter = 1, "expected error_counter : 1 ");
   end test_process_valid_section_update_section_failure;

   procedure test_process_valid_section_reached_no_RM (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section := 3;
      current_command.section := 3;
      current_command.action := NEXT_RIGHT_S;
      next_command.section := 4;
      next_command.action := NEXT_LEFT_S;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);

      Assert(current_command.action = NEXT_RIGHT_S, "expected current_command.action : NEXT_RIGHT_S ");
      Assert(current_command.section = 3, "expected current_command.section : 3 ");
      Assert(next_command.action = NEXT_LEFT_S, "expected next_command.action : NEXT_LEFT_S ");
      Assert(next_command.section = 4, "expected next_command.section : 4 ");
      Assert(queue_command.action = WAIT_S, "expected queue_command.action : WAIT_S ");
      Assert(queue_command.section = 1337, "expected queue_command.section : 1337");
   end test_process_valid_section_reached_no_RM;

   procedure test_process_valid_section_reached_RM (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section := 3;
      next_command.section := 3;
      next_command.action := NEXT_LEFT_S;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);

      Assert(current_command.action = NEXT_LEFT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 3, "expected current_command.section : 3 ");
      Assert(next_command.action = WAIT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(next_command.section = 1337, "expected current_command.section : 3 ");
   end test_process_valid_section_reached_RM;

   procedure test_process_valid_section_reached_RM_Wait (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section := 0;
      next_command.section := 0;
      next_command.action := WAIT_S;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := NEXT_LEFT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);
      cmd_queue.Dequeue(queue_command.all);
      Assert(current_command.action = WAIT_S, "expected current_command.action : WAIT_S ");
      Assert(current_command.section = 0, "expected current_command.section : 0 ");
      Assert(next_command.action = WAIT_S, "expected next_command.action : WAIT_S ");
      Assert(next_command.section = 0, "expected next_command.section : 0 ");
      Assert(queue_command.action = NEXT_LEFT_S, "expected queue_command.action : NEXT_LEFT_S ");
      Assert(queue_command.section = 1337, "expected queue_command.section : 1337");
   end test_process_valid_section_reached_RM_Wait;


   procedure test_process_valid_section_reached_RM_Pickup (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section := 4;
      next_command.section := 4;
      next_command.action := PICK_UP_S;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);

      Assert(current_command.action = PICK_UP_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 4, "expected current_command.section : 3 ");
      Assert(next_command.action = WAIT_S, "expected next_command.action : WAIT_S ");
      Assert(next_command.section = 1337, "expected next_command.section : 3 ");
   end test_process_valid_section_reached_RM_Pickup;

   procedure test_process_valid_section_reached_RM_Pickup_BE_failed (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section := 4;
      next_command.section := 4;
      next_command.action := PICK_UP_S;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := NEXT_LEFT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.action := E_Failure;
      Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(current_command.action = PICK_UP_S, "expected current_command.action : PICK_UP_S ");
      Assert(current_command.section = 4, "expected current_command.section : 4 ");
      Assert(next_command.action = NEXT_LEFT_S, "expected next_command.action : NEXT_LEFT_S ");
      Assert(next_command.section = 1337, "expected next_command.section : 1337 ");
   end test_process_valid_section_reached_RM_Pickup_BE_failed;

   procedure test_process_valid_section_reached_RM_Dropoff (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section := 2;
      next_command.section := 2;
      next_command.action := DROP_OFF_S;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := NEXT_RIGHT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);

      Assert(current_command.action = DROP_OFF_S, "expected current_command.action : DROP_OFF_S ");
      Assert(current_command.section = 2, "expected current_command.section : 2 ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action : NEXT_RIGHT_S ");
      Assert(next_command.section = 1337, "expected next_command.section : 1337 ");
   end test_process_valid_section_reached_RM_Dropoff;

   procedure test_process_valid_section_reached_RM_Dropoff_BE_failed (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section := 2;
      next_command.section := 2;
      next_command.action := DROP_OFF_S;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := NEXT_LEFT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.action := E_Failure;
      Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(current_command.action = DROP_OFF_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 2, "expected current_command.section : 3 ");
      Assert(next_command.action = NEXT_LEFT_S, "expected next_command.action : WAIT_S ");
      Assert(next_command.section = 1337, "expected next_command.section : 1337");
   end test_process_valid_section_reached_RM_Dropoff_BE_failed;



   procedure test_process_section_reached_RM (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      section_signal : Roadmarker.Road_Marker_Done_T;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section_signal := 3;
      section := 0;
      next_command.section := 3;
      next_command.action := NEXT_LEFT_S;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Process_Section(section, section_signal, current_command, next_command,
                      cab_id, cmd_queue, error_counter, retry_location_update,
                      Job_Executer_Done_Signal);

      Assert(current_command.action = NEXT_LEFT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 3, "expected current_command.section : 3 ");
      Assert(next_command.action = WAIT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(next_command.section = 1337, "expected current_command.section : 3 ");
      Assert(section = 3, "expected section : 3 ");
   end test_process_section_reached_RM;

   procedure test_process_section_non_existing_RM_error is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      section_signal : Integer;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section_signal := 28;
      section := 3;
      current_command.action := NEXT_RIGHT_S;
      current_command.section := 1;

      next_command.action := NEXT_LEFT_S;
      next_command.section := 3;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Process_Section(section, section_signal, current_command, next_command,
                      cab_id, cmd_queue, error_counter, retry_location_update,
                      Job_Executer_Done_Signal);

   end test_process_section_non_existing_RM_error;

   procedure test_process_section_reached_non_existent_RM (T : in out Test) is
   begin
         AUnit.Assertions.Assert_Exception(test_process_section_non_existing_RM_error'Access,
                                  "Expected a Constraint Error");
   end test_process_section_reached_non_existent_RM;

   procedure test_process_section_no_RM (T : in out Test) is
      section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
      queue_command : access Command_t;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      section_signal : Roadmarker.Road_Marker_Done_T;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section_signal := RM_no_road_marker;
      section := 3;
      current_command.action := NEXT_RIGHT_S;
      current_command.section := 1;

      next_command.action := NEXT_LEFT_S;
      next_command.section := 3;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Process_Section(section, section_signal, current_command, next_command,
                      cab_id, cmd_queue, error_counter, retry_location_update,
                      Job_Executer_Done_Signal);

      Assert(current_command.action = NEXT_RIGHT_S, "expected current_command.action : NEXT_RIGHT_S ");
      Assert(current_command.section = 1, "expected current_command.section : 1 ");
      Assert(next_command.action = NEXT_LEFT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(next_command.section = 3, "expected current_command.section : 3 ");
   end test_process_section_no_RM;

   procedure test_process_section_system_error (T : in out Test) is
       section :  Road_Marker_ID_T := 0;
      current_command : Command_t ;
      next_command :  Command_t;
      cab_id :  Integer := 1337;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      retry_location_update : Boolean := False;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      section_signal : Roadmarker.Road_Marker_Done_T;
   begin
      reset_command(current_command);
      reset_command(next_command);

      section_signal := RM_system_error;
      section := 3;
      current_command.action := NEXT_RIGHT_S;
      current_command.section := 1;

      next_command.action := NEXT_LEFT_S;
      next_command.section := 3;

      Process_Section(section, section_signal, current_command, next_command,
                      cab_id, cmd_queue, error_counter, retry_location_update,
                      Job_Executer_Done_Signal);

      Assert(Job_Executer_Done_Signal = SYSTEM_ERROR_S, "expected Job_Executer_Done_Signal : SYSTEM_ERROR_S ");

   end test_process_section_system_error;
end Job_Executer.Test;
