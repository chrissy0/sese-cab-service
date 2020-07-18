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

   procedure test_Reached_expected_roadmarker_constraint_error is
      next_command : Command_t;
      return_value : Boolean;
      section : Integer := Road_Marker_ID_T'Last +1;
   begin
      next_command.section := section;
      return_value := Reached_expected_roadmarker(section, next_command);
   end test_Reached_expected_roadmarker_constraint_error;

   procedure test_Reached_expected_roadmarker_wrong_section (T : in out Test) is
   begin
      AUnit.Assertions.Assert_Exception(test_Reached_expected_roadmarker_constraint_error'Access,
                                        "Expected a Constraint Error if section > Road_Marker_ID_T'Last");
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

   procedure test_process_section_update_section_success (T : in out Test) is
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
      section_signal := 3;
      Process_Section(section, section_signal, current_command, next_command,
                      cab_id, cmd_queue, error_counter, retry_location_update,
                      Job_Executer_Done_Signal);
      Assert(retry_location_update = False, "expected retry_location_update : False ");
      Assert(error_counter = 0, "expected error_counter : 0 ");
   end test_process_section_update_section_success;

   procedure test_process_section_update_section_failure (T : in out Test) is
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
      section_signal := 3;
      Job_Executer_Testing.Callbacks.action := E_Failure;
      Process_Section(section, section_signal, current_command, next_command,
                      cab_id, cmd_queue, error_counter, retry_location_update,
                      Job_Executer_Done_Signal);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(retry_location_update = True, "expected retry_location_update : True ");
      Assert(error_counter = 1, "expected error_counter : 1 ");
   end test_process_section_update_section_failure;

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
      Assert(next_command.action = WAIT_S, "expected next_command.action : NEXT_LEFT_S ");
      Assert(next_command.section = 1337, "expected next_command.section : 3 ");
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
      next_command.section := 4;

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
      Assert(next_command.action = NEXT_LEFT_S, "expected next_command.action : NEXT_LEFT_S ");
      Assert(next_command.section = 4, "expected next_command.section : 3 ");
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

   procedure test_update_route_success_not_in_section (T : in out Test) is
      section :  Roadmarker.Road_Marker_Done_T := 3;
      current_command : Command_t;
      next_command : Command_t;
      cab_id : Integer := 0;
      cmd_queue : cmd_queue_access_t;
      error_counter : Integer := 0;
      retry_location_update : Boolean := False;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      cab_version : Integer := 0;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_LEFT_S;
      current_command.section := 3;
      update_route(section, current_command, next_command, cab_id, cmd_queue,
                   error_counter, retry_location_update, Job_Executer_Done_Signal, cab_version);

      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action : NEXT_UNKOWN_S ");
      Assert(current_command.section = -1, "expected current_command.section : -1 ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action : NEXT_RIGHT_S ");
      Assert(next_command.section = 1, "expected next_command.section : 1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = PICK_UP_S, "expected 0. Dequeue current_command.action : PICK_UP_S ");
      Assert(current_command.section = 2, "expected 0. Dequeue current_command.section : 2 ");
      Assert(current_command.customer_ID = 0, "expected 0. Dequeue current_command.customer_ID : 0 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = PICK_UP_S, "expected 1. Dequeue current_command.action : PICK_UP_S ");
      Assert(current_command.section = 2, "expected 1. Dequeue current_command.section : 2");
      Assert(current_command.customer_ID = 1, "expected 1. Dequeue current_command.customer_ID : 1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_RIGHT_S, "expected 2. Dequeue current_command.action : NEXT_RIGHT_S ");
      Assert(current_command.section = 4, "expected 2. Dequeue current_command.section : 4 ");
      Assert(current_command.customer_ID = -1, "expected 2. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = DROP_OFF_S, "expected 3. Dequeue current_command.action : DROP_OFF_S ");
      Assert(current_command.section = 5, "expected 3. Dequeue current_command.section : 5");
      Assert(current_command.customer_ID = 0, "expected 3. Dequeue current_command.customer_ID : 0 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = PICK_UP_S, "expected 4. Dequeue current_command.action : PICK_UP_S ");
      Assert(current_command.section = 5, "expected 4. Dequeue current_command.section : 5");
      Assert(current_command.customer_ID = 2, "expected 4. Dequeue current_command.customer_ID : 2 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_RIGHT_S, "expected 5. Dequeue current_command.action : NEXT_RIGHT_S ");
      Assert(current_command.section = 7, "expected 5. Dequeue current_command.section : 7 ");
      Assert(current_command.customer_ID = -1, "expected 5. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = DROP_OFF_S, "expected 6. Dequeue current_command.action : DROP_OFF_S ");
      Assert(current_command.section = 8, "expected 6. Dequeue current_command.section : 8");
      Assert(current_command.customer_ID = 1, "expected 6. Dequeue current_command.customer_ID : 1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = DROP_OFF_S, "expected 7. Dequeue current_command.action : DROP_OFF_S ");
      Assert(current_command.section = 8, "expected 7. Dequeue current_command.section : 8");
      Assert(current_command.customer_ID = 2, "expected 7. Dequeue current_command.customer_ID : 2 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_LEFT_S, "expected 8. Dequeue current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 10, "expected 8. Dequeue current_command.section : 10 ");
      Assert(current_command.customer_ID = -1, "expected 8. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_LEFT_S, "expected 9. Dequeue current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 12, "expected 9. Dequeue current_command.section : 12 ");
      Assert(current_command.customer_ID = -1, "expected 9. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_RIGHT_S, "expected 10. Dequeue current_command.action : NEXT_RIGHT_S ");
      Assert(current_command.section = 14, "expected 10. Dequeue current_command.section : 14 ");
      Assert(current_command.customer_ID = -1, "expected 10. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = WAIT_S, "expected 10. Dequeue current_command.action : WAIT_S ");
      Assert(current_command.section = 0, "expected 10. Dequeue current_command.section : 0 ");
      Assert(current_command.customer_ID = -1, "expected 10. Dequeue current_command.customer_ID : -1 ");
   end test_update_route_success_not_in_section;

   procedure test_update_route_success_in_section (T : in out Test) is
      section :  Roadmarker.Road_Marker_Done_T := 1;
      current_command : Command_t;
      next_command : Command_t;
      cab_id : Integer := 0;
      cmd_queue : cmd_queue_access_t;
      error_counter : Integer := 0;
      retry_location_update : Boolean := False;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      cab_version : Integer := 0;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_LEFT_S;
      current_command.section := 3;
      update_route(section, current_command, next_command, cab_id, cmd_queue,
                   error_counter, retry_location_update, Job_Executer_Done_Signal, cab_version);

      Assert(current_command.action = NEXT_RIGHT_S, "expected current_command.action : NEXT_RIGHT_S ");
      Assert(current_command.section = 1, "expected current_command.section : 1 ");
      Assert(next_command.action = PICK_UP_S, "expected next_command.action : PICK_UP_S ");
      Assert(next_command.section = 2, "expected next_command.section : 2 ");
      Assert(next_command.customer_ID = 0, "expected next_command.section : 0 ");
      Assert(cab_version = 1,  "expected Version : 1 ");

      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = PICK_UP_S, "expected 1. Dequeue current_command.action : PICK_UP_S ");
      Assert(current_command.section = 2, "expected 1. Dequeue current_command.section : 2 ");
      Assert(current_command.customer_ID = 1, "expected 1. Dequeue current_command.customer_ID : 1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_RIGHT_S, "expected 2. Dequeue current_command.action : NEXT_RIGHT_S ");
      Assert(current_command.section = 4, "expected 2. Dequeue current_command.section : 4 ");
      Assert(current_command.customer_ID = -1, "expected 2. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = DROP_OFF_S, "expected 3. Dequeue current_command.action : DROP_OFF_S ");
      Assert(current_command.section = 5, "expected 3. Dequeue current_command.section : 5");
      Assert(current_command.customer_ID = 0, "expected 3. Dequeue current_command.customer_ID : 0 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = PICK_UP_S, "expected 4. Dequeue current_command.action : PICK_UP_S ");
      Assert(current_command.section = 5, "expected 4. Dequeue current_command.section : 5");
      Assert(current_command.customer_ID = 2, "expected 4. Dequeue current_command.customer_ID : 2 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_RIGHT_S, "expected 5. Dequeue current_command.action : NEXT_RIGHT_S ");
      Assert(current_command.section = 7, "expected 5. Dequeue current_command.section : 7 ");
      Assert(current_command.customer_ID = -1, "expected 5. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = DROP_OFF_S, "expected 6. Dequeue current_command.action : DROP_OFF_S ");
      Assert(current_command.section = 8, "expected 6. Dequeue current_command.section : 8");
      Assert(current_command.customer_ID = 1, "expected 6. Dequeue current_command.customer_ID : 1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = DROP_OFF_S, "expected 7. Dequeue current_command.action : DROP_OFF_S ");
      Assert(current_command.section = 8, "expected 7. Dequeue current_command.section : 8");
      Assert(current_command.customer_ID = 2, "expected 7. Dequeue current_command.customer_ID : 2 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_LEFT_S, "expected 8. Dequeue current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 10, "expected 8. Dequeue current_command.section : 10 ");
      Assert(current_command.customer_ID = -1, "expected 8. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_LEFT_S, "expected 9. Dequeue current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 12, "expected 9. Dequeue current_command.section : 12 ");
      Assert(current_command.customer_ID = -1, "expected 9. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = NEXT_RIGHT_S, "expected 10. Dequeue current_command.action : NEXT_RIGHT_S ");
      Assert(current_command.section = 14, "expected 10. Dequeue current_command.section : 14 ");
      Assert(current_command.customer_ID = -1, "expected 10. Dequeue current_command.customer_ID : -1 ");
      cmd_queue.Dequeue(current_command);
      Assert(current_command.action = WAIT_S, "expected 10. Dequeue current_command.action : WAIT_S ");
      Assert(current_command.section = 0, "expected 10. Dequeue current_command.section : 0 ");
      Assert(current_command.customer_ID = -1, "expected 10. Dequeue current_command.customer_ID : -1 ");
   end test_update_route_success_in_section;


   procedure test_update_route_failure (T : in out Test) is
      section :  Roadmarker.Road_Marker_Done_T := 3;
      current_command : Command_t;
      next_command : Command_t;
      cab_id : Integer := 0;
      cmd_queue : cmd_queue_access_t;
      error_counter : Integer := 0;
      retry_location_update : Boolean := False;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      cab_version : Integer := 0;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_LEFT_S;
      current_command.section := 3;
      next_command.action := NEXT_LEFT_S;
      next_command.section := 5;
      Job_Executer_Testing.Callbacks.action := E_Failure;
      update_route(section, current_command, next_command, cab_id, cmd_queue,
                   error_counter, retry_location_update, Job_Executer_Done_Signal, cab_version);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(current_command.action = NEXT_LEFT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 3, "expected current_command.section : 3");
      Assert(next_command.action = NEXT_LEFT_S, "expected next_command.action : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section : 1 ");
      Assert(error_counter = 1, "expected error_counter  : 1 ");
   end test_update_route_failure;

   procedure test_update_route_incomplete_JSON (T : in out Test) is
      section :  Roadmarker.Road_Marker_Done_T := 3;
      current_command : Command_t;
      next_command : Command_t;
      cab_id : Integer := 0;
      cmd_queue : cmd_queue_access_t;
      error_counter : Integer := 0;
      retry_location_update : Boolean := False;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      cab_version : Integer := 0;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_LEFT_S;
      current_command.section := 3;
      next_command.action := NEXT_LEFT_S;
      next_command.section := 5;
      Job_Executer_Testing.Callbacks.action := E_incomplete_JSON;
      update_route(section, current_command, next_command, cab_id, cmd_queue,
                   error_counter, retry_location_update, Job_Executer_Done_Signal, cab_version);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(current_command.action = NEXT_LEFT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 3, "expected current_command.section : 3");
      Assert(next_command.action = NEXT_LEFT_S, "expected next_command.action : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section : 1 ");
      Assert(error_counter = 1, "expected error_counter  : 1 ");
   end test_update_route_incomplete_JSON;

   procedure test_update_route_wrong_JSON (T : in out Test) is
      section :  Roadmarker.Road_Marker_Done_T := 3;
      current_command : Command_t;
      next_command : Command_t;
      cab_id : Integer := 0;
      cmd_queue : cmd_queue_access_t;
      error_counter : Integer := 0;
      retry_location_update : Boolean := False;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      cab_version : Integer := 0;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_LEFT_S;
      current_command.section := 3;
      next_command.action := NEXT_LEFT_S;
      next_command.section := 5;
      Job_Executer_Testing.Callbacks.action := E_wrong_JSON;

      update_route(section, current_command, next_command, cab_id, cmd_queue,
                   error_counter, retry_location_update, Job_Executer_Done_Signal, cab_version);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(current_command.action = NEXT_LEFT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 3, "expected current_command.section : 3");
      Assert(next_command.action = NEXT_LEFT_S, "expected next_command.action : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section : 1 ");
      Assert(error_counter = 1, "expected error_counter  : 1 ");
   end test_update_route_wrong_JSON;

   procedure test_update_route_timeout (T : in out Test) is
      section :  Roadmarker.Road_Marker_Done_T := 3;
      current_command : Command_t;
      next_command : Command_t;
      cab_id : Integer := 0;
      cmd_queue : cmd_queue_access_t;
      error_counter : Integer := 0;
      retry_location_update : Boolean := False;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      cab_version : Integer := 0;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_LEFT_S;
      current_command.section := 3;
      next_command.action := NEXT_LEFT_S;
      next_command.section := 5;
      Job_Executer_Testing.Callbacks.action := E_Timeout;

      update_route(section, current_command, next_command, cab_id, cmd_queue,
                   error_counter, retry_location_update, Job_Executer_Done_Signal, cab_version);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(current_command.action = NEXT_LEFT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 3, "expected current_command.section : 3");
      Assert(next_command.action = NEXT_LEFT_S, "expected next_command.action : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section : 1 ");
      Assert(error_counter = 1, "expected error_counter  : 1 ");
   end test_update_route_timeout;

   procedure test_update_route_same_version(T : in out Test) is
      section :  Roadmarker.Road_Marker_Done_T := 3;
      current_command : Command_t;
      next_command : Command_t;
      cab_id : Integer := 0;
      cmd_queue : cmd_queue_access_t;
      error_counter : Integer := 0;
      retry_location_update : Boolean := False;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      cab_version : Integer := 1;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_LEFT_S;
      current_command.section := 3;
      next_command.action := NEXT_LEFT_S;
      next_command.section := 5;

      update_route(section, current_command, next_command, cab_id, cmd_queue,
                   error_counter, retry_location_update, Job_Executer_Done_Signal, cab_version);

      Assert(current_command.action = NEXT_LEFT_S, "expected current_command.action : NEXT_LEFT_S ");
      Assert(current_command.section = 3, "expected current_command.section : 3");
      Assert(next_command.action = NEXT_LEFT_S, "expected next_command.action : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section : 1 ");
      Assert(error_counter = 0, "expected error_counter  : 0 ");
   end test_update_route_same_version;

   procedure test_determine_done_signal_LEFT(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_LEFT_S;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal = NEXT_LEFT_S, "expected Job_Executer_Done_Signal  : NEXT_LEFT_S ");
   end test_determine_done_signal_LEFT;

   procedure test_determine_done_signal_RIGHT(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_RIGHT_S;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal = NEXT_RIGHT_S, "expected Job_Executer_Done_Signal  : NEXT_RIGHT_S ");
   end test_determine_done_signal_RIGHT;

   procedure test_determine_done_signal_UNKNOWN(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := NEXT_UNKOWN_S;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal = NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
   end test_determine_done_signal_UNKNOWN;

   procedure test_determine_done_signal_WAIT(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := WAIT_S;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal = STOP_S, "expected Job_Executer_Done_Signal  : STOP_S ");
   end test_determine_done_signal_WAIT;


   procedure test_determine_done_signal_STOP(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := STOP_S;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal = STOP_S, "expected Job_Executer_Done_Signal  : STOP_S ");
   end test_determine_done_signal_STOP;

   procedure test_determine_done_signal_PICKUP_instant_success(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := PICK_UP_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      Job_Executer_Testing.Callbacks.completed := True;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal = NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_PICKUP_instant_success;

   procedure test_determine_done_signal_double_PICKUP_instant_success(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
      queue_command : access Command_t;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := PICK_UP_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := PICK_UP_S;
      next_command.section := 3;
      next_command.customer_ID := 1;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);
      Job_Executer_Testing.Callbacks.completed := True;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal = NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = WAIT_S, "expected next_command.action  : WAIT_S ");
      Assert(next_command.section = 1337, "expected next_command.section  : 1337 ");
   end test_determine_done_signal_double_PICKUP_instant_success;

   procedure test_determine_done_signal_PICKUP_failure(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := PICK_UP_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := True;
      Job_Executer_Testing.Callbacks.action := E_Failure;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_PICKUP_failure;

   procedure test_determine_done_signal_PICKUP_incomplete_JSON(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := PICK_UP_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := True;
      Job_Executer_Testing.Callbacks.action := E_incomplete_JSON;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_PICKUP_incomplete_JSON;

   procedure test_determine_done_signal_PICKUP_wrong_JSON(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := PICK_UP_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := True;
      Job_Executer_Testing.Callbacks.action := E_wrong_JSON;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_PICKUP_wrong_JSON;

   procedure test_determine_done_signal_PICKUP_timeout(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := PICK_UP_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := True;
      Job_Executer_Testing.Callbacks.action := E_Timeout;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_PICKUP_timeout;

   procedure test_determine_done_signal_PICKUP_late_failure(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);
      current_command.action := PICK_UP_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := False;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Failure;
      error_counter := errors_till_backend_failed;
      Job_Executer_Done_Signal := EMPTY_S;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(current_command.section = 3, "expected next_command.section  : 5 ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : WAIT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 1337 ");

   end test_determine_done_signal_PICKUP_late_failure;

   procedure test_determine_done_signal_PICKUP_not_completed(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := PICK_UP_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := False;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal =  STOP_S, "expected Job_Executer_Done_Signal  : PICK_UP_S ");
      Assert(current_command.action = PICK_UP_S, "expected current_command.action  : PICK_UP_S ");
      Assert(current_command.section = 3, "expected next_command.section  : 3 ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_PICKUP_not_completed;



   procedure test_determine_done_signal_DROPOFF_instant_success(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := DROP_OFF_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      Job_Executer_Testing.Callbacks.completed := True;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal = NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_DROPOFF_instant_success;

   procedure test_determine_done_signal_double_DROPOFF_instant_success(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
      queue_command : access Command_t;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := DROP_OFF_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := DROP_OFF_S;
      next_command.section := 3;
      next_command.customer_ID := 1;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);
      Job_Executer_Testing.Callbacks.completed := True;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal = NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = WAIT_S, "expected next_command.action  : WAIT_S ");
      Assert(next_command.section = 1337, "expected next_command.section  : 1337 ");
   end test_determine_done_signal_double_DROPOFF_instant_success;

   procedure test_determine_done_signal_DROPOFF_failure(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := DROP_OFF_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := True;
      Job_Executer_Testing.Callbacks.action := E_Failure;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_DROPOFF_failure;

   procedure test_determine_done_signal_DROPOFF_incomplete_JSON(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := DROP_OFF_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := True;
      Job_Executer_Testing.Callbacks.action := E_incomplete_JSON;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_DROPOFF_incomplete_JSON;


   procedure test_determine_done_signal_DROPOFF_wrong_JSON(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := DROP_OFF_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := True;
      Job_Executer_Testing.Callbacks.action := E_wrong_JSON;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_DROPOFF_wrong_JSON;

   procedure test_determine_done_signal_DROPOFF_timeout(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := errors_till_backend_failed;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := DROP_OFF_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := True;
      Job_Executer_Testing.Callbacks.action := E_Timeout;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;
      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_DROPOFF_timeout;


   procedure test_determine_done_signal_DROPOFF_late_failure(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);
      current_command.action := DROP_OFF_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := False;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Failure;
      error_counter := errors_till_backend_failed;
      Job_Executer_Done_Signal := EMPTY_S;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);
      Job_Executer_Testing.Callbacks.action := E_Success;

      Assert(Job_Executer_Done_Signal =  NEXT_UNKOWN_S, "expected Job_Executer_Done_Signal  : NEXT_UNKOWN_S ");
      Assert(current_command.action = NEXT_UNKOWN_S, "expected current_command.action  : NEXT_UNKOWN_S ");
      Assert(current_command.section = 3, "expected next_command.section  : 5 ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : WAIT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 1337 ");

   end test_determine_done_signal_DROPOFF_late_failure;

   procedure test_determine_done_signal_DROPOFF_not_completed(T : in out Test) is
      Job_Executer_Done_Signal : Job_Executer_Done_T := EMPTY_S;
      current_command : Command_t;
      next_command : Command_t;
      queue_command : access Command_t;
      cmd_queue :  cmd_queue_access_t;
      error_counter :  Integer := 0;
      cab_id:  Integer := 1337;
      pickup_completed :  Boolean := False;
      dropoff_completed : Boolean := False;
   begin
      reset_command(current_command);
      reset_command(next_command);

      current_command.action := DROP_OFF_S;
      current_command.section := 3;
      current_command.customer_ID := 0;

      next_command.action := NEXT_RIGHT_S;
      next_command.section := 5;

      cmd_queue := new Cmd_Queue_p.Queue;
      queue_command := new Command_t;
      queue_command.section := 1337;
      queue_command.action := WAIT_S;
      queue_command.customer_ID := -1;
      cmd_queue.Enqueue(queue_command.all);

      Job_Executer_Testing.Callbacks.completed := False;
      determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

      Assert(Job_Executer_Done_Signal =  STOP_S, "expected Job_Executer_Done_Signal  : PICK_UP_S ");
      Assert(current_command.action = DROP_OFF_S, "expected current_command.action  : PICK_UP_S ");
      Assert(current_command.section = 3, "expected next_command.section  : 3 ");
      Assert(next_command.action = NEXT_RIGHT_S, "expected next_command.action  : NEXT_RIGHT_S ");
      Assert(next_command.section = 5, "expected next_command.section  : 5 ");
   end test_determine_done_signal_DROPOFF_not_completed;
end Job_Executer.Test;


