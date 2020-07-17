pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Roadmarker; use Roadmarker;
with ec2b; use ec2b;
with AWS.Messages; use AWS.Messages;
with Ada;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body Job_Executer is

   procedure Log_Line(Message : String) is
   begin
      Put_Line("[Job_Executer] " & Message);
   end Log_Line;

   type Road_Marker_ID_T is new Integer range 0 .. 15;

   function Reached_expected_roadmarker(section : Roadmarker.Road_Marker_Done_T;
                                        next_command : Command_t) return Boolean is
   begin
      return Integer(section) = next_command.section;

   end Reached_expected_roadmarker;

   procedure  register_new_cab(cab_name : in out Ada.Strings.Unbounded.Unbounded_String;
                               cab_id : in out Integer;
                               start_section: in Integer;
                               error_counter : in out Integer;
                               retry_register : in out Boolean;
                               Job_Executer_Done_Signal : in out Job_Executer_Done_T)
   is
   return_code : AWS.Messages.Status_Code;
   begin
      if retry_register then
         return_code := register_cab(To_String(cab_name), start_section, cab_id);
         if (not EC2B.success(return_code, error_counter)) then
            retry_register := True;
            Job_Executer_Done_Signal := STOP_S;
            Put_Line("Could not register at the Backend.");
            Put_Line("Error_code: " & return_code'Image);
            if (return_code = AWS.Messages.S408) then
               Put_Line("Connection to backend: Timeout");
            elsif (return_code = AWS.Messages.S409) then
               Put_Line("Did you enter a unique cab_name?");
            end if;
         else
            Put_Line("Successfully Registered");
            retry_register := False;
         end if;

      end if;
   end register_new_cab;

   procedure receive_section(Roadmarker_Task : Roadmarker_Task_T;
                             section : out Roadmarker.Road_Marker_Done_T ;
                             running : in out Boolean;
                             timeout : in Duration) is
   begin
      select
         delay timeout;
         Log_Line("road_marker_done timed out, shutting down...");
         running := False;
      then abort
         Roadmarker_Task.road_marker_done(section);
      end select;
   end receive_section;

   procedure debug_actions(current_command_old : in out Command_t;
                           current_command : in out Command_t;
                           next_command_old : in out Command_t;
                           next_command : in out Command_t) is
      begin
         if (current_command.action /= current_command_old.action) then
            Put_Line("Current Action: " & current_command.action'Image);
         end if;
         current_command_old := current_command;
         if (next_command.action /= next_command_old.action) then
            Put_Line("Next Action: " & Next_command.action'Image);
         end if;
         Next_command_old := Next_command;
   end debug_actions;

   procedure reset_command(command : in out Command_t) is
      begin
      command.action := NEXT_UNKOWN_S;
      command.customer_ID := -1;
      command.section := -1;
   end reset_command;



   procedure Process_valid_Section(section : in Roadmarker.Road_Marker_Done_T;
                             current_command : in out Command_t;
                             next_command : in out Command_t;
                             cab_id : in Integer;
                             cmd_queue : in out cmd_queue_access_t;
                             error_counter : in out Integer;
                             retry_location_update : in out Boolean
                            ) is
   return_code : AWS.Messages.Status_Code;
   begin
      Put_Line("In section" & section'Image);
      return_code := update_cabLocation(cab_id, section);
      if (not EC2B.success(return_code, error_counter)) then
         retry_location_update := True;
      end if;
      if (Reached_expected_roadmarker(section, next_command)) then
         Put_Line(section'Image & " = " & next_command.section'Image);
         current_command := next_command;
         if (current_command.action = PICK_UP_S) then
            -- Wont check the return code here, because if it failed it will
            -- be retried in the next iteration anyway.
            if (error_counter < errors_till_backend_failed) then
               return_code := request_pickup(cab_id, next_command.customer_ID);
            else
               Put_Line("Connection to backend failed, wont pickup");
            end if;
         elsif (current_command.action = DROP_OFF_S) then
            if (error_counter < errors_till_backend_failed) then
               return_code := request_dropoff(cab_id, next_command.customer_ID);
            else
               Put_Line("Connection to backend failed, wont Drop off");
            end if;
         end if;

         if (current_command.action /= WAIT_S) then
            cmd_queue.Dequeue(next_command);
            Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.section'Image);
         end if;
      end if;


   end Process_valid_Section;
   procedure Process_Section(section : in out Roadmarker.Road_Marker_Done_T;
                             section_signal : in Roadmarker.Road_Marker_Done_T;
                                current_command : in out Command_t;
                                next_command : in out Command_t;
                                cab_id : in Integer;
                                cmd_queue : in out cmd_queue_access_t;
                                error_counter : in out Integer;
                                retry_location_update : in out Boolean;
                                Job_Executer_Done_Signal: in out Job_Executer_Done_T
                               ) is
   begin
      if (section_signal in Road_Marker_valid_T) then
         section := section_signal;
         Process_valid_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);
         elsif (section = RM_no_road_marker) then
         null;
      elsif (section = RM_system_error) then
         Job_Executer_Done_Signal := SYSTEM_ERROR_S;
      end if;
   end Process_Section;
   procedure retry_update_cab_location(cab_id : Integer;
                                       section : Road_Marker_Done_T;
                                       retry_update_cab_location : in out Boolean;
                                      connection_errors : in out Integer) is
      return_code : AWS.Messages.Status_Code;
   begin
      if (retry_update_cab_location) then
         return_code := update_cabLocation(cab_id, section);
         if (EC2B.success(return_code, connection_errors)) then
            retry_update_cab_location := False;
         end if;
      end if;
   end retry_update_cab_location;

   procedure update_route(section : in Roadmarker.Road_Marker_Done_T;
                          current_command : in out Command_t;
                          next_command : in out Command_t;
                          cab_id : in Integer;
                          cmd_queue : in out cmd_queue_access_t;
                          error_counter : in out Integer;
                          retry_location_update : in out Boolean;
                          Job_Executer_Done_Signal: in out Job_Executer_Done_T;
                          cab_version : in out Integer) is
      return_code : AWS.Messages.Status_Code;
      cab_version_old : Integer := cab_version;
      begin
      return_code := request_route(cmd_queue, cab_id, cab_version);
      if (EC2B.success(return_code, error_counter)) then
         if (cab_version_old /= cab_version) then
            Put_Line("Received new Route\n");
            cmd_queue.Dequeue(next_command);
            Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.section'Image);
            reset_command(current_command);
            Process_valid_Section(section, current_command, next_command, cab_id,
                            cmd_queue, error_counter, retry_location_update);

         end if;
      end if;
   end update_route;

   procedure determine_done_signal(Job_Executer_Done_Signal : in out Job_Executer_Done_T;
                                   current_command : in out Command_t;
                                   next_command : in out Command_t;
                                   cmd_queue :  in out cmd_queue_access_t;
                                   error_counter : in out Integer;
                                   cab_id: in out Integer;
                                   pickup_completed : in out Boolean;
                                   dropoff_completed : in out Boolean) is
      return_code : AWS.Messages.Status_Code;
      begin
      while (Job_Executer_Done_Signal = EMPTY_S) loop
         case (current_command.action) is
         when NEXT_LEFT_S => Job_Executer_Done_Signal := NEXT_LEFT_S ;
         when NEXT_RIGHT_S => Job_Executer_Done_Signal := NEXT_RIGHT_S;
         when NEXT_UNKOWN_S => Job_Executer_Done_Signal := NEXT_UNKOWN_S;
         when WAIT_S => Job_Executer_Done_Signal := STOP_S;
         when STOP_S => Job_Executer_Done_Signal := STOP_S;
         when PICK_UP_S =>
            if (error_counter > errors_till_backend_failed) then
               Put_Line("Could not connect to the backend, stop pickups");
               current_command := next_command;
               cmd_queue.Dequeue(next_command);
               Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.section'Image);
            else

               return_code := pickup_complete(cab_id, pickup_completed);
               if (EC2B.success(return_code, error_counter) and pickup_completed ) then
                  if (current_command.section = next_command.section) then
                     current_command := next_command;
                     cmd_queue.Dequeue(next_command);
                     Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.section'Image);
                  else
                     current_command.action := NEXT_UNKOWN_S;
                  end if;

               else
                  -- Not checking return_code here, if it failed it will be retried
                  -- in the next iteration
                  return_code := request_pickup(cab_id, next_command.customer_ID);
                  Job_Executer_Done_Signal := STOP_S;
               end if;
            end if;
         when DROP_OFF_S =>
            if (error_counter > errors_till_backend_failed) then
               Put_Line("Could not connect to the backend, stop dropoff");
               current_command := next_command;
               cmd_queue.Dequeue(next_command);
               Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.section'Image);
            else
               return_code := dropoff_complete(cab_id, dropoff_completed);
               if(EC2B.success(return_code, error_counter) and dropoff_completed) then
                  if (current_command.section = next_command.section) then
                     current_command := next_command;
                     cmd_queue.Dequeue(next_command);
                     Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.section'Image);
                  else
                     current_command.action := NEXT_UNKOWN_S;
                  end if;
               else
                  -- Not checking return_code here, if it failed it will be retried
                  -- in the next iteration
                  return_code := request_dropoff(cab_id, next_command.customer_ID);
                  Job_Executer_Done_Signal := STOP_S;
               end if;
            end if;
         when EMPTY_S => Job_Executer_Done_Signal := SYSTEM_ERROR_S;
         when SYSTEM_ERROR_S => Job_Executer_Done_Signal := SYSTEM_ERROR_S;
         end case;
      end loop;
   end determine_done_signal;
   procedure send_done_Signal(Motor_Controller_Task : Motor_Controller_Task_Access_T;
                              Job_Executer_Done_Signal : Job_Executer_Done_T;
                              timeout : Duration;
                              running : in out Boolean ) is
      begin
      select
         delay timeout;
         Log_Line("job_executer_done timed out, shutting down...");
         running := False;
      then abort
         Motor_Controller_Task.job_executer_done(Signal => Job_Executer_Done_Signal);
      end select;
   end send_done_Signal;

   procedure receive_next_signal(Motor_Controller_Task   : Motor_Controller_Task_Access_T;
                                 Job_Executer_Next_Signal : in out Job_Executer_Next_t;
                                 timeout : Duration;
                                 running : in out Boolean;
                                 error_counter : in out Integer;
                                 RM_next : out Road_Marker_Next_T;
                                 cab_id : Integer) is
      return_code : AWS.Messages.Status_Code;
      ignore_success : Boolean; -- Stay grounded
   begin
      select
         delay timeout;
         Log_Line("job_executer_next timed out, shutting down...");
         running := False;
      then abort
         Motor_Controller_Task.job_executer_next(Signal => Job_Executer_Next_Signal);
         case Job_Executer_Next_Signal is
            when SHUTDOWN_S =>
               RM_next := SHUTDOWN_S;
            when EMPTY_S =>
               RM_next := EMPTY_S;
               return_code := set_blocked_status(cab_id  => cab_id, blocked => False);
               ignore_success := EC2B.success(return_code, error_counter);

            when BLOCKED_S =>
               return_code := set_blocked_status(cab_id  => cab_id, blocked => True);
               ignore_success := EC2B.success(return_code, error_counter);
            when NOT_FUNCTIONAL =>
               null; -- TODO
         end case;
      end select;
   end receive_next_signal;

   procedure send_RM_next_signal(Roadmarker_Task : Roadmarker_Task_T;
                                 RM_next : Road_Marker_Next_T;
                                 timeout : Duration;
                                 running : in out Boolean
                                ) is
   begin
      select
         delay timeout;
         Log_Line("road_marker_next timed out, shutting down...");
         running := False;
      then abort
         Roadmarker_Task.road_marker_next(RM_next);
      end select;
   end send_RM_next_signal;

   task body Job_Executer_Task_T is
      Motor_Controller_Task   : Motor_Controller_Task_Access_T;
      timeout                 : Duration;
      running                 : Boolean := True;
      Job_Executer_Done_Signal: Job_Executer_Done_T := EMPTY_S;
      Job_Executer_Next_Signal: Job_Executer_Next_T := EMPTY_S;
      Roadmarker_Task         : Roadmarker_Task_T;
      RM_get_sensor_value     : get_roadmarker_sensor_value_access;
      RM_next                 : Roadmarker.Road_Marker_Next_T;
      section                 : Roadmarker.Road_Marker_Done_T;
      section_signal          : Roadmarker.Road_Marker_Done_T;
      rounds                  : Integer := 0;
      section_old             : Roadmarker.Road_Marker_Done_T := 17;
      cab_id                  : Integer := -1;
      cmd_queue               : cmd_queue_access_t;
      next_command            : Command_t;
      current_command         : Command_t;
      error_counter           : Integer := 0;
      retry_location_update   : Boolean := False;
      cab_version             : Integer := -1;
      cab_version_old         : Integer := -1;
      return_code             : AWS.Messages.Status_Code;
      cab_name                : Ada.Strings.Unbounded.Unbounded_String;
      start_section           : Integer := 0;
      retry_register          : Boolean := True;
      pickup_completed        : Boolean := False;
      dropoff_completed       : Boolean := False;
      backend_failed          : Boolean := False;
      next_command_old        : Command_t;
      current_command_old     : Command_t;
      ignored_boolean         : Boolean;

   begin
      reset_command(current_command);
      reset_command(next_command);
      reset_command(current_command_old);
      reset_command(next_command_old);

      Log_Line("Starting Module");
      Log_Line("Waiting for Constructor call..");
      accept Constructor
        (Motor_Controller_Task_A : in Motor_Controller_Task_Access_T;
         timeout_v               : in Duration;
         RM_get_sensor_value_a   : in get_roadmarker_sensor_value_access;
         cab_name_arg            : in Ada.Strings.Unbounded.Unbounded_String;
         start_section_arg       : in Integer
        )
      do
         Motor_Controller_Task := Motor_Controller_Task_A;
         timeout               := timeout_v;
         RM_get_sensor_value   := RM_get_sensor_value_a;
         Roadmarker_Task.Construct(get_sensor_value_a => RM_get_sensor_value, timeout_v => timeout, MC_Task => Motor_Controller_Task);
         cab_name := cab_name_arg;
         start_section := start_section_arg;
         section := start_section;
      end Constructor;
      Log_Line("Constructor done!");


      register_new_cab(cab_name, cab_id, start_section, error_counter, retry_register, Job_Executer_Done_Signal);

      -- main loop
      while running loop
         -- Reset Job_Executer_Done
         Job_Executer_Done_Signal := EMPTY_S;

         -- Retry register if necessary
         register_new_cab(cab_name, cab_id, start_section, error_counter, retry_register, Job_Executer_Done_Signal);

         debug_actions(current_command_old, current_command, next_command_old, next_command);

         receive_section(Roadmarker_Task, section_signal, running, timeout);

         retry_update_cab_location(cab_id, section,  retry_location_update, error_counter);

         Process_Section(section, section_signal, current_command, next_command, cab_id,
                         cmd_queue, error_counter, retry_location_update,
                           Job_Executer_Done_Signal);

         update_route(section, current_command, next_command, cab_id, cmd_queue,
                      error_counter, retry_location_update, Job_Executer_Done_Signal, cab_version);


         determine_done_signal(Job_Executer_Done_Signal, current_command, next_command, cmd_queue, error_counter, cab_id, pickup_completed, dropoff_completed);

         send_done_Signal(Motor_Controller_Task, Job_Executer_Done_Signal, timeout, running);

         receive_next_signal(Motor_Controller_Task, Job_Executer_Next_Signal, timeout, running, error_counter, RM_next, cab_id);

         send_RM_next_signal(Roadmarker_Task, RM_next, timeout, running);

         return_code := update_sensor_manipulation(cab_id);
         ignored_boolean := EC2B.success(return_code, error_counter);
      end loop;


   end Job_Executer_Task_T;

end Job_Executer;
