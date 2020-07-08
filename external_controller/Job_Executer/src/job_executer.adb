pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Roadmarker; use Roadmarker;
with ec2b; use ec2b;
with AWS.Messages;
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
   procedure Process_Section(section : Roadmarker.Road_Marker_Done_T;
                             current_command : in out Command_t;
                             next_command : in out Command_t;
                             cab_id : Integer;
                             cmd_queue : in out cmd_queue_access_t;
                             error_counter : in out Integer;
                             retry_location_update : in out Boolean
                            ) is
   return_code : AWS.Messages.Status_Code;
   begin

      if (section = RM_system_error) then
         -- TODO
         null;
         return;
      end if;

      if (section = RM_no_road_marker) then
         -- TODO Probably just return?
         null;
         return;
      end if;
      Put_Line("In section" & section'Image);
      return_code := update_cabLocation(cab_id, section);
      if (not success(return_code, error_counter)) then
         retry_location_update := True;
      end if;
      if (Reached_expected_roadmarker(section, next_command)) then
         Put_Line(section'Image & " = " & next_command.section'Image);
         current_command := next_command;
         if (current_command.action = PICK_UP_S) then
            -- Wont check the return code here, because if it failed it will
            -- be retried in the next iteration anyway.
            return_code := request_pickup(cab_id, next_command.customer_ID);
         elsif (current_command.action = DROP_OFF_S) then
               return_code := request_dropoff(cab_id, next_command.customer_ID);
         end if;

         if (current_command.action /= WAIT_S) then
            cmd_queue.Dequeue(next_command);
            Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.section'Image);
         end if;
      else
         -- TODO
         null;
      end if;


   end Process_Section;

   procedure retry_update_cab_location(cab_id : Integer;
                                       section : Road_Marker_Done_T;
                                       retry_update_cab_location : in out Boolean;
                                      connection_errors : in out Integer) is
      return_code : AWS.Messages.Status_Code;
   begin
      return_code := update_cabLocation(cab_id, section);
      if (success(return_code, connection_errors)) then
         retry_update_cab_location := False;
      else
         retry_update_cab_location := True;
      end if;
   end retry_update_cab_location;
   ---------------------------
   -- Front_Distance_Task_T --
   ---------------------------



   task body Job_Executer_Task_T is
      Motor_Controller_Task   : Motor_Controller_Task_Access_T;
      timeout                 : Duration;
      running                 : Boolean := True;
      Job_Executer_Done_Signal: Job_Executer_Done_T;
      Job_Executer_Next_Signal: Job_Executer_Next_T;
      Roadmarker_Task         : Roadmarker_Task_T;
      RM_get_sensor_value     : get_roadmarker_sensor_value_access;
      RM_next                 : Roadmarker.Road_Marker_Next_T;
      section                 : Roadmarker.Road_Marker_Done_T;
      rounds                  : Integer := 0;
      section_old             : Roadmarker.Road_Marker_Done_T := 17;
      cab_id                  : Integer;
      cmd_queue               : cmd_queue_access_t;
      next_command            : Command_t;
      current_command         : Command_t;
      error_counter           : Integer := 0;
      retry_location_update   : Boolean := False;
      cab_version             : Integer := -1;
      cab_version_old         : Integer := -1;
      return_code             : AWS.Messages.Status_Code;
      cab_name                : String := "Dieter3";   -- TODO Move into constructor
      start_section           : Integer := 0;         -- TODO Move into constructor
      retry_register          : Boolean := False;
      pickup_completed        : Boolean;
      dropoff_completed       : Boolean;
      next_command_old            : Command_t;
      current_command_old         : Command_t;

   begin
      -- TODO INIT Correctly

      current_command.action := NEXT_UNKOWN_S;
      current_command.customer_ID := 0;
      current_command.section := 99;
      next_command.action := NEXT_UNKOWN_S;
      next_command.customer_ID := 0;
      next_command.section := 99;
      -- END TODO
      Log_Line("Starting Module");
      Log_Line("Waiting for Constructor call..");
      accept Constructor
        (Motor_Controller_Task_A : in Motor_Controller_Task_Access_T;
         timeout_v               : in Duration;
         RM_get_sensor_value_a   : in get_roadmarker_sensor_value_access
        )
      do
         null;
         Motor_Controller_Task := Motor_Controller_Task_A;
         timeout               := timeout_v;
         RM_get_sensor_value   := RM_get_sensor_value_a;
         Roadmarker_Task.Construct(get_sensor_value_a => RM_get_sensor_value, timeout_v => timeout, MC_Task => Motor_Controller_Task);
      end Constructor;
      Log_Line("Constructor done!");

      return_code := register_cab(cab_name, start_section, cab_id);
      if (not success(return_code, error_counter)) then
         retry_register := True;
      end if;

      -- Todo for testing only
      if retry_register then
         cab_id := 1;
         retry_register := False;
      end if;

      Put_Line("Register cab returned: " & cab_id'Image);
      return_code := update_cabLocation(cab_id, 1); -- TODO just for testing
      return_code := request_route(cmd_queue, cab_id, cab_version);
      if (success(return_code, error_counter)) then
         cmd_queue.Dequeue(next_command);
         Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.section'Image);
      end if;


      -- main loop
      while running loop
         if (current_command.action /= current_command_old.action) then
            Put_Line("Current Action: " & current_command.action'Image);
         end if;
         current_command_old := current_command;
         if (next_command.action /= next_command_old.action) then
            Put_Line("Next Action: " & Next_command.action'Image);
         end if;
         Next_command_old := Next_command;

         select
            delay timeout;
            Log_Line("road_marker_done timed out, shutting down...");
            running := False;
         then abort
           Roadmarker_Task.road_marker_done(section);
         end select;

         if (retry_location_update) then
            retry_update_cab_location(cab_id, section, retry_location_update, error_counter);
         end if;

         Process_Section(section, current_command, next_command, cab_id, cmd_queue, error_counter, retry_location_update);

         cab_version_old := cab_version;
         return_code := request_route(cmd_queue, cab_id, cab_version);
         if (success(return_code, error_counter)) then
            if (cab_version_old /= cab_version) then
               Put_Line("Received new Route\n");
               cmd_queue.Dequeue(next_command);
               current_command.action := NEXT_UNKOWN_S; -- TODO Init correct.
            end if;
         end if;

         Job_Executer_Done_Signal := EMPTY_S;
         while (Job_Executer_Done_Signal = EMPTY_S) loop
            case (current_command.action) is
            when NEXT_LEFT_S => Job_Executer_Done_Signal := NEXT_LEFT_S ;
            when NEXT_RIGHT_S => Job_Executer_Done_Signal := NEXT_RIGHT_S;
            when NEXT_UNKOWN_S => Job_Executer_Done_Signal := NEXT_UNKOWN_S;
            when WAIT_S => Job_Executer_Done_Signal := STOP_S;
            when STOP_S => Job_Executer_Done_Signal := STOP_S;
            when PICK_UP_S =>
               return_code := pickup_complete(cab_id, pickup_completed);
               if (success(return_code, error_counter) and pickup_completed ) then
                  current_command.action := NEXT_UNKOWN_S;
               else
                  -- Not checking return_code here, if it failed it will be retried
                  -- in the next iteration
                  return_code := request_pickup(cab_id, next_command.customer_ID);
                  Job_Executer_Done_Signal := STOP_S;
               end if;
            when DROP_OFF_S =>
               return_code := dropoff_complete(cab_id, dropoff_completed);
               if(success(return_code, error_counter) and dropoff_completed) then
                  current_command.action := NEXT_UNKOWN_S;
               else
                  -- Not checking return_code here, if it failed it will be retried
                  -- in the next iteration
                  return_code := request_dropoff(cab_id, next_command.customer_ID);
                  Job_Executer_Done_Signal := STOP_S;
               end if;

            when EMPTY_S => Job_Executer_Done_Signal := SYSTEM_ERROR_S;
            when SYSTEM_ERROR_S => Job_Executer_Done_Signal := SYSTEM_ERROR_S;
            end case;
         end loop;

         select
            delay timeout;
            Log_Line("job_executer_done timed out, shutting down...");
            running := False;
         then abort
           Motor_Controller_Task.job_executer_done(Signal => Job_Executer_Done_Signal);
         end select;

         select
            delay timeout;
            Log_Line("job_executer_next timed out, shutting down...");
            running := False;
         then abort
            Motor_Controller_Task.job_executer_next(Signal => Job_Executer_Next_Signal);
            case Job_Executer_Next_Signal is
               when SHUTDOWN_S =>
                  RM_next := SHUTDOWN_S; -- TODO
               when EMPTY_S =>
                  RM_next := EMPTY_S; -- TODO
            end case;

         end select;


         select
            delay timeout;
            Log_Line("road_marker_next timed out, shutting down...");
            running := False;
         then abort
           Roadmarker_Task.road_marker_next(RM_next);
         end select;

      end loop;


   end Job_Executer_Task_T;

end Job_Executer;
