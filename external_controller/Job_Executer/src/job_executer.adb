pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Roadmarker; use Roadmarker;
with ec2b; use ec2b;
package body Job_Executer is

   procedure Log_Line(Message : String) is
   begin
      Put_Line("[Job_Executer] " & Message);
   end Log_Line;

   type Road_Marker_ID_T is new Integer range 0 .. 15;

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
      cab_version : Integer := 0;
      cab_version_old : Integer := 0;


   begin
      current_command.action := NEXT_UNKOWN_S;
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
      end Constructor;
      Log_Line("Constructor done!");

      Roadmarker_Task.Construct(get_sensor_value_a => RM_get_sensor_value);
      cab_id := register_cab("Dieter3", 0); -- Todo use real cab_name
      -- Todo error handling
      if cab_id < 0 then
         cab_id := 1;
      end if;

      Put_Line("Register cab returned: " & cab_id'Image);
      update_cabLocation(cab_id, 14);
      cmd_queue := request_route(cmd_queue, cab_id, cab_version);
      cmd_queue.Dequeue(next_command);
      Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.marker'Image);

      -- main loop
      while running loop

         select
            delay timeout;
            Log_Line("road_marker_done timed out, shutting down...");
            running := False;
         then abort
           Roadmarker_Task.road_marker_done(section);
         end select;


         case section is
         when 0 .. 15 =>
            update_cabLocation(cab_id, section);
            if (Integer(section) = next_command.marker) then
               Put_Line(section'Image & " = " & next_command.marker'Image);
               current_command := next_command;
               case(current_command.action) is
               when PICK_UP_S => request_pickup(cab_id, next_command.customer_ID);
                  cmd_queue.Dequeue(next_command);
                  Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.marker'Image);

               when DROP_OFF_S => request_dropoff(cab_id, next_command.customer_ID);
                  cmd_queue.Dequeue(next_command);
                  Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.marker'Image);
               when WAIT_S => null;
               when others =>
                  cmd_queue.Dequeue(next_command);
                  Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.marker'Image);
               end case;

            end if;
            when 16 => null; -- TODO
            when 17 => null; -- TODO
         end case;

         cab_version_old := cab_version;
         cmd_queue := request_route(cmd_queue, cab_id, cab_version);
         if (cab_version_old /= cab_version) then
            cmd_queue.Dequeue(next_command);
            current_command.action := NEXT_UNKOWN_S;
         end if;

         Job_Executer_Done_Signal := EMPTY_S;
         Put_Line("NEXT ACTION: " & next_command.action'Image);
         while (Job_Executer_Done_Signal = EMPTY_S) loop
            case (current_command.action) is
            when NEXT_LEFT_S => Job_Executer_Done_Signal := NEXT_LEFT_S ;
            when NEXT_RIGHT_S => Job_Executer_Done_Signal := NEXT_RIGHT_S;
            when NEXT_UNKOWN_S => Job_Executer_Done_Signal := NEXT_UNKOWN_S;
            when WAIT_S => Job_Executer_Done_Signal := STOP_S;
            when STOP_S => Job_Executer_Done_Signal := STOP_S;
            when PICK_UP_S =>
               if(pickup_complete(cab_id)) then
                  cmd_queue.Dequeue(next_command);
               else
                  request_pickup(cab_id, next_command.customer_ID);
                  Job_Executer_Done_Signal := STOP_S;
               end if;
            when DROP_OFF_S =>
               if(dropoff_complete(cab_id)) then
                  cmd_queue.Dequeue(next_command);
               else
                  request_dropoff(cab_id, next_command.customer_ID);
                  Job_Executer_Done_Signal := STOP_S;
               end if;

            when EMPTY_S => Job_Executer_Done_Signal := SYSTEM_ERROR_S;
            when SYSTEM_ERROR_S => Job_Executer_Done_Signal := SYSTEM_ERROR_S;
            end case;
         end loop;

         Put_Line("Job Executor Done ACTION: " & Job_Executer_Done_Signal'Image);

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
