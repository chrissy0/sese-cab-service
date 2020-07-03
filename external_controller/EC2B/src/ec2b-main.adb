with ec2b; use ec2b;
with Ada.Text_IO; use Ada.Text_IO;


procedure Ec2B.Main is
   cab_id : Integer;
   cmd_queue: cmd_queue_access_t;
   next_command : Command_t;
   roadmarkers_input :  array (1 .. 6) of Road_Marker_Done_T :=  (1, 4, 7, 10, 12, 14);
   -- roadmarkers_input :  array (1 .. 6) of Road_Marker_ID_T :=  (1, 3, 8, 11, 12, 14);
   cab_version : Integer := 0;
   cab_version_old : Integer := 0;
   Current_RM_ID : Road_Marker_Done_T := 0;
   Job_Executer_Done_Signal: Job_Executer_Done_T;
   --Job_Executer_Next_Signal: Job_Executer_Next_T;
begin

   cab_id := register_cab("Dieter3", 0);
   -- Todo error handling
   if cab_id < 0 then
      cab_id := 1;
   end if;

   Put_Line("Register cab returned: " & cab_id'Image);
   update_cabLocation(cab_id,1);
   cmd_queue := request_route(cmd_queue, cab_id, cab_version);
   cmd_queue.Dequeue(next_command);
   Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.marker'Image);
   --for I in 1 .. cmd_queue.Current_Use loop
   --   cmd_queue.Dequeue(next_command);
   --end loop;

      for I in  roadmarkers_input'Range loop

      case roadmarkers_input(I) is
         when 0 .. 15 =>
            Current_RM_ID := roadmarkers_input(I);
            update_cabLocation(cab_id, Current_RM_ID);
            if (Integer(Current_RM_ID) = next_command.marker) then
               Put_Line(Current_RM_ID'Image & " = " & next_command.marker'Image);

               case(next_command.action) is
               when PICK_UP_S => request_pickup(cab_id, next_command.customer_ID);

               when DROP_OFF_S => request_dropoff(cab_id, next_command.customer_ID);
               when WAIT_S => null;
               when others =>
                  cmd_queue.Dequeue(next_command);
                  Put_Line("Dequeued: " & "Action :" & next_command.action'Image & " Marker: " & next_command.marker'Image);
               end case;

            end if;
            when 16..17 => null;
      end case;

      cab_version_old := cab_version;
      cmd_queue := request_route(cmd_queue, cab_id, cab_version);
      if (cab_version_old /= cab_version) then
         cmd_queue.Dequeue(next_command);
      end if;

      Job_Executer_Done_Signal := EMPTY_S;
      Put_Line("NEXT ACTION: " & next_command.action'Image);
      while (Job_Executer_Done_Signal = EMPTY_S) loop
         case (next_command.action) is
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


          --Motor_Controller_Task.job_executer_done(Signal => Job_Executer_Done_Signal);

           --Motor_Controller_Task.job_executer_next(Signal => Job_Executer_Next_Signal);

      end loop;

   end Ec2B.Main;
