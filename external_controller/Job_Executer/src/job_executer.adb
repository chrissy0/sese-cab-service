pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Job_Executer is

   procedure Log_Line(Message : String) is
   begin
      Put_Line("[Job_Executer] " & Message);
   end Log_Line;

   type Road_Marker_ID_T is new Integer range 0 .. 15;

   function calculate_rm_next_output
     (Current_RM_ID  : in Road_Marker_ID_T) return Job_Executer_Done_T is
   begin
      if Current_RM_ID mod 2 = 0 then
         return NEXT_LEFT_S;
      else
         return NEXT_RIGHT_S;
      end if;
   end calculate_rm_next_output;

   ---------------------------
   -- Front_Distance_Task_T --
   ---------------------------


   task body Job_Executer_Task_T is
      Motor_Controller_Task   : Motor_Controller_Task_Access_T;
      timeout                 : Duration;
      running                 : Boolean := True;
      Current_RM_ID           : Road_Marker_ID_T;
      Job_Executer_Done_Signal: Job_Executer_Done_T;
      Job_Executer_Next_Signal: Job_Executer_Next_T;
      section                 : Integer;
   begin
      Log_Line("Starting Module");
      Log_Line("Waiting for Constructor call..");
      accept Constructor
        (Motor_Controller_Task_A : in Motor_Controller_Task_Access_T;
         timeout_v               : in Duration
        )
      do
         null;
         Motor_Controller_Task := Motor_Controller_Task_A;
         timeout               := timeout_v;
      end Constructor;
      Log_Line("Constructor done!");

      -- main loop
      while running loop

         select
            accept road_marker_done (Signal : in Road_Marker_Done_T) do
               case Signal is
                  when 0 .. 15 => Current_RM_ID := Road_Marker_ID_T(Signal);
                  when 16      => Log_Line("Road_Marker system error!");
                  when 17      => null; -- empty signal
               end case;
            end road_marker_done;
         or
            delay timeout;
            Log_Line("road_marker_done timed out! Critical Error!");
            running := False;
         end select;

         Job_Executer_Done_Signal := calculate_rm_next_output
           (Current_RM_ID => Current_RM_ID);

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
         end select;

         select
            accept road_marker_next (Signal : out Road_Marker_Next_T) do
               case Job_Executer_Next_Signal is
                  when EMPTY_S => Signal := EMPTY_S;
                  when SHUTDOWN_S => Signal := SHUTDOWN_S;
               end case;

            end road_marker_next;
         or
            delay timeout;
            Log_Line("road_marker_next timed out! Critical Error!");
            running := False;
         end select;






      end loop;


   end Job_Executer_Task_T;

end Job_Executer;
