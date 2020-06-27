pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Roadmarker; use Roadmarker;
package body Job_Executer is

   procedure Log_Line(Message : String) is
   begin
      Put_Line("[Job_Executer] " & Message);
   end Log_Line;

   type Road_Marker_ID_T is new Integer range 0 .. 15;

   function calculate_rm_next_output
     (Current_RM_ID  : in Road_Marker_Done_T) return Job_Executer_Done_T is
   begin
      if Current_RM_ID mod 2 = 0 then
         return NEXT_RIGHT_S;
      else
         return NEXT_LEFT_S;
      end if;
   end calculate_rm_next_output;

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

   begin
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

      -- main loop
      while running loop

         select
            delay timeout;
            Log_Line("road_marker_done timed out, shutting down...");
            running := False;
         then abort
           Roadmarker_Task.road_marker_done(section);
         end select;

         Job_Executer_Done_Signal := calculate_rm_next_output(section);

         Log_Line("------------------------------ CURRENT SECTION: " & section'Image & " -------------------------");
         Log_Line("------------------------------ CURRENT SECTION: " & section'Image & " -------------------------");
         Log_Line("------------------------------ CURRENT SECTION: " & section'Image & " -------------------------");
         Log_Line("------------------------------ CURRENT SECTION: " & section'Image & " -------------------------");
         Log_Line("------------------------------ CURRENT SECTION: " & section'Image & " -------------------------");
         Log_Line("------------------------------ CURRENT SECTION: " & section'Image & " -------------------------");
         Log_Line("------------------------------ CURRENT SECTION: " & section'Image & " -------------------------");
         Log_Line("------------------------------ CURRENT SECTION: " & section'Image & " -------------------------");
         Log_Line("------------------------------ CURRENT SECTION: " & section'Image & " -------------------------");

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
                  RM_next := SHUTDOWN_S;
               when EMPTY_S =>
                  RM_next := EMPTY_S;
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
