pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Front_Distance is

   procedure Log_Line(Message : String) is
   begin
      Put_Line("[front_distance] " & Message);
   end Log_Line;


   ---------------------------
   -- Front_Distance_Task_T --
   ---------------------------

   task body Front_Distance_Task_T is
      Sensor_Values                    : Distance_Sensor_Values_Array_T;
      US_Obst_Thresh                   : Long_Float;
      get_distance_sensor_value        : get_distance_sensor_value_t;
      Motor_Controller_Task            : Motor_Controller_Task_Access_T;
      running                          : Boolean := True;
      Output                           : Front_Distance_Done_t;
      Next_Signal                      : Front_Distance_Next_t;
   begin
      Log_Line("Starting Front_Distance Thread.");
      Log_Line("Front_Distance: Waiting for Construct...");
      accept Construct
        (get_distance_sensor_value_access : in get_distance_sensor_value_t;
         us_thresh : in Long_Float;
         Motor_Controller_Task_A : in Motor_Controller_Task_Access_T
        )
      do
         US_Obst_Thresh := us_thresh;
         get_distance_sensor_value := get_distance_sensor_value_access;
         Motor_Controller_Task := Motor_Controller_Task_A;
      end Construct;
      Log_Line("... Front_Distance constructor done");

      -- main loop
      while running loop

         -- reading sensor values:
         for ID in Distance_Sensor_ID_T loop
            Sensor_Values(ID) := get_distance_sensor_value(ID);
         end loop;


         -- calculate output:
         Output := FRONT_CLEAR_S;

         -- check if one sensor senses front blocked
         -- system erro when both sensors in same direction mailfunctioning
         -- blocked when at least one sensor detects block
         -- (CENTER_0, CENTER_1, LEFT_0, LEFT_1, RIGHT_0, RIGHT_1);
         if (Sensor_Values(CENTER_0) = 0.0 and Sensor_Values(CENTER_1) = 0.0)
           or (Sensor_Values(LEFT_0) = 0.0 and Sensor_Values(LEFT_1) = 0.0)
           or (Sensor_Values(RIGHT_0) = 0.0 and Sensor_Values(RIGHT_1) = 0.0)
         then
            Output := SYSTEM_ERROR_S;
            Log_Line("throwing System Error!");
         else
            -- if one sensor shows value smaller than thresh, front is blocked
            for ID in Distance_Sensor_ID_T loop
               if Sensor_Values(ID) < US_Obst_Thresh and Sensor_Values(ID) > 0.0 then
                  Output := FRONT_BLOCKED_S;
                  Log_Line("Front is blocked!");
                  exit;
               end if;
            end loop;
         end if;


        -- Log_Line("Front_Distance: sending front_distance_done with value " & Output'Image & "...");
         -- Output signal
         select
           Motor_Controller_Task.front_distance_done(Output);
         then abort
            delay 2.0;
            Log_Line("front_distance_done timed out, shutting Front_Distance down...");
            running := False;
            goto Continue;
         end select;
         --Log_Line("Front_Distance: ...  recieved!");


         --Log_Line("Front_Distance: sending front_distance_next...");
         -- wait for signal to start next iteration
         select
            delay 2.0;
            Log_Line("lane_detection_next timed out, shutting Front_Distance down...");
            running := False;
            goto Continue;
         then abort
           Motor_Controller_Task.front_distance_next(Signal => Next_Signal);
         end select;
         --Log_Line("Front_Distance: ...  recieved!");


         <<Continue>>
      end loop;
      Log_Line
        ("Front_Distance shutting down. So long, and thanks for all the distance (?)");


   end Front_Distance_Task_T;

end Front_Distance;
