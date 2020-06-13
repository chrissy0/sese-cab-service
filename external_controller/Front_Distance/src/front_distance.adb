pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Front_Distance is

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
   begin
      Put_Line("Startinf Front_Distance Thread.");
      Put_Line("Front_Distance: Waiting for Construct...");
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
      Put_Line("... Front_Distance constructor done");

      -- main loop
      while running loop

         -- reading sensor values:
         for ID in Distance_Sensor_ID_T loop
            Sensor_Values(ID) := get_distance_sensor_value(ID);
         end loop;


         -- calculate output:
         Output := FRONT_CLEAR_S;
         -- TODO Handle Sensor Failures

         -- check if one sensor senses front blocked
         -- -> output front_blocked, else clear
         for ID in Distance_Sensor_ID_T loop
            if Sensor_Values(ID) < US_Obst_Thresh then
               Output := FRONT_BLOCKED_S;

            end if;

         end loop;


         Put_Line("Front_Distance: sending front_distance_done...");
         -- Output signal
         select
           Motor_Controller_Task.front_distance_done(Output);
         then abort
            delay 2.0;
            Put_Line("front_distance_done timed out, shutting Front_Distance down...");
            running := False;
            goto Continue;
         end select;
         Put_Line("Front_Distance: ...  recieved!");


         Put_Line("Front_Distance: sending front_distance_next...");
         -- wait for signal to start next iteration
         select
            delay 2.0;
            Put_Line("lane_detection_next timed out, shutting Front_Distance down...");
            running := False;
            goto Continue;
         then abort
           Motor_Controller_Task.front_distance_next;
         end select;
         Put_Line("Front_Distance: ...  recieved!");


         <<Continue>>
      end loop;
      Put_Line
        ("Front_Distance shutting down. So long, and thanks for all the distance (?)");


   end Front_Distance_Task_T;

end Front_Distance;
