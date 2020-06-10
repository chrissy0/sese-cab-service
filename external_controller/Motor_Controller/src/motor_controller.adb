pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Motor_Controller is

   type Motor_Controller_State_T is (SYSTEM_ERROR, NORMAL_DRIVING);
   type Normal_Driving_State_T is (FRONT_CLEAR, FRONT_BLOCKED);
   type Front_Clear_State_T is (DRIVE, STOP);
   type Drive_State_T is (STRAIGHT, LEFT, RIGHT);
   type System_Error_State_T is
     (STOP, LEFT, RIGHT, DRIVE_OVER_CURB, STAND_ON_TRACK, STAND_OFF_TRACK);

   ---------------------------
   -- Motor_Controller_Task --
   ---------------------------

   task body Motor_Controller_Task is
      Motor_Controller_State : Motor_Controller_State_T := NORMAL_DRIVING;
      Normal_Driving_State   : Normal_Driving_State_T   := FRONT_CLEAR;
      Front_Clear_State      : Front_Clear_State_T      := DRIVE;
      Drive_State            : Drive_State_T            := STRAIGHT;
      System_Error_State     : System_Error_State_T     := STOP;

      procedure drive_state_output is
      begin
         case Drive_State is
            when STRAIGHT =>
               Put_Line ("driving straight");
            when LEFT =>
               Put_Line ("driving left");
            when RIGHT =>
               Put_Line ("driving right");
         end case;
      end drive_state_output;

      procedure front_clear_state_output is
      begin
         case Front_Clear_State is
            when DRIVE =>
               Put_Line ("Drive state");
               drive_state_output;
            when STOP =>
               Put_Line ("Stop state");
         end case;
      end front_clear_state_output;

      procedure normal_driving_state_output is
      begin
         case Normal_Driving_State is
            when FRONT_CLEAR =>
               Put_Line ("Front clear state");
               front_clear_state_output;
            when FRONT_BLOCKED =>
               Put_Line ("Front Blocked State");
         end case;
      end normal_driving_state_output;

      procedure motor_controller_state_output is
      begin
         case Motor_Controller_State is
            when NORMAL_DRIVING =>
               Put_Line ("Normal Driving state");
               normal_driving_state_output;
            when SYSTEM_ERROR =>
               Put_Line ("System error state");
         end case;
      end motor_controller_state_output;

   begin

      accept Construct; -- accept constructor call

      while True loop

         -- Output stuff depending on State
         motor_controller_state_output;

         -- look for all signals -> order not set Break when every task raised
         -- each task will send one Signal

         -- Signal from Main
         accept lane_detection_done
           (Lane_Detection_Signal_Value : in Lane_Detection_Done_T)
         do
            case Lane_Detection_Signal_Value is
               when SYSTEM_ERROR_S =>
                  Motor_Controller_State := SYSTEM_ERROR;
               when GO_STRAIGHT_S =>
                  Drive_State := STRAIGHT;
               when GO_LEFT_S =>
                  Drive_State := LEFT;
               when GO_RIGHT_S =>
                  Drive_State := RIGHT;
               when EMPTY_S =>
                  null;
            end case;
         end lane_detection_done;

         -- all tasks wait before the motor_controller does its transistion
         -- Signal all tasks to unless it is system_error
         accept lane_detection_next do
            null;
         end lane_detection_next;

      end loop;

   end Motor_Controller_Task;

end Motor_Controller;
