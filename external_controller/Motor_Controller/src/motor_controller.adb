pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with WC2EC;       use WC2EC;
package body Motor_Controller is

   procedure print_motor_value (f : Float) is
   begin
      Put (f'Image);
   end print_motor_value;

   type Motor_Controller_State_T is (SYSTEM_ERROR, NORMAL_DRIVING);
   type Normal_Driving_State_T is (FRONT_CLEAR, FRONT_BLOCKED);
   type Front_Clear_State_T is (DRIVE, STOP);
   type Drive_State_T is (STRAIGHT, LEFT, RIGHT, INIT);
   type System_Error_State_T is
     (STOP, LEFT, RIGHT, DRIVE_OVER_CURB, STAND_ON_TRACK, STAND_OFF_TRACK);

   ---------------------------
   -- Motor_Controller_Task --
   ---------------------------

   task body Motor_Controller_Task_T is
      Motor_Controller_State : Motor_Controller_State_T := NORMAL_DRIVING;
      Normal_Driving_State   : Normal_Driving_State_T   := FRONT_CLEAR;
      Front_Clear_State      : Front_Clear_State_T      := DRIVE;
      Drive_State            : Drive_State_T            := STRAIGHT;
      System_Error_State     : System_Error_State_T     := STOP;
      Motor_Straight_Speed   : Long_Float;
      Motor_Turn_Speed       : Long_Float;
      WC2EC_Driver           : wc2ec_thread_access_t;
      Last_Drive_State       : Drive_State_T            := INIT;

      procedure drive_state_output is
      begin
         if Drive_State /= Last_Drive_State then
            case Drive_State is
               when STRAIGHT =>
                  Put_Line ("driving straight");
                  WC2EC.set_motor_sensor_data ("wheel1", Motor_Straight_Speed);
                  WC2EC.set_motor_sensor_data ("wheel3", Motor_Straight_Speed);
                  WC2EC.set_motor_sensor_data ("wheel2", Motor_Straight_Speed);
                  WC2EC.set_motor_sensor_data ("wheel4", Motor_Straight_Speed);
               when LEFT =>
                  Put_Line ("driving left");
                  WC2EC.set_motor_sensor_data ("wheel1", -Motor_Turn_Speed);
                  WC2EC.set_motor_sensor_data ("wheel3", -Motor_Turn_Speed);
                  WC2EC.set_motor_sensor_data ("wheel2", Motor_Turn_Speed);
                  WC2EC.set_motor_sensor_data ("wheel4", Motor_Turn_Speed);
               when RIGHT =>
                  Put_Line ("driving right");
                  WC2EC.set_motor_sensor_data ("wheel1", Motor_Turn_Speed);
                  WC2EC.set_motor_sensor_data ("wheel3", Motor_Turn_Speed);
                  WC2EC.set_motor_sensor_data ("wheel2", -Motor_Turn_Speed);
                  WC2EC.set_motor_sensor_data ("wheel4", -Motor_Turn_Speed);
               when INIT =>
                  Put_Line ("driving Init");
                  WC2EC.set_motor_sensor_data ("wheel1", 0.0);
                  WC2EC.set_motor_sensor_data ("wheel3", 0.0);
                  WC2EC.set_motor_sensor_data ("wheel2", 0.0);
                  WC2EC.set_motor_sensor_data ("wheel4", 0.0);

            end case;
         end if;
         Last_Drive_State := Drive_State;

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
      -- accept constructor call
      accept Construct
        (WC2EC_Driver_A : in wc2ec_thread_access_t;
         Straight_Speed : in Long_Float; Turn_Speed : in Long_Float)
      do
         WC2EC_Driver         := WC2EC_Driver_A;
         Motor_Straight_Speed := Straight_Speed;
         Motor_Turn_Speed     := Turn_Speed;
      end Construct;
      while True loop

         -- look for all signals -> order not set Break when every task raised
         -- each task will send one Signal

         -- Signal from Main
         accept lane_detection_done
           (Lane_Detection_Signal_Value : in Lane_Detection_Done_T)
         do
            case Lane_Detection_Signal_Value is
               when SYSTEM_ERROR_S =>
                  Motor_Controller_State := SYSTEM_ERROR;
                  WC2EC.set_motor_sensor_data ("wheel1", 0.0);
                  WC2EC.set_motor_sensor_data ("wheel3", 0.0);
                  WC2EC.set_motor_sensor_data ("wheel2", 0.0);
                  WC2EC.set_motor_sensor_data ("wheel4", 0.0);
                  loop
                     delay (100.0);
                  end loop;

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

         -- Output stuff depending on State
         motor_controller_state_output;

      end loop;

   end Motor_Controller_Task_T;

end Motor_Controller;
