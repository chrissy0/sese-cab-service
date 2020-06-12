pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Motor_Controller is


   procedure print_motor_value (f : Float) is
   begin
      Put (f'Image);
   end print_motor_value;

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
      Last_Drive_State       : Drive_State_T            := INIT;
      set_motor_value        : set_motor_value_procedure_t;

      procedure drive_state_output is
      begin
         if Drive_State /= Last_Drive_State then
            case Drive_State is
               when STRAIGHT =>
                  Put_Line ("driving straight");
                  set_motor_value (MOTOR_FRONT_LEFT, Motor_Straight_Speed);
                  set_motor_value (MOTOR_BACK_LEFT, Motor_Straight_Speed);
                  set_motor_value (MOTOR_FRONT_RIGHT, Motor_Straight_Speed);
                  set_motor_value (MOTOR_BACK_RIGHT, Motor_Straight_Speed);
               when LEFT =>
                  Put_Line ("driving left");
                  set_motor_value (MOTOR_FRONT_LEFT, 0.0);
                  set_motor_value (MOTOR_BACK_LEFT, 0.0);
                  set_motor_value (MOTOR_FRONT_RIGHT, Motor_Turn_Speed);
                  set_motor_value (MOTOR_BACK_RIGHT, Motor_Turn_Speed);
               when RIGHT =>
                  Put_Line ("driving right");
                  set_motor_value (MOTOR_FRONT_LEFT, Motor_Turn_Speed);
                  set_motor_value (MOTOR_BACK_LEFT, Motor_Turn_Speed);
                  set_motor_value (MOTOR_FRONT_RIGHT, 0.0);
                  set_motor_value (MOTOR_BACK_RIGHT, 0.0);
               when INIT =>
                  Put_Line ("driving Init");
                  set_motor_value (MOTOR_FRONT_LEFT, 0.0);
                  set_motor_value (MOTOR_BACK_LEFT, 0.0);
                  set_motor_value (MOTOR_FRONT_RIGHT, 0.0);
                  set_motor_value (MOTOR_BACK_RIGHT, 0.0);

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

      procedure handle_lane_detection_signal
        (Lane_Detection_Signal_Value : in Lane_Detection_Done_T)
      is
      begin
         case Lane_Detection_Signal_Value is
            when SYSTEM_ERROR_S =>
               Motor_Controller_State := SYSTEM_ERROR;
               set_motor_value (MOTOR_FRONT_LEFT, 0.0);
               set_motor_value (MOTOR_BACK_LEFT, 0.0);
               set_motor_value (MOTOR_FRONT_RIGHT, 0.0);
               set_motor_value (MOTOR_BACK_RIGHT, 0.0);
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
      end handle_lane_detection_signal;

   begin
      -- accept constructor call
      accept Construct
        (MC_State : in Motor_Controller_State_T;
         ND_State : in Normal_Driving_State_T;
         FC_State : in Front_Clear_State_T; D_State : in Drive_State_T;
         SE_State : in System_Error_State_T; MS_Speed : in Long_Float;
         MT_Speed : in Long_Float;
         LD_State : in Drive_State_T;
         set_motor_value_access : in set_motor_value_procedure_t)
      do
         Motor_Controller_State := MC_State;
         Normal_Driving_State   := ND_State;
         Front_Clear_State      := FC_State;
         Drive_State            := D_State;
         System_Error_State     := SE_State;
         Motor_Straight_Speed   := MS_Speed;
         Motor_Turn_Speed       := MT_Speed;
         Last_Drive_State       := LD_State;
         set_motor_value        := set_motor_value_access;
      end Construct;
      while True loop

         -- look for all signals -> order not set Break when every task raised
         -- each task will send one Signal

         -- Signal from Main
         accept lane_detection_done
           (Lane_Detection_Signal_Value : in Lane_Detection_Done_T)
         do
            handle_lane_detection_signal (Lane_Detection_Signal_Value);
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
