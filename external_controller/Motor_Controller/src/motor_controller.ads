package Motor_Controller is

   type Motor_ID_T is
     (MOTOR_FRONT_LEFT, MOTOR_FRONT_RIGHT, MOTOR_BACK_LEFT, MOTOR_BACK_RIGHT);

   type set_motor_value_procedure_t is access procedure
     (ID : Motor_ID_T; Value : Long_Float);

   type Lane_Detection_Done_T is
     (SYSTEM_ERROR_S, GO_STRAIGHT_S, GO_LEFT_S, GO_RIGHT_S, EMPTY_S);

   type Front_Distance_Done_t is
     (FRONT_BLOCKED_S, FRONT_CLEAR_S, SYSTEM_ERROR_S, EMPTY_S);

   type Motor_Controller_State_T is (SYSTEM_ERROR, NORMAL_DRIVING);
   type Normal_Driving_State_T is (FRONT_CLEAR, FRONT_BLOCKED);
   type Front_Clear_State_T is (DRIVE, STOP);
   type Drive_State_T is (STRAIGHT, LEFT, RIGHT, INIT);
   type System_Error_State_T is
     (STOP, LEFT, RIGHT, DRIVE_OVER_CURB, STAND_ON_TRACK, STAND_OFF_TRACK);

   task type Motor_Controller_Task_T is
      entry Construct
        (MC_State               : in Motor_Controller_State_T;
         ND_State               : in Normal_Driving_State_T;
         FC_State               : in Front_Clear_State_T;
         D_State                : in Drive_State_T;
         SE_State               : in System_Error_State_T;
         MS_Speed               : in Long_Float;
         MT_Speed               : in Long_Float;
         set_motor_value_access : in set_motor_value_procedure_t);

      entry lane_detection_done
        (Lane_Detection_Signal_Value : in Lane_Detection_Done_T);

      entry lane_detection_next;

      entry front_distance_done
        (Front_Distance_Signal_Value : in Front_Distance_Done_t);

      entry front_distance_next;

   end Motor_Controller_Task_T;

   type Motor_Controller_Task_Access_T is access Motor_Controller_Task_T;

end Motor_Controller;
