package Motor_Controller is

   ----------------------------------
   -- TYPES USED IN OTHER PACKAGES --
   ----------------------------------

   -- type to motor actor
   type Motor_ID_T is
     (MOTOR_FRONT_LEFT, MOTOR_FRONT_RIGHT, MOTOR_BACK_LEFT, MOTOR_BACK_RIGHT);

   type set_motor_value_procedure_t is access procedure
     (ID : Motor_ID_T; Value : Long_Float);

   -- type of values send with signal from and to each component
   type Lane_Detection_Done_T is
     (SYSTEM_ERROR_S, GO_STRAIGHT_S, ROTATE_LEFT_S, ROTATE_RIGHT_S, EMPTY_S);

   type Lane_Detection_Next_T is
     (LEAN_LEFT_S, LEAN_RIGHT_S, NO_LEAN_S, EMPTY_S, SHUTDOWN_S);


   type Front_Distance_Done_t is
     (FRONT_BLOCKED_S, FRONT_CLEAR_S, FD_FAULT_S, EMPTY_S);

   type Front_Distance_Next_t is
     (SHUTDOWN_S, EMPTY_S);


   type Job_Executer_Done_T is
     (SYSTEM_ERROR_S, NEXT_LEFT_S, NEXT_RIGHT_S, NEXT_UNKOWN_S, EMPTY_S, STOP_S);

   type Job_Executer_Next_t is
     (SHUTDOWN_S, EMPTY_S);

   ----------------------------------------
   -- type of internal states            --
   -- public for initialization purposes --
   ----------------------------------------

   type Motor_Controller_State_T is (SYSTEM_ERROR, NORMAL_DRIVING);
   type Normal_Driving_State_T is (FRONT_CLEAR, FRONT_BLOCKED);
   type Front_Clear_State_T is (DRIVE, STOP);
   type Drive_State_T is (STRAIGHT, ROTATE_LEFT, ROTATE_RIGHT, INIT);
   type Lean_State_T is (NEXT_LEFT, NEXT_RIGHT, NEXT_UNKOWN);
   type System_Error_State_T is
     (STOP, LEFT, RIGHT, DRIVE_OVER_CURB, STAND_ON_TRACK, STAND_OFF_TRACK);

   ---------------------------
   -- task (or thread) type --
   ---------------------------
   task type Motor_Controller_Task_T is
      entry Constructor
        (MC_State               : in Motor_Controller_State_T;
         ND_State               : in Normal_Driving_State_T;
         FC_State               : in Front_Clear_State_T;
         D_State                : in Drive_State_T;
         LE_State               : in Lean_State_T;
         SE_State               : in System_Error_State_T;
         MS_Speed               : in Long_Float;
         MT_Speed               : in Long_Float;

         -- function to set motor values with
         set_motor_value_access : in set_motor_value_procedure_t;
         timeout_v              : in Duration;
         iteration_delay_s      : in Duration
        );


      entry lane_detection_done
        (Signal : in Lane_Detection_Done_T);

      entry lane_detection_next
        (Signal : out Lane_Detection_Next_T);


      entry front_distance_done
        (Signal : in Front_Distance_Done_t);

      entry front_distance_next
        (Signal : out Front_Distance_Next_t);


      entry job_executer_done
        (Signal : in Job_Executer_Done_T);

      entry job_executer_next
        (Signal : out Job_Executer_Next_t);

      entry main_shutdown_signal
        (is_shutdown : in Boolean);


      entry rm_hotfix_signal
        (Signal : in Boolean);

   end Motor_Controller_Task_T;

   type Motor_Controller_Task_Access_T is access Motor_Controller_Task_T;

end Motor_Controller;
