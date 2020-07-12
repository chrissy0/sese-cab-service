package Motor_Controller is

   ----------------------------------
   -- TYPES USED IN OTHER PACKAGES --
   ----------------------------------

   -- type to motor actor
   type Vertical_Position_T is (FRONT, BACK);
   type Horizontal_Position_T is (LEFT, RIGHT);

   type set_motor_value_procedure_access_t is access procedure
     (
      vertical   : Vertical_Position_T;
      horizontal : Horizontal_Position_T;
      value      : Long_Float
     );

   -- type of values send with signal from and to each component
   type Lane_Detection_Done_T is
     (SYSTEM_ERROR_S, GO_STRAIGHT_S, ROTATE_LEFT_S, ROTATE_RIGHT_S, EMPTY_S);

   type Lane_Detection_Next_T is
     (LEAN_LEFT_S, LEAN_RIGHT_S, EMPTY_S, SHUTDOWN_S, LEAN_FROM_LINE);


   type Front_Distance_Done_t is
     (FRONT_BLOCKED_S, FRONT_CLEAR_S, FD_FAULT_S, EMPTY_S);

   type Front_Distance_Next_t is
     (SHUTDOWN_S, EMPTY_S);


   type Job_Executer_Done_T is
     (SYSTEM_ERROR_S, NEXT_LEFT_S, NEXT_RIGHT_S, NEXT_UNKOWN_S, EMPTY_S, STOP_S);

   type Job_Executer_Next_t is
     (SHUTDOWN_S, EMPTY_S, BLOCKED_S);
   ---------------------------
   -- task (or thread) type --
   ---------------------------
   task type Motor_Controller_Task_T is
      entry Constructor
        (
         -- function to set motor values with
         set_motor_value_access : in set_motor_value_procedure_access_t;
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

private
   MOTOR_ROTATE_SPEED : constant Long_Float := 1.0;
   MOTOR_DRIVE_SPEED  : constant Long_Float := 3.0;

   type Motor_Values_T is array (Vertical_Position_T, Horizontal_Position_T) of Long_Float;

   type Module_Tasks is (LANE_DETECTION, JOB_EXECUTER, FRONT_DISTANCE);
   type Boolean_Tasks_Arrays is array (Module_Tasks) of Boolean;
   ----------------------------------------
   -- type of internal states            --
   -- public for initialization purposes --
   ----------------------------------------

   type Motor_Controller_State_T is (SYSTEM_ERROR, NO_SYSTEM_ERROR, SHUTDOWN);
   type System_Error_State_T is
     (FINAL_SAFE_STATE, STAND_ON_TRACK);
   type No_System_Error_State_T is (FRONT_CLEAR, FRONT_BLOCKED);
   type Front_Clear_State_T is (DRIVE, STOP);
   type Drive_State_T is (STRAIGHT, ROTATE_LEFT, ROTATE_RIGHT, INIT);
   type Lean_State_T is (NEXT_LEFT, NEXT_RIGHT, LEAN_FROM_LINE);

   type Cab_State_T is record
      Base            : Motor_Controller_State_T;
      System_Error    : System_Error_State_T;
      No_System_Error : No_System_Error_State_T;
      Front_Is_Clear  : Front_Clear_State_T;
      Driving         : Drive_State_T;
      Leaning         : Lean_State_T;
      Forcing_Left    : Boolean;
   end record;


   procedure calculate_output
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T;
      LD_Next_Signal : out Lane_Detection_Next_T;
      FD_Next_Signal : out Front_Distance_Next_t;
      JE_Next_Signal : out Job_Executer_Next_t
     );


   procedure output_no_system_error
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T;
      JE_Next_Signal : out Job_Executer_Next_t
     );

   procedure output_front_is_clear
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T
     );

   procedure output_driving
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T
     );

end Motor_Controller;
