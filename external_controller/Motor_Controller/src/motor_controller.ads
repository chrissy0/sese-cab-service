-- @summary
-- Motor controller package specification. Communicates with Lane Detection,
-- Front Distance, Road Marker and Job Executer and sets the wheel speed
--
-- @author Julian Hartmer
-- @description
-- This package communicates with all cab tasks. It manages the other tasks
-- and controls the cab's wheels.

package Motor_Controller is

   -- Wheel vertical position
   -- @value FRONT Front wheel
   -- @value BACK Wheel at the bacl
   type Vertical_Position_T is (FRONT, BACK);

   -- Wheel horizontal position
   -- @value LEFT Wheel on the left
   -- @value RIGHT Wheel on the right
   type Horizontal_Position_T is (LEFT, RIGHT);

   -- Access to motor setter function
   -- @param vertical vertical motor position
   -- @param horizontal horizontal motor position
   -- @param value value to set the sensor to
   type set_motor_value_procedure_access_t is access procedure
     (
      vertical   : Vertical_Position_T;
      horizontal : Horizontal_Position_T;
      value      : Long_Float
     );

   -- Access to motor setter function
   type elevate_curb_sensor_access_t is access procedure;

   -- Value of done signal sent by Lane Detection to Motor Controller
   -- @value SYSTEM_ERROR_S Lane Detection in error state
   -- @value GO_STRAIGHT_S Drive straight
   -- @value ROTATE_LEFT_S Rotate counter clockwise
   -- @value ROTATE_RIGHT_S Rotate clockwise
   -- @value EMPTY_S Nothing new
   type Lane_Detection_Done_T is
     (SYSTEM_ERROR_S, GO_STRAIGHT_S, ROTATE_LEFT_S, ROTATE_RIGHT_S, EMPTY_S);

   -- Value of next signal sent to Lane Detection by Motor Controller
   -- @value LEAN_LEFT_S Follow the left lane
   -- @value LEAN_RIGHT_S Follow the right lane
   -- @value EMPTY_S Nothing new
   -- @value SHUTDOWN_S Shutdown the Lane Detection thread
   -- @value LEAN_FROM_LINE Job Executer failed, use the line color to navigate to the sidetrack
   type Lane_Detection_Next_T is
     (LEAN_LEFT_S, LEAN_RIGHT_S, EMPTY_S, SHUTDOWN_S, LEAN_FROM_LINE);

   -- Value of done signal sent by Front Distance to Motor Controller
   -- @value FRONT_BLOCKED_S Front is blocked
   -- @value FRONT_CLEAR_S Front is clear
   -- @value FD_FAULT_S Front distance sensor are in an error state
   -- @value EMPTY_S Nothing new
   type Front_Distance_Done_t is
     (FRONT_BLOCKED_S, FRONT_CLEAR_S, FD_FAULT_S, EMPTY_S);

   -- Value of next signal sent by Motor Controller to Front Distance
   -- @value EMPTY_S Nothing new
   -- @value SHUTDOWN_S Shutdown the Front Distance thread
   type Front_Distance_Next_t is
     (SHUTDOWN_S, EMPTY_S);


   -- Value of done signal sent by Job Executer to Motor Controller
   -- @value SYSTEM_ERROR_S Job Executer failed
   -- @value NEXT_LEFT_S Go left on next intersection
   -- @value NEXT_RIGHT_S Go right on next intersection
   -- @value NEXT_UNKOWN_S Do not know next intersection action yet
   -- @value EMPTY_S Nothing new
   -- @value STOP_S Halt the cab (for passenger pickup/drop-off)
   type Job_Executer_Done_T is
     (SYSTEM_ERROR_S, NEXT_LEFT_S, NEXT_RIGHT_S, NEXT_UNKOWN_S, EMPTY_S, STOP_S);

   -- Value of next signal sent by Motor Controller to Job Executer
   -- @value SHUTDOWN_S Shutdown the Job Executer Thread
   -- @value EMPTY_S Nothing new
   -- @value BLOCKED_S Front is blocked
   type Job_Executer_Next_t is
     (SHUTDOWN_S, EMPTY_S, BLOCKED_S, NOT_FUNCTIONAL);

   -- Task to evaluate all other tasks' output and set the motor actor
   -- accordingly. Communicates with Lane_Detection, Roadmarker, Job_Executer
   -- and Front_Distance Task
   task type Motor_Controller_Task_T is

      -- Motor Task constructor. Tasks wait after spawning for constructor
      -- to initialize the task.
      -- @param set_motor_value_access Access to motor sensor setter procedure
      -- @param elevate_sensors_access Access to procedure to elevate the curb sensors.
      -- @param timeout_v Rendezvous synchronization timeout
      -- @param iteration_delay_s Delay between iteration starts
      entry Constructor
        (
         set_motor_value_access : in set_motor_value_procedure_access_t;
         elevate_sensors_access : in elevate_curb_sensor_access_t;
         timeout_v              : in Duration;
         iteration_delay_s      : in Duration
        );


      -- Rendezvous synchronization for lane detection to get iteration result.
      -- @param Signal lane detection iteration result
      -- @param is_curb_detection true: curb detection on, false: line detection on
      entry lane_detection_done
        (Signal : in Lane_Detection_Done_T; is_curb_detection : in Boolean);

      -- Rendezvous synchronization for lane detection to wait for next iteration.
      -- @param Signal Command for next iteration
      entry lane_detection_next
        (Signal : out Lane_Detection_Next_T);



      -- Rendezvous synchronization for front distance to get iteration result.
      -- @param Signal front distance iteration result
      entry front_distance_done
        (Signal : in Front_Distance_Done_t);

      -- Rendezvous synchronization for front distance to wait for next iteration.
      -- @param Signal Command for next iteration
      entry front_distance_next
        (Signal : out Front_Distance_Next_t);


      -- Rendezvous synchronization for job executer to get iteration result.
      -- @param Signal job executer iteration result
      entry job_executer_done
        (Signal : in Job_Executer_Done_T);

      -- Rendezvous synchronization for job executer to wait for next iteration.
      -- @param Signal job executer next iteration command.
      entry job_executer_next
        (Signal : out Job_Executer_Next_t);


      -- Rendezvous synchronization for builder to start next iteration or shutdown external controller.
      -- @param is_shutdown true: Shutdown external controller, false: start next iteration.
      entry main_shutdown_signal
        (is_shutdown : in Boolean);


      -- Rendezvous synchronization for road marker so it can set force lean left.
      -- @param Signal true: force lean left, false: do not force lean left
      entry rm_hotfix_signal
        (Signal : in Boolean);

   end Motor_Controller_Task_T;

   type Motor_Controller_Task_Access_T is access Motor_Controller_Task_T;

private
   -- Wheel speed when rotating
   MOTOR_ROTATE_SPEED : constant Long_Float := 1.0;
   -- Wheel speed when driving forwards
   MOTOR_DRIVE_SPEED  : constant Long_Float := 3.0;
   -- Number of iterations to rotate 90 degrees
   ITERAION_NUM_90_DEGREE : constant Natural := 90;
   -- Number of iterations to drive off track when rotated to left or right
   ITERAION_NUM_DRIVE_OFF : constant Natural := 70;

   type Motor_Values_T is array (Vertical_Position_T, Horizontal_Position_T) of Long_Float;

   -- Enumeration of all tasks which send done and next signals
   -- @value LANE_DETECTION lane detection task
   -- @value JOB_EXECUTER job executer task
   -- @value FRONT_DISTANCE front distance task
   type Module_Tasks is (LANE_DETECTION, JOB_EXECUTER, FRONT_DISTANCE);

   -- Array type to store which tasks already sent done and next signals
   type Boolean_Tasks_Arrays is array (Module_Tasks) of Boolean;

   -- Base state of the motor controller
   -- @value SYSTEM_ERROR Error State.
   -- @value NO_SYSTEM_ERROR Normal operation state
   -- @value SHUTDOWN Turning off state
   type Motor_Controller_State_T is (SYSTEM_ERROR, NO_SYSTEM_ERROR, SHUTDOWN);

   -- State variable type to further describe error state
   -- @value FINAL_SAFE_STATE Drive the cab off the track
   -- @value STAND_ON_TRACK Stop the cab on the track
   type System_Error_State_T is
     (FINAL_SAFE_STATE, STAND_ON_TRACK);

   -- State variable type to fruther describe the Motor_Controller_State_T's errorless state NO_SYSTEM_ERROR
   -- @value FRONT_CLEAR Front is clear
   -- @value FRONT_BLOCKED Front is blocked, halt the cab
   type No_System_Error_State_T is (FRONT_CLEAR, FRONT_BLOCKED);

   -- State variable type to futher describe the No_System_Error_State_T's FRONT_CLEAR state
   -- @value DRIVE Drive the cab
   -- @value STOP Stop the cab
   type Front_Clear_State_T is (DRIVE, STOP);

   -- State variable to further describe the Front_Clear_State_T's DRIVE state
   -- @value STRAIGHT drive straight
   -- @value ROTATE_LEFT rotate the cab counter clockwise
   -- @value ROTATE_RIGHT rotate the cab clockwise
   -- @value INIT Initial state
   type Drive_State_T is (STRAIGHT, ROTATE_LEFT, ROTATE_RIGHT, INIT);

   -- State variabel to describe the cab decision on the next intersection
   -- @value NEXT_LEFT Follow the left lane
   -- @value NEXT_RIGHT Follow the right lane
   -- @value LEAN_FROM_LINE Use the line color to find the path to the sidetrack
   type Lean_State_T is (NEXT_LEFT, NEXT_RIGHT, LEAN_FROM_LINE);

   -- State variable type to further describe the System_Error_State_T´s FINAL_SAFE_STATE
   -- @value ROTATE_LEFT_90 Rotate to the left side to prepare driving off track
   -- @value DRIVE_OFF_LEFT Drive off the left side of the track
   -- @value ROTATE_RIGHT_180_DEGREE Rotate to the right side of the track. Done if left side is blocked (wall).
   -- @value DRIVE_OFF_RIGHT Drive off the right side of the track
   -- @value DONE Final Safe state finished
   type Final_Safe_State_State_t is (ROTATE_LEFT_90, DRIVE_OFF_LEFT, ROTATE_RIGHT_180_DEGREE, DRIVE_OFF_RIGHT, DONE);


   -- Cab states representation
   -- @field Base Base state
   -- @field System_Error System error state. Further describes Base´s SYSTEM_ERROR state
   -- @field Final_Safe_State Final safe state state. Further describes System_Error's FINAL_SAFE_STATE
   -- @field No_System_Error Normal functionality state. Further describes Base's NO_SYSTEM_ERROR state
   -- @field Front_Is_Clear Front is not blocked state. Further describes No_System_Error's FRONT_CLEAR state
   -- @field Driving Driving state. Further describes Front_Is_Clear's DRIVING state.
   -- @field Leaning Direction state. Describes which path to follow on an intersection.
   -- @field Forcing_Left Road-Marker's forcing left path following state. If true, override the Job Executer's leaning value.
   -- @field Counter counter used in Final_Safe_State to rotate 90 or 180 degrees
   type Cab_State_T is record
      Base             : Motor_Controller_State_T;
      System_Error     : System_Error_State_T;
      Final_Safe_State : Final_Safe_State_State_t;
      No_System_Error  : No_System_Error_State_T;
      Front_Is_Clear   : Front_Clear_State_T;
      Driving          : Drive_State_T;
      Leaning          : Lean_State_T;
      Forcing_Left     : Boolean;
      Counter : Natural;
   end record;


   -- Calculate the Motor Controller output by using the state variable only
   -- @value state Cab state
   -- @value motor_values Wheel speed output values
   -- @value LD_Next_Signal Output value sent to Lane Detection on Lane_Detection_Next_Signal
   -- @value FD_Next_Signal Output value sent to Front Distance on Front_Distance_Next_Signal
   -- @value JE_Next_Signal Output value sent to Job Executer on Job_Executer_Next_Signal
   procedure calculate_output
     (
      state                 : Cab_State_T;
      curb_detection_active : Boolean;
      motor_values          : out Motor_Values_T;
      LD_Next_Signal        : out Lane_Detection_Next_T;
      FD_Next_Signal        : out Front_Distance_Next_t;
      JE_Next_Signal        : out Job_Executer_Next_t
     );

   -- Calculate outputs by using the state's System_Error variable
   -- @value state Cab state
   -- @value motor_values Wheel speed output values
   -- @value JE_Next_Signal Output value sent to Job Executer on Job_Executer_Next_Signal
   procedure output_system_error
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T;
      JE_Next_Signal : out Job_Executer_Next_t
     );

   -- Calculate outputs by using the state's Final_Safe_State_State_t variable
   -- @value state Cab state
   -- @value motor_values Wheel speed output values
   -- @value JE_Next_Signal Output value sent to Job Executer on Job_Executer_Next_Signal
   procedure output_final_safe_state
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T;
      JE_Next_Signal : out Job_Executer_Next_t
     );

   -- Calculate outputs by using the state's No_System_Error variable
   -- @value state Cab state
   -- @value motor_values Wheel speed output values
   -- @value JE_Next_Signal Output value sent to Job Executer on Job_Executer_Next_Signal
   procedure output_no_system_error
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T;
      JE_Next_Signal : out Job_Executer_Next_t
     );

   -- Calculate outputs by using the state's Front_Is_Clear variable
   -- @value state Cab state
   -- @value motor_values Wheel speed output values
   procedure output_front_is_clear
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T
     );


   -- Calculate outputs by using the state's Driving variable
   -- @value state Cab state
   -- @value motor_values Wheel speed output values
   procedure output_driving
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T
     );

   -- Do state transitions using the done signals
   -- @value state Cab state
   -- @value JE_Signal Job Executer's done signal value
   -- @value FD_Signal Front Distance's done signal value
   -- @value LD_Signal Lane Detection's done signal value
   -- @value RM_Force_Left Road Marker's force left signal value
   -- @value is_shutdown External Controller's shutdown signal value
   procedure do_state_transition
     (
      state         : in out Cab_State_T;
      JE_Signal     : Job_Executer_Done_T;
      FD_Signal     : Front_Distance_Done_t;
      LD_Signal     : Lane_Detection_Done_T;
      RM_Force_Left : Boolean;
      is_shutdown   : Boolean
     );

end Motor_Controller;
