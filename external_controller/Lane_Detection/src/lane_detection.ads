-- @summary
-- Lane detection controller package specification.
--
-- @author Julian Hartmer
-- @description
-- This package controls the lane detection by pulling and evaluating the lane
-- detection sensor values. Communicates with Motor Controller Task.

with Motor_Controller; use Motor_Controller;
with Ada.Text_IO;      use Ada.Text_IO;

package Lane_Detection is

   -- Enumartion to reference the line sensor by position.
   -- @value CENTER middle sensor
   -- @value LEFT left sensor
   -- @value RIGHT right sensor
   type Line_Sensor_Position_T is
     (LEFT, CENTER, RIGHT);

   -- Enumeration to reference the curb sensor by position.
   -- @value FRONT sensor at front
   -- @value BACK sensor at back
   -- @value CENTER sensor in the center
   type Curb_Sensor_Position_T is
     (FRONT, BACK, CENTER);

   -- Enumartion to reference the curb sensor by orientation.
   -- @value LEFT sensor oriented to the left of the cab
   -- @value RIGHT sensor oriented to the right of the cab
   type Sensor_Orientation_T is
     (LEFT, RIGHT);

   -- Acces type to getter function for road line detection sensor values.
   -- The sensor is referenced by ID and a Boolean. The boolean indicates
   -- whether the normal sensor (False) or the default sensor (True) is read.
   -- @param ID sensor ID
   -- @param is_backup_sensor true: access backup sensor, false: access default sensor
   -- @return current line sensor value
   type get_line_sensor_value_access is access
     function
       (
        ID               : Line_Sensor_Position_T;
        is_backup_sensor : Boolean
       ) return Long_Float;

   -- Acces type to getter function for curb sensor values.
   -- The sensor is referenced by ID and a Boolean. The boolean indicates
   -- whether the normal sensor (False) or the default sensor (True) is read.
   -- @param pos sensor position
   -- @param orientation sensor orientation
   -- @param is_backup true: access backup sensor, false: access default sensor
   -- @return current curb sensor value
   type get_curb_sensor_value_access is access
     function
       (
        pos         : Curb_Sensor_Position_T;
        orientation : Sensor_Orientation_T;
        is_backup   : Boolean
       ) return Long_Float;

   -- Acces type to getter function for curb sensor values.
   -- The sensor is referenced by ID and a Boolean. The boolean indicates
   -- whether the normal sensor (False) or the default sensor (True) is read.
   -- @param orientation sensor orientation
   -- @param is_backup true: access backup sensor, false: access default sensor
   -- @return current wall sensor value
   type get_wall_sensor_value_access is access
     function
       (
        orientation : Sensor_Orientation_T;
        is_backup   : Boolean
       ) return Long_Float;

   -- Task to fetch and evaluate road marker sensor values. Communicates
   -- with the Job Executer Task by road_marker_done and road_marker_next.
   task type Lane_Detection_Taks_T is

      -- Roadmarker task constructor. Tasks wait after spawning for constructor
      -- to initialize the task.
      -- @param Motor_Task_A access to motor task
      -- @param get_line_sensor_value_a access to line sensor getter function
      -- @param get_curb_sensor_value_a access to curb sensor getter function
      -- @param get_wall_sensor_value_a access to wall sensor getter function
      -- @param timeout_v Rendezvous synchronization timeout
      entry Construct
        (
         Motor_Task_A            : in Motor_Controller_Task_Access_T;
         get_line_sensor_value_a : in get_line_sensor_value_access;
         get_curb_sensor_value_a : in get_curb_sensor_value_access;
         get_wall_sensor_value_a : in get_wall_sensor_value_access;
         timeout_v               : in Duration
        );
   end Lane_Detection_Taks_T;

private

   -- line detection value for dark grey line
   LINE_DARK_GREY  : constant Long_Float := 246.0;

   -- line detection value for light grey line
   LINE_LIGHT_GREY : constant Long_Float := 286.0;

   -- line detection value for white line
   LINE_WHITE      : constant Long_Float := 365.0;

   -- line detection value for black line
   LINE_BLACK      : constant Long_Float := 217.0;

   -- difference from value to min and max value of detection range
   LINE_DELTA      : constant Long_Float := 15.0;

   -- Threshold to detect a line (any color)
   LINE_FOLLOW_THRESHHOLD : constant Long_Float := line_light_grey + line_delta;

   -- min Threshhold to detect curbs
   CURB_MIN_DETECTION_RANGE        : constant Long_Float := 350.0;

   -- max Threshhold to detect curbs
   CURB_MAX_DETECTION_RANGE        : constant Long_Float := 550.0;

   -- max Threshhold to detect curbs
   CURB_MAX_VALUE                  : constant Long_Float := 1_000.0;

   -- threshhold to detect walls
   WALL_THRESHHOLD        : constant Long_Float := 999.0;

   -- threshhold to detect walls
   SENSOR_FAULT           : constant Long_Float := -1.0;

   -- array of sensor values. Second index true => access backup sensor
   type Line_Sensor_Values_Array_T is array (Line_Sensor_Position_T, Boolean) of Long_Float;

   -- array of sensor values. Second index true => access backup sensor
   type Curb_Sensor_Values_Array_T is array (Curb_Sensor_Position_T, Sensor_Orientation_T, Boolean) of Long_Float;

   -- array of sensor values. Second index true => access backup sensor
   type Wall_Sensor_Values_Array_T is array (Sensor_Orientation_T, Boolean) of Long_Float;

   -- array of sensor values. Second index true => access backup sensor
   type Line_Sensor_Detected_Array_T is array (Line_Sensor_Position_T, Boolean) of Boolean;

   -- array to monitor line sensor array failures. True => access backup sensor
   type Line_Sensor_Array_Failure_Array_T is array (Boolean) of Boolean;

   -- curb sensor detection states
   -- @value DETECTED_TOO_CLOSE curb detected and curb too close
   -- @value DETECTED curb detected and in right range
   -- @value DETECTED_TOO_FAR curb detected, but too far away
   -- @value FAILURE curb detection sensor fault
   -- @value NOT_DETECTED curb not detected
   type Curb_Detected_T is (DETECTED_TOO_CLOSE, DETECTED, DETECTED_TOO_FAR, FAILURE, NOT_DETECTED);

   -- array of curb detection states
   type Curb_Detected_Array_T is array (Sensor_Orientation_T) of Curb_Detected_T;


   -- set all_sensor_values with new values from driver
   -- @param get_line_sensor_value access to line sensor getter function from driver interface
   -- @param get_curb_sensor_value access to curb sensor getter function from driver interface
   -- @param get_wall_sensor_value access to wall sensor getter function from driver interface
   -- @param line_sensor_values array filled with new line sensor values
   -- @param curb_sensor_values array filled with new curb sensor values
   -- @param wall_sensor_values array filled with new wall sensor values
   procedure retrieve_all_sensor_values
     (
      get_line_sensor_value : in get_line_sensor_value_access;
      get_curb_sensor_value : in get_curb_sensor_value_access;
      get_wall_sensor_value : in get_wall_sensor_value_access;
      line_sensor_values : out Line_Sensor_Values_Array_T;
      curb_sensor_values : out Curb_Sensor_Values_Array_T;
      wall_sensor_values : out Wall_Sensor_Values_Array_T
     );

   -- Calculate the output sent to Motor Controller Task. May change Leaning_Left
   -- when is_lean_from_line is true.
   -- @param line_sensor_values initialized line sensor values
   -- @param curb_sensor_values initialized curb sensor values
   -- @param wall_sensor_values initialized wall sensor values
   -- @param is_lean_from_line True when leaning should be done from line
   -- @param Leaning_Left lean state. May change when is_lean_from_line true
   -- @return Signal sent to Motor Controller task
   function calculate_output
     (
      line_sensor_values : Line_Sensor_Values_Array_T;
      curb_sensor_values : Curb_Sensor_Values_Array_T;
      wall_sensor_values : Wall_Sensor_Values_Array_T;
      is_lean_from_line  : Boolean;
      Leaning_Left       : in out Boolean;
      is_curb_detection  : out Boolean
     ) return Lane_Detection_Done_T;

   -- Detectes lanes using the line sensor values and threshholds.
   -- Also detects sensor array failures.
   -- @param all_sensor_values initialized line sensor values
   -- @param detected_array Entry true: Lane detected
   -- @param sensor_array_failure Entry true: Sensor array failure
   procedure detect_lanes
     (
      all_sensor_values    : Line_Sensor_Values_Array_T;
      detected_array       : out Line_Sensor_Detected_Array_T;
      sensor_array_failure : out Line_Sensor_Array_Failure_Array_T
     );

   -- For lean left, the output is as follows:
   -- l | c | r || Output
   -- 0 | 0 | 0 || Error
   -- 0 | 0 | 1 || Go_Right
   -- 0 | 1 | 0 || Go_Left
   -- 0 | 1 | 1 || Go_Straight
   -- 1 | 0 | 0 || Go_Left
   -- 1 | 0 | 1 || Go_Left
   -- 1 | 1 | 0 || Go_Left
   -- 1 | 1 | 1 || Go_Left
   -- For the leaning right, Left and right have to switched.
   -- @param detected_array Entry true: Lane detected
   -- @param Leaning_Left lean state
   -- @param sensor_array_failure Entry true: Sensor array failure
   -- @return signal sent to Motor Controller Task
   function output_from_line_detection
     (
      detected_array : Line_Sensor_Detected_Array_T;
      Leaning_Left   : Boolean;
      sensor_array_failure : Line_Sensor_Array_Failure_Array_T
     ) return Lane_Detection_Done_T;

   -- Calculates Signal sent to motor controller from curb detection.
   -- @param curb_sensor_values initialized curb sensor values
   -- @return  signal sent to Motor Controller Task
   function output_from_curb_detection
     (
      curb_sensor_values       : Curb_Sensor_Values_Array_T
     ) return Lane_Detection_Done_T;

   -- Evaluate line color to set the lean state.
   -- @param line_sensor_values initialized line sensor values
   -- @param sensor_array_failure true: Sensor array failure
   -- @param old_lean old lean state
   -- @return new lean state
   function get_lean_from_line_color
     (
      line_sensor_values   : Line_Sensor_Values_Array_T;
      sensor_array_failure : Line_Sensor_Array_Failure_Array_T;
      old_lean             : Boolean
     ) return Boolean;

end Lane_Detection;
