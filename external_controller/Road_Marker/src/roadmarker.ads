-- @summary
-- Road Marker controller package specification.
--
-- @author Julian Hartmer and Chanki Hong
-- @description
-- This package controls the roadmarker by pulling and evaluating the road
-- marker sensor values. Communicates with Motor Controller Task.

with Ada.Text_IO;      use Ada.Text_IO;
with Motor_Controller; use Motor_Controller;

-- @summary
-- Detecting and parsing road markers
--
-- @description
-- This package provides a task for parsing road marker and sending the current
-- road marker.
package Roadmarker is

   -- 0-15 Road Marker IDs
   -- 16   System Error
   -- 17   No Road Marker
   RM_system_error : constant Integer   := 16;
   RM_no_road_marker : constant Integer := 17;
   subtype Road_Marker_Done_T is Integer range 0 .. 17;
   subtype Road_Marker_valid_T is Integer range 0 .. 15;

   type Road_Marker_History_T is array (Road_Marker_Done_T) of Natural;

   -- Commands the road marker can recieve with road_marker_next.
   -- @value EMPTY_S No command
   -- @value SHUTDOWN_S Shutdown signal
   type Road_Marker_Next_T is (EMPTY_S, SHUTDOWN_S);


   -- Enumartion to referenced road marker sensors by their position
   -- @value FRONT_LEFT outer, front left sensor
   -- @value FRONT_RIGHT outer, front right sensor
   -- @value BEHIND_LEFT outer, sensor on the back left side
   -- @value BEHIND_RIGHT outer, sensor on the back right side
   -- @value RM_FL inner, front left sensor
   -- @value RM_FR inner, front right sensor
   -- @value RM_BL inner sensor on the back left side
   -- @value RM_BR inner sensor on the back right side
   type Roadmarker_Sensor_ID_T is
     (FRONT_LEFT, FRONT_RIGHT, BEHIND_LEFT, BEHIND_RIGHT, RM_FL, RM_FR, RM_BL,
      RM_BR);

   -- Acces type to getter function for road marker sensor values.
   -- The sensor is referenced by ID and a Boolean. The boolean indicates
   -- whether the normal sensor (False) or the default sensor (True) is read.
   -- @param ID sensor ID
   -- @param is_backup_sensor true: access backup sensor, false: access default sensor
   -- @return Sensor value
   type get_roadmarker_sensor_value_access is access
     function (ID : in Roadmarker_Sensor_ID_T; is_backup_sensor : Boolean) return Long_Float;


   -- Task to fetch and evaluate road marker sensor values. Communicates
   -- with the Job Executer Task by road_marker_done and road_marker_next.
   task type Roadmarker_Task_T is

      -- Roadmarker task constructor. Tasks wait after spawning for constructor
      -- to initialize the task.
      -- @param get_sensor_value_a Access to road marker sensor getter function
      -- @param timeout_v Rendezvous synchronization timeout
      -- @param MC_Task Access to motor controller task
      entry Construct
        (
         get_sensor_value_a   : in get_roadmarker_sensor_value_access;
         timeout_v            : in Duration;
         MC_Task              : in Motor_Controller_Task_Access_T
        );

      -- Rendezvous synchronization to send iteration result to job executer.
      -- @param Signal iteration result
      entry road_marker_done (Signal : out Road_Marker_Done_T);

      -- Rendezvous synchronization to wait for job executer for next iteration command.
      -- @param Signal Next iteration command (shutdown or normal)
      entry road_marker_next (Signal : in Road_Marker_Next_T);
   end Roadmarker_Task_T;

   type Roadmarker_Task_Acces_T is access Roadmarker_Task_T;

private
   ROADMARKER_THRESH_MAX : constant Long_Float := 290.0;
   ROADMARKER_THRESH_MIN : constant Long_Float := 270.0;
   SENSOR_FAULT          : constant Long_Float := -1.0;


   -- array of sensor values. Second index true => access backup sensor
   -- Global for testing purposes.
   type All_Sensor_Values_Array_T is array (Roadmarker_Sensor_ID_T, Boolean) of Long_Float;


   -- Returns the road marker with the history entry, prioritising road markers
   -- with higher road marker numbers (15-0). If there is no such entry,
   -- return no road marker.
   -- Should only be used in Roadmarker_Task.
   -- @param history history of last read roadmarkers
   -- @retrn Output calculated from history
   function calculate_output_from_history
     (
      history   : in Road_Marker_History_T
     ) return Road_Marker_Done_T;

   -- Fill array with 0s.
   -- Global for testing purposes.
   -- @param history history to be filled with 0s
   procedure empty_history(history: in out Road_Marker_History_T);

   -- check normal and then backup sensors for error. If both are in an error
   -- state, output RM_system_error. The active sensor is the normal one. In case of
   -- an error in the normal sensor, the backup sensors are the active sensors.
   -- While the outer sensors detetcts that the cab is on a roadmarker,
   -- add each read roadmarker to the history. When the cab stops detecting
   -- a roadmarker, it searches the history for the most oftenly read roadmarker
   -- and returns it. Otherwise, this function returns RM_no_road_marker.
   -- Should only be used in Roadmarker_Task.
   -- @param all_sensor_values array with current sensor values
   -- @param history history of previously read road markers
   -- @param was_on_hotfix_rm flag to show that cab was on hotfix road marker (forcing left)
   function calculate_output
     (
      all_sensor_values : All_Sensor_Values_Array_T;
      history           : in out Road_Marker_History_T;
      was_on_hotfix_rm  : in out Boolean
     ) return Road_Marker_Done_T;

   -- Return true if the default or backup sensor array contains an error,
   -- depending on the is_backup_sensor value.
   -- @param all_sensor_values array filled with sensor values to be tested
   -- @param is_backup_sensor True => check backup sensor array, false => check normal sensor array
   -- @return True => error in array, False => no error in selected arrray
   function check_error_sensor_array
     (
      all_sensor_values : All_Sensor_Values_Array_T;
      is_backup_sensor  : Boolean
     ) return Boolean;
end Roadmarker;
