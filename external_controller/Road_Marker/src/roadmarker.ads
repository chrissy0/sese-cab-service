with Ada.Text_IO;      use Ada.Text_IO;

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
   subtype Road_Marker_Done_T is Integer range 0 .. 17;

   -- Commands the road marker can recieve with road_marker_next.
   -- @value EMPTY_S No command
   -- @value SHUTODN_S Shutdown signal
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
   type get_roadmarker_sensor_value_access is access
     function (ID : in Roadmarker_Sensor_ID_T; is_backup_sensor : Boolean) return Long_Float;

   -- Task to fetch and evaluate road marker sensor values. Communicates
   -- with the Job Executer Task by road_marker_done and road_marker_next.
   task type Roadmarker_Task_T is
      entry Construct
        (get_sensor_value_a   : in get_roadmarker_sensor_value_access);
      entry road_marker_done (Signal : out Road_Marker_Done_T);
      entry road_marker_next (Signal : in Road_Marker_Next_T);
   end Roadmarker_Task_T;

   type Roadmarker_Task_Acces_T is access Roadmarker_Task_T;
end Roadmarker;
