with Ada.Text_IO;      use Ada.Text_IO;

package Roadmarker is

   -- 0-15 Road Marker IDs
   -- 16   System Error
   -- 17   No Road Marker
   subtype Road_Marker_Done_T is Integer range 0 .. 17;

   type Road_Marker_Next_T is (EMPTY_S, SHUTDOWN_S);

   type Roadmarker_Sensor_ID_T is
     (FRONT_LEFT, FRONT_RIGHT, BEHIND_LEFT, BEHIND_RIGHT, RM_FL, RM_FR, RM_BL, RM_BR);


   type get_roadmarker_sensor_value_access is access
     function (ID : in Roadmarker_Sensor_ID_T) return Long_Float;


   task type Roadmarker_Task_T is
      entry Construct
        (get_sensor_value_a   : in get_roadmarker_sensor_value_access);
      entry road_marker_done (Signal : out Road_Marker_Done_T);
      entry road_marker_next (Signal : in Road_Marker_Next_T);
   end Roadmarker_Task_T;

   type Roadmarker_Task_Acces_T is access Roadmarker_Task_T;
end Roadmarker;
