with Motor_Controller; use Motor_Controller;
package Front_Distance is

   type Distance_Sensor_ID_T is
     (CENTER_0, CENTER_1, LEFT_0, LEFT_1, RIGHT_0, RIGHT_1);

   type get_distance_sensor_value_t is access
     function (ID : in Distance_Sensor_ID_T) return Long_Float;

   type Distance_Sensor_Values_Array_T is array (Distance_Sensor_ID_T) of Long_Float;

   task type Front_Distance_Task_T is
      entry Construct
        (get_distance_sensor_value_access : in get_distance_sensor_value_t;
         us_thresh                        : in Long_Float;
         Motor_Controller_Task_A : in Motor_Controller_Task_Access_T
        );
   end Front_Distance_Task_T;

end Front_Distance;
