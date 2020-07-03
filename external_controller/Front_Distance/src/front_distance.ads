with Motor_Controller; use Motor_Controller;
package Front_Distance is

   type Sensor_Position_T is
     (CENTER, LEFT, RIGHT);

   subtype Sensor_Number_T is Integer range 0..1;

   type Sensor_Type_T is (IR, US);

   type get_sensor_value_access is access
     function
       (
        typ : in Sensor_Type_T;
        pos : in Sensor_Position_T;
        num : in Sensor_Number_T
       ) return Long_Float;


   task type Front_Distance_Task_T is
      entry Construct
        (
         get_sensor_value_a               : in get_sensor_value_access;
         us_thresh                        : in Long_Float;
         ir_thresh                        : in Long_Float;
         Motor_Controller_Task_A          : in Motor_Controller_Task_Access_T
        );
   end Front_Distance_Task_T;

end Front_Distance;
