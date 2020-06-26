pragma Ada_2012;
with Motor_Controller; use Motor_Controller;
with Front_Distance; use Front_Distance;
package body WC2EC_Interface is

   ---------------------
   -- set_motor_value --
   ---------------------

   procedure set_motor_value
     (ID : Motor_Controller.Motor_ID_T; Value : Long_Float)
   is
   begin
      case ID is
         when MOTOR_FRONT_LEFT =>
            WC2EC.set_motor_sensor_data("wheel1", Value);

         when MOTOR_FRONT_RIGHT =>
            WC2EC.set_motor_sensor_data("wheel2", Value);

         when MOTOR_BACK_LEFT =>
            WC2EC.set_motor_sensor_data("wheel3", Value);

         when MOTOR_BACK_RIGHT =>
            WC2EC.set_motor_sensor_data("wheel4", Value);
      end case;
   end set_motor_value;

   ------------------------------
   -- get_front_distance_value --
   ------------------------------

   function get_front_distance_value
     (ID : Front_Distance.Distance_Sensor_ID_T)
   return Long_Float is
   begin
      case ID is
         when CENTER_0 =>
            return WC2EC.get_distance_sensor_data("dist_c");
         when CENTER_1 =>
            return WC2EC.get_distance_sensor_data("dist_c2");

         when LEFT_0 =>
            return WC2EC.get_distance_sensor_data("dist_l");
         when LEFT_1 =>
            return WC2EC.get_distance_sensor_data("dist_l2");

         when RIGHT_0 =>
            return WC2EC.get_distance_sensor_data("dist_r");
         when RIGHT_1 =>
            return WC2EC.get_distance_sensor_data("dist_r2");
      end case;
   end get_front_distance_value;

end WC2EC_Interface;
