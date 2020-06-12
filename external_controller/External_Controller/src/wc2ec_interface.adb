pragma Ada_2012;
with Motor_Controller; use Motor_Controller;
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

end WC2EC_Interface;
