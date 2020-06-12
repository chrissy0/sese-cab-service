with WC2EC;
with Motor_Controller;
package WC2EC_Interface is

   procedure set_motor_value (ID: Motor_Controller.Motor_ID_T;
                              Value : Long_Float);

end WC2EC_Interface;
