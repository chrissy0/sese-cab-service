
with WC2EC; use WC2EC;
procedure Main is
   wc2ec_thread : wc2ec_thread_access_t;
begin
   wc2ec_thread := new wc2ec_thread_t;

       for I in 0..10 loop
      delay 1.0;
   end loop;

   WC2EC.set_motor_sensor_data("wheel1", 6.0);
   WC2EC.set_motor_sensor_data("wheel2", 6.0);
   WC2EC.set_motor_sensor_data("wheel3", 6.0);
   WC2EC.set_motor_sensor_data("wheel4", 6.0);
    for I in 0..10 loop
      delay 1.0;
    end loop;

   null;
end Main;
