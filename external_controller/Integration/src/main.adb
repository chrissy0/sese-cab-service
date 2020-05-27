

with Ada.Text_IO; use Ada.Text_IO;

with WC2EC; use WC2EC;

with Ada.Strings.Fixed;

--use WC2EC.sensor_map_p;
with distance_sensor_p; use distance_sensor_p;

procedure Main is



   r : wc2ec_thread_access_t;
   ds : distance_sensor_p.distance_sensor_t;

begin
  Put_Line("STUFF");
   r := new wc2ec_thread_t; -- allocate and start thread
   Put_Line("STUFF");
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
   ds := WC2EC.get_distance_sensor_data("inf_cent");
   Put_Line("value: " & Long_Float'Image(ds.distance));

   end Main;
