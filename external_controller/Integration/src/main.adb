

with Ada.Text_IO; use Ada.Text_IO;

with WC2EC; use WC2EC;

with Ada.Strings.Fixed;
with Webots_Function; use Webots_Function;

--use WC2EC.sensor_map_p;
with distance_sensor_p; use distance_sensor_p;

procedure Main is
   r : wc2ec_thread_access_t;
   rc : Standard.Integer;
   leftSpeed :  Long_Float;
   rightSpeed :  Long_Float;
   leftSpeed_old : Long_Float;
   rightSpeed_old : Long_Float;

begin
  Put_Line("STUFF");
   r := new wc2ec_thread_t; -- allocate and start thread
   leftSpeed_old := 0.0;
   rightSpeed_old := 0.0;

   while WC2EC.ready /= true loop
      delay 1.0;
   end loop;

   for I in 0..200000 loop
      rc := Webots_Function.follow_Line(6.0, leftSpeed, rightSpeed);
      if (leftSpeed_old /= leftSpeed) then
         WC2EC.set_motor_sensor_data("wheel1", leftSpeed);
         WC2EC.set_motor_sensor_data("wheel3", leftSpeed);
         leftSpeed_old := leftSpeed;
      end if;
      if (rightSpeed_old /= rightSpeed) then
        WC2EC.set_motor_sensor_data("wheel2", rightSpeed);
        WC2EC.set_motor_sensor_data("wheel4", rightSpeed);
        rightSpeed_old := rightSpeed;
      end if;

      delay 0.01;
   end loop;

   end Main;
