package WC2EC is

   type distance_sensor is record
      distance : Long_Float;
   end record;

   WC2EC_SENSOR_DATA_ENTRIES: constant Integer       := 16;
   type distence_sensor_A is array(1 .. WC2EC_SENSOR_DATA_ENTRIES) of distance_sensor;

   --function get_distance_sensor_data(sensor_name : String) return distance_sensor_A;
   --procedure set_motor_sensor_data(sensor_name : String; velocity : Long_Float);


end WC2EC;
