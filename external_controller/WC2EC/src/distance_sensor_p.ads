package distance_sensor_p is
 type distance_sensor_t is record
   distance : Long_Float;
 end record;
procedure print_Distance_Sensor(ds : distance_sensor_t);

end distance_sensor_p;
