with Ada.Text_IO; use Ada.Text_IO;
package body distance_sensor_p is

procedure print_Distance_Sensor(ds : distance_sensor_t) is
 begin
     Ada.Text_IO.Put_Line (Long_Float'Image(ds.distance));
 end print_Distance_Sensor;


end distance_sensor_p;
