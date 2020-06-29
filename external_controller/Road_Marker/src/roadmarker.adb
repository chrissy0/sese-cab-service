pragma Ada_2012;
with Roadmarker_Functions; use Roadmarker_Functions;
package body Roadmarker is

   procedure Log_Line(msg : String) is
   begin
      Put_Line("[Road_Marker]" & msg);
   end Log_Line;

   type Roadmarker_Sensor_Array is array (Roadmarker_Sensor_ID_T) of Long_Float;

   task body Roadmarker_Task_T is
      Current_Marker                : Long_Float := 0.0;
      get_sensor_value              : get_roadmarker_sensor_value_access;
      sensors                       : Roadmarker_Sensor_Array;
      running                       : Boolean := True;
      is_on_RM                      : Boolean := True;
   begin

      accept Construct
        (get_sensor_value_a   : in get_roadmarker_sensor_value_access)
      do
         get_sensor_value  := get_sensor_value_a;
      end Construct;

      while (running) loop

         -- Read sensor values
         -- Put_Line ("Reading Sensor data ...");
         for I in Roadmarker_Sensor_ID_T loop
            sensors(I) := get_sensor_value(I);
         end loop;

         -- TODO signal system error, when marker fail.
         is_on_RM := False;

         if
           on_Road_Marker(sensors(FRONT_LEFT), sensors(FRONT_RIGHT),
                          sensors(BEHIND_LEFT), sensors(BEHIND_RIGHT))
         then
            Current_Marker :=
              get_Road_MarkerID( sensors(RM_FL) , sensors(RM_FR) ,
                                 sensors(RM_BL) , sensors(RM_BR));
            is_on_RM := True;
         end if;

         select
            accept road_marker_done(Signal : out Road_Marker_Done_T) do
               Signal := Integer(Current_Marker);
            end road_marker_done;
         or
            delay 2.0;
            Log_Line("road_marker_done timed out!");
            running := false;
            goto Continue;
         end select;

         select
            accept road_marker_next(Signal : in Road_Marker_Next_T) do
               case Signal is
                  when EMPTY_S =>
                     null;

                  when SHUTDOWN_S =>
                     running := FALSE;

                  Log_Line("road_marker_next returned system error!");
               end case;
            end road_marker_next;
         or
            delay 2.0;
            Log_Line("road_marker_done timed out!");
            running := false;
            goto Continue;
         end select;


         <<Continue>>
      end loop;
   end  Roadmarker_Task_T;

end Roadmarker;
