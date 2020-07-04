pragma Ada_2012;
with Roadmarker_Functions; use Roadmarker_Functions;

package body Roadmarker is

   procedure Log_Line(msg : String) is
   begin
      Put_Line("[Road_Marker]" & msg);
   end Log_Line;

   -- array of sensor values. Second index true => access backup sensor
   type All_Sensor_Values_Array_T is array (Roadmarker_Sensor_ID_T, Boolean) of Long_Float;

   -- set all_sensor_values with new values from driver
   procedure retrieve_all_sensor_values
     (
      all_sensor_values : out All_Sensor_Values_Array_T;
      get_sensor_value  : in get_roadmarker_sensor_value_access
     )
   is
   begin
      for ID in Roadmarker_Sensor_ID_T loop
         for I in Boolean loop
            all_sensor_values(ID, I) := get_sensor_value(ID, I);
         end loop;
      end loop;
   end retrieve_all_sensor_values;


   -- Return true if the default or backup sensor array contains an error,
   -- depending on the is_backup_sensor value.
   -- @param all_sensor_value array filled with sensor values to be tested
   -- @
   function check_error_sensor_array
     (
      all_sensor_values : All_Sensor_Values_Array_T;
      is_backup_sensor  : Boolean
     ) return Boolean
   is
      is_error : Boolean := False;
   begin
      for ID in Roadmarker_Sensor_ID_T loop
         is_error := all_sensor_values(ID, is_backup_sensor) = -1.0;
         exit when is_error;
      end loop;

      return is_error;
   end check_error_sensor_array;

   function read_road_marker
     (
      sensors : All_Sensor_Values_Array_T;
      is_backup_sensor  : Boolean
     ) return Road_Marker_Done_T
   is
      -- initial value: not on road marker
      Current_Marker : Road_Marker_Done_T := 17;
   begin
      if
        on_Road_Marker( sensors(FRONT_LEFT, is_backup_sensor),
                        sensors(FRONT_RIGHT, is_backup_sensor),
                        sensors(BEHIND_LEFT, is_backup_sensor),
                        sensors(BEHIND_RIGHT, is_backup_sensor)
                      )
      then
         Current_Marker :=
           Integer( get_Road_MarkerID( sensors(RM_FL, is_backup_sensor),
                                       sensors(RM_FR, is_backup_sensor),
                                       sensors(RM_BL, is_backup_sensor),
                                       sensors(RM_BR, is_backup_sensor) ) );
      end if;

      return Integer(Current_Marker);
   end read_road_marker;

   -- check both normal and backup sensors for error. If both are in an error
   -- state, output RM_FAULT_S. The active sensor is the normal one. In case of
   -- an error in the normal sensor, the backup sensors are the active sensors.
   -- If the outer active sensors detect a roadmarker, read the roadmarker ID
   -- from the inner sensors.
   -- Returns 17 on error, 16 when there is no road marker and else the
   -- road marker's section ID
   function calculate_output
     (
      all_sensor_values : All_Sensor_Values_Array_T
     ) return Road_Marker_Done_T
   is
      faulty : Boolean;
      Output       : Road_Marker_Done_T;
   begin
      for I in Boolean loop
         faulty := check_error_sensor_array(all_sensor_values, I);
         exit when not faulty;

         Output := read_road_marker(all_sensor_values, I);

      end loop;

      case faulty is
         when True =>
            -- system error
            return 16;
         when False =>
            return Output;
      end case;
   end calculate_output;


   type Roadmarker_Sensor_Array is array (Roadmarker_Sensor_ID_T) of Long_Float;

   task body Roadmarker_Task_T is
      Current_Marker                : Integer := 0;
      all_sensor_values             : All_Sensor_Values_Array_T;
      get_sensor_value              : get_roadmarker_sensor_value_access;
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
         retrieve_all_sensor_values(all_sensor_values, get_sensor_value);

         -- TODO signal system error, when marker fail.
         is_on_RM := False;

         Current_Marker := calculate_output(all_sensor_values);

         select
            accept road_marker_done(Signal : out Road_Marker_Done_T) do
               Signal := Current_Marker;
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
