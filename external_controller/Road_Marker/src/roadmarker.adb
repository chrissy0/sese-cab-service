pragma Ada_2012;
with Roadmarker_Functions; use Roadmarker_Functions;

package body Roadmarker is

   procedure Log_Line(msg : String) is
   begin
      Put_Line("[Road_Marker]" & msg);
   end Log_Line;

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


   ------------------------------
   -- check_error_sensor_array --
   ------------------------------

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


   ----------------------
   -- read_road_marker --
   ----------------------

   function read_road_marker
     (
      sensors : All_Sensor_Values_Array_T;
      is_backup_sensor  : Boolean
     ) return Road_Marker_Done_T
   is
      -- initial value: not on road marker
      Current_Marker : Road_Marker_Done_T := RM_no_road_marker;
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


   -------------------
   -- empty_history --
   -------------------

   procedure empty_history(history: in out Road_Marker_History_T)
   is
   begin
      for I in Road_Marker_Done_T loop
         history(I) := 0;
      end loop;
   end empty_history;

   -- Returns the road marker with the history entry, prioritising road markers
   -- with higher road marker numbers (15-0). If there is no such entry,
   -- return no road marker.
   function calculate_output_from_history
     (
      history   : in Road_Marker_History_T
     ) return Road_Marker_Done_T
   is
      max_value : Integer   := 0;
      output    : Road_Marker_Done_T := RM_no_road_marker;
   begin
      for I in reverse 0 .. 15 loop
         if history(I) > max_value then
            max_value := history(I);
            output    := I;
         end if;
      end loop;

      return output;
   end calculate_output_from_history;


   ----------------------
   -- calculate_output --
   ----------------------

   function calculate_output
     (
      all_sensor_values : All_Sensor_Values_Array_T;
      history           : in out Road_Marker_History_T
     ) return Road_Marker_Done_T
   is
      faulty     : Boolean;
      Output     : Road_Marker_Done_T;
      current_RM : Road_Marker_Done_T;
   begin
      -- check both RM sensors
      for I in Boolean loop
         current_RM := read_road_marker(all_sensor_values, I);

         faulty := check_error_sensor_array(all_sensor_values, I);
         exit when not faulty;
      end loop;

      history(current_RM) := history(current_RM) + 1;

      case faulty is
         when True =>
            Output := RM_system_error;
            empty_history(history);
         when False =>
            if current_RM = RM_no_road_marker then
               Output := calculate_output_from_history(history);
               empty_history(history);
            else
               -- keep history
               Output := RM_no_road_marker;
            end if;

      end case;

      return Output;
   end calculate_output;


   type Roadmarker_Sensor_Array is array (Roadmarker_Sensor_ID_T) of Long_Float;


   ------------------------
   -- Roadmarkeer_Task_T --
   ------------------------

   task body Roadmarker_Task_T is
      Output                        : Integer := 0;
      all_sensor_values             : All_Sensor_Values_Array_T;
      get_sensor_value              : get_roadmarker_sensor_value_access;
      running                       : Boolean := True;
      is_on_RM                      : Boolean := True;
      history                       : Road_Marker_History_T;
   begin

      accept Construct
        (get_sensor_value_a   : in get_roadmarker_sensor_value_access)
      do
         get_sensor_value  := get_sensor_value_a;
         empty_history(history => history);
      end Construct;

      while (running) loop

         -- Read sensor values
         -- Put_Line ("Reading Sensor data ...");
         retrieve_all_sensor_values(all_sensor_values, get_sensor_value);

         -- TODO signal system error, when marker fail.
         is_on_RM := False;

         Output := calculate_output(all_sensor_values, history);

         select
            accept road_marker_done(Signal : out Road_Marker_Done_T) do
               if Output < 16 then
                  Log_Line(Output'Image);
               end if;

               Signal := Output;
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
