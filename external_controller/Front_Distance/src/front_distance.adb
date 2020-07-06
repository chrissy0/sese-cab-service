pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Front_Distance is

   procedure Log_Line(Message : String) is
   begin
      Put_Line("[front_distance] " & Message);
   end Log_Line;

   -- Returns the Front_Distance_Done_T Signal from sensor data.
   -- It returns FD_FAULT_S when all sensor types are faulty. A sensor type
   -- is fault, when every sensor of this type at one position is faulty.
   -- If a sensor type is not faulty and at least one sensor detects an object,
   -- the function returns FRONT_BLOCKED. In every other case, it returns
   -- FRONT_CLEAR.
   function calculate_output
     (
      all_sensor_values : in All_Sensor_Values_Array_T;
      threshholds       : in Threshhold_Array_T
     ) return Front_Distance_Done_t
   is

      type minimal_position_values_t is array (Sensor_Type_T, Sensor_Position_T) of Long_Float;
      type type_boolean_array_t is array (Sensor_Type_T) of Boolean;
      is_sensor_type_fault    : type_boolean_array_t;
      object_detected         : type_boolean_array_t;
      is_front_distance_error : Boolean := True;
      front_blocked           : Boolean := False;
      Output                  : Front_Distance_Done_t;
   begin
      -- check each type for error
      -- error: -1 for all sensors at the same position
      for typ in Sensor_Type_T loop
         object_detected(typ) := False;
         for pos in Sensor_Position_T loop
            -- default: no error detected for (typ, num)
            is_sensor_type_fault(typ) := True;
            for num in Sensor_Number_T loop
               -- if one valid value for all nums is found, then there is no
               -- error for this sensor type for this postion
               if all_sensor_values(typ, pos, num) >= 0.0 then
                  is_sensor_type_fault(typ) := False;

                  -- if one sensor detects object, there is an object
                  if all_sensor_values(typ, pos, num) < threshholds(typ) then
                     object_detected(typ) := True;
                  end if;
               end if;
            end loop;

            -- when every num for one position is faulty, the sensor type is
            -- faulty. No need to check the other positions of this type.
            -- Go straight to next type.
            exit when is_sensor_type_fault(typ);
         end loop;
      end loop;

      for typ in Sensor_Type_T loop
         is_front_distance_error :=
           is_front_distance_error and is_sensor_type_fault(typ);

         -- only consider blocked when not faulty sensors
         front_blocked := front_blocked or (object_detected(typ) and not is_sensor_type_fault(typ));
      end loop;

      case is_front_distance_error is
         when True =>
            Output := FD_FAULT_S;
         when False =>
            case front_blocked is
               when True =>
                  Output := FRONT_BLOCKED_S;
               when False =>
                  Output := FRONT_CLEAR_S;
            end case;
      end case;
      return Output;
   end calculate_output;

   procedure retrieve_all_sensor_values
     (
      all_sensor_values : out All_Sensor_Values_Array_T;
      get_sensor_value  : in get_sensor_value_access
     )
   is
   begin
      for typ in Sensor_Type_T loop
         for pos in Sensor_Position_T loop
            for num in Sensor_Number_T loop
               all_sensor_values(typ, pos, num) := get_sensor_value(typ, pos, num);
            end loop;
         end loop;
      end loop;
   end retrieve_all_sensor_values;


   ---------------------------
   -- Front_Distance_Task_T --
   ---------------------------

   task body Front_Distance_Task_T is
      threshholds                      : Threshhold_Array_T;
      get_sensor_value_func            : get_sensor_value_access;
      Motor_Controller_Task            : Motor_Controller_Task_Access_T;
      running                          : Boolean := True;
      Output                           : Front_Distance_Done_t;
      Next_Signal                      : Front_Distance_Next_t;
      active_sensor_type               : Sensor_Type_T := US;
      all_sensor_values                : All_Sensor_Values_Array_T;
      timeout                          : Duration;
   begin
      Log_Line("Starting Front_Distance Thread.");
      Log_Line("Front_Distance: Waiting for Construct...");
      accept Construct
        (get_sensor_value_a       : in get_sensor_value_access;
         us_thresh                : in Long_Float;
         ir_thresh                : in Long_Float;
         Motor_Controller_Task_A  : in Motor_Controller_Task_Access_T;
         timeout_v                : in Duration
        )
      do
         threshholds(IR) := ir_thresh;
         threshholds(US) := us_thresh;
         get_sensor_value_func := get_sensor_value_a;
         Motor_Controller_Task := Motor_Controller_Task_A;
         timeout := timeout_v;
      end Construct;
      Log_Line("... Front_Distance constructor done");

      -- main loop
      while running loop

         -- retrieve sensor values
         retrieve_all_sensor_values(all_sensor_values, get_sensor_value_func);

         -- calculate output:
         Output := calculate_output(all_sensor_values => all_sensor_values,
                                    threshholds       => threshholds);

        -- Log_Line("Front_Distance: sending front_distance_done with value " & Output'Image & "...");
         -- Output signal
         select
           Motor_Controller_Task.front_distance_done(Output);
         then abort
            delay timeout;
            Log_Line("front_distance_done timed out, shutting Front_Distance down...");
            running := False;
            goto Continue;
         end select;
         --Log_Line("Front_Distance: ...  recieved!");


         --Log_Line("Front_Distance: sending front_distance_next...");
         -- wait for signal to start next iteration
         select
            delay timeout;
            Log_Line("lane_detection_next timed out, shutting Front_Distance down...");
            running := False;
            goto Continue;
         then abort
           Motor_Controller_Task.front_distance_next(Signal => Next_Signal);
         end select;
         --Log_Line("Front_Distance: ...  recieved!");


         <<Continue>>
      end loop;
      Log_Line
        ("Front_Distance shutting down. So long, and thanks for all the distance (?)");


   end Front_Distance_Task_T;

end Front_Distance;
