pragma Ada_2012;
package body Lane_Detection is

   procedure Log_Line(Message : String) is
   begin
      Put_Line("[Lane_Detection] " & Message);
   end Log_Line;


   --------------------------------
   -- retrieve_all_sensor_values --
   --------------------------------

   procedure retrieve_all_sensor_values
     (
      get_line_sensor_value : in get_line_sensor_value_access;
      get_curb_sensor_value : in get_curb_sensor_value_access;
      get_wall_sensor_value : in get_wall_sensor_value_access;
      line_sensor_values : out Line_Sensor_Values_Array_T;
      curb_sensor_values : out Curb_Sensor_Values_Array_T;
      wall_sensor_values : out Wall_Sensor_Values_Array_T
     )
   is
   begin
      for pos in Line_Sensor_Position_T loop
         for I in Boolean loop
            line_sensor_values(pos, I) := get_line_sensor_value(pos, I);
         end loop;
      end loop;

      for pos in Curb_Sensor_Position_T loop
         for ori in Sensor_Orientation_T loop
            for I in Boolean loop
               curb_sensor_values(pos, ori, I) := get_curb_sensor_value(pos, ori, I);
            end loop;
         end loop;
      end loop;

      for ori in Sensor_Orientation_T loop
         for I in Boolean loop
            wall_sensor_values(ori, I) := get_wall_sensor_value(ori, I);
         end loop;
      end loop;
   end retrieve_all_sensor_values;


   ------------------
   -- detect_lanes --
   ------------------

   procedure detect_lanes
     (
      all_sensor_values    : Line_Sensor_Values_Array_T;
      threshhold           : Long_Float;
      detected_array       : out Line_Sensor_Detected_Array_T;
      sensor_array_failure : out Line_Sensor_Array_Failure_Array_T
     )
   is
   begin
      sensor_array_failure := (others => False);
      for pos in Line_Sensor_Position_T loop
         for I in Boolean loop
            detected_array(pos, I) := all_sensor_values(pos, I) < threshhold;
            sensor_array_failure(I) := sensor_array_failure(I) or all_sensor_values(pos, I) < 0.0;
         end loop;
      end loop;
   end detect_lanes;


   ------------------------------------------
   -- calculate_output_from_line_detection --
   ------------------------------------------

   function output_from_line_detection
     (
      detected_array       : Line_Sensor_Detected_Array_T;
      Leaning_Left         : Boolean;
      sensor_array_failure : Line_Sensor_Array_Failure_Array_T
     ) return Lane_Detection_Done_T
   is
   begin

      for I in Boolean loop
         -- if sensor array failed, dont use it.
         if not sensor_array_failure(I) then

            -- handle lean right
            if not Leaning_Left then
               if detected_array(RIGHT, I) or
                 ( detected_array(CENTER, I) and not detected_array(LEFT, I) )
               then
                  return ROTATE_RIGHT_S;
               elsif (detected_array(LEFT, I) and detected_array(CENTER, I)) then
                  return GO_STRAIGHT_S;
               elsif detected_array(LEFT, I) then
                  return ROTATE_LEFT_S;
               end if;
            else -- handle lean left and lean unkown
               if detected_array(LEFT, I) or
                 ( detected_array(CENTER, I) and not detected_array(RIGHT, I) )
               then
                  return ROTATE_LEFT_S;
               elsif (detected_array(RIGHT, I) and detected_array(CENTER, I)) then
                  return GO_STRAIGHT_S;
               elsif detected_array(RIGHT, I) then
                  return ROTATE_RIGHT_S;
               end if;
            end if;
         end if;

      end loop;
      -- if nothing was detected: System error
      return SYSTEM_ERROR_S;
   end output_from_line_detection;


   --------------------------------
   -- output_from_curb_detection --
   --------------------------------

   function output_from_curb_detection
     (
      curb_sensor_values : Curb_Sensor_Values_Array_T;
      wall_sensor_values : Wall_Sensor_Values_Array_T;
      curb_threshhold    : Long_Float;
      wall_threshhold    : Long_Float
     ) return Lane_Detection_Done_T
   is
      Leaning_Left        : Boolean := True;
      Output              : Lane_Detection_Done_T := SYSTEM_ERROR_S;
      Wall_Sensor_Failure : Boolean;
   begin
      -- if wall sensor failed, we cannot operate in this mode
      for I in Boolean loop
         Wall_Sensor_Failure := False;
         for pos in Curb_Sensor_Position_T loop
            if wall_sensor_values(RIGHT, I) < 0.0 then

               if Wall_Sensor_Failure then
                  return System_Error_S;
               else
                  Wall_Sensor_Failure := True;
               end if;

            end if;
         end loop;
      end loop;

      -- if there is a wall on the right side -> go right
      for I in Boolean loop
         if wall_sensor_values(RIGHT, I) <= curb_threshhold then
            Leaning_Left := False;
         end if;
      end loop;

      -- TODO
      return SYSTEM_ERROR_S;
   end output_from_curb_detection;


   ----------------------
   -- calculate_output --
   ----------------------

   function calculate_output
     (
      line_sensor_values : Line_Sensor_Values_Array_T;
      curb_sensor_values : Curb_Sensor_Values_Array_T;
      wall_sensor_values : Wall_Sensor_Values_Array_T;
      Leaning_Left       : Boolean;
      line_threshhold    : Long_Float;
      curb_threshhold    : Long_Float;
      wall_threshhold    : Long_Float
     ) return Lane_Detection_Done_T
   is
      detected_array     : Line_Sensor_Detected_Array_T;
      Output             : Lane_Detection_Done_T;
      line_failure_array : Line_Sensor_Array_Failure_Array_T;
   begin
      detect_lanes(all_sensor_values    => line_sensor_values,
                   threshhold           => line_threshhold,
                   detected_array       => detected_array,
                   sensor_array_failure => line_failure_array);

      Output := output_from_line_detection(detected_array, Leaning_Left, line_failure_array);

      if Output = SYSTEM_ERROR_S then
         Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values,
                                              wall_sensor_values => wall_sensor_values,
                                              curb_threshhold         => curb_threshhold,
                                              wall_threshhold    => wall_threshhold);
      end if;

      return Output;
   end calculate_output;


   ---------------------------
   -- Lane_Detection_Taks_T --
   ---------------------------

   task body Lane_Detection_Taks_T is
      Curb_Threshhold       : Long_Float;
      Line_Threshhold       : Long_Float;
      Wall_Threshhold       : Long_Float;
      Motor_Controller_Task : Motor_Controller_Task_Access_T;
      next_signal           : Lane_Detection_Next_T := NO_LEAN_S;
      running               : Boolean := True;
      Leaning_Left          : Boolean := True;
      Output                : Lane_Detection_Done_T;

      line_sensor_values    : Line_Sensor_Values_Array_T;
      curb_sensor_values    : Curb_Sensor_Values_Array_T;
      wall_sensor_values    : Wall_Sensor_Values_Array_T;
      get_line_sensor_value : get_line_sensor_value_access;
      get_curb_sensor_value : get_curb_sensor_value_access;
      get_wall_sensor_value : get_wall_sensor_value_access;
      timeout               : Duration;

   begin

      Log_Line("Starting Thread.");
      Log_Line("Waiting for Construct...");
      accept Construct
        (
         Curb_Threshhold_v       : in Long_Float;
         Line_Threshhold_v       : in Long_Float;
         Wall_Threshhold_v       : in Long_Float;
         Motor_Task_A            : in Motor_Controller_Task_Access_T;
         get_line_sensor_value_a : in get_line_sensor_value_access;
         get_curb_sensor_value_a : in get_curb_sensor_value_access;
         get_wall_sensor_value_a : in get_wall_sensor_value_access;
         timeout_v               : in Duration
        )
      do
         Motor_Controller_Task := Motor_Task_A;
         Curb_Threshhold       := Curb_Threshhold_v;
         Line_Threshhold       := Line_Threshhold_v;
         Wall_Threshhold       := Wall_Threshhold_v;
         get_line_sensor_value := get_line_sensor_value_a;
         get_curb_sensor_value := get_curb_sensor_value_a;
         get_wall_sensor_value := get_wall_sensor_value_a;
         timeout               := timeout_v;
      end Construct;
      Log_Line("... constructor done");

      while running loop
         retrieve_all_sensor_values(get_line_sensor_value => get_line_sensor_value,
                                    get_curb_sensor_value => get_curb_sensor_value,
                                    get_wall_sensor_value => get_wall_sensor_value,
                                    line_sensor_values    => line_sensor_values,
                                    curb_sensor_values    => curb_sensor_values,
                                    wall_sensor_values    => wall_sensor_values);


         output := calculate_output(line_sensor_values => line_sensor_values,
                                    curb_sensor_values => curb_sensor_values,
                                    wall_sensor_values => wall_sensor_values,
                                    Leaning_Left       => Leaning_Left,
                                    line_threshhold    => Line_Threshhold,
                                    curb_threshhold    => Curb_Threshhold,
                                    wall_threshhold    => Wall_Threshhold);

         select
           Motor_Controller_Task.lane_detection_done(Output);
         then abort
            delay timeout;
            Log_Line("lane_detection_done timed out, shutting down...");
            running := False;
            goto Continue;
         end select;
         --Log_Line(" ... lane_detection_done recieved!");

        -- Log_Line ("Waiting for lane_detection_next");
          -- wait for all signals to be processed
         select
            Motor_Controller_Task.lane_detection_next(next_signal);


            case next_signal is
            when LEAN_LEFT_S =>
               Leaning_Left := True;
            when LEAN_RIGHT_S=>
               Leaning_Left := False;
            when NO_LEAN_S =>
               Leaning_Left := True;
            when EMPTY_S =>
               null;
            when SHUTDOWN_S =>
               Leaning_Left := True;
               running := False;
               goto Continue;
            end case;

         then abort
            delay timeout;
            Log_Line("lane_detection_next out, shutting down...");
            running := False;
            goto Continue;
         end select;
         --Log_Line("... lane_detection_next recieved!");


         -- handle next signal
         <<Continue>>
      end loop;
      Log_Line("Shutting down. So long, and thanks for all the lanes!");
   end Lane_Detection_Taks_T;

end Lane_Detection;
