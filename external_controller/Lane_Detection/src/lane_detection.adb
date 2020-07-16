-- @summary
-- Lane detection controller package body.
--
-- @author Julian Hartmer
-- @description
-- This package controls the lane detection by pulling and evaluating the lane
-- detection sensor values. Communicates with Motor Controller Task.

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
      line_sensor_values    : out Line_Sensor_Values_Array_T;
      curb_sensor_values    : out Curb_Sensor_Values_Array_T;
      wall_sensor_values    : out Wall_Sensor_Values_Array_T
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
      detected_array       : out Line_Sensor_Detected_Array_T;
      sensor_array_failure : out Line_Sensor_Array_Failure_Array_T
     )
   is
   begin
      sensor_array_failure := (others => False);
      for pos in Line_Sensor_Position_T loop
         for I in Boolean loop
            detected_array(pos, I) := all_sensor_values(pos, I) < LINE_FOLLOW_THRESHHOLD;
            sensor_array_failure(I) := sensor_array_failure(I) or (all_sensor_values(pos, I) < 0.0);
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


   function detect_curb
     (
      curb_sensor_values       : Curb_Sensor_Values_Array_T
     ) return Curb_Detected_Array_T
   is
      curb_detected_array      : Curb_Detected_Array_T;
   begin
      curb_detected_array := (others => FAILURE);
      for ori in Sensor_Orientation_T loop
         for I in Boolean loop
            if curb_detected_array(ori) = FAILURE then
               if curb_sensor_values(FRONT, ori, I) < 0.0 then
                  curb_detected_array(ori) := FAILURE;
               elsif curb_sensor_values(FRONT, ori, I) < CURB_MIN_DETECTION_RANGE then
                  curb_detected_array(ori) := DETECTED_TOO_CLOSE;
               elsif curb_sensor_values(FRONT, ori, I) < CURB_MAX_DETECTION_RANGE then
                  curb_detected_array(ori) := DETECTED;
               elsif curb_sensor_values(FRONT, ori, I) < CURB_MAX_VALUE then
                  curb_detected_array(ori) := DETECTED_TOO_FAR;
               else
                  curb_detected_array(ori) := NOT_DETECTED;
               end if;

            end if;

         end loop;

      end loop;

      return curb_detected_array;

   end detect_curb;

   --------------------------------
   -- output_from_curb_detection --
   --------------------------------

   function output_from_curb_detection
     (
      curb_sensor_values  : Curb_Sensor_Values_Array_T
     ) return Lane_Detection_Done_T
   is
      Output              : Lane_Detection_Done_T := SYSTEM_ERROR_S;
      sensor_value        : Long_Float            := SENSOR_FAULT;
      curb_detected_array : Curb_Detected_Array_T;
   begin
      curb_detected_array := detect_curb(curb_sensor_values => curb_sensor_values);

      if curb_detected_array(LEFT) = FAILURE or curb_detected_array(LEFT) = FAILURE then
         Output := SYSTEM_ERROR_S;
      else
         case curb_detected_array(LEFT) is
            when FAILURE =>
               Output := SYSTEM_ERROR_S;
            when DETECTED_TOO_CLOSE =>
               Output := ROTATE_RIGHT_S;
            when DETECTED =>
               Output := GO_STRAIGHT_S;
            when DETECTED_TOO_FAR =>
               if curb_detected_array(RIGHT) = DETECTED_TOO_CLOSE
                 or curb_detected_array(RIGHT) = DETECTED
               then
                  Output := GO_STRAIGHT_S;
               else

                  Output := ROTATE_LEFT_S;
               end if;

            when NOT_DETECTED =>
               case curb_detected_array(RIGHT) is
                  when DETECTED_TOO_CLOSE =>
                     Output := ROTATE_LEFT_S;
                  when DETECTED =>
                     Output := GO_STRAIGHT_S;
                  when DETECTED_TOO_FAR =>
                     Output := ROTATE_RIGHT_S;
                  when NOT_DETECTED =>
                     Output := GO_STRAIGHT_S;
                  when FAILURE =>
                     Output := SYSTEM_ERROR_S;
               end case;
         end case;
      end if;
      return Output;
   end output_from_curb_detection;


   ------------------------------
   -- get_lean_from_line_color --
   ------------------------------

   function get_lean_from_line_color
     (
      line_sensor_values   : Line_Sensor_Values_Array_T;
      sensor_array_failure : Line_Sensor_Array_Failure_Array_T;
      old_lean             : Boolean
     ) return Boolean
   is
      Output : Boolean := True;
   begin
      for I in Boolean loop
         -- if sensor array failed, dont use it.
         if not sensor_array_failure(I) then
            for pos in Line_Sensor_Position_T loop
               if
                 line_sensor_values(pos, I) < line_dark_grey + line_delta and
                 line_dark_grey - line_delta < line_sensor_values(pos, I)
               then
                  return old_lean;
               elsif
                 line_sensor_values(pos, I) < line_light_grey + line_delta and
                 line_light_grey - line_delta < line_sensor_values(pos, I)
               then
                  Output := False;
               elsif
                 line_sensor_values(pos, I) < line_black + line_delta and
                 line_black - line_delta < line_sensor_values(pos, I)
               then
                  Output := True;
               end if;
            end loop;
         end if;
      end loop;
      return Output;
   end get_lean_from_line_color;


   ----------------------
   -- calculate_output --
   ----------------------

   function calculate_output
     (
      line_sensor_values : Line_Sensor_Values_Array_T;
      curb_sensor_values : Curb_Sensor_Values_Array_T;
      wall_sensor_values : Wall_Sensor_Values_Array_T;
      is_lean_from_line  : Boolean;
      Leaning_Left       : in out Boolean;
      is_curb_detection  : out Boolean
     ) return Lane_Detection_Done_T
   is
      detected_array     : Line_Sensor_Detected_Array_T;
      Output             : Lane_Detection_Done_T;
      line_failure_array : Line_Sensor_Array_Failure_Array_T;
   begin

      is_curb_detection := False;

      detect_lanes(all_sensor_values    => line_sensor_values,
                   detected_array       => detected_array,
                   sensor_array_failure => line_failure_array);

      if is_lean_from_line then
         Leaning_Left := get_lean_from_line_color(line_sensor_values   => line_sensor_values,
                                                  sensor_array_failure => line_failure_array,
                                                  old_lean             => Leaning_Left);
      end if;

      Output := output_from_line_detection(detected_array, Leaning_Left, line_failure_array);

      if Output = SYSTEM_ERROR_S then
         Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);
         is_curb_detection := True;
      end if;

      return Output;
   end calculate_output;


   ---------------------------
   -- Lane_Detection_Taks_T --
   ---------------------------

   task body Lane_Detection_Taks_T is
      Motor_Controller_Task : Motor_Controller_Task_Access_T;
      next_signal           : Lane_Detection_Next_T := LEAN_LEFT_S;
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
      is_lean_from_line     : Boolean := False;
      is_curb_detection     : Boolean := False;
   begin

      Log_Line("Starting Thread.");
      Log_Line("Waiting for Construct...");
      accept Construct
        (
         Motor_Task_A            : in Motor_Controller_Task_Access_T;
         get_line_sensor_value_a : in get_line_sensor_value_access;
         get_curb_sensor_value_a : in get_curb_sensor_value_access;
         get_wall_sensor_value_a : in get_wall_sensor_value_access;
         timeout_v               : in Duration
        )
      do
         Motor_Controller_Task := Motor_Task_A;
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
                                    is_lean_from_line  => is_lean_from_line,
                                    is_curb_detection  => is_curb_detection);
         select
           Motor_Controller_Task.lane_detection_done(Output, is_curb_detection);
         then abort
            delay timeout;
            Log_Line("lane_detection_done timed out, shutting down...");
            running := False;
            goto Continue;
         end select;
         select
            Motor_Controller_Task.lane_detection_next(next_signal);


            case next_signal is
            when LEAN_LEFT_S =>
               Leaning_Left := True;
               is_lean_from_line := False;
            when LEAN_RIGHT_S=>
               Leaning_Left := False;
               is_lean_from_line := False;
            when EMPTY_S =>
               null;
            when LEAN_FROM_LINE =>
               is_lean_from_line := True;
            when SHUTDOWN_S =>
               running := False;
               goto Continue;
            end case;

         then abort
            delay timeout;
            Log_Line("lane_detection_next out, shutting down...");
            running := False;
            goto Continue;
         end select;
         <<Continue>>
      end loop;
      Log_Line("Shutting down. So long, and thanks for all the lanes!");
   end Lane_Detection_Taks_T;

end Lane_Detection;
