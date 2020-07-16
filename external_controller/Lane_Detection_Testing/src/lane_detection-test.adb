-- @summary
-- Lane Detection controller child unit test package body.
--
-- @author Julian Hartmer

pragma Ada_2012;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;
package body Lane_Detection.Test is

   -----------------------
   -- test_detect_lanes --
   -----------------------

   procedure test_detect_lanes (T : in out Test) is
      pragma Unreferenced (T);

      sensor_values  : Line_Sensor_Values_Array_T;
      detected_array : Line_Sensor_Detected_Array_T;
      sensor_array_failure : Line_Sensor_Array_Failure_Array_T;

      one_above      : constant Long_Float := LINE_FOLLOW_THRESHHOLD + LINE_DELTA;
      one_below      : constant Long_Float := LINE_FOLLOW_THRESHHOLD - LINE_DELTA;
      max_val        : constant Long_Float := 1_000.0;
      failure        : constant Long_Float := -1.0;
   begin

      -- test all sensors off one below
      sensor_values := (others => (others => one_above));

      detect_lanes(all_sensor_values => sensor_values,
                   detected_array    => detected_array,
                   sensor_array_failure => sensor_array_failure
                  );
      for pos2 in Line_Sensor_Position_T loop
         for I2 in Boolean loop
            Assert(not detected_array(pos2, I2), "expected not detected, got detected");
            Assert(not sensor_array_failure(I2), "expected no sensor array failure, got sensor array failure");
         end loop;
      end loop;


      -- test all sensors off max value
      sensor_values := (others => (others => max_val));

      detect_lanes(all_sensor_values => sensor_values,
                   detected_array    => detected_array,
                   sensor_array_failure => sensor_array_failure);
      for pos2 in Line_Sensor_Position_T loop
         for I2 in Boolean loop
            Assert(not detected_array(pos2, I2), "expected not detected, got detected");
            Assert(not sensor_array_failure(I2), "expected no sensor array failure, got sensor array failure");
         end loop;
      end loop;

      -- test each sensor detecting once
      for pos in Line_Sensor_Position_T loop
         for I in Boolean loop
            sensor_values := (others => (others => one_above));
            sensor_values(pos, I) := one_below;
            detect_lanes(all_sensor_values => sensor_values,
                         detected_array    => detected_array,
                         sensor_array_failure => sensor_array_failure);

            for pos2 in Line_Sensor_Position_T loop
               for I2 in Boolean loop
                  if pos2 = pos and I2 = I then
                     Assert(detected_array(pos, I), "expected detected, got not detected");
                     Assert(not sensor_array_failure(I2), "expected no sensor array failure, got sensor array failure");
                  else
                     Assert(not detected_array(pos2, I2), "expected not detected, got detected");
                     Assert(not sensor_array_failure(I2), "expected no sensor array failure, got sensor array failure");
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;

      -- test all sensors on one below threshhold
      sensor_values := (others => (others => one_below));

      detect_lanes(all_sensor_values => sensor_values,
                   detected_array    => detected_array,
                   sensor_array_failure => sensor_array_failure);
      for pos2 in Line_Sensor_Position_T loop
         for I2 in Boolean loop
            Assert(detected_array(pos2, I2), "expected detected, got not detected");
            Assert(not sensor_array_failure(I2), "expected no sensor array failure, got sensor array failure");
         end loop;
      end loop;

      -- test all sensors on 0
      sensor_values := (others => (others => 0.0));

      detect_lanes(all_sensor_values => sensor_values,
                   detected_array    => detected_array,
                   sensor_array_failure => sensor_array_failure);
      for pos2 in Line_Sensor_Position_T loop
         for I2 in Boolean loop
            Assert(detected_array(pos2, I2), "expected detected, got not detected");
            Assert(not sensor_array_failure(I2), "expected no sensor array failure, got sensor array failure");
         end loop;
      end loop;

      -- test each sensor failing once
      for pos in Line_Sensor_Position_T loop
         for I in Boolean loop
            sensor_values         := (others => (others => one_below));
            sensor_values(pos, I) :=  failure;
            detect_lanes(all_sensor_values => sensor_values,
                         detected_array    => detected_array,
                         sensor_array_failure => sensor_array_failure);

            Assert(sensor_array_failure(I), "expected sensor array failure, but didnt get one!");
         end loop;
      end loop;
   end test_detect_lanes;

   -------------------------------------
   -- test_output_from_line_detection --
   -------------------------------------

   procedure test_output_from_line_detection (T : in out Test) is
      pragma Unreferenced (T);
      detected_array       : Line_Sensor_Detected_Array_T;
      Leaning_Left         : Boolean;
      sensor_array_failure : Line_Sensor_Array_Failure_Array_T;
      Output               : Lane_Detection_Done_T;
   begin
      -- check left sensor on while lean left => Go_Left, no sensor array failures
      sensor_array_failure := (others => False);
      Leaning_Left := True;


      for pos in Line_Sensor_Position_T loop
         -- if leaning left, and left line is detected, always rotate left
         detected_array := (others => (others => False));
         detected_array(LEFT, False) := True;
         detected_array(pos, False) := True;

         Output := output_from_line_detection(detected_array       => detected_array,
                                              Leaning_Left         => Leaning_Left,
                                              sensor_array_failure => sensor_array_failure);
         Assert(Output = ROTATE_LEFT_S,
                "Expected GO_LEFT_S, got something else!");
      end loop;

      -- not left
      -- not l, not c, not r => Error
      detected_array := (others => (others => False));


      Output := output_from_line_detection(detected_array       => detected_array,
                                           Leaning_Left         => Leaning_Left,
                                           sensor_array_failure => sensor_array_failure);
      Assert(Output = SYSTEM_ERROR_S,
             "Expected SYSTEM_ERROR_S, got something else!");

      -- not left
      -- not l, not c, r => Go_Right
      detected_array := (others => (others => False));
      detected_array(RIGHT, False) := True;


      Output := output_from_line_detection(detected_array       => detected_array,
                                           Leaning_Left         => Leaning_Left,
                                           sensor_array_failure => sensor_array_failure);
      Assert(Output = ROTATE_RIGHT_S,
             "Expected GO_RIGHT_S, got something else!");

      -- not left
      -- not l, c, not r => Go_Left
      detected_array := (others => (others => False));
      detected_array(CENTER, False) := True;


      Output := output_from_line_detection(detected_array       => detected_array,
                                           Leaning_Left         => Leaning_Left,
                                           sensor_array_failure => sensor_array_failure);
      Assert(Output = ROTATE_LEFT_S,
             "Expected GO_LEFT_S, got something else!");

      -- not left
      -- not l, c,  r => Go_Straight
      detected_array := (others => (others => False));
      detected_array(CENTER, False) := True;
      detected_array(RIGHT, False) := True;


      Output := output_from_line_detection(detected_array       => detected_array,
                                           Leaning_Left         => Leaning_Left,
                                           sensor_array_failure => sensor_array_failure);
      Assert(Output = GO_STRAIGHT_S,
             "Expected GO_STRAIGHT_S, got something else!");


      -- normal failing => get output from backup sensor
      sensor_array_failure(False) := True;

      -- test lean right on backup sensor
      Leaning_Left := False;

      for pos in Line_Sensor_Position_T loop
         -- if leaning left, and left line is detected, always rotate left
         detected_array := (others => (others => False));
         detected_array(RIGHT, True) := True;
         detected_array(pos, True) := True;

         Output := output_from_line_detection(detected_array       => detected_array,
                                              Leaning_Left         => Leaning_Left,
                                              sensor_array_failure => sensor_array_failure);
         Assert(Output = ROTATE_RIGHT_S,
                "Expected GO_RIGHT_S, got something else!");
      end loop;

      -- not left
      -- not l, not c, not r => Error
      detected_array := (others => (others => False));


      Output := output_from_line_detection(detected_array       => detected_array,
                                           Leaning_Left         => Leaning_Left,
                                           sensor_array_failure => sensor_array_failure);
      Assert(Output = SYSTEM_ERROR_S,
             "Expected SYSTEM_ERROR_S, got something else!");

      -- not r, not c, l => GO_LEFT_S
      detected_array := (others => (others => False));
      detected_array(LEFT, True) := True;


      Output := output_from_line_detection(detected_array       => detected_array,
                                           Leaning_Left         => Leaning_Left,
                                           sensor_array_failure => sensor_array_failure);
      Assert(Output = ROTATE_LEFT_S,
             "Expected GO_LEFT_S, got something else!");

      -- not r, c, not l => GO_RIGHT_S
      detected_array := (others => (others => False));
      detected_array(CENTER, TRUE) := True;


      Output := output_from_line_detection(detected_array       => detected_array,
                                           Leaning_Left         => Leaning_Left,
                                           sensor_array_failure => sensor_array_failure);
      Assert(Output = ROTATE_RIGHT_S,
             "Expected GO_RIGHT_S, got something else!");

      -- not r, c,  l => Go_Straight
      detected_array := (others => (others => False));
      detected_array(CENTER, TRUE) := True;
      detected_array(LEFT, TRUE)   := True;


      Output := output_from_line_detection(detected_array       => detected_array,
                                           Leaning_Left         => Leaning_Left,
                                           sensor_array_failure => sensor_array_failure);
      Assert(Output = GO_STRAIGHT_S,
             "Expected GO_STRAIGHT_S, got something else!");



   end test_output_from_line_detection;


   -----------------------------------
   -- test_get_lean_from_line_color --
   -----------------------------------

   procedure test_get_lean_from_line_color (T : in out Test) is
      sensor_values  : Line_Sensor_Values_Array_T;
      old_lean       : Boolean;
      sensor_array_failure : Line_Sensor_Array_Failure_Array_T;
   begin
      sensor_array_failure := (others => False);
      old_lean := True;
      sensor_values := (others => (others => LINE_BLACK));

      -- check old lean true and on black lane => return true

      Assert(get_lean_from_line_color(line_sensor_values   => sensor_values,
                                      sensor_array_failure => sensor_array_failure,
                                      old_lean             => old_lean),
             "expected lean left, got lean right");


      sensor_values := (others => (others => LINE_LIGHT_GREY));

      -- check old lean true and on light grey lane => return false

      Assert(not get_lean_from_line_color(line_sensor_values   => sensor_values,
                                          sensor_array_failure => sensor_array_failure,
                                          old_lean             => old_lean),
             "expected lean right, got lean left");


      -- old lean left false and on dark grey line => return false
      sensor_values := (others => (others => LINE_DARK_GREY));

      Assert(not get_lean_from_line_color(line_sensor_values => sensor_values,
                                      sensor_array_failure  => sensor_array_failure,
                                      old_lean              => False),
             "expected lean left, got lean right");


      -- old lean left TRUE and on dark grey line => return TRUE
      sensor_values := (others => (others => LINE_DARK_GREY));

      Assert(get_lean_from_line_color(line_sensor_values => sensor_values,
                                      sensor_array_failure  => sensor_array_failure,
                                      old_lean              => True),
             "expected lean left, got lean right");

      -- test all three lines detected and old lean true => return true
      sensor_values(LEFT, False) := LINE_LIGHT_GREY;
      sensor_values(CENTER, False) := LINE_DARK_GREY;
      sensor_values(RIGHT, False) := LINE_BLACK;

      Assert(get_lean_from_line_color(line_sensor_values => sensor_values,
                                      sensor_array_failure  => sensor_array_failure,
                                      old_lean              => True),
             "expected lean left, got lean right");

      Assert(not get_lean_from_line_color(line_sensor_values => sensor_values,
                                      sensor_array_failure  => sensor_array_failure,
                                      old_lean              => False),
             "expected lean right, got lean Left");

      -- test first sensor array failing
      sensor_array_failure(False) := True;

      -- check on black lane => return true
      sensor_values := (others => (others => LINE_BLACK));
      Assert(get_lean_from_line_color(line_sensor_values   => sensor_values,
                                      sensor_array_failure => sensor_array_failure,
                                      old_lean             => old_lean),
             "expected lean left, got lean right");

      -- check on light grey lane => return false
      sensor_values := (others => (others => LINE_LIGHT_GREY));

      Assert(not get_lean_from_line_color(line_sensor_values   => sensor_values,
                                          sensor_array_failure => sensor_array_failure,
                                          old_lean             => old_lean),
             "expected lean right, got lean left");

      -- check on dark grey lane and old lean false => return false
      sensor_values := (others => (others => LINE_DARK_GREY));

      Assert(not get_lean_from_line_color(line_sensor_values   => sensor_values,
                                          sensor_array_failure => sensor_array_failure,
                                          old_lean             => False),
             "expected lean right, got lean left");

      -- check on dark grey lane and old lean true => return true
      sensor_values := (others => (others => LINE_DARK_GREY));

      Assert(get_lean_from_line_color(line_sensor_values   => sensor_values,
                                          sensor_array_failure => sensor_array_failure,
                                          old_lean             => True),
             "expected lean left, got lean right");

      -- test all three lines detected and old lean true => return true
      sensor_values(LEFT, True) := LINE_LIGHT_GREY;
      sensor_values(CENTER, True) := LINE_DARK_GREY;
      sensor_values(RIGHT, True) := LINE_BLACK;

      Assert(get_lean_from_line_color(line_sensor_values => sensor_values,
                                      sensor_array_failure  => sensor_array_failure,
                                      old_lean              => True),
             "expected lean left, got lean right");

      Assert(not get_lean_from_line_color(line_sensor_values => sensor_values,
                                      sensor_array_failure  => sensor_array_failure,
                                      old_lean              => False),
             "expected lean right, got lean Left");
   end test_get_lean_from_line_color;



   -------------------------------------
   -- test_output_from_curb_detection --
   -------------------------------------

   procedure test_output_from_curb_detection (T : in out Test) is
      pragma Unreferenced (T);
      curb_sensor_values : Curb_Sensor_Values_Array_T;
      Output             : Lane_Detection_Done_T;
   begin
      -- all curb sensors detect curb => go straight
      curb_sensor_values := (others => (others => (others => 400.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = GO_STRAIGHT_S, "Expected GO_STRAIGHT_S, got " & Output'Image);

      -- 2 curb sensors fail => SYSTEM_ERROR_S
      curb_sensor_values := (others => (others => (others => 400.0)));
      curb_sensor_values(FRONT, LEFT, True) := SENSOR_FAULT;
      curb_sensor_values(FRONT, LEFT, FAlse) := SENSOR_FAULT;
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = SYSTEM_ERROR_S, "Expected SYSTEM_ERROR_S, got " & Output'Image);


      -- left  detected => go straight
      curb_sensor_values := (others => (LEFT => (others => 400.0), RIGHT => (others => 0.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = GO_STRAIGHT_S, "Expected GO_STRAIGHT_S, got " & Output'Image);

      -- left detected too close => go right
      curb_sensor_values := (others => (LEFT => (others => 200.0), RIGHT => (others => 0.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = ROTATE_RIGHT_S, "Expected GO_STRAIGHT_S, got " & Output'Image);

      -- left detected too far => go left
      curb_sensor_values := (others => (LEFT => (others => 600.0), RIGHT => (others => 0.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = ROTATE_LEFT_S, "Expected ROTATE_LEFT_S, got " & Output'Image);


      -- left not detected, right detected => go straight
      curb_sensor_values := (others => (LEFT => (others => 400.0), RIGHT => (others => 400.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = GO_STRAIGHT_S, "Expected GO_STRAIGHT_S, got " & Output'Image);

      -- left detected too close => go right
      curb_sensor_values := (others => (LEFT => (others => 200.0), RIGHT => (others => 400.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = ROTATE_RIGHT_S, "Expected GO_STRAIGHT_S, got " & Output'Image);

      -- left detected too far => go left
      curb_sensor_values := (others => (LEFT => (others => 600.0), RIGHT => (others => 400.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = ROTATE_LEFT_S, "Expected ROTATE_LEFT_S, got " & Output'Image);


      -- left not detected, right detected => go straight
      curb_sensor_values := (others => (RIGHT => (others => 400.0), LEFT => (others => 1000.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = GO_STRAIGHT_S, "Expected GO_STRAIGHT_S, got " & Output'Image);

      -- left detected too close => go right
      curb_sensor_values := (others => (RIGHT => (others => 200.0), LEFT => (others => 1000.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = ROTATE_LEFT_S, "Expected ROTATE_LEFT_S, got " & Output'Image);

      -- left detected too far => go left
      curb_sensor_values := (others => (RIGHT => (others => 600.0), LEFT => (others => 1000.0)));
      Output := output_from_curb_detection(curb_sensor_values => curb_sensor_values);

      Assert(Output = ROTATE_RIGHT_S, "Expected ROTATE_RIGHT_S, got " & Output'Image);





   end test_output_from_curb_detection;

end Lane_Detection.Test;
