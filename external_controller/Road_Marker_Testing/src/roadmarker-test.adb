pragma Ada_2012;
with AUnit.Assertions; use AUnit.Assertions;
with Roadmarker; use Roadmarker;
package body Roadmarker.Test is

   ----------
   -- Name --
   ----------

   function Name (T : Roadmarker_Functions.Test.Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Roadmarker package");
   end Name;
   ----------------------------------------
   -- test_calculate_output_from_history --
   ----------------------------------------

   procedure test_calculate_output_from_history (T : in out Roadmarker_Functions.Test.Test) is
      history : Road_Marker_History_T;
      Output   : Road_Marker_Done_T;
   begin
      empty_history(history => history);

      Output := calculate_output_from_history(history);
      Assert(Output = RM_no_road_marker, "Calling with empty history. Expected RM_no_road_marker, got " & Output'Image);

      -- test 5 => 1
      history(5) := 1;

      Output := calculate_output_from_history(history);
      Assert(Output = 5, "Calling with 5 = 1, else 0 history. Expected 5, got " & Output'Image);

      -- test 5 =>1, 2 => 1
      history := (5 => 1, 2 => 1, others => 0);
      Output := calculate_output_from_history(history);
      Assert(Output = 5, "history(5) = 1, history(2) = 1, else 0 history. Expected 5, got " & Output'Image);

      -- test 5 =>1, 2 => 2
      history := (5 => 1, 2 => 2, others => 0);
      Output := calculate_output_from_history(history);
      Assert(Output = 2, "history(5) = 1, history(2) = 2, else 0 history. Expected 2, got " & Output'Image);

      -- test 5 =>1, 2 => 2
      history := (5 => 1, 2 => 2, 10 => 1, others => 0);
      Output := calculate_output_from_history(history);
      Assert(Output = 2, "history(5) = 1, history(2) = 2, history(10) = 1, else 0 history. Expected 2, got " & Output'Image);

      -- test 5 =>1, 2 => 2, 10 => 2
      history := (5 => 1, 2 => 2, 10 => 2, others => 0);
      Output := calculate_output_from_history(history);
      Assert(Output = 10, "history(5) = 1, history(2) = 2, history(10) = 2, else 0 history. Expected 10, got " & Output'Image);

      -- test all 1s
      history := (others => 1);
      Output := calculate_output_from_history(history);
      Assert(Output = 15, "history = 1. Expected 15, got " & Output'Image);
   end test_calculate_output_from_history;

   ---------------------------
   -- test_calculate_output --
   ---------------------------

   procedure test_calculate_output (T : in out Roadmarker_Functions.Test.Test) is
      sensors  : All_Sensor_Values_Array_T;
      history  : Road_Marker_History_T;
      Output   : Road_Marker_Done_T;
      black_on : constant Long_Float := 217.0;
      grey_on  : constant Long_Float := 247.0;
      Off      : constant Long_Float := 300.0;
      failure  : constant Long_Float := -1.0;
      was_on_hotfix_rm : Boolean := False;
   begin
      --
      -- read no rm
      --

      -- read no rm
      sensors := (others => (others => Off));
      history := (others => 0);
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm
                                 );
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");

      for I in Road_Marker_Done_T loop
         Assert(history(I) = 0, "expected history = 0, but history(" & I'Image & ") = " & history(I)'Image);
      end loop;

      --
      -- read 15 once
      --

      -- read rm 15
      sensors := (others => (others => black_on));
      history := (others => 0);
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      for I in Road_Marker_Done_T loop
         if I = 15 then
            Assert(history(I) = 1, "expected histor(15) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;


      -- read rm 15 again
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      for I in Road_Marker_Done_T loop
         if I = 15 then
            Assert(history(I) = 2, "expected histor(15) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;

      -- now read empty RM
      sensors := (others => (others => Off));
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = 15, "expected 15, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");

      for I in Road_Marker_Done_T loop
         Assert(history(I) = 0, "expected history = 0, but history(" & I'Image & ") = " & history(I)'Image);
      end loop;

      --
      -- read 15 2 times and 1 3 times
      --

      -- read rm 15
      sensors := (others => (others => black_on));
      history := (others => 0);
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      for I in Road_Marker_Done_T loop
         if I = 15 then
            Assert(history(I) = 1, "expected histor(15) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;

      -- read rm 15 again
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      for I in Road_Marker_Done_T loop
         if I = 15 then
            Assert(history(I) = 2, "expected histor(15) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;

      -- read rm 0
      sensors := (FRONT_LEFT => (others => black_on),
                  FRONT_RIGHT => (others => black_on),
                  BEHIND_LEFT => (others => black_on),
                  BEHIND_RIGHT => (others => black_on),
                  others => (others => Off));
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      for I in Road_Marker_Done_T loop
         if I = 15 then
            Assert(history(I) = 2, "expected histor(15) = 2, but history(" & I'Image & ") = " & history(I)'Image);
         elsif I = 0 then
            Assert(history(I) = 1, "expected histor(1) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;

      -- read rm 0 again
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      for I in Road_Marker_Done_T loop
         if I = 15 then
            Assert(history(I) = 2, "expected histor(15) = 2, but history(" & I'Image & ") = " & history(I)'Image);
         elsif I = 0 then
            Assert(history(I) = 2, "expected histor(1) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;

      -- read rm 0 again
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      for I in Road_Marker_Done_T loop
         if I = 15 then
            Assert(history(I) = 2, "expected histor(15) = 2, but history(" & I'Image & ") = " & history(I)'Image);
         elsif I = 0 then
            Assert(history(I) = 3, "expected histor(1) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;

      -- now read empty RM
      sensors := (others => (others => Off));
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = 0, "expected 0, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");

      --
      -- read 0 on default sensor and 15 on backup sensor
      --
      history := (others => 0);
      sensors := (FRONT_LEFT => (others => black_on),
                  FRONT_RIGHT => (others => black_on),
                  BEHIND_LEFT => (others => black_on),
                  BEHIND_RIGHT => (others => black_on),
                  others => (True => black_on, False => Off));


      -- read rm 0
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      for I in Road_Marker_Done_T loop
         if I = 0 then
            Assert(history(I) = 1, "expected histor(0) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;

      --
      -- test default sensor failing => read 15 from backup sensor
      --
      history := (others => 0);
      sensors := (FRONT_LEFT => (others => black_on),
                  FRONT_RIGHT => (others => black_on),
                  BEHIND_LEFT => (others => black_on),
                  BEHIND_RIGHT => (others => black_on),
                  RM_FL => (False => failure, others => black_on),
                  others => (True => black_on, False => Off));


      -- read rm 15
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      for I in Road_Marker_Done_T loop
         if I = 15 then
            Assert(history(I) = 1, "expected histor(15) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history("& I'Image & ") = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;


      --
      -- test both sensors failing
      --
      history := (others => 0);
      sensors := (FRONT_LEFT => (others => black_on),
                  FRONT_RIGHT => (others => black_on),
                  BEHIND_LEFT => (others => black_on),
                  BEHIND_RIGHT => (others => black_on),
                  RM_FL => (False => failure, others => failure),
                  others => (True => black_on, False => Off));


      -- read RM_system_error
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = RM_system_error, "expected RM_system_error, got " & Output'Image);
      Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
      for I in Road_Marker_Done_T loop
         Assert(history(I) = 0, "expected history("& I'Image & ") = 0, but history(" & I'Image & ") = " & history(I)'Image);
      end loop;


   end test_calculate_output;


   ----------------------------------------------
   -- test_calculate_output_was_on_hotfix_rm --
   ----------------------------------------------

   procedure test_calculate_output_was_on_hotfix_rm (T : in out Roadmarker_Functions.Test.Test) is
      sensors  : All_Sensor_Values_Array_T;
      history  : Road_Marker_History_T;
      Output   : Road_Marker_Done_T;
      black_on : constant Long_Float := 217.0;
      grey_on  : constant Long_Float := 247.0;
      Off      : constant Long_Float := 300.0;
      failure  : constant Long_Float := -1.0;
      was_on_hotfix_rm : Boolean := False;
   begin
      --
      -- check only one hotfix detected -> no RM or
      --

      for I in Roadmarker_Sensor_ID_T loop
         sensors := (others => (others => Off));
         sensors(I, False) := grey_on;
         sensors(I, True) := grey_on;

         Output := calculate_output(all_sensor_values => sensors,
                                    history           => history,
                                    was_on_hotfix_rm => was_on_hotfix_rm);

         Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
         for I in Road_Marker_Done_T loop
            Assert(history(I) = 0, "expected history("& I'Image & ") = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end loop;
      end loop;

      --
      -- check two hotfix detected -> no RM or
      --

      for I in Roadmarker_Sensor_ID_T loop
         for J in Roadmarker_Sensor_ID_T loop
            if I /= J then
               sensors := (others => (others => Off));
               sensors(I, False) := grey_on;
               sensors(I, True) := grey_on;
               sensors(J, False) := grey_on;
               sensors(J, True) := grey_on;

               Output := calculate_output(all_sensor_values => sensors,
                                          history           => history,
                                          was_on_hotfix_rm => was_on_hotfix_rm);

               Assert(was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
               for I in Road_Marker_Done_T loop
                  Assert(history(I) = 0, "expected history("& I'Image & ") = 0, but history(" & I'Image & ") = " & history(I)'Image);
               end loop;
            end if;
         end loop;
      end loop;

      --
      -- check RM detected and hotfix at same time:
      --

      for I in Roadmarker_Sensor_ID_T loop
         for J in Roadmarker_Sensor_ID_T loop
            if I /= J then
               sensors := (FRONT_LEFT  => (others => black_on),
                           FRONT_RIGHT  => (others => black_on),

                           BEHIND_LEFT  => (others => black_on),

                           BEHIND_RIGHT => (others => black_on),
                           others       => (others => Off));
               sensors(I, False) := grey_on;
               sensors(I, True) := grey_on;
               sensors(J, False) := grey_on;
               sensors(J, True) := grey_on;

               Output := calculate_output(all_sensor_values => sensors,
                                          history           => history,
                                          was_on_hotfix_rm => was_on_hotfix_rm);

               Assert(was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
               for I in Road_Marker_Done_T loop
                  Assert(history(I) = 0, "expected history("& I'Image & ") = 0, but history(" & I'Image & ") = " & history(I)'Image);
               end loop;
            end if;
         end loop;
      end loop;

      --
      -- check RM detected and only one hotfix rm at same time:
      -- should not set was_on_rm
      --

      for I in Roadmarker_Sensor_ID_T loop
         sensors := (FRONT_LEFT  => (others => black_on),
                     FRONT_RIGHT  => (others => black_on),
                     BEHIND_LEFT  => (others => black_on),
                     BEHIND_RIGHT => (others => black_on),
                     others       => (others => Off));
         sensors(I, False) := grey_on;
         sensors(I, True) := grey_on;

         Output := calculate_output(all_sensor_values => sensors,
                                    history           => history,
                                    was_on_hotfix_rm => was_on_hotfix_rm);

         Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");

      end loop;
      null;

      --
      -- read rm 0: Test both was_on_hotfix_rm True False
      --
      sensors := (FRONT_LEFT => (others => black_on),
                  FRONT_RIGHT => (others => black_on),
                  BEHIND_LEFT => (others => black_on),
                  BEHIND_RIGHT => (others => black_on),
                  others => (others => Off));
      for J in Boolean loop

         history := (others => 0);

         was_on_hotfix_rm := J;
         Output := calculate_output(all_sensor_values => sensors,
                                    history           => history,
                                    was_on_hotfix_rm => was_on_hotfix_rm);
         Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
         Assert(not was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");
         for I in Road_Marker_Done_T loop
            if I = 0 then
               Assert(history(I) = 1, "expected histor(1) = 1, but history(" & I'Image & ") = " & history(I)'Image);
            else
               Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
            end if;
         end loop;
      end loop;


      --
      -- test was_on_hotfix_rm True and no RM detected => was_on_hotfix_rm still true
      --
      sensors := (others => (others => Off));

      history := (others => 0);

      was_on_hotfix_rm := True;
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history,
                                 was_on_hotfix_rm => was_on_hotfix_rm);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      Assert(was_on_hotfix_rm, "expected was_on_hotfix_rm = False, got True");


   end test_calculate_output_was_on_hotfix_rm;


   -----------------------------------
   -- test_check_error_sensor_array --
   -----------------------------------

   procedure test_check_error_sensor_array (T : in out Roadmarker_Functions.Test.Test) is
      sensors : All_Sensor_Values_Array_T;
      Threshhold : constant Long_Float := 250.0;
      black_on : constant Long_Float := 217.0;
      grey_on  : constant Long_Float := 247.0;
      Off      : constant Long_Float := 300.0;
      failure  : constant Long_Float := -1.0;
   begin

      --
      -- check all sensors on
      --
      sensors := (others => (others => black_on));
      for I in Boolean loop
         Assert(not check_error_sensor_array(sensors, I),
                "All sensors on. Expected False, got True");
      end loop;

      --
      -- check all sensors off
      --
      sensors := (others => (others => Off));
      for I in Boolean loop
         Assert(not check_error_sensor_array(sensors, I),
                "All sensors off. Expected False, got True");
      end loop;

      --
      -- check each sensor single failure
      --
      for ID in Roadmarker_Sensor_ID_T loop
         sensors := (others => (others => Off));

         for I in Boolean loop
            sensors(ID, I) := failure;
            Assert(check_error_sensor_array(sensors, I),
                   ID'Image &"error. Expected True, got False");
         end loop;
      end loop;

      --
      -- check all failure
      --
      sensors := (others => (others => failure));
         for I in Boolean loop
            Assert(check_error_sensor_array(sensors, I),
                   "All sensors failing. Expected True, got False");
         end loop;


   end test_check_error_sensor_array;

end Roadmarker.Test;
