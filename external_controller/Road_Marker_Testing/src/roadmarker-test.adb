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
      Assert(Output = 10, "history(5) = 1, history(2) = 2, history(10) = 1, else 0 history. Expected 10, got " & Output'Image);

      -- test all 1s
      history := (others => 1);
      Output := calculate_output_from_history(history);
      Assert(Output = 15, "history = 1. Expected 15, got " & Output'Image);
   end test_calculate_output_from_history;

   ---------------------------
   -- test_calculate_output --
   ---------------------------

   procedure test_calculate_output (T : in out Roadmarker_Functions.Test.Test) is
      sensors : All_Sensor_Values_Array_T;
      history : Road_Marker_History_T;
      Output   : Road_Marker_Done_T;
      Threshhold : constant Long_Float := 250.0;
      On         : constant Long_Float := Threshhold - 0.1;
      Off        : constant Long_Float := Threshhold + 0.1;
      failure    : constant Long_Float := -1.0;
   begin
      --
      -- read no rm
      --

      -- read no rm
      sensors := (others => (others => Off));
      history := (others => 0);
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);

      for I in Road_Marker_Done_T loop
         Assert(history(I) = 0, "expected histor = 0, but history(" & I'Image & ") = " & history(I)'Image);
      end loop;

      --
      -- read 15 once
      --

      -- read rm 15
      sensors := (others => (others => On));
      history := (others => 0);
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history);
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
                                 history           => history);
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
                                 history           => history);
      Assert(Output = 15, "expected 15, got " & Output'Image);

      for I in Road_Marker_Done_T loop
         Assert(history(I) = 0, "expected history = 0, but history(" & I'Image & ") = " & history(I)'Image);
      end loop;

      --
      -- read 15 2 times and 1 3 times
      --

      -- read rm 15
      sensors := (others => (others => On));
      history := (others => 0);
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history);
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
                                 history           => history);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
      for I in Road_Marker_Done_T loop
         if I = 15 then
            Assert(history(I) = 2, "expected histor(15) = 1, but history(" & I'Image & ") = " & history(I)'Image);
         else
            Assert(history(I) = 0, "expected history(I) = 0, but history(" & I'Image & ") = " & history(I)'Image);
         end if;
      end loop;

      -- read rm 0
      sensors := (FRONT_LEFT => (others => On),
                  FRONT_RIGHT => (others => On),
                  BEHIND_LEFT => (others => On),
                  BEHIND_RIGHT => (others => On),
                  others => (others => Off));
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
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
                                 history           => history);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
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
                                 history           => history);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
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
                                 history           => history);
      Assert(Output = 0, "expected 0, got " & Output'Image);

      --
      -- read 0 on default sensor and 15 on backup sensor
      --
      history := (others => 0);
      sensors := (FRONT_LEFT => (others => On),
                  FRONT_RIGHT => (others => On),
                  BEHIND_LEFT => (others => On),
                  BEHIND_RIGHT => (others => On),
                  others => (True => On, False => Off));


      -- read rm 0
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
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
      sensors := (FRONT_LEFT => (others => On),
                  FRONT_RIGHT => (others => On),
                  BEHIND_LEFT => (others => On),
                  BEHIND_RIGHT => (others => On),
                  RM_FL => (False => failure, others => On),
                  others => (True => On, False => Off));


      -- read rm 15
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history);
      Assert(Output = RM_no_road_marker, "expected RM_no_road_marker, got " & Output'Image);
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
      sensors := (FRONT_LEFT => (others => On),
                  FRONT_RIGHT => (others => On),
                  BEHIND_LEFT => (others => On),
                  BEHIND_RIGHT => (others => On),
                  RM_FL => (False => failure, others => failure),
                  others => (True => On, False => Off));


      -- read RM_system_error
      Output := calculate_output(all_sensor_values => sensors,
                                 history           => history);
      Assert(Output = RM_system_error, "expected RM_system_error, got " & Output'Image);
      for I in Road_Marker_Done_T loop
         Assert(history(I) = 0, "expected history("& I'Image & ") = 0, but history(" & I'Image & ") = " & history(I)'Image);
      end loop;


   end test_calculate_output;


   -----------------------------------
   -- test_check_error_sensor_array --
   -----------------------------------

   procedure test_check_error_sensor_array (T : in out Roadmarker_Functions.Test.Test) is
      sensors : All_Sensor_Values_Array_T;
      Threshhold : constant Long_Float := 250.0;
      On         : constant Long_Float := Threshhold - 0.1;
      Off        : constant Long_Float := Threshhold + 0.1;
      failure    : constant Long_Float := -1.0;
   begin

      --
      -- check all sensors on
      --
      sensors := (others => (others => On));
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
