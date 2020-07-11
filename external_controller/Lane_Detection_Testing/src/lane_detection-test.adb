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
      threshhold     : constant Long_Float := 200.0;
      detected_array : Line_Sensor_Detected_Array_T;

      one_above      : constant Long_Float := threshhold + 0.1;
      one_below      : constant Long_Float := threshhold - 0.1;
      max_val        : constant Long_Float := 1_000.0;
   begin

      -- test all sensors off one below
      sensor_values := (others => (others => one_above));

      detect_lanes(all_sensor_values => sensor_values,
                   threshhold        => threshhold,
                   detected_array    => detected_array);
      for pos2 in Line_Sensor_Position_T loop
         for I2 in Boolean loop
            Assert(not detected_array(pos2, I2), "expected not detected, got detected");
         end loop;
      end loop;


      -- test all sensors off max value
      sensor_values := (others => (others => max_val));

      detect_lanes(all_sensor_values => sensor_values,
                   threshhold        => threshhold,
                   detected_array    => detected_array);
      for pos2 in Line_Sensor_Position_T loop
         for I2 in Boolean loop
            Assert(not detected_array(pos2, I2), "expected not detected, got detected");
         end loop;
      end loop;

      -- test each sensor detecting once
      for pos in Line_Sensor_Position_T loop
         for I in Boolean loop
            sensor_values := (others => (others => one_above));
            sensor_values(pos, I) := one_below;
            detect_lanes(all_sensor_values => sensor_values,
                         threshhold        => threshhold,
                         detected_array    => detected_array);

            for pos2 in Line_Sensor_Position_T loop
               for I2 in Boolean loop
                  if pos2 = pos and I2 = I then
                     Assert(detected_array(pos, I), "expected detected, got not detected");
                  else
                     Assert(not detected_array(pos2, I2), "expected not detected, got detected");
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;

      -- test all sensors on one below threshhold
      sensor_values := (others => (others => one_below));

      detect_lanes(all_sensor_values => sensor_values,
                   threshhold        => threshhold,
                   detected_array    => detected_array);
      for pos2 in Line_Sensor_Position_T loop
         for I2 in Boolean loop
            Assert(detected_array(pos2, I2), "expected detected, got not detected");
         end loop;
      end loop;

      -- test all sensors on 0
      sensor_values := (others => (others => 0.0));

      detect_lanes(all_sensor_values => sensor_values,
                   threshhold        => threshhold,
                   detected_array    => detected_array);
      for pos2 in Line_Sensor_Position_T loop
         for I2 in Boolean loop
            Assert(detected_array(pos2, I2), "expected detected, got not detected");
         end loop;
      end loop;

   end test_detect_lanes;

   -------------------------------------
   -- test_output_from_line_detection --
   -------------------------------------

   procedure test_output_from_line_detection (T : in out Test) is
      pragma Unreferenced (T);
   begin
      null;
   end test_output_from_line_detection;

   -------------------------------------
   -- test_output_from_curb_detection --
   -------------------------------------

   procedure test_output_from_curb_detection (T : in out Test) is
      pragma Unreferenced (T);
   begin
      null;
   end test_output_from_curb_detection;

   ---------------------------
   -- test_calculate_output --
   ---------------------------

   procedure test_calculate_output (T : in out Test) is
      pragma Unreferenced (T);
   begin
      null;
   end test_calculate_output;

end Lane_Detection.Test;
