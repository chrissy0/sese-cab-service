-- @summary
-- Front distance controller child test package body.
--
-- @author Julian Hartmer

pragma Ada_2012;
with Motor_Controller; use Motor_Controller;
with Front_Distance;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;
package body Front_Distance.Test is

   procedure test_calculate_output (T : in out Test) is
      all_sensor_values : All_Sensor_Values_Array_T;
      thresholds        : Threshhold_Array_T;
      IR_Threshhold     : constant := 80.0;
      US_Threshhold     : constant := 80.0;
      Sensor_Delta      : constant := 0.1;
      Sensor_Error      : constant := -1.0;
      output            : Front_Distance_Done_t;
      procedure set_all_values
        (
         ir_value : in Long_Float;
         us_value : in Long_Float
        )
      is
      begin
         for pos in Sensor_Position_T loop
            for num in Sensor_Number_T loop
               all_sensor_values(US, pos, num) := us_value;
               all_sensor_values(IR, pos, num) := ir_value;
            end loop;
         end loop;
      end set_all_values;
   begin
      -- init thresholds
      for pos in Sensor_Position_T loop
         thresholds(IR, pos) := IR_Threshhold;
         thresholds(US, pos) := US_Threshhold;
      end loop;


      -- test all clean
      set_all_values(ir_value => IR_Threshhold + Sensor_Delta,
                     us_value => US_Threshhold + Sensor_Delta);
      output := calculate_output(all_sensor_values, thresholds);
      Assert(output = FRONT_CLEAR_S, "expected front clear, got "& output'Image);

      -- test one sensor faulty but front clear
      all_sensor_values(IR, LEFT, 0) := Sensor_Error;

      output := calculate_output(all_sensor_values, thresholds);
      Assert(output = FRONT_CLEAR_S, "expected front clear, got "& output'Image);

      -- test more failure, but so that both IR and US still working
      all_sensor_values(IR, RIGHT, 0) := Sensor_Error;
      all_sensor_values(IR, CENTER, 1) := Sensor_Error;

      output := calculate_output(all_sensor_values, thresholds);
      Assert(output = FRONT_CLEAR_S, "expected front clear, got "& output'Image);

      -- now let IR fail, front still clear due to US
      all_sensor_values(IR, RIGHT, 1) := Sensor_Error;

      output := calculate_output(all_sensor_values, thresholds);
      Assert(output = FRONT_CLEAR_S, "expected front clear, got "& output'Image);

      -- set one not-failing IR Sensor to blocked, output should still be clear
      -- because IR failed -> ignore IR blocked sensor
      all_sensor_values(IR, CENTER, 0) := IR_Threshhold - Sensor_Delta;
      Assert(output = FRONT_CLEAR_S, "expected front clear, got "& output'Image);


      -- set 3 sensor fault in US, but so that is still works
      all_sensor_values(US, RIGHT, 1) := Sensor_Error;
      all_sensor_values(US, LEFT, 0) := Sensor_Error;
      all_sensor_values(US, CENTER, 1) := Sensor_Error;

      output := calculate_output(all_sensor_values, thresholds);
      Assert(output = FRONT_CLEAR_S, "expected front clear, got "& output'Image);

      -- one non faulty US sensor to blocked -> blocked
      all_sensor_values(US, CENTER, 0) := US_Threshhold - Sensor_Delta;
      output := calculate_output(all_sensor_values, thresholds);
      Assert(output = FRONT_BLOCKED_S, "expected front blocked, got "& output'Image);

      -- set one more US sensor faulty --> error
      all_sensor_values(US, RIGHT, 0) := Sensor_Error;

      output := calculate_output(all_sensor_values, thresholds);
      Assert(output = FD_FAULT_S, "expected fault, got "& output'Image);

      -- restore the last faulty IR sensor with clear -> front blocked
      all_sensor_values(IR, RIGHT, 1) := IR_Threshhold + Sensor_Delta;
      output := calculate_output(all_sensor_values, thresholds);
      Assert(output = FRONT_BLOCKED_S, "expected front blocked, got "& output'Image);

      -- set the one blocked IR sensor to clear -> front clear
      all_sensor_values(IR, CENTER, 0) := IR_Threshhold + Sensor_Delta;
      output := calculate_output(all_sensor_values, thresholds);
      Assert(output = FRONT_CLEAR_S, "expected front clear, got "& output'Image);

      --test different threshold left and cener
      set_all_values(ir_value => IR_Threshhold + Sensor_Delta,
                     us_value => US_Threshhold + Sensor_Delta);
      thresholds(IR, LEFT) := IR_Threshhold * 2.0;
      all_sensor_values(IR, LEFT, 0) := IR_Threshhold * 2.0 - 1.0;


   end test_calculate_output;

end Front_Distance.Test;
