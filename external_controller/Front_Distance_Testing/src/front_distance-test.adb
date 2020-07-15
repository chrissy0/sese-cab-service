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

   -- typpe to simulate motor values
   --  type Motor_Values_Array_T is array (Motor_ID_T) of Long_Float;
   --
   --  protected type Motor_Values_T is
   --     procedure set (ID : Motor_ID_T; Value : Long_Float);
   --     function get (ID : Motor_ID_T) return Long_Float;
   --  private
   --     Motor_Values_Array : Motor_Values_Array_T;
   --  end Motor_Values_T;
   --
   --  protected body Motor_Values_T is
   --     procedure set (ID : Motor_ID_T; Value : Long_Float) is
   --     begin
   --        Motor_Values_Array (ID) := Value;
   --     end set;
   --
   --     function get (ID : Motor_ID_T) return Long_Float is
   --     begin
   --        return Motor_Values_Array (ID);
   --     end get;
   --  end Motor_Values_T;
   --
   --  type Motor_Values_Acces_T is access Motor_Values_T;
   --  Motor_Values : Motor_Values_T;
   --
   --  protected type Front_Distance_Values_T is
   --     procedure set
   --       (
   --        typ : in Sensor_Type_T;
   --        pos : in Sensor_Position_T;
   --        num : in Sensor_Number_T;
   --        Value : Long_Float
   --       );
   --     function get
   --       (
   --        typ : in Sensor_Type_T;
   --        pos : in Sensor_Position_T;
   --        num : in Sensor_Number_T
   --       ) return Long_Float;
   --  private
   --     Front_Distance_Values_Array : All_Sensor_Values_Array_T;
   --  end Front_Distance_Values_T;
   --
   --  protected body Front_Distance_Values_T is
   --     procedure set
   --       (
   --        typ : in Sensor_Type_T;
   --        pos : in Sensor_Position_T;
   --        num : in Sensor_Number_T;
   --        Value : Long_Float
   --       )
   --     is
   --     begin
   --        Front_Distance_Values_Array (typ, pos, num) := Value;
   --     end set;
   --
   --     function get
   --       (
   --        typ : in Sensor_Type_T;
   --        pos : in Sensor_Position_T;
   --        num : in Sensor_Number_T
   --       ) return Long_Float
   --     is
   --     begin
   --        return Front_Distance_Values_Array (typ, pos, num);
   --     end get;
   --  end Front_Distance_Values_T;
   --
   --  Front_Distance_Values : Front_Distance_Values_T;
   --
   --  -- dummy functions
   --
   --  procedure dummy_M_set (ID : Motor_ID_T; value : Long_Float) is
   --  begin
   --     Motor_Values.set (ID, value);
   --  end dummy_M_set;
   --
   --  function dummy_FD_get
   --       (
   --        typ : in Sensor_Type_T;
   --        pos : in Sensor_Position_T;
   --        num : in Sensor_Number_T
   --       ) return Long_Float is
   --  begin
   --     return Front_Distance_Values.get (typ, pos, num);
   --  end dummy_FD_get;
   --
   --  -------------------------
   --  -- all_below_thresh --
   --  -------------------------
   --
   --  procedure check_motor_values (T : in out Test) is
   --     pragma Unreferenced (T);
   --     type Motor_Controller_Task_Array_T is
   --       array
   --         (Motor_Controller.Lane_Detection_Done_T)
   --         of Motor_Controller.Motor_Controller_Task_T;
   --     Motor_Controller_Task : Motor_Controller.Motor_Controller_Task_Access_T := new Motor_Controller_Task_T;
   --     Front_Distance_Task   : Front_Distance_Task_T;
   --     US_THRESH_V           : constant := 40.0;
   --     IR_THRESH_V           : constant := 80.0;
   --     SENSOR_ERROR_V        : constant := -1.9;
   --     SENSOR_DELTA          : constant := 0.1;
   --     SPEED_STRAIGHT        : constant := 6.0;
   --     SPEED_TURN            : constant := 2.0;
   --     lane_detection_next_signal : Lane_Detection_Next_T;
   --     job_executer_next_signal : Job_Executer_Next_t;
   --
   --     procedure proceed is begin
   --        Motor_Controller_Task.lane_detection_done(EMPTY_S);
   --        Motor_Controller_Task.job_executer_done(EMPTY_S);
   --
   --        Motor_Controller_Task.main_shutdown_signal(False);
   --
   --        Motor_Controller_Task.lane_detection_next(lane_detection_next_signal);
   --        Motor_Controller_Task.job_executer_next(job_executer_next_signal);
   --     end proceed;
   --
   --     procedure set_all_values
   --       (
   --        ir_value : in Long_Float;
   --        us_value : in Long_Float
   --       )
   --     is
   --     begin
   --        for pos in Sensor_Position_T loop
   --           for num in Sensor_Number_T loop
   --              Front_Distance_Values.set (US, pos, num, us_value);
   --              Front_Distance_Values.set (IR, pos, num, ir_value);
   --           end loop;
   --        end loop;
   --     end set_all_values;
   --
   --  begin
   --     -- set front clear
   --     set_all_values(IR_THRESH_V + SENSOR_DELTA, US_THRESH_V + SENSOR_DELTA);
   --
   --     Motor_Controller_Task.Constructor(MC_State               => NORMAL_DRIVING,
   --                                       ND_State               => FRONT_CLEAR,
   --                                       FC_State               => DRIVE,
   --                                       D_State                => STRAIGHT,
   --                                       LE_State               => NEXT_UNKOWN,
   --                                       SE_State               => STOP,
   --                                       MS_Speed               => SPEED_STRAIGHT,
   --                                       MT_Speed               => SPEED_TURN,
   --                                       set_motor_value_access => dummy_M_set'Access,
   --                                       timeout_v => 2.0,
   --                                       iteration_delay_s => 0.2
   --                                      );
   --
   --     Front_Distance_Task.Construct(get_sensor_value_a => dummy_FD_get'Access,
   --                                   us_thresh                        => US_THRESH_V,
   --                                   ir_thresh                        => IR_THRESH_V,
   --                                   Motor_Controller_Task_A          => Motor_Controller_Task,
   --                                   timeout_v => 2.0
   --                                  );
   --
   --     -- system should react in two iterations
   --     proceed;
   --     proceed;
   --
   --     for I in Motor_Controller.Motor_ID_T loop
   --        Assert (SPEED_STRAIGHT = Motor_Values.get(I), "Motor not driving straight even though front clear! Expected motor value SPEED_STRAIGHT, got " & Motor_Values.get(I)'Image);
   --     end loop;
   --
   --     -- all below threshold -> front blocked
   --     set_all_values(IR_THRESH_V - SENSOR_DELTA, US_THRESH_V - SENSOR_DELTA);
   --     proceed;
   --     proceed;
   --
   --     for I in Motor_Controller.Motor_ID_T loop
   --        Assert (0.0 = Motor_Values.get(I), "Motor not driving straight even though front clear! Expected motor value 0.0, got " & Motor_Values.get(I)'Image);
   --     end loop;
   --
   --     -- all above thresh again -> front clear
   --     set_all_values(IR_THRESH_V + SENSOR_DELTA, US_THRESH_V + SENSOR_DELTA);
   --
   --     proceed;
   --     proceed;
   --
   --     for I in Motor_Controller.Motor_ID_T loop
   --        Assert (SPEED_STRAIGHT = Motor_Values.get(I), "Motor not driving straight even though front clear! Expected motor value SPEED_STRAIGHT, got " & Motor_Values.get(I)'Image);
   --     end loop;
   --
   --     -- ir_above_us_below -> front blocked
   --     set_all_values(IR_THRESH_V + SENSOR_DELTA, US_THRESH_V - SENSOR_DELTA);
   --
   --     proceed;
   --     proceed;
   --
   --     for I in Motor_Controller.Motor_ID_T loop
   --        Assert (0.0 = Motor_Values.get(I), "Motor driving even though front blocked! Expected motor value 0.0, got " & Motor_Values.get(I)'Image);
   --     end loop;
   --
   --     -- all above thresh again -> front clear
   --     set_all_values(IR_THRESH_V + SENSOR_DELTA, US_THRESH_V + SENSOR_DELTA);
   --
   --     proceed;
   --     proceed;
   --
   --     for I in Motor_Controller.Motor_ID_T loop
   --        Assert (SPEED_STRAIGHT = Motor_Values.get(I), "Motor not driving straight even though front clear! Expected motor value SPEED_STRAIGHT, got " & Motor_Values.get(I)'Image);
   --     end loop;
   --
   --     -- us above ir below -> front blocked
   --     set_all_values(IR_THRESH_V - SENSOR_DELTA, US_THRESH_V + SENSOR_DELTA);
   --
   --     proceed;
   --     proceed;
   --
   --
   --  end check_motor_values;


   procedure test_calculate_output (T : in out Test) is
      all_sensor_values : All_Sensor_Values_Array_T;
      thresholds        : Threshhold_Array_T;
      IR_Threshhold     : constant := 60.0;
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


   end test_calculate_output;

end Front_Distance.Test;
