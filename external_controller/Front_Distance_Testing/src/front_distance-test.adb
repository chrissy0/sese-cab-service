pragma Ada_2012;
with Motor_Controller; use Motor_Controller;
with Front_Distance;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;
package body Front_Distance.Test is

   -- typpe to simulate motor values
   type Motor_Values_Array_T is array (Motor_ID_T) of Long_Float;

   protected type Motor_Values_T is
      procedure set (ID : Motor_ID_T; Value : Long_Float);
      function get (ID : Motor_ID_T) return Long_Float;
   private
      Motor_Values_Array : Motor_Values_Array_T;
   end Motor_Values_T;

   protected body Motor_Values_T is
      procedure set (ID : Motor_ID_T; Value : Long_Float) is
      begin
         Motor_Values_Array (ID) := Value;
      end set;

      function get (ID : Motor_ID_T) return Long_Float is
      begin
         return Motor_Values_Array (ID);
      end get;
   end Motor_Values_T;

   type Motor_Values_Acces_T is access Motor_Values_T;
   Motor_Values : Motor_Values_T;

   -- type to simulate front distacen sensors:
   type Front_Distance_Value_Array_T is array (Front_Distance.Distance_Sensor_ID_T) of Long_Float;

   protected type Front_Distance_Values_T is
      procedure set (ID : Front_Distance.Distance_Sensor_ID_T; Value : Long_Float);
      function get (ID : Front_Distance.Distance_Sensor_ID_T) return Long_Float;
   private
      Front_Distance_Values_Array : Front_Distance_Value_Array_T;
   end Front_Distance_Values_T;

   protected body Front_Distance_Values_T is
      procedure set (ID : Front_Distance.Distance_Sensor_ID_T; Value : Long_Float) is
      begin
         Front_Distance_Values_Array (ID) := Value;
      end set;

      function get (ID : Front_Distance.Distance_Sensor_ID_T) return Long_Float is
      begin
         return Front_Distance_Values_Array (ID);
      end get;
   end Front_Distance_Values_T;

   type Front_Distance_Values_Acces_T is access Front_Distance_Value_Array_T;
   Front_Distance_Values : Front_Distance_Values_T;

   -- dummy functions

   procedure dummy_M_set (ID : Motor_ID_T; value : Long_Float) is
   begin
      Motor_Values.set (ID, value);
   end dummy_M_set;

   function dummy_FD_get (ID : Front_Distance.Distance_Sensor_ID_T) return Long_Float is
   begin
      return Front_Distance_Values.get (ID);
   end dummy_FD_get;

   -------------------------
   -- test_front_distance --
   -------------------------

   procedure test_front_distance (T : in out Test) is
      pragma Unreferenced (T);
      type Motor_Controller_Task_Array_T is
        array
          (Motor_Controller.Lane_Detection_Done_T)
          of Motor_Controller.Motor_Controller_Task_T;
      Motor_Controller_Task : Motor_Controller.Motor_Controller_Task_Access_T := new Motor_Controller_Task_T;
      Front_Distance_Task   : Front_Distance_Task_T;
      US_THRES_V            : constant := 40.0;
      US_ERROR_V            : constant := 0.0;
   begin

      for I in Front_Distance.Distance_Sensor_ID_T loop
         Front_Distance_Values.set(I, US_THRES_V + 1.0);
      end loop;

      Motor_Controller_Task.Construct(MC_State               => NORMAL_DRIVING,
                                      ND_State               => FRONT_CLEAR,
                                      FC_State               => DRIVE,
                                      D_State                => STRAIGHT,
                                      SE_State               => STOP,
                                      MS_Speed               => 6.0,
                                      MT_Speed               => 1.0,
                                      set_motor_value_access => dummy_M_set'Access);

      Front_Distance_Task.Construct(get_distance_sensor_value_access => dummy_FD_get'Access,
                                    us_thresh                        => US_THRES_V,
                                    Motor_Controller_Task_A          => Motor_Controller_Task);
      -- test front clear

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      for I in Motor_Controller.Motor_ID_T loop
         Assert (6.0 = Motor_Values.get(I), "Motor not driving straight even though front clear! Expected motor value 6.0, got " & Motor_Values.get(I)'Image);
      end loop;

      -- Test front blocked
      for I in Front_Distance.Distance_Sensor_ID_T loop
         Front_Distance_Values.set(I, US_THRES_V - 1.0);
      end loop;

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      for I in Motor_Controller.Motor_ID_T loop
         Assert (0.0 = Motor_Values.get(I), "Motor driving even though front blocked! Expected motor value 0.0, got " & Motor_Values.get(I)'Image);
      end loop;

      -- test front clear again
      for I in Front_Distance.Distance_Sensor_ID_T loop
         Front_Distance_Values.set(I, US_THRES_V + 1.0);
      end loop;

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      for I in Motor_Controller.Motor_ID_T loop
         Assert (6.0 = Motor_Values.get(I), "Motor not driving straight even though front clear! Expected motor value 6.0, got " & Motor_Values.get(I)'Image);
      end loop;

      -- test only one row of sensors working -> no erro, normal driving
      Front_Distance_Values.set(CENTER_1, 0.0);
      Front_Distance_Values.set(LEFT_0, 0.0);
      Front_Distance_Values.set(RIGHT_1, 0.0);

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      for I in Motor_Controller.Motor_ID_T loop
         Assert (6.0 = Motor_Values.get(I), "Motor not driving straight even though front clear (one sensor row mailfunctioning)! Expected motor value 6.0, got " & Motor_Values.get(I)'Image);
      end loop;

      -- one more sensor failure: system error
      Front_Distance_Values.set(LEFT_1, 0.0);

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      Motor_Controller_Task.lane_detection_done(GO_STRAIGHT_S);

      Motor_Controller_Task.lane_detection_next;

      for I in Motor_Controller.Motor_ID_T loop
         Assert (0.0 = Motor_Values.get(I), "Motor driving straight even though system error! Expected motor value 0.0, got " & Motor_Values.get(I)'Image);
      end loop;
   end test_front_distance;

end Front_Distance.Test;
