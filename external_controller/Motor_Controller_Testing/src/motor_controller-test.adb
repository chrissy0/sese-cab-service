pragma Ada_2012;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;

package body Motor_Controller.Test is

   procedure test_calculate_output (T : in out Test)
   is
   begin
      Assert(False, "TODO");
   end test_calculate_output;


   procedure test_do_state_transition (T : in out Test)
   is
      input_state         : Cab_State_T;
      output_state         : Cab_State_T;
      expected_state : Cab_State_T;
      RM_Force_Left : Boolean;
      is_shutdown   : Boolean;
   begin
      is_shutdown := False;
      RM_Force_Left := False;
      input_state := (Base             => NO_SYSTEM_ERROR,
                      System_Error     => STAND_ON_TRACK,
                      No_System_Error  => FRONT_BLOCKED,
                      Final_Safe_State => ROTATE_LEFT_90,
                      Front_Is_Clear   => Drive,
                      Driving          => Init,
                      Leaning          => NEXT_LEFT,
                      Forcing_Left     => False,
                      Counter => 0);


      -- test all Job_Executer_Done_T signals and others empty/False
      for I in Job_Executer_Done_T loop
         expected_state := input_state;
         output_state := input_state;
         do_state_transition(state         => output_state,
                             JE_Signal     => I,
                             FD_Signal     => EMPTY_S,
                             LD_Signal     => EMPTY_S,
                             RM_Force_Left => False,
                             is_shutdown   => False);

         case I is
            when SYSTEM_ERROR_S =>
               expected_state.Leaning := LEAN_FROM_LINE;
               expected_state.Leaning := LEAN_FROM_LINE;
            when NEXT_LEFT_S =>
               expected_state.Front_Is_Clear := DRIVE;
               expected_state.Leaning := NEXT_LEFT;
            when NEXT_RIGHT_S =>
               expected_state.Front_Is_Clear := DRIVE;
               expected_state.Leaning := NEXT_RIGHT;
            when NEXT_UNKOWN_S =>
               -- keep lean value
               expected_state.Front_Is_Clear := DRIVE;
            when EMPTY_S =>
               null;
            when STOP_S =>
               expected_state.Front_Is_Clear := STOP;
         end case;

         Assert(expected_state = output_state, "states differ on input " & I 'Image);

      end loop;

      -- test all Front_Distance_Done_t signals and others empty/False
      for I in Front_Distance_Done_t loop
         expected_state := input_state;
         output_state := input_state;
         do_state_transition(state         => output_state,
                             JE_Signal     => EMPTY_S,
                             FD_Signal     => I,
                             LD_Signal     => EMPTY_S,
                             RM_Force_Left => False,
                             is_shutdown   => False);

         case I is
         when FRONT_BLOCKED_S =>
            expected_state.No_System_Error := FRONT_BLOCKED;
         when FRONT_CLEAR_S =>
            expected_state.No_System_Error := FRONT_CLEAR;
         when FD_FAULT_S =>
            expected_state.Base            := SYSTEM_ERROR;
            expected_state.System_Error    := STAND_ON_TRACK;
         when EMPTY_S =>
            null;
         end case;

         Assert(expected_state = output_state, "states differ on input " & I 'Image);

      end loop;

      -- test all Lane_Detection_Done_T signals and others empty/False
      for I in Lane_Detection_Done_T loop
         expected_state := input_state;
         output_state := input_state;
         do_state_transition(state         => output_state,
                             JE_Signal     => EMPTY_S,
                             FD_Signal     => EMPTY_S,
                             LD_Signal     => I,
                             RM_Force_Left => False,
                             is_shutdown   => False);

         case I is
         when SYSTEM_ERROR_S =>
            expected_state.System_Error    := FINAL_SAFE_STATE;
            expected_state.Base            := SYSTEM_ERROR;
         when GO_STRAIGHT_S =>
            expected_state.Driving := STRAIGHT;
         when ROTATE_LEFT_S =>
            expected_state.Driving := ROTATE_LEFT;
         when ROTATE_RIGHT_S =>
            expected_state.Driving := ROTATE_RIGHT;
         when EMPTY_S =>
            null;
         end case;

         Assert(expected_state = output_state, "states differ on input " & I 'Image);

      end loop;


      -- test all Job_Executer_Done_T signals when RM_Force_Left is true
      for I in Job_Executer_Done_T loop
         expected_state := input_state;
         output_state := input_state;
         do_state_transition(state         => output_state,
                             JE_Signal     => I,
                             FD_Signal     => EMPTY_S,
                             LD_Signal     => EMPTY_S,
                             RM_Force_Left => True,
                             is_shutdown   => False);

         case I is
            when SYSTEM_ERROR_S =>
               expected_state.Leaning := NEXT_LEFT;
            when NEXT_LEFT_S =>
               expected_state.Front_Is_Clear := DRIVE;
               expected_state.Leaning := NEXT_LEFT;
            when NEXT_RIGHT_S =>
               expected_state.Front_Is_Clear := DRIVE;
               expected_state.Leaning := NEXT_LEFT;
            when NEXT_UNKOWN_S =>
               -- keep lean value
               expected_state.Front_Is_Clear := DRIVE;
               expected_state.Leaning := NEXT_LEFT;
            when EMPTY_S =>
               expected_state.Leaning := NEXT_LEFT;
            when STOP_S =>
               expected_state.Front_Is_Clear := STOP;
               expected_state.Leaning := NEXT_LEFT;
         end case;

         Assert(expected_state = output_state, "states differ on input " & I 'Image);

      end loop;

      -- test shutdown signal true and all Job_Executer_Done_T signals
      for I in Job_Executer_Done_T loop
         expected_state := input_state;
         output_state := input_state;
         do_state_transition(state         => output_state,
                             JE_Signal     => I,
                             FD_Signal     => EMPTY_S,
                             LD_Signal     => EMPTY_S,
                             RM_Force_Left => False,
                             is_shutdown   => True);

         case I is
            when SYSTEM_ERROR_S =>
               expected_state.Base := SHUTDOWN;
            when NEXT_LEFT_S =>
               expected_state.Base := SHUTDOWN;
            when NEXT_RIGHT_S =>
               expected_state.Base := SHUTDOWN;
            when NEXT_UNKOWN_S =>
               expected_state.Base := SHUTDOWN;
            when EMPTY_S =>
               null;
               expected_state.Base := SHUTDOWN;
            when STOP_S =>
               expected_state.Base := SHUTDOWN;
         end case;

         Assert(expected_state.Base = output_state.Base, "states differ on input " & I'Image);

      end loop;

   end test_do_state_transition;


   procedure test_output_no_system_error (T : in out Test)
   is
   begin
      Assert(False, "TODO");
   end test_output_no_system_error;


   procedure test_output_front_is_clear (T : in out Test)
   is
   begin
      Assert(False, "TODO");
   end test_output_front_is_clear;


   procedure test_output_driving (T : in out Test)
   is
   begin
      Assert(False, "TODO");
   end test_output_driving;


   --  type Motor_Values_Long_Float_Array_T is array (Motor_ID_T) of Long_Float;
   --
   --  protected type Motor_Values_T is
   --     procedure set (ID : Motor_ID_T; Value : Long_Float);
   --     function get (ID : Motor_ID_T) return Long_Float;
   --  private
   --     Motor_Values_Array : Motor_Values_Long_Float_Array_T;
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
   --  procedure dummy_set (ID : Motor_ID_T; Value : Long_Float) is
   --  begin
   --     Motor_Values.set (ID, Value);
   --  end dummy_set;
   --
   --  procedure Log_Line(Test_Name : String; Message : String) is
   --  begin
   --     Put_Line("[" & Test_Name & "] " & Message);
   --  end Log_Line;
   --
   --
   --  -------------------------------
   --  -- test_normal_driving_states --
   --  -------------------------------
   --
   --  -- only tests output in each state, not transistions
   --
   --  procedure test_normal_driving_states (T : in out Test) is
   --     pragma Unreferenced (T);
   --     type Motor_Controller_Task_Array_T is
   --       array
   --         (Motor_Controller.Normal_Driving_State_T)
   --         of Motor_Controller.Motor_Controller_Task_Access_T;
   --
   --     Motor_Controller_Task_Array : Motor_Controller_Task_Array_T;
   --     Motor_Straight_Speed        : constant := 5.0;
   --     Motor_Turn_Speed            : constant := 3.0;
   --     timeout                     : constant := 2.0;
   --     Test_Name                   : String := "test_normal_driving_states";
   --     front_distance_next_v       : Front_Distance_Next_t;
   --     job_executer_next_v         : Job_Executer_Next_t;
   --     lane_detection_next_v       : Lane_Detection_Next_T;
   --
   --     procedure proceed(motor_task_a : in Motor_Controller_Task_Access_T) is
   --     begin
   --        null;
   --        motor_task_a.front_distance_done(EMPTY_S);
   --        motor_task_a.job_executer_done(EMPTY_S);
   --        motor_task_a.lane_detection_done(EMPTY_S);
   --        motor_task_a.main_shutdown_signal(is_shutdown => False);
   --        motor_task_a.front_distance_next(Signal => front_distance_next_v);
   --        motor_task_a.job_executer_next(job_executer_next_v);
   --        motor_task_a.lane_detection_next(lane_detection_next_v);
   --     end proceed;
   --
   --  begin
   --     -- check all drive states
   --     for I in Motor_Controller.Normal_Driving_State_T loop
   --        Put_Line ("Starting case " & I'Image);
   --        Motor_Controller_Task_Array (I) := new Motor_Controller_Task_T;
   --        Motor_Controller_Task_Array (I).Constructor(MC_State               => NORMAL_DRIVING,
   --                                                    ND_State               => I,
   --                                                    FC_State               => DRIVE,
   --                                                    D_State                => STRAIGHT,
   --                                                    LE_State               => NEXT_UNKOWN,
   --                                                    SE_State               => STOP,
   --                                                    MS_Speed               => Motor_Straight_Speed,
   --                                                    MT_Speed               => Motor_Turn_Speed,
   --                                                    set_motor_value_access => dummy_set'Access,
   --                                                    timeout_v              => timeout);
   --        Put_Line ("Constructor done:  " & I'Image);
   --
   --        proceed(Motor_Controller_Task_Array (I));
   --        proceed(Motor_Controller_Task_Array (I));
   --
   --           -- ouput of motor controller is delayed by one iteration -> wait an
   --           -- additional iteration
   --
   --        case I is
   --           when FRONT_CLEAR =>
   --              for J in Motor_ID_T loop
   --                 Assert
   --                   (Motor_Values.get (J) = Motor_Straight_Speed,
   --                    "State FRONT_CLEAR: Expected motor speed Motor_Straight_Speed, got non Motor_Straight_Speed");
   --              end loop;
   --              null;
   --           when FRONT_BLOCKED =>
   --              for J in Motor_ID_T loop
   --                 Assert
   --                   (Motor_Values.get (J) = 0.0,
   --                    "State FRONT_BLOCKED: Expected motor speed 0.0, got non 0.0");
   --              end loop;
   --        end case;
   --        Put_Line ("Finishing case " & I'Image);
   --
   --        -- Shutting down everything
   --        Motor_Controller_Task_Array (I).front_distance_done(EMPTY_S);
   --        Motor_Controller_Task_Array (I).job_executer_done(EMPTY_S);
   --        Motor_Controller_Task_Array (I).lane_detection_done(EMPTY_S);
   --
   --        Motor_Controller_Task_Array (I).main_shutdown_signal(is_shutdown => True);
   --
   --        Motor_Controller_Task_Array (I).front_distance_next(Signal => front_distance_next_v);
   --        Assert(front_distance_next_v = SHUTDOWN_S, "front_distance_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --        Motor_Controller_Task_Array (I).job_executer_next(job_executer_next_v);
   --        Assert(job_executer_next_v = SHUTDOWN_S, "job_executer_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --        Motor_Controller_Task_Array (I).lane_detection_next(lane_detection_next_v);
   --        Assert(lane_detection_next_v = SHUTDOWN_S, "lane_detection_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --     end loop;
   --     Put_Line ("Finishing test case");
   --  end test_normal_driving_states;
   --
   --
   --  ----------------------------
   --  -- test_front_clear_state --
   --  ----------------------------
   --
   --  -- only tests output in each state, not transistions
   --
   --  procedure test_front_clear_states (T : in out Test) is
   --     pragma Unreferenced (T);
   --     type Motor_Controller_Task_Array_T is
   --       array
   --         (Motor_Controller.Front_Clear_State_T)
   --         of Motor_Controller.Motor_Controller_Task_Access_T;
   --
   --     Motor_Controller_Task_Array : Motor_Controller_Task_Array_T;
   --     Motor_Straight_Speed        : constant := 5.0;
   --     Motor_Turn_Speed            : constant := 3.0;
   --     timeout                     : constant := 2.0;
   --     Test_Name                   : String := "test_normal_driving_states";
   --     front_distance_next_v       : Front_Distance_Next_t;
   --     job_executer_next_v         : Job_Executer_Next_t;
   --     lane_detection_next_v       : Lane_Detection_Next_T;
   --
   --     procedure proceed(motor_task_a : in Motor_Controller_Task_Access_T) is
   --     begin
   --        null;
   --        motor_task_a.front_distance_done(EMPTY_S);
   --        motor_task_a.job_executer_done(EMPTY_S);
   --        motor_task_a.lane_detection_done(EMPTY_S);
   --        motor_task_a.main_shutdown_signal(is_shutdown => False);
   --        motor_task_a.front_distance_next(Signal => front_distance_next_v);
   --        motor_task_a.job_executer_next(job_executer_next_v);
   --        motor_task_a.lane_detection_next(lane_detection_next_v);
   --     end proceed;
   --
   --  begin
   --     -- check all drive states
   --     for I in Motor_Controller.Front_Clear_State_T loop
   --        Put_Line ("Starting case " & I'Image);
   --        Motor_Controller_Task_Array (I) := new Motor_Controller_Task_T;
   --        Motor_Controller_Task_Array (I).Constructor(MC_State               => NORMAL_DRIVING,
   --                                                    ND_State               => FRONT_CLEAR,
   --                                                    FC_State               => I,
   --                                                    D_State                => STRAIGHT,
   --                                                    LE_State               => NEXT_UNKOWN,
   --                                                    SE_State               => STOP,
   --                                                    MS_Speed               => Motor_Straight_Speed,
   --                                                    MT_Speed               => Motor_Turn_Speed,
   --                                                    set_motor_value_access => dummy_set'Access,
   --                                                    timeout_v              => timeout);
   --        Put_Line ("Constructor done:  " & I'Image);
   --
   --        proceed(Motor_Controller_Task_Array (I));
   --        proceed(Motor_Controller_Task_Array (I));
   --
   --           -- ouput of motor controller is delayed by one iteration -> wait an
   --           -- additional iteration
   --
   --        case I is
   --           when DRIVE =>
   --              for J in Motor_ID_T loop
   --                 Assert
   --                   (Motor_Values.get (J) = Motor_Straight_Speed,
   --                    "State DRIVE: Expected motor speed Motor_Straight_Speed, got non Motor_Straight_Speed");
   --              end loop;
   --              null;
   --           when STOP =>
   --              for J in Motor_ID_T loop
   --                 Assert
   --                   (Motor_Values.get (J) = 0.0,
   --                    "State STOP: Expected motor speed 0.0, got non 0.0");
   --              end loop;
   --        end case;
   --        Put_Line ("Finishing case " & I'Image);
   --
   --        -- Shutting down everything
   --        Motor_Controller_Task_Array (I).front_distance_done(EMPTY_S);
   --        Motor_Controller_Task_Array (I).job_executer_done(EMPTY_S);
   --        Motor_Controller_Task_Array (I).lane_detection_done(EMPTY_S);
   --
   --        Motor_Controller_Task_Array (I).main_shutdown_signal(is_shutdown => True);
   --
   --        Motor_Controller_Task_Array (I).front_distance_next(Signal => front_distance_next_v);
   --        Assert(front_distance_next_v = SHUTDOWN_S, "front_distance_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --        Motor_Controller_Task_Array (I).job_executer_next(job_executer_next_v);
   --        Assert(job_executer_next_v = SHUTDOWN_S, "job_executer_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --        Motor_Controller_Task_Array (I).lane_detection_next(lane_detection_next_v);
   --        Assert(lane_detection_next_v = SHUTDOWN_S, "lane_detection_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --     end loop;
   --     Put_Line ("Finishing test case");
   --  end test_front_clear_states;
   --
   --
   --  -----------------------
   --  -- test_drive_states --
   --  -----------------------
   --
   --  -- only tests output in each state, not transistions
   --
   --  procedure test_drive_states (T : in out Test) is
   --     pragma Unreferenced (T);
   --     type Motor_Controller_Task_Array_T is
   --       array
   --         (Motor_Controller.Drive_State_T)
   --         of Motor_Controller.Motor_Controller_Task_Access_T;
   --
   --     Motor_Controller_Task_Array : Motor_Controller_Task_Array_T;
   --     Motor_Straight_Speed        : constant := 5.0;
   --     Motor_Turn_Speed            : constant := 3.0;
   --     timeout                     : constant := 2.0;
   --     Test_Name                   : String := "test_drive_states";
   --     front_distance_next_v       : Front_Distance_Next_t;
   --     job_executer_next_v         : Job_Executer_Next_t;
   --     lane_detection_next_v       : Lane_Detection_Next_T;
   --
   --     procedure proceed(motor_task_a : in Motor_Controller_Task_Access_T) is
   --     begin
   --        null;
   --        motor_task_a.front_distance_done(EMPTY_S);
   --        motor_task_a.job_executer_done(EMPTY_S);
   --        motor_task_a.lane_detection_done(EMPTY_S);
   --        Log_Line(Test_Name, "sending shutdown false...");
   --        motor_task_a.main_shutdown_signal(is_shutdown => False);
   --        Log_Line(Test_Name, "sending shutdown false done!");
   --        motor_task_a.front_distance_next(Signal => front_distance_next_v);
   --        Log_Line(Test_Name, "sending front_distance_next done!");
   --        motor_task_a.job_executer_next(job_executer_next_v);
   --        motor_task_a.lane_detection_next(lane_detection_next_v);
   --     end proceed;
   --
   --  begin
   --     -- check all drive states
   --     for I in Motor_Controller.Drive_State_T loop
   --        Put_Line ("Starting case " & I'Image);
   --        Motor_Controller_Task_Array (I) := new Motor_Controller_Task_T;
   --        Motor_Controller_Task_Array (I).Constructor(MC_State               => NORMAL_DRIVING,
   --                                                    ND_State               => FRONT_CLEAR,
   --                                                    FC_State               => DRIVE,
   --                                                    D_State                => I,
   --                                                    LE_State               => NEXT_UNKOWN,
   --                                                    SE_State               => STOP,
   --                                                    MS_Speed               => Motor_Straight_Speed,
   --                                                    MT_Speed               => Motor_Turn_Speed,
   --                                                    set_motor_value_access => dummy_set'Access,
   --                                                    timeout_v              => timeout);
   --        Put_Line ("Constructor done:  " & I'Image);
   --
   --        proceed(Motor_Controller_Task_Array (I));
   --        proceed(Motor_Controller_Task_Array (I));
   --
   --           -- ouput of motor controller is delayed by one iteration -> wait an
   --           -- additional iteration
   --
   --        case I is
   --           when STRAIGHT =>
   --              for J in Motor_ID_T loop
   --                 Assert
   --                   (Motor_Values.get (J) = Motor_Straight_Speed,
   --                    "Setting System_Error: Expected motor speed Motor_Straight_Speed, got non Motor_Straight_Speed");
   --              end loop;
   --              null;
   --           when LEFT =>
   --              Assert
   --                (Motor_Values.get (MOTOR_FRONT_LEFT) = 0.0,
   --                 "Go_Left: Expected motor_front_left = 0, got non 0");
   --              Assert
   --                (Motor_Values.get (MOTOR_BACK_LEFT) = 0.0,
   --                 "Go_Left: Expected motor_back_left = 0, got non 0");
   --              Assert
   --                (Motor_Values.get (MOTOR_FRONT_RIGHT) = Motor_Turn_Speed,
   --                 "Go_Left: Expected MOTOR_FRONT_RIGHT = Motor_Turn_Speed, got non Motor_Turn_Speed");
   --              Assert
   --                (Motor_Values.get (MOTOR_BACK_RIGHT) = Motor_Turn_Speed,
   --                 "Go_Left: Expected MOTOR_BACK_RIGHT = Motor_Turn_Speed, got non Motor_Turn_Speed");
   --              null;
   --           when RIGHT =>
   --              Assert
   --                (Motor_Values.get (MOTOR_FRONT_LEFT) = Motor_Turn_Speed,
   --                 "Go_Left: Expected motor_front_left = Motor_Turn_Speed, got non Motor_Turn_Speed");
   --              Assert
   --                (Motor_Values.get (MOTOR_BACK_LEFT) = Motor_Turn_Speed,
   --                 "Go_Left: Expected motor_back_left = Motor_Turn_Speed, got non Motor_Turn_Speed");
   --              Assert
   --                (Motor_Values.get (MOTOR_FRONT_RIGHT) = 0.0,
   --                 "Go_Left: Expected MOTOR_FRONT_RIGHT = 0.0, got non 0.0");
   --              Assert
   --                (Motor_Values.get (MOTOR_BACK_RIGHT) = 0.0,
   --                 "Go_Left: Expected MOTOR_BACK_RIGHT = 0.0, got non 0.0");
   --              null;
   --           when INIT =>
   --              for J in Motor_ID_T loop
   --                 Assert
   --                   (Motor_Values.get (J) = 0.0,
   --                    "Setting System_Error: Expected motor speed 0, got non 0");
   --              end loop;
   --        end case;
   --        Put_Line ("Finishing case " & I'Image);
   --
   --        -- Shutting down everything
   --        Motor_Controller_Task_Array (I).front_distance_done(EMPTY_S);
   --        Motor_Controller_Task_Array (I).job_executer_done(EMPTY_S);
   --        Motor_Controller_Task_Array (I).lane_detection_done(EMPTY_S);
   --
   --        Motor_Controller_Task_Array (I).main_shutdown_signal(is_shutdown => True);
   --
   --        Motor_Controller_Task_Array (I).front_distance_next(Signal => front_distance_next_v);
   --        Assert(front_distance_next_v = SHUTDOWN_S, "front_distance_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --        Motor_Controller_Task_Array (I).job_executer_next(job_executer_next_v);
   --        Assert(job_executer_next_v = SHUTDOWN_S, "job_executer_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --        Motor_Controller_Task_Array (I).lane_detection_next(lane_detection_next_v);
   --        Assert(lane_detection_next_v = SHUTDOWN_S, "lane_detection_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --     end loop;
   --     Put_Line ("Finishing test case");
   --  end test_drive_states;
   --
   --
   --
   --  ----------------------
   --  -- test_lean_states --
   --  ----------------------
   --
   --  -- only tests output in each state, not transistions
   --
   --  procedure test_lean_states (T : in out Test) is
   --     pragma Unreferenced (T);
   --     type Motor_Controller_Task_Array_T is
   --       array
   --         (Motor_Controller.Lean_State_T)
   --         of Motor_Controller.Motor_Controller_Task_Access_T;
   --
   --     Motor_Controller_Task_Array : Motor_Controller_Task_Array_T;
   --     Motor_Straight_Speed        : constant := 5.0;
   --     Motor_Turn_Speed            : constant := 3.0;
   --     timeout                     : constant := 2.0;
   --     Test_Name                   : String := "test_drive_states";
   --     front_distance_next_v       : Front_Distance_Next_t;
   --     job_executer_next_v         : Job_Executer_Next_t;
   --     lane_detection_next_v       : Lane_Detection_Next_T;
   --
   --     procedure proceed(motor_task_a : in Motor_Controller_Task_Access_T) is
   --     begin
   --        null;
   --        motor_task_a.front_distance_done(EMPTY_S);
   --        motor_task_a.job_executer_done(EMPTY_S);
   --        motor_task_a.lane_detection_done(EMPTY_S);
   --        Log_Line(Test_Name, "sending shutdown false...");
   --        motor_task_a.main_shutdown_signal(is_shutdown => False);
   --        Log_Line(Test_Name, "sending shutdown false done!");
   --        motor_task_a.front_distance_next(Signal => front_distance_next_v);
   --        Log_Line(Test_Name, "sending front_distance_next done!");
   --        motor_task_a.job_executer_next(job_executer_next_v);
   --        motor_task_a.lane_detection_next(lane_detection_next_v);
   --     end proceed;
   --
   --  begin
   --     -- check all drive states
   --     for I in Motor_Controller.Lean_State_T loop
   --        Put_Line ("Starting case " & I'Image);
   --        Motor_Controller_Task_Array (I) := new Motor_Controller_Task_T;
   --        Motor_Controller_Task_Array (I).Constructor(MC_State               => NORMAL_DRIVING,
   --                                                    ND_State               => FRONT_CLEAR,
   --                                                    FC_State               => DRIVE,
   --                                                    D_State                => STRAIGHT,
   --                                                    LE_State               => I,
   --                                                    SE_State               => STOP,
   --                                                    MS_Speed               => Motor_Straight_Speed,
   --                                                    MT_Speed               => Motor_Turn_Speed,
   --                                                    set_motor_value_access => dummy_set'Access,
   --                                                    timeout_v              => timeout);
   --        Put_Line ("Constructor done:  " & I'Image);
   --
   --        proceed(Motor_Controller_Task_Array (I));
   --        proceed(Motor_Controller_Task_Array (I));
   --
   --           -- ouput of motor controller is delayed by one iteration -> wait an
   --           -- additional iteration
   --
   --        case I is
   --           when NEXT_LEFT =>
   --              Assert(lane_detection_next_v = LEAN_LEFT_S, "State NEXT_LEFT: expected LEAN_LEFT_S in lane_detection_left, got " & lane_detection_next_v'Image);
   --              null;
   --           when NEXT_RIGHT =>
   --              Assert(lane_detection_next_v = LEAN_RIGHT_S, "State NEXT_LEFT: expected LEAN_RIGHT_S in lane_detection_left, got " & lane_detection_next_v'Image);
   --              null;
   --           when NEXT_UNKOWN =>
   --              Assert(lane_detection_next_v = NO_LEAN_S, "State NEXT_UNKOWN: expected NO_LEAN_S in lane_detection_left, got " & lane_detection_next_v'Image);
   --              null;
   --        end case;
   --        Put_Line ("Finishing case " & I'Image);
   --
   --        -- Shutting down everything
   --        Motor_Controller_Task_Array (I).front_distance_done(EMPTY_S);
   --        Motor_Controller_Task_Array (I).job_executer_done(EMPTY_S);
   --        Motor_Controller_Task_Array (I).lane_detection_done(EMPTY_S);
   --
   --        Motor_Controller_Task_Array (I).main_shutdown_signal(is_shutdown => True);
   --
   --        Motor_Controller_Task_Array (I).front_distance_next(Signal => front_distance_next_v);
   --        Assert(front_distance_next_v = SHUTDOWN_S, "front_distance_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --        Motor_Controller_Task_Array (I).job_executer_next(job_executer_next_v);
   --        Assert(job_executer_next_v = SHUTDOWN_S, "job_executer_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --        Motor_Controller_Task_Array (I).lane_detection_next(lane_detection_next_v);
   --        Assert(lane_detection_next_v = SHUTDOWN_S, "lane_detection_next_v not SHUTDOWN_S after main_shutdown_signal sent!");
   --     end loop;
   --     Put_Line ("Finishing test case");
   --  end test_lean_states;

end Motor_Controller.Test;
