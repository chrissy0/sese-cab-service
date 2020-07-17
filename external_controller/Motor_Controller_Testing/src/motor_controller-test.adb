pragma Ada_2012;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;

package body Motor_Controller.Test is

   procedure test_calculate_output (T : in out Test)
   is
      input_state             : Cab_State_T;
      output_state            : Cab_State_T;
      motor_values            : Motor_Values_T;
      je_next_signal          : Job_Executer_Next_t;
      fd_next_signal          : Front_Distance_Next_t;
      ld_next_signal          : Lane_Detection_Next_T;
      expected_state          : Cab_State_T;
      expected_motor_values   : Motor_Values_T;
      expected_je_next_signal : Job_Executer_Next_t;
      expected_ld_next_signal : Lane_Detection_Next_T;
      expected_fd_next_signal : Front_Distance_Next_t;
   begin
    for I in Lean_State_T loop

       input_state := (Base             => NO_SYSTEM_ERROR,
                      System_Error     => STAND_ON_TRACK,
                      No_System_Error  => FRONT_BLOCKED,
                      Final_Safe_State => ROTATE_LEFT_90,
                      Front_Is_Clear   => Drive,
                      Driving          => Init,
                      Leaning          => I,
                      Forcing_Left     => False,
                       Counter => 0);

         expected_state:= input_state;
         output_state := input_state;
         calculate_output(output_state, motor_values,ld_next_signal,fd_next_signal, je_next_signal);

         case I is
          when NEXT_LEFT =>
            expected_ld_next_signal := LEAN_LEFT_S;
          when NEXT_RIGHT =>
            expected_ld_next_signal := LEAN_RIGHT_S;
          when LEAN_FROM_LINE =>
            expected_ld_next_signal := LEAN_FROM_LINE;
         end case;

      Assert( expected_ld_next_signal= ld_next_signal, "states differ on input" &I'Image);

     end loop;


     for I in Motor_Controller_State_T loop

       input_state := (Base             => I,
                      System_Error     => STAND_ON_TRACK,
                      No_System_Error  => FRONT_BLOCKED,
                      Final_Safe_State => ROTATE_LEFT_90,
                      Front_Is_Clear   => Drive,
                      Driving          => Init,
                      Leaning          => NEXT_LEFT,
                      Forcing_Left     => False,
                       Counter => 0);

         expected_state:= input_state;
         output_state := input_state;
         calculate_output(output_state, motor_values,ld_next_signal,fd_next_signal, je_next_signal);

         case I is
          when SYSTEM_ERROR =>
            expected_fd_next_signal := EMPTY_S;
            output_system_error(state          => expected_state,
                                motor_values   => expected_motor_values,
                                JE_Next_Signal => expected_je_next_signal);
          when NO_SYSTEM_ERROR =>
            expected_fd_next_signal := EMPTY_S;
            output_no_system_error(state          => expected_state,
                                   motor_values   => expected_motor_values,
                                   JE_Next_Signal => expected_je_next_signal);
          when SHUTDOWN =>
            expected_motor_values := (others => (others => 0.0));
            expected_ld_next_signal := SHUTDOWN_S;
            expected_fd_next_signal := SHUTDOWN_S;
            expected_je_next_signal := SHUTDOWN_S;
         end case;

         Assert(expected_state= output_state and expected_motor_values = motor_values and expected_je_next_signal = je_next_signal
                and expected_fd_next_signal= fd_next_signal, "states differ on input : " &I'Image);
     end loop;


    end test_calculate_output;


   procedure test_do_state_transition (T : in out Test)
   is
      input_state         : Cab_State_T;
      output_state        : Cab_State_T;
      expected_state      : Cab_State_T;
      RM_Force_Left       : Boolean;
      is_shutdown         : Boolean;
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

   procedure test_output_final_safe_state (T : in out Test)
   is
      input_state             : Cab_State_T;
      output_state            : Cab_State_T;
      motor_values            : Motor_Values_T;
      je_next_signal          : Job_Executer_Next_t;
      expected_state          : Cab_State_T;
      expected_motor_values   : Motor_Values_T;
      expected_je_next_signal : Job_Executer_Next_t;
      MOTOR_ROTATE_SPEED : constant Long_Float := 1.0;
      MOTOR_DRIVE_SPEED  : constant Long_Float := 3.0;
   begin
     for I in Final_Safe_State_State_t loop
      input_state := (Base             => NO_SYSTEM_ERROR,
                      System_Error     => STAND_ON_TRACK,
                      No_System_Error  => FRONT_CLEAR,
                      Final_Safe_State => I,
                      Front_Is_Clear   => DRIVE,
                      Driving          => INIT,
                      Leaning          => NEXT_LEFT,
                      Forcing_Left     => False,
                      Counter => 0);

         expected_state:= input_state;
         output_state := input_state;
         output_final_safe_state(output_state, motor_values, je_next_signal);

         case I is
          when ROTATE_LEFT_90 =>
              expected_je_next_signal := BLOCKED_S;
              for v in Vertical_Position_T loop
                 expected_motor_values(v, LEFT)  := 0.0;
                 expected_motor_values(v, RIGHT) := MOTOR_ROTATE_SPEED * 2.0;
              end loop;
              expected_je_next_signal := BLOCKED_S;
          when DRIVE_OFF_LEFT =>
              expected_je_next_signal := BLOCKED_S;
              expected_motor_values := (others => (others => MOTOR_DRIVE_SPEED * 2.0));
          when ROTATE_RIGHT_180_DEGREE =>
              expected_je_next_signal := BLOCKED_S;
              for v in Vertical_Position_T loop
                 expected_motor_values(v, LEFT)  := MOTOR_ROTATE_SPEED * 2.0;
                 expected_motor_values(v, RIGHT) := 0.0;
              end loop;
          when DRIVE_OFF_RIGHT =>
              expected_je_next_signal := BLOCKED_S;
              expected_motor_values := (others => (others => MOTOR_DRIVE_SPEED * 2.0));

          when DONE =>
              expected_je_next_signal := EMPTY_S;
              expected_motor_values := (others => (others => 0.0));
         end case;

       Assert(expected_state = output_state and expected_motor_values = motor_values and expected_je_next_signal = je_next_signal  , "states differ on input" &I'Image);
      end loop;


   end test_output_final_safe_state;




   procedure test_output_system_error (T : in out Test)
   is
      input_state             : Cab_State_T;
      output_state            : Cab_State_T;
      motor_values            : Motor_Values_T;
      je_next_signal          : Job_Executer_Next_t;
      expected_state          : Cab_State_T;
      expected_motor_values   : Motor_Values_T;
      expected_je_next_signal : Job_Executer_Next_t;
   begin
      for I in System_Error_State_T loop
       input_state := (Base             => NO_SYSTEM_ERROR,
                      System_Error     => I,
                      No_System_Error  => FRONT_CLEAR,
                      Final_Safe_State => DONE,
                      Front_Is_Clear   => DRIVE,
                      Driving          => INIT,
                      Leaning          => NEXT_LEFT,
                      Forcing_Left     => False,
                      Counter => 0);

         expected_state:= input_state;
         output_state := input_state;
         output_system_error(output_state, motor_values, je_next_signal);

         case I is
            when FINAL_SAFE_STATE =>
                output_final_safe_state(state          => expected_state,
                                       motor_values   => expected_motor_values,
                                       JE_Next_Signal => expected_je_next_signal);
            when STAND_ON_TRACK =>
               expected_je_next_signal := BLOCKED_S;
               expected_motor_values := (others => (others => 0.0));
         end case;

         Assert(expected_state = output_state and expected_motor_values = motor_values and expected_je_next_signal = je_next_signal  , "states differ on input" &I'Image);
         end loop;
    end test_output_system_error;

   procedure test_output_no_system_error (T : in out Test)
   is
      input_state             : Cab_State_T;
      output_state            : Cab_State_T;
      motor_values            : Motor_Values_T;
      je_next_signal          : Job_Executer_Next_t;
      expected_state          : Cab_State_T;
      expected_motor_values   : Motor_Values_T;
      expected_je_next_signal : Job_Executer_Next_t;
   begin
      for I in No_System_Error_State_T loop
      input_state := (Base             => NO_SYSTEM_ERROR,
                      System_Error     => STAND_ON_TRACK,
                      No_System_Error  => I,
                      Final_Safe_State => DONE,
                      Front_Is_Clear   => DRIVE,
                      Driving          => INIT,
                      Leaning          => NEXT_LEFT,
                      Forcing_Left     => False,
                      Counter => 0);

         expected_state:= input_state;
         output_state := input_state;
         output_no_system_error(output_state, motor_values, je_next_signal);

         case I is
            when FRONT_CLEAR =>
               expected_je_next_signal := EMPTY_S;
               output_front_is_clear(state          => expected_state,
                                     motor_values   => expected_motor_values);
            when FRONT_BLOCKED =>
               expected_je_next_signal := BLOCKED_S;
               expected_motor_values := (others => (others => 0.0));
         end case;

         Assert(expected_state = output_state and expected_motor_values = motor_values and expected_je_next_signal = je_next_signal  , "states differ on input" &I'Image);
         end loop;
   end test_output_no_system_error;


   procedure test_output_front_is_clear (T : in out Test)
   is
      input_state           : Cab_State_T;
      output_state          : Cab_State_T;
      motor_values          : Motor_Values_T;
      expected_state        : Cab_State_T;
      expected_motor_values : Motor_Values_T;
   begin
      for I in Front_Clear_State_T loop
       input_state := (Base             => NO_SYSTEM_ERROR,
                      System_Error     => STAND_ON_TRACK,
                      No_System_Error  => FRONT_BLOCKED,
                      Final_Safe_State => DONE,
                      Front_Is_Clear   => I,
                      Driving          => INIT,
                      Leaning          => NEXT_LEFT,
                      Forcing_Left     => False,
                       Counter => 0);

         expected_state:= input_state;
         output_state := input_state;
         output_front_is_clear(output_state, motor_values);

         case I is
            when DRIVE =>
                output_driving(state          => expected_state,
                               motor_values   => expected_motor_values);
            when STOP =>
               expected_motor_values := (others => (others => 0.0));
         end case;
         Assert(expected_state = output_state and expected_motor_values = motor_values , "states differ on input" &I'Image);
       end loop;
   end test_output_front_is_clear;


   procedure test_output_driving (T : in out Test)
   is
      input_state           : Cab_State_T;
      output_state          : Cab_State_T;
      motor_values          : Motor_Values_T;
      expected_state        : Cab_State_T;
      expected_motor_values : Motor_Values_T;
      MOTOR_DRIVE_SPEED  : constant Long_Float := 3.0;
   begin
      for I in Drive_State_T loop
       input_state := (Base             => NO_SYSTEM_ERROR,
                      System_Error     => STAND_ON_TRACK,
                      No_System_Error  => FRONT_BLOCKED,
                      Final_Safe_State => DONE,
                      Front_Is_Clear   => Drive,
                      Driving          => I,
                      Leaning          => NEXT_LEFT,
                      Forcing_Left     => False,
                      Counter => 0);


         expected_state:= input_state;
         output_state := input_state;
         output_driving(output_state, motor_values);

         case I is
            when STRAIGHT =>
               expected_state.Driving := STRAIGHT;
               expected_motor_values := (others => (others => MOTOR_DRIVE_SPEED));
            when ROTATE_LEFT =>
               expected_state.Driving := ROTATE_LEFT;
               for v in Vertical_Position_T loop
               expected_motor_values(v, LEFT)  := -MOTOR_ROTATE_SPEED;
               expected_motor_values(v, RIGHT) := MOTOR_ROTATE_SPEED;
               end loop;
            when ROTATE_RIGHT =>
               expected_state.Driving := ROTATE_RIGHT;
               for v in Vertical_Position_T loop
               expected_motor_values(v, LEFT)  := MOTOR_ROTATE_SPEED;
               expected_motor_values(v, RIGHT) := -MOTOR_ROTATE_SPEED;
               end loop;
            when INIT =>
               expected_state.Driving := INIT;
               expected_motor_values := (others => (others => 0.0));
         end case;

         Assert(expected_state = output_state and expected_motor_values = motor_values , "states differ on input" &I'Image);
      end loop;
   end test_output_driving;




end Motor_Controller.Test;















