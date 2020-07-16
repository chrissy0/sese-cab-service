-- @summary
-- Motor Controller child test package body.
--
-- @author Julian Hartmer

pragma Ada_2012;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;

package body Motor_Controller.Test is

   procedure test_calculate_output (T : in out Test)
   is
   begin
      Assert(True, "TODO");
   end test_calculate_output;


   procedure test_output_no_system_error (T : in out Test)
   is
   begin
      Assert(True, "TODO");
   end test_output_no_system_error;


   procedure test_output_front_is_clear (T : in out Test)
   is
   begin
      Assert(True, "TODO");
   end test_output_front_is_clear;


   procedure test_output_driving (T : in out Test)
   is
   begin
      Assert(True, "TODO");
   end test_output_driving;


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


end Motor_Controller.Test;
