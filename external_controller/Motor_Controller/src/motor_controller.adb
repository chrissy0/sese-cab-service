-- Final safe state not implemented yet!
pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Motor_Controller is

   procedure Log_Line (message : String) is
   begin
      Put_Line ("[motor_controller] " & message);
   end Log_Line;


   procedure do_state_transition
     (
      state         : in out Cab_State_T;
      JE_Signal     : Job_Executer_Done_T;
      FD_Signal     : Front_Distance_Done_t;
      LD_Signal     : Lane_Detection_Done_T;
      RM_Force_Left : Boolean;
      is_shutdown   : Boolean
     )
   is
   begin
      case JE_Signal is
         when SYSTEM_ERROR_S =>
            state.Leaning := LEAN_FROM_LINE;
         when NEXT_LEFT_S =>
            state.Front_Is_Clear := DRIVE;
            state.Leaning := NEXT_LEFT;
         when NEXT_RIGHT_S =>
            state.Front_Is_Clear := DRIVE;
            state.Leaning := NEXT_RIGHT;
         when NEXT_UNKOWN_S =>
            -- keep lean value
            state.Front_Is_Clear := DRIVE;
         when EMPTY_S =>
            null;
         when STOP_S =>
            state.Front_Is_Clear := STOP;
      end case;

      case FD_Signal is
         when FRONT_BLOCKED_S =>
            state.No_System_Error := FRONT_BLOCKED;
         when FRONT_CLEAR_S =>
            state.No_System_Error := FRONT_CLEAR;
         when FD_FAULT_S =>
            state.Base            := SYSTEM_ERROR;
            state.System_Error    := STAND_ON_TRACK;
         when EMPTY_S =>
            null;
      end case;

      case LD_Signal is
         when SYSTEM_ERROR_S =>
            state.System_Error    := FINAL_SAFE_STATE;
            state.Base            := SYSTEM_ERROR;
         when GO_STRAIGHT_S =>
            state.Driving := STRAIGHT;
         when ROTATE_LEFT_S =>
            state.Driving := ROTATE_LEFT;
         when ROTATE_RIGHT_S =>
            state.Driving := ROTATE_RIGHT;
         when EMPTY_S =>
            null;
      end case;

      if RM_Force_Left then
         state.Leaning := NEXT_LEFT;
      end if;

      if is_shutdown then
         state.Base := SHUTDOWN;
      end if;


   end do_state_transition;




   -- calculate motor actor output for drive state
   procedure calculate_output
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T;
      LD_Next_Signal : out Lane_Detection_Next_T;
      FD_Next_Signal : out Front_Distance_Next_t;
      JE_Next_Signal : out Job_Executer_Next_t
     )
   is
   begin

      case state.Leaning is
         when NEXT_LEFT =>
            LD_Next_Signal := LEAN_LEFT_S;
         when NEXT_RIGHT =>
            LD_Next_Signal := LEAN_RIGHT_S;
         when LEAN_FROM_LINE =>
            LD_Next_Signal := LEAN_FROM_LINE;
      end case;

      case state.Base is
         when SYSTEM_ERROR =>
            null;
         when NO_SYSTEM_ERROR =>
            FD_Next_Signal := EMPTY_S;
            output_no_system_error(state          => state,
                                   motor_values   => motor_values,
                                   JE_Next_Signal => JE_Next_Signal);
         when SHUTDOWN =>
            motor_values := (others => (others => 0.0));
            LD_Next_Signal := SHUTDOWN_S;
            FD_Next_Signal := SHUTDOWN_S;
            JE_Next_Signal := SHUTDOWN_S;
      end case;

   end calculate_output;

   procedure output_no_system_error
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T;
      JE_Next_Signal : out Job_Executer_Next_t
     )
   is
   begin
      case state.No_System_Error is
         when FRONT_CLEAR =>
            JE_Next_Signal := EMPTY_S;
            output_front_is_clear(state          => state,
                                  motor_values   => motor_values);
         when FRONT_BLOCKED =>
            JE_Next_Signal := BLOCKED_S;
            motor_values := (others => (others => 0.0));

      end case;
   end output_no_system_error;

   procedure output_front_is_clear
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T
     )
   is
   begin
      case state.Front_Is_Clear is
         when DRIVE =>
            output_driving(state          => state,
                           motor_values   => motor_values);
         when STOP =>
            motor_values := (others => (others => 0.0));
      end case;
   end output_front_is_clear;

   procedure output_driving
     (
      state          : Cab_State_T;
      motor_values   : out Motor_Values_T
     )
   is
   begin
      case state.Driving is
         when STRAIGHT =>
            motor_values := (others => (others => MOTOR_DRIVE_SPEED));

         when ROTATE_LEFT =>
            for v in Vertical_Position_T loop
               motor_values(v, LEFT)  := -MOTOR_ROTATE_SPEED;
               motor_values(v, RIGHT) := MOTOR_ROTATE_SPEED;
            end loop;
         when ROTATE_RIGHT =>
            for v in Vertical_Position_T loop
               motor_values(v, LEFT)  := MOTOR_ROTATE_SPEED;
               motor_values(v, RIGHT) := -MOTOR_ROTATE_SPEED;
            end loop;
         when INIT =>
            motor_values := (others => (others => 0.0));
      end case;
   end output_driving;

   procedure apply_motor_values
     (
      motor_values : Motor_Values_T;
      set_motor_value : set_motor_value_procedure_access_t
     )
   is
   begin
      for V in Vertical_Position_T loop
         for H in Horizontal_Position_T loop
            set_motor_value(V, H, motor_values(V, H));
         end loop;
      end loop;
   end;


   -- set all_task_done_array to false => no package task done yet!
   procedure reset_all_tasks_done(all_tasks_done_array : in out Boolean_Tasks_Arrays)
   is
   begin
      for I in Module_Tasks loop
         all_tasks_done_array(I) := False;
      end loop;
   end reset_all_tasks_done;

   -- check if all package tasks are done
   function are_all_tasks_done(all_tasks_done_array : in out Boolean_Tasks_Arrays) return Boolean
   is
   begin
      for I in Module_Tasks loop
         if not all_tasks_done_array(I) then
            return False;
         end if;
      end loop;
      return True;

   end are_all_tasks_done;

   ---------------------------
   -- Motor_Controller_Task --
   ---------------------------


   task body Motor_Controller_Task_T is
      state           : Cab_State_T := (Base            => NO_SYSTEM_ERROR,
                                        System_Error    => STAND_ON_TRACK,
                                        No_System_Error => FRONT_BLOCKED,
                                        Front_Is_Clear  => Drive,
                                        Driving         => Init,
                                        Leaning         => NEXT_LEFT,
                                        Forcing_Left    => False
                                       );

      set_motor_value            : set_motor_value_procedure_access_t;
      motor_values               : Motor_Values_T;
      running                    : Boolean                  := True;
      timeout                    : Duration;
      Iteration_Delay            : Duration;

      Job_Executer_Next_Signal   : Job_Executer_Next_t      := EMPTY_S;
      Lane_Detection_Next_Signal : Lane_Detection_Next_T    := EMPTY_S;
      Front_Distance_Next_Signal : Front_Distance_Next_t    := EMPTY_S;
      Job_Executer_Next_Signal_o   : Job_Executer_Next_t      := EMPTY_S;
      Lane_Detection_Next_Signal_o : Lane_Detection_Next_T    := EMPTY_S;
      Front_Distance_Next_Signal_o : Front_Distance_Next_t    := EMPTY_S;

      Job_Executer_Done_Signal      : Job_Executer_Done_t      := EMPTY_S;
      Lane_Detection_Done_Signal    : Lane_Detection_Done_T    := EMPTY_S;
      Front_Distance_Done_Signal    : Front_Distance_Done_t    := EMPTY_S;
      Job_Executer_Done_Signal_o      : Job_Executer_Done_t      := EMPTY_S;
      Lane_Detection_Done_Signal_o    : Lane_Detection_Done_T    := EMPTY_S;
      Front_Distance_Done_Signal_o    : Front_Distance_Done_t    := EMPTY_S;
      Road_Marker_Force_Left_Signal : Boolean                  := False;
      Main_Force_Shutdown_Signal    : Boolean                  := False;

      task_done_array            : Boolean_Tasks_Arrays;
      got_force_left             : Boolean                  := False;
   begin

      Log_Line("Starting Thread.");
      Log_Line("Waiting for Construct...");

      accept Constructor
        (
         set_motor_value_access : in set_motor_value_procedure_access_t;
         timeout_v              : in Duration;
         iteration_delay_s      : in Duration
        )
      do
         set_motor_value        := set_motor_value_access;
         timeout                := timeout_v;
         Iteration_Delay        := iteration_delay_s;

      end Constructor;
      Log_Line("... constructor done");

      -- main loop
      while running loop


         -- initialize task_done_array
         reset_all_tasks_done(all_tasks_done_array => task_done_array);
         got_force_left := False;

         while not (are_all_tasks_done(task_done_array) and got_force_left) loop
            select
               accept job_executer_done (Signal : in Job_Executer_Done_T) do
                  Job_Executer_Done_Signal := Signal;
                  task_done_array(JOB_EXECUTER) := True;
               end job_executer_done;
            or
               accept lane_detection_done
                 (Signal : in Lane_Detection_Done_T)
               do
                  Lane_Detection_Done_Signal := Signal;
                  task_done_array(LANE_DETECTION) := True;

               end lane_detection_done;
            or

               accept front_distance_done
                 (Signal : in Front_Distance_Done_t)
               do
                  Front_Distance_Done_Signal := Signal;
                  task_done_array(FRONT_DISTANCE) := True;
               end front_distance_done;
            or
               accept rm_hotfix_signal
                 (Signal : in Boolean)
               do
                  Road_Marker_Force_Left_Signal := Signal;
                  got_force_left := True;
               end rm_hotfix_signal;

            or
               delay timeout;
               Log_Line
                 ("done signals timed out, killing External_Controller");

               Log_Line
                 ("rm_hotfix_signal := " & got_force_left'Image);
               for I in Module_Tasks loop
                  Log_Line("tasks_done_array(" & I'Image & ") = " & task_done_array(I)'Image);
               end loop;

               running := False;

               goto Continue;
            end select;

         end loop;



         -- check for main task to exit
         select
            accept main_shutdown_signal (is_shutdown : in Boolean) do
               Main_Force_Shutdown_Signal := is_shutdown;
               if is_shutdown then
                  running := False;
                  Log_Line ("Shutting down..");
               end if;
            end main_shutdown_signal;
         or
            delay timeout;
            Log_Line
              ("main_shutdown_signal timed out, killing External_Controller");
            running := False;
            goto Continue;
         end select;


         -- Output stuff depending on State
         if Front_Distance_Done_Signal /= Front_Distance_Done_Signal_o then
            Log_Line("Front_Distance_Done_Signal = " & Front_Distance_Done_Signal'Image);
            Front_Distance_Done_Signal_o := Front_Distance_Done_Signal;
         end if;
         -- Output stuff depending on State
         if Job_Executer_Done_Signal /= Job_Executer_Done_Signal_o then
            Log_Line("Job_Executer_Done_Signal = " & Job_Executer_Done_Signal'Image);
            Job_Executer_Done_Signal_o := Job_Executer_Done_Signal;
         end if;

         do_state_transition(state         => state,
                             JE_Signal     => Job_Executer_Done_Signal,
                             FD_Signal     => Front_Distance_Done_Signal,
                             LD_Signal     => Lane_Detection_Done_Signal,
                             RM_Force_Left => Road_Marker_Force_Left_Signal,
                             Is_Shutdown   => Main_Force_Shutdown_Signal
                            );

         calculate_output(state          => state,
                          motor_values   => motor_values,
                          LD_Next_Signal => Lane_Detection_Next_Signal,
                          FD_Next_Signal => Front_Distance_Next_Signal,
                          JE_Next_Signal => Job_Executer_Next_Signal);

         -- Output stuff depending on State
         if Lane_Detection_Next_Signal /= Lane_Detection_Next_Signal_o then
            Log_Line("Lane_Detection_Next_Signal = " & Lane_Detection_Next_Signal'Image);
            Lane_Detection_Next_Signal_o := Lane_Detection_Next_Signal;
         end if;

         -- Output stuff depending on State
         if Front_Distance_Next_Signal /= Front_Distance_Next_Signal_o then
            Log_Line("Front_Distance_Next_Signal = " & Front_Distance_Next_Signal'Image);
            Front_Distance_Next_Signal_o := Front_Distance_Next_Signal;
         end if;
         -- Output stuff depending on State
         if Job_Executer_Next_Signal /= Job_Executer_Next_Signal_o then
            Log_Line("Job_Executer_Next_Signal = " & Job_Executer_Next_Signal'Image);
            Job_Executer_Next_Signal_o := Job_Executer_Next_Signal;
         end if;


         apply_motor_values(motor_values    => motor_values,
                            set_motor_value => set_motor_value);

         delay Iteration_Delay;


         reset_all_tasks_done(all_tasks_done_array => task_done_array);

         -- all tasks wait before the motor_controller does its transistion
         -- Signal all tasks to unless it is system_error

         while not are_all_tasks_done(task_done_array) loop
            select
               accept job_executer_next (Signal : out Job_Executer_Next_t)
               do
                  Signal := Job_Executer_Next_Signal;
                  task_done_array(JOB_EXECUTER) := True;
               end job_executer_next;
            or
               accept lane_detection_next
                 (Signal : out Lane_Detection_Next_T)
               do
                  Signal := Lane_Detection_Next_Signal;
                  task_done_array(LANE_DETECTION) := True;
               end lane_detection_next;
            or
               accept front_distance_next (Signal : out Front_Distance_Next_t) do
                  Signal := Front_Distance_Next_Signal;
                  task_done_array(FRONT_DISTANCE) := True;
               end front_distance_next;
            or
               delay timeout;
               Log_Line
                 ("next signals timed out, killing External_Controller");

               for I in Module_Tasks loop
                  Log_Line("tasks_done_array(" & I'Image & ") = " & task_done_array(I)'Image);
               end loop;

               running := False;

               goto Continue;
            end select;

         end loop;

         <<Continue>>
      end loop;
      Log_Line
        ("Motor_Controller shutting down. So long, and thanks for all the gasoline");

   end Motor_Controller_Task_T;

end Motor_Controller;
