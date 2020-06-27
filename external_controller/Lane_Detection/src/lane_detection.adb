pragma Ada_2012;
with Roadmarker_Functions; use Roadmarker_Functions;
package body Lane_Detection is

   procedure Log_Line(Message : String) is
   begin
      Put_Line("[lane_detection] " & Message);
   end Log_Line;

   ---------------------------
   -- Lane_Detection_Taks_T --
   ---------------------------

   task body Lane_Detection_Taks_T is
      US_Curb_Threshhold, IR_Lane_Threshhold  : Long_Float;
      US_Curb_Max_Value                       : Long_Float;
      Motor_Controller_Task                   : Motor_Controller_Task_Access_T;
      WC2EC_Driver                            : wc2ec_thread_access_t;
      IR_Lane_Right_Value, IR_Lane_Left_Value : Long_Float;
      IR_Lane_Mid_Value                       : Long_Float;
      US_Curb_Right_Value, US_Curb_Left_Value : Long_Float;
      next_signal                             : Lane_Detection_Next_T := NO_LEAN_S;
      running                                 : Boolean := True;
      R_Detected                              : Boolean;
      L_Detected                              : Boolean;
      C_Detected                              : Boolean;
      Leaning_Left                            : Boolean := True;
      Output                                  : Lane_Detection_Done_T;

   begin

      accept Construct
        (IR_Threshhold, US_Threshhold : in Long_Float;
         US_Max_Value                 : in Long_Float;
         Motor_Task_A                 : in Motor_Controller_Task_Access_T;
         WC2EC_Driver_A               : in wc2ec_thread_access_t)
      do
         Motor_Controller_Task := Motor_Task_A;
         US_Curb_Threshhold    := US_Threshhold;
         IR_Lane_Threshhold    := IR_Threshhold;
         WC2EC_Driver          := WC2EC_Driver_A;
         US_Curb_Max_Value     := US_Max_Value;
      end Construct;
      -- each iteration has three steps: 1. Read sensor data and calculate
      -- outputs 2. and send output via lane_detection_don(value) 3. Wait
      -- for lane_detection_next and start next iteration
      while running loop
         -- Read sensor values
         Log_Line ("Reading Sensor data ...");
         US_Curb_Left_Value := WC2EC.get_distance_sensor_data ("dist_l");

         US_Curb_Right_Value := WC2EC.get_distance_sensor_data ("dist_r");

         IR_Lane_Right_Value := WC2EC.get_distance_sensor_data ("inf_right");

         IR_Lane_Left_Value := WC2EC.get_distance_sensor_data ("inf_left");

         IR_Lane_Mid_Value := WC2EC.get_distance_sensor_data ("inf_cent");



         Log_Line ("Read Sensor Data:\n ------");

         Log_Line
           (ASCII.HT & "US_Curb_Left_Value := " & US_Curb_Left_Value'Image);
         Log_Line
           (ASCII.HT & "US_Curb_Right_Value := " & US_Curb_Right_Value'Image);
         Log_Line
           (ASCII.HT & "IR_Lane_Right_Value := " & IR_Lane_Right_Value'Image);
         Log_Line
           (ASCII.HT & "IR_Lane_Left_Value := " & IR_Lane_Left_Value'Image);
         Log_Line
           (ASCII.HT & "IR_Lane_Mid_Value := " & IR_Lane_Mid_Value'Image);
         Log_Line (" ------");


         L_Detected := IR_Lane_Left_Value < IR_Lane_Threshhold;
         R_Detected := IR_Lane_Right_Value < IR_Lane_Threshhold;
         C_Detected := IR_Lane_Mid_Value < IR_Lane_Threshhold;

         next_signal := LEAN_RIGHT_S;


         if not Leaning_Left then
            if R_Detected then
               Output := GO_RIGHT_S;
            elsif C_Detected then
               OUTPUT := GO_STRAIGHT_S;
            elsif L_Detected then
               OUTPUT := GO_LEFT_S;
            else
               OUTPUT := SYSTEM_ERROR_S;
            end if;
         else
            if L_Detected then
               Output := GO_LEFT_S;
            elsif C_Detected then
               OUTPUT := GO_STRAIGHT_S;
            elsif R_Detected then
               OUTPUT := GO_RIGHT_S;
            else
			   Log_Line("No line found, switching to curb detection!");
               if
                 (US_Curb_Left_Value > US_Curb_Threshhold and
                    US_Curb_Left_Value <1000.0 )
               then
                  Put_Line("Sending Go Right_Curb");
                  Output := GO_RIGHT_S;

               elsif
                 (US_Curb_Right_Value > US_Curb_Threshhold and
                    US_Curb_Right_Value <1000.0  )
               then
                  Put_Line("Sending Go Left_Curb");
                  Output := GO_LEFT_S;

               elsif
                 (US_Curb_Left_Value = 1000.0)
               then
                  Put_Line("Curb Error");
                  Put_Line("System Error");

               else
                  Put_Line("Sending Go Straight_Curb");
                  Output := GO_STRAIGHT_S;
               end if;
            end if;
         end if;


         -- Output Signal
         Log_Line ("Sending lane_detection_done...");
         select
           Motor_Controller_Task.lane_detection_done(Output);
         then abort
            delay 2.0;
            Log_Line("lane_detection_done timed out, shutting down...");
            running := False;
            goto Continue;
         end select;
         Log_Line(" ... lane_detection_done recieved!");

         Log_Line ("Waiting for lane_detection_next");
          -- wait for all signals to be processed
         select
            Motor_Controller_Task.lane_detection_next(next_signal);


            case next_signal is
            when LEAN_LEFT_S =>
               Leaning_Left := True;
            when LEAN_RIGHT_S=>
               Leaning_Left := False;
            when NO_LEAN_S =>
               Leaning_Left := True;
            when EMPTY_S =>
               null;
            when SHUTDOWN_S =>
               Leaning_Left := True;
               running := False;
               goto Continue;
            end case;

         then abort
            delay 2.0;
            Log_Line("lane_detection_next out, shutting down...");
            running := False;
            goto Continue;
         end select;
         Log_Line("... lane_detection_next recieved!");


         -- handle next signal
         <<Continue>>
      end loop;
      Log_Line("Shutting down. So long, and thanks for all the lanes!");
   end Lane_Detection_Taks_T;

end Lane_Detection;
