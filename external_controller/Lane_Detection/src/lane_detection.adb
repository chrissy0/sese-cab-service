pragma Ada_2012;
with Roadmarker_Functions; use Roadmarker_Functions;
package body Lane_Detection is

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
      loop
         -- Read sensor values
        US_Curb_Left_Value := WC2EC.get_distance_sensor_data ("dist_l");

         US_Curb_Right_Value := WC2EC.get_distance_sensor_data ("dist_r");

         IR_Lane_Right_Value := WC2EC.get_distance_sensor_data ("inf_right");
         -- IR_Lane_Right_Value := 0.0;

         IR_Lane_Left_Value := WC2EC.get_distance_sensor_data ("inf_left");
         -- IR_Lane_Left_Value := 0.0;


         IR_Lane_Mid_Value := WC2EC.get_distance_sensor_data ("inf_cent");
         -- IR_Lane_Mid_Value := 0.0;

         Output := EMPTY_S;



            if
              (IR_Lane_Right_Value < IR_Lane_Threshhold and
                 IR_Lane_Left_Value > IR_Lane_Threshhold)
            then
               Put_Line ("Sending Go Right_Infrared");
               Output := GO_RIGHT_S;
            elsif
              (IR_Lane_Left_Value < IR_Lane_Threshhold and
                 IR_Lane_Right_Value > IR_Lane_Threshhold)
            then
               Put_Line ("Sending Go Left_Infrared");

               Output := GO_LEFT_S;
            elsif
              (IR_Lane_Left_Value > IR_Lane_Threshhold and
                 IR_Lane_Right_Value > IR_Lane_Threshhold and
                   IR_Lane_Mid_Value > IR_Lane_Threshhold)
            then
               Put_Line ("Infrared Error!!!!!!!!!!!!!!!!!");
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

            else
                Put_Line("Sending Go Straight_Curb");
                Output := GO_STRAIGHT_S;
            end if;





         -- Output Signal
         Put_Line ("Sending lane_detection_done");
         Motor_Controller_Task.lane_detection_done (Output);

         Put_Line ("Waiting for main_next");
         Motor_Controller_Task
           .lane_detection_next; -- wait for all signals to be processed
         Put_Line ("Main_next recieved!");
         Put_Line ("");
      end loop;
   end Lane_Detection_Taks_T;

end Lane_Detection;
