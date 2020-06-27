pragma Ada_2012;
with Roadmarker_Functions; use Roadmarker_Functions;


package body Lane_Detection is


   function Lane_Detection_Process (Output_T : in  Lane_Detection_Done_T;
                                        IR_Lane_Left_Value : in Long_Float;
                                        IR_Lane_Right_Value  : in Long_Float;
                                        IR_Lane_Mid_Value   : in Long_Float;
                                        US_Curb_Left_Value  : in Long_Float;
                                        US_Curb_Right_Value : in Long_Float;
                                        IR_Lane_Threshhold  : in Long_Float;
                                        US_Curb_Threshhold  : in Long_Float)
                                        return Lane_Detection_Done_T is
   Output  : Lane_Detection_Done_T;
   begin
      Output := Output_T;




            if
              (IR_Lane_Left_Value < IR_Lane_Threshhold and
               IR_Lane_Right_Value > IR_Lane_Threshhold)
            then
               Put_Line ("Sending Go Left_Infrared");
               Output := GO_LEFT_S;



            elsif
              (IR_Lane_Left_Value > IR_Lane_Threshhold and
               IR_Lane_Right_Value < IR_Lane_Threshhold)
            then
               Put_Line ("Sending Go Right_Infrared");
               Output := GO_RIGHT_S;


            elsif
              (IR_Lane_Left_Value > IR_Lane_Threshhold and
                 IR_Lane_Right_Value > IR_Lane_Threshhold and
                   IR_Lane_Mid_Value > IR_Lane_Threshhold)
            then
               Put_Line ("Infrared Error");
               if
                 (US_Curb_Left_Value < US_Curb_Threshhold )
               then
                  Put_Line("Sending Go Right_Curb");
                  Output := GO_RIGHT_S;

               elsif
                 (US_Curb_Right_Value < US_Curb_Threshhold )

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
                Put_Line("Sending Go Straight_Infrared");
            Output := GO_STRAIGHT_S;


         end if;

         return Output;

     end Lane_Detection_Process;

   -- Lane_Detection_Taks_T --
   ---------------------------

   task body Lane_Detection_Taks_T is

      US_Curb_Threshhold, IR_Lane_Threshhold  : Long_Float;
      US_Curb_Max_Value                       : Long_Float;

      IR_Lane_Right_Value, IR_Lane_Left_Value : Long_Float;
      IR_Lane_Mid_Value                       : Long_Float;
      US_Curb_Right_Value, US_Curb_Left_Value : Long_Float;

      Output                                  : Lane_Detection_Done_T;
      Output_Send                                  : Lane_Detection_Done_T;






   begin

      accept Construct
        (IR_Threshhold, US_Threshhold : in Long_Float )

      do

         US_Curb_Threshhold    := US_Threshhold;
         IR_Lane_Threshhold    := IR_Threshhold;

      end Construct;
      -- each iteration has three steps: 1. Read sensor data and calculate
      -- outputs 2. and send output via lane_detection_don(value) 3. Wait
      -- for lane_detection_next and start next iteration
      loop
         -- Read sensor values

         Output_Send:= Lane_Detection_Process(Output,
                                              IR_Lane_Left_Value ,
                                              IR_Lane_Right_Value  ,
                                              IR_Lane_Mid_Value   ,
                                              US_Curb_Left_Value  ,
                                              US_Curb_Right_Value ,
                                              IR_Lane_Threshhold  ,
                                              US_Curb_Threshhold  );


         -- Output Signal

         Put_Line ("");
      end loop;
   end Lane_Detection_Taks_T;

end Lane_Detection;
