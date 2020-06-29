--with WC2EC;            use WC2EC;
--with Motor_Controller; use Motor_Controller;
with Roadmarker_Functions; use Roadmarker_Functions;
with Ada.Text_IO;      use Ada.Text_IO;

package Lane_Detection is



   type Lane_Detection_Done_T is
     (SYSTEM_ERROR_S, GO_STRAIGHT_S, GO_LEFT_S, GO_RIGHT_S, EMPTY_S);
   type Lane_ID_T is (Lane_Mid_Value, Lane_Left_Value, Lane_Right_Value, Curb_Left_Value, Curb_Right_Value);
   type set_lane_value_procedure_t is access procedure (ID: Lane_ID_T;
                                                         Value : Long_Float);



   function Lane_Detection_Process    ( Output_T :  in Lane_Detection_Done_T;
                                        IR_Lane_Left_Value : in Long_Float;
                                        IR_Lane_Right_Value  : in Long_Float;
                                        IR_Lane_Mid_Value   : in Long_Float;
                                        US_Curb_Left_Value  : in Long_Float;
                                        US_Curb_Right_Value : in Long_Float;
                                        IR_Lane_Threshhold  : in Long_Float;
                                        US_Curb_Threshhold  : in Long_Float)
                                        return Lane_Detection_Done_T;


   task type Lane_Detection_Taks_T is
      entry Construct
        (IR_Threshhold, US_Threshhold : in Long_Float   );




   end Lane_Detection_Taks_T;

type Lane_Detection_Task_Access_T is access Lane_Detection_Taks_T;

end Lane_Detection;
