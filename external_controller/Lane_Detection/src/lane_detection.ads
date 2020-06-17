with WC2EC;            use WC2EC;
with Motor_Controller; use Motor_Controller;
with Roadmarker_Functions; use Roadmarker_Functions;
with Ada.Text_IO;      use Ada.Text_IO;

package Lane_Detection is


   task type Lane_Detection_Taks_T is
      entry Construct
        (IR_Threshhold, US_Threshhold : in Long_Float;
         US_Max_Value                 : in Long_Float;
         Motor_Task_A                 : in Motor_Controller_Task_Access_T;
         WC2EC_Driver_A               : in wc2ec_thread_access_t);


   end Lane_Detection_Taks_T;

type Lane_Detection_Task_Access_T is access Lane_Detection_Taks_T;

end Lane_Detection;
