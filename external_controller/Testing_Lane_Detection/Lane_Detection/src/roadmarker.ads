with WC2EC;            use WC2EC;
with Motor_Controller; use Motor_Controller;
with Lane_Detection;   use Lane_Detection;
with Ada.Text_IO;      use Ada.Text_IO;

package Roadmarker is


   task type Roadmarker_Task_T is
      entry Construct
        (
         WC2EC_Driver_A               : in wc2ec_thread_access_t);
   end Roadmarker_Task_T;




end Roadmarker;
