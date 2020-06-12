with WC2EC; use WC2EC;

package Motor_Controller is

   procedure print_motor_value (f : Float);

   type Lane_Detection_Done_T is
     (SYSTEM_ERROR_S, GO_STRAIGHT_S, GO_LEFT_S, GO_RIGHT_S, EMPTY_S);

   task type Motor_Controller_Task_T is
      entry Construct
        (WC2EC_Driver_A             : in wc2ec_thread_access_t;
         Straight_Speed, Turn_Speed : in Long_Float); -- Constructor

      entry lane_detection_done
        (Lane_Detection_Signal_Value : in Lane_Detection_Done_T);
      entry lane_detection_next;
   end Motor_Controller_Task_T;

   type Motor_Controller_Task_Access_T is access Motor_Controller_Task_T;

end Motor_Controller;
