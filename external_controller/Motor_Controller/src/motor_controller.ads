with Synchronized_Data;
package Motor_Controller is

   type Lane_Detection_Done_T is
     (SYSTEM_ERROR_S, GO_STRAIGHT_S, GO_LEFT_S, GO_RIGHT_S, EMPTY_S);

   task type Motor_Controller_Task is
      entry Construct; -- Constructor

      entry lane_detection_done
        (Lane_Detection_Signal_Value : in Lane_Detection_Done_T);
      entry lane_detection_next;
   end Motor_Controller_Task;

end Motor_Controller;
