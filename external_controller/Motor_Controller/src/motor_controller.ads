with Synchronized_Data;
package Motor_Controller is

   Task Type Motor_Controller_Task is
      entry System_Error_State_R;         -- set to system error
      entry Change_Front_State_R;  -- blocked to clear and vice versa
      entry Construct;
      entry main_done;
      entry main_next;
   end Motor_Controller_Task;

end Motor_Controller;
