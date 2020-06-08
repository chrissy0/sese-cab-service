with Synchronized_Data;
package Motor_Controller is
   package System_Error is new Synchronized_Data (Boolean);

   type System_Error_Access is access System_Error.Synchronized_Data_T;

   package Front_Blocked is new Synchronized_Data (Boolean);

   type Front_Blocked_Access is access Front_Blocked.Synchronized_Data_T;

   Task Type Motor_Controller_Task is
      entry Construct(is_System_Error_Access_Copy : out System_Error_Access;
                     is_Front_Blocked_Access_Copy : out Front_Blocked_Access);
   end Motor_Controller_Task;

end Motor_Controller;
