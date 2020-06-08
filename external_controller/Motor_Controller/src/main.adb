with Ada.Text_IO; use Ada.Text_IO;
with Motor_Controller;

procedure main is
   motor_task    : Motor_Controller.Motor_Controller_Task;
   system_error  : Motor_Controller.System_Error_Access;
   front_blocked : Motor_Controller.Front_Blocked_Access;
begin
   motor_task.Construct(is_System_Error_Access_Copy  => system_error,
                        is_Front_Blocked_Access_Copy => front_blocked);
   delay 1.0;
   Put_line("");
   Put_line("");
   Put_Line("Setting Front blocked to True");
   front_blocked.set(False);
   delay 1.0;
   Put_line("");
   Put_line("");
   Put_Line("Setting Front blocked to True");
   front_blocked.set(True);
   delay 1.0;
   Put_line("");
   Put_line("");
   Put_Line("Setting system_error to True");
   system_error.set(False);
   delay 1.0;
   Put_line("");
   Put_line("");
   Put_Line("Setting Front blocked to False");
   front_blocked.set(False);
   
end main;
