with Ada.Text_IO; use Ada.Text_IO;
with Motor_Controller;

procedure main is
   motor_task    : Motor_Controller.Motor_Controller_Task;
begin
   motor_task.Construct;
   for I in 0 .. 100 loop
      Put_Line("I = " & I'Image);
   
      if I = 10 then
         Put_Line("Sending system Error");
         motor_task.System_Error_State_R;
      elsif I mod 2 = 0 then
         Put_Line("sending main_done");
         motor_task.main_done;
      elsif I = 10 then
         Put_Line("Sending system Error");
         motor_task.System_Error_State_R;
      else
         Put_Line("sending change_front_state_R");
         motor_task.Change_Front_State_R;
      end if;
      Put_Line("Waiting for main_next");
      motor_task.main_next; -- wait for all signals to be processed
      Put_Line("Main_next recieved!");
      Put_Line("");
   end loop;
   delay 1.0;
end main;
