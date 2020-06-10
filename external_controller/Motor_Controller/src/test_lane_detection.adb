with Ada.Text_IO; use Ada.Text_IO;
with Motor_Controller;

procedure test_lane_detection is
   motor_task    : Motor_Controller.Motor_Controller_Task;
begin
   -- each iteration has three steps:
   -- 1. Read sensor data and calculate outputs
   -- 2. and send output via lane_detection_don(value)
   -- 3. Wait for lane_detection_next and start next iteration
   motor_task.Construct;
   for I in 0 .. 100 loop
      Put_Line("I = " & I'Image);
   
      if I = 10 then
         Put_Line("Sending system Error");
         motor_task.lane_detection_done(Motor_Controller.SYSTEM_ERROR_S);
      elsif I mod 2 = 0 then
         Put_Line("sending empty");
         motor_task.lane_detection_done(Motor_Controller.EMPTY_S);
      else
         Put_Line("sending go left");
         motor_task.lane_detection_done(Motor_Controller.GO_LEFT_S);
      end if;
      Put_Line("Waiting for main_next");
      motor_task.lane_detection_next; -- wait for all signals to be processed
      Put_Line("Main_next recieved!");
      Put_Line("");
   end loop;
   delay 1.0;
end test_lane_detection;
