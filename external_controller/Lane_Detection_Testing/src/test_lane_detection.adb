with Ada.Text_IO; use Ada.Text_IO;
with Motor_Controller;

procedure test_lane_detection is
   motor_task   : Motor_Controller.Motor_Controller_Task_T;
   WC2EC_Driver : wc2ec_thread_access_t;
begin
   -- each iteration has three steps: 1. Read sensor data and calculate
   -- outputs 2. and send output via lane_detection_don(value) 3. Wait
   -- for lane_detection_next and start next iteration\
   WC2EC_Driver := new WC2EC.wc2ec_thread_t;
   while WC2EC.ready /= True loop
      delay 1.0;
   end loop;
  -- motor_task.Construct (WC2EC_Driver_A => WC2EC_Driver);

   for I in 0 .. 100 loop
      Put_Line ("I = " & I'Image);

      Put_Line ("sending go straight");
      motor_task.lane_detection_done (Motor_Controller.GO_STRAIGHT_S);
      motor_task.lane_detection_next; -- wait for all signals to be processed
      delay 3.0;
      Put_Line ("sending go left");
      motor_task.lane_detection_done (Motor_Controller.GO_LEFT_S);
      motor_task.lane_detection_next; -- wait for all signals to be processed
      delay 3.0;
      Put_Line ("sending go right");
      motor_task.lane_detection_done (Motor_Controller.GO_RIGHT_S);
      motor_task.lane_detection_next; -- wait for all signals to be processed
      delay 3.0;
   end loop;
end test_lane_detection;
