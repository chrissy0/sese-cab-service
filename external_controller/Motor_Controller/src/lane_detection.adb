with Ada.Text_IO; use Ada.Text_IO;
with Motor_Controller;

procedure Lane_Detection is


   motor_task    : Motor_Controller.Motor_Controller_Task;
   ir_lane_left : Integer := 700 ; --must receive Datas from Webots
   ir_lane_mid : Integer := 200 ; --must receive Datas from Webots
   ir_lane_right : Integer := 700 ; --must receive Datas from Webots

   curb_distance_side : Integer := 600;
   curb_distance_mid : Integer := 10;
   us_curb_left : Integer  :=1; --must receive Datas from Webots
   us_curb_right : Integer := 1; --must receive Datas from Webots

begin
    -- each iteration has three steps:
   -- 1. Read sensor data and calculate outputs
   -- 2. and send output via lane_detection_don(value)
   -- 3. Wait for lane_detection_next and start next iteration
   motor_task.Construct;
      for I in 0 .. 100 loop
         Put_Line("I = " & I'Image);

         if(ir_lane_right < 250 and ir_lane_left > 250)then
            Put_Line("Sending Go Left_Infrared");
            motor_task.lane_detection_done(Motor_Controller.GO_LEFT_S);
         elsif (ir_lane_left < 250 and ir_lane_right > 250)then
            Put_Line("Sending Go Right_Infrared");
            motor_task.lane_detection_done(Motor_Controller.GO_RIGHT_S);
         elsif (ir_lane_mid <250 and ir_lane_right > 250 and ir_lane_left > 250 ) then
            Put_Line("Sending Go Straight_Infrared");
            motor_task.lane_detection_done(Motor_Controller.GO_STRAIGHT_S);
         else
            Put_Line("Infrared Error");
            if(us_curb_left > curb_distance_side and us_curb_left <1000 ) then
               Put_Line("Sending Go Left_Curb");
               motor_task.lane_detection_done(Motor_Controller.GO_LEFT_S);

            elsif(us_curb_right > curb_distance_side and us_curb_right <1000  ) then
               Put_Line("Sending Go Right_Curb");
               motor_task.lane_detection_done(Motor_Controller.GO_RIGHT_S);
            elsif(us_curb_left = 1000) then
               Put_Line("Curb Error");Put_Line("System Error");

            else Put_Line("Sending Go Straight_Curb");
               motor_task.lane_detection_done(Motor_Controller.GO_STRAIGHT_S);
            end if;


         end if;
         ir_lane_mid:= ir_lane_mid+1;
         Put_Line("Waiting for main_next");
         --  delay 1.0;
         motor_task.lane_detection_next; -- wait for all signals to be processed
         Put_Line("Main_next recieved!");
         Put_Line("");
      end loop;

end Lane_Detection;
