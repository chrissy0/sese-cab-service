with Lane_Detection;   use Lane_Detection;
with Motor_Controller; use Motor_Controller;
with WC2EC;            use WC2EC;
with Ada.Text_IO;      use Ada.Text_IO;

procedure External_Controller is
   Lane_Detection_Task   : Lane_Detection_Taks_T;
   Motor_Controller_Task : Motor_Controller_Task_Access_T;
   WC2EC_Driver          : wc2ec_thread_access_t;
begin
   Put_Line ("Setting up WC2EC_Driver...");
   WC2EC_Driver := new wc2ec_thread_t;
   while WC2EC.ready /= True loop
      delay 1.0;
   end loop;
   Put_Line ("Setting up Motor_Controller_Task...");
   Motor_Controller_Task := new Motor_Controller_Task_T;
   Motor_Controller_Task.Construct
     (WC2EC_Driver_A => WC2EC_Driver, Straight_Speed => 3.0,
      Turn_Speed     => 1.0);
   Put_Line ("Setting up Lane_Detection_Task...");
   Lane_Detection_Task.Construct
     (IR_Threshhold => 300.0, US_Threshhold => 870.0, US_Max_Value => 1_000.0,
      Motor_Task_A  => Motor_Controller_Task, WC2EC_Driver_A => WC2EC_Driver);
   delay (10_000.0);

end External_Controller;
