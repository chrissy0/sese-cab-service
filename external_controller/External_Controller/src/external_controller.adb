with Lane_Detection;   use Lane_Detection;
with Motor_Controller; use Motor_Controller;
with WC2EC;            use WC2EC;
with Ada.Text_IO;      use Ada.Text_IO;
with WC2EC_Interface;



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

   Motor_Controller_Task.Construct(MC_State       => NORMAL_DRIVING,
                                   ND_State       => FRONT_CLEAR,
                                   FC_State       => DRIVE,
                                   D_State        => INIT,
                                   SE_State       => STOP,
                                   MS_Speed       => 3.0,
                                   MT_Speed       => 1.0,
                                   LD_State       => INIT,
                                   set_motor_value_access => WC2EC_Interface.set_motor_value'Access);

   Put_Line ("Setting up Lane_Detection_Task...");
   Lane_Detection_Task.Construct
     (IR_Threshhold => 250.0, US_Threshhold => 870.0, US_Max_Value => 1_000.0,
      Motor_Task_A  => Motor_Controller_Task, WC2EC_Driver_A => WC2EC_Driver);

end External_Controller;
