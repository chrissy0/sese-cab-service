-- @summary
-- External Controller builder file.
--
-- @author Julian Hartmer
-- @description
-- This executable file builds the external controller by starting each task
-- and providing accesses to the nedded ressources.

with Lane_Detection;   use Lane_Detection;
with Motor_Controller; use Motor_Controller;
with WC2EC;            use WC2EC;
with Ada.Text_IO;      use Ada.Text_IO;
with Front_Distance;   use Front_Distance;
with Job_Executer;     use Job_Executer;
with WC2EC_Interface;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets; use GNAT.Sockets;

procedure External_Controller is
   Lane_Detection_Task   : Lane_Detection_Taks_T;
   Front_Distance_Task   : Front_Distance_Task_T;
   Motor_Controller_Task : Motor_Controller_Task_Access_T;
   Job_Executer_Task     : Job_Executer_Task_T;
   WC2EC_Driver          : wc2ec_thread_access_t;
   ip                    : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String("127.0.0.1");
   port                  : Port_Type := 27015;
   cab_name              : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String("default");
   start_section         : Integer := 0;
   timeout               : Duration := 2.0;

   procedure Log_Line(message : String) is
   begin
      Put_Line("[external_controller] " & message);
   end Log_Line;

begin
   Put_Line("Command line arguments: " & Ada.Command_Line.Argument_Count'Image);
   if (Ada.Command_Line.Argument_Count = 4) then
      ip := To_Unbounded_String(Argument(1));
      port := Port_Type'Value(Argument(2));
      cab_name := To_Unbounded_String(Argument(3));
      start_section := Integer'Value(Argument(4));
      if (start_section < 0 or start_section > 15) then
         Put_Line("Start section has to be between 0 and 15");
         raise Constraint_Error;
      end if;
      Put_Line("IP: " & To_string(ip));
      Put_Line("Port: " & port'Image);
      Put_Line("Cab_name: " & To_String(cab_name));
      Put_Line("Start_section " & start_section'Image);
   end if;
   Log_Line ("Setting up WC2EC_Driver...");
   WC2EC_Driver := new wc2ec_thread_t;
   WC2EC_Driver.Constructor(ip, port);
   while WC2EC.ready /= True loop
      delay 1.0;
   end loop;

   Log_Line ("Setting up Motor_Controller_Task...");
   Motor_Controller_Task := new Motor_Controller_Task_T;

   Motor_Controller_Task.Constructor(
                                     set_motor_value_access => WC2EC_Interface.set_motor_value'Access,
                                     elevate_sensors_access => WC2EC_Interface.elevate_curb_sensor'Access,
                                     timeout_v              => timeout,
                                     iteration_delay_s      => 0.01);

   Log_Line ("Setting up Lane_Detection_Task...");
   Lane_Detection_Task.Construct
     (
      Motor_Task_A            => Motor_Controller_Task,
      get_line_sensor_value_a => WC2EC_Interface.get_line_detection_sensor_value'Access,
      get_curb_sensor_value_a => WC2EC_Interface.get_curb_detection_sensor_value'Access,
      get_wall_sensor_value_a => WC2EC_Interface.get_wall_detection_sensor_value'Access,
      timeout_v               => timeout
     );

   Log_Line("Setting up Front_Distance_Task ...");
   Front_Distance_Task.Construct
     (
      get_sensor_value_a      => WC2EC_Interface.get_front_distance_value'Access,
      us_thresh_front         => 450.0,
      ir_thresh_front         => 450.0,
      us_thresh_side          => 350.0,
      ir_thresh_side          => 350.0,
      Motor_Controller_Task_A => Motor_Controller_Task,
      timeout_v               => timeout
     );
   Log_Line("All set up!");

   Job_Executer_Task.Constructor(Motor_Controller_Task_A => Motor_Controller_Task,
                                 timeout_v               => timeout,
                                 RM_get_sensor_value_a   => WC2EC_Interface.get_rm_sensor_value'Access,
                                 cab_name_arg => cab_name,
                                 start_section_arg => start_section);

   loop
      Motor_Controller_Task.main_shutdown_signal(False);
   end loop;

  exception
      when Constraint_Error =>
      Put_Line("CONSTRAINT ERROR");
      Put_Line("Your probably entered an invalid argument");
      put_line("Arguments are:");
      Put_Line("IP (string) PORT (Unsigned Integer 0 -  65 535) cab_name (string) start_section (integer 0 - 15)");
         return;
      when Error : others =>
         Put_Line("Unknown error");
         return;
end External_Controller;
