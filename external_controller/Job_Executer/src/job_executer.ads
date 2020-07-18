with Motor_Controller; use Motor_Controller;
with Roadmarker; use Roadmarker;
with Ada.Strings.Unbounded;
with ec2b; use ec2b;

package Job_Executer is
   subtype Road_Marker_ID_T is Integer range 0 .. 15;
   type Intersection_Option_T is (Left, Right);
    errors_till_backend_failed : constant Integer := 10;
   task type Job_Executer_Task_T is
      entry Constructor
        (Motor_Controller_Task_A : in Motor_Controller_Task_Access_T;
         timeout_v               : in Duration;
         RM_get_sensor_value_a   : in get_roadmarker_sensor_value_access;
         cab_name_arg            : in Ada.Strings.Unbounded.Unbounded_String;
         start_section_arg         : in Integer 
        );
   end Job_Executer_Task_T;
   type Job_Executer_Task_Access_T is access Job_Executer_Task_T;
   
   function Reached_expected_roadmarker(section : Road_Marker_ID_T;
                                        next_command : Command_t) return Boolean;
   
   procedure  register_new_cab(cab_name : in out Ada.Strings.Unbounded.Unbounded_String;
                               cab_id : in out Integer;
                               start_section: in Integer;
                               error_counter : in out Integer;
                               retry_register : in out Boolean;
                               Job_Executer_Done_Signal : in out Job_Executer_Done_T);
   
   
   procedure Process_valid_Section(section : in Road_Marker_ID_T;
                             current_command : in out Command_t;
                             next_command : in out Command_t;
                             cab_id : in Integer;
                             cmd_queue : in out cmd_queue_access_t;
                             error_counter : in out Integer;
                             retry_location_update : in out Boolean
                                  );
   
   
   procedure Process_Section(section : in out Roadmarker.Road_Marker_Done_T;
                             section_signal : in Roadmarker.Road_Marker_Done_T;
                                current_command : in out Command_t;
                                next_command : in out Command_t;
                                cab_id : in Integer;
                                cmd_queue : in out cmd_queue_access_t;
                                error_counter : in out Integer;
                                retry_location_update : in out Boolean;
                                Job_Executer_Done_Signal: in out Job_Executer_Done_T
                            );
   
   procedure retry_update_cab_location(cab_id : Integer;
                                       section : Road_Marker_Done_T;
                                       retry_update_cab_location : in out Boolean;
                                       connection_errors : in out Integer);
   
   
   procedure update_route(section : in Roadmarker.Road_Marker_Done_T;
                          current_command : in out Command_t;
                          next_command : in out Command_t;
                          cab_id : in Integer;
                          cmd_queue : in out cmd_queue_access_t;
                          error_counter : in out Integer;
                          retry_location_update : in out Boolean;
                          Job_Executer_Done_Signal: in out Job_Executer_Done_T;
                          cab_version : in out Integer);
   
   procedure determine_done_signal(Job_Executer_Done_Signal : in out Job_Executer_Done_T;
                                   current_command : in out Command_t;
                                   next_command : in out Command_t;
                                   cmd_queue :  in out cmd_queue_access_t;
                                   error_counter : in out Integer;
                                   cab_id: in out Integer;
                                   pickup_completed : in out Boolean;
                                   dropoff_completed : in out Boolean);
   
   procedure send_done_Signal(Motor_Controller_Task : Motor_Controller_Task_Access_T;
                              Job_Executer_Done_Signal : Job_Executer_Done_T;
                              timeout : Duration;
                              running : in out Boolean );

   procedure receive_next_signal(Motor_Controller_Task   : Motor_Controller_Task_Access_T;
                                 Job_Executer_Next_Signal : in out Job_Executer_Next_t;
                                 timeout : Duration;
                                 running : in out Boolean;
                                 error_counter : in out Integer;
                                 RM_next : out Road_Marker_Next_T;
                                 cab_id : Integer);
   
   procedure send_RM_next_signal(Roadmarker_Task : Roadmarker_Task_T;
                                 RM_next : Road_Marker_Next_T;
                                 timeout : Duration;
                                 running : in out Boolean
                                );

end Job_Executer;
