with Motor_Controller; use Motor_Controller;
with System; use System;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Roadmarker; use Roadmarker;
with AWS.Client; use AWS.Client;
with AWS.Messages; use AWS;

package ec2b is

   connection : HTTP_Connection := Create("http://167.71.35.10:8081");
   --connection : HTTP_Connection := Create("http://127.0.0.1:8081");

   function failed(status_code : Messages.Status_Code) return Boolean;
   function success(status_code : Messages.Status_Code; connection_erros : in out Integer) return Boolean;

    type Ec2B_action_t is
     (SYSTEM_ERROR_S, NEXT_LEFT_S, NEXT_RIGHT_S, NEXT_UNKOWN_S, EMPTY_S, STOP_S, PICK_UP_S, DROP_OFF_S, WAIT_S);

   type Command_t is record
      section : Integer;
      customer_ID : Integer;
      action : Ec2B_action_t;
   end record;

   package Cmd_interface is
     new Ada.Containers.Synchronized_Queue_Interfaces(Element_Type => Command_t);
   package Cmd_Queue_p is
     new Ada.Containers.Unbounded_Synchronized_Queues(Cmd_interface);

   type cmd_queue_access_t is access Cmd_Queue_p.Queue;

   function request_route(cmd_queue: in out cmd_queue_access_t; cab_id : Integer; cab_version : in out Integer) return Messages.Status_Code;
   function register_cab(cabname : String; section : Road_Marker_Done_T; cab_id : out Integer)  return Messages.Status_Code;
   function update_cabLocation(cab_id : Integer; cab_section : Road_Marker_Done_T) return Messages.Status_Code;
   function request_pickup(cab_id : Integer; customer_id : Integer) return Messages.Status_Code;
   function pickup_complete(cab_id : Integer; pickup_completed : out Boolean)  return Messages.Status_Code;
   function request_dropoff(cab_id : Integer; customer_id : Integer)  return Messages.Status_Code;
   function dropoff_complete(cab_id : Integer; dropoff_completed : out Boolean)  return Messages.Status_Code;
end ec2b;
