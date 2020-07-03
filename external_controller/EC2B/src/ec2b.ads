with Motor_Controller; use Motor_Controller;
with System; use System;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Roadmarker; use Roadmarker;
with AWS.Client; use AWS.Client;

package ec2b is

   connection : HTTP_Connection := Create("http://167.71.35.10:8081");



    type Ec2B_action_t is
     (SYSTEM_ERROR_S, NEXT_LEFT_S, NEXT_RIGHT_S, NEXT_UNKOWN_S, EMPTY_S, STOP_S, PICK_UP_S, DROP_OFF_S, WAIT_S);

   type Command_t is record
      marker : Integer;
      customer_ID : Integer;
      action : Ec2B_action_t;
   end record;

   package Cmd_interface is
     new Ada.Containers.Synchronized_Queue_Interfaces(Element_Type => Command_t);
   package Cmd_Queue_p is
     new Ada.Containers.Unbounded_Synchronized_Queues(Cmd_interface);

   type cmd_queue_access_t is access Cmd_Queue_p.Queue;

   function request_route(cmd_queue: in out cmd_queue_access_t; cab_id : Integer; cab_version : in out Integer) return cmd_queue_access_t;
   function register_cab(cabname : String; section : Road_Marker_Done_T) return Integer;
   procedure update_cabLocation(cab_id : Integer; cab_section : Road_Marker_Done_T);
   procedure request_pickup(cab_id : Integer; customer_id : Integer);
   function pickup_complete(cab_id : Integer) return Boolean;
   procedure request_dropoff(cab_id : Integer; customer_id : Integer);
   function dropoff_complete(cab_id : Integer) return Boolean;
end ec2b;
