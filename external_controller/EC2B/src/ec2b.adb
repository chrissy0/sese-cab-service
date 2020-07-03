pragma Ada_2012;
with AWS.Client;
with AWS.Response;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
use Ada.Strings;

with AWS.Messages;
use type AWS.Messages.Status_Code;
with GNATCOLL.JSON; use GNATCOLL.JSON;
use AWS;
with Ada.Strings.Fixed;
with json_Wrappers;
use JSON_Wrappers;
package body ec2b is

   -------------------
   -- request_route --
   -------------------

   function request_route(cmd_queue: in out cmd_queue_access_t; cab_id : Integer; cab_version : in out Integer) return cmd_queue_access_t is
      cab_id_str : String := Ada.Strings.Fixed.Trim(cab_id'Image, Ada.Strings.Left);
      cab_version_str : String := Ada.Strings.Fixed.Trim(cab_version'Image , Ada.Strings.Left);
      parameters : param_map_p.Map;
      response_json : JSON_Value;
      command_json : JSON_Value;
      route_json : JSON_Array;
      version : Integer;
      status_code : Messages.Status_Code;
      action : Ada.Strings.Unbounded.Unbounded_String;
      command : access Command_t;
      direction : Ada.Strings.Unbounded.Unbounded_String;
   begin
      parameters.Insert("id", cab_id_str);
      parameters.Insert("version", cab_version_str);
      status_code := get_JSON(connection, "/api/ec/requestRoute", parameters, response_json);
      -- todo case (status_code)
      version := GNATCOLL.JSON.Get(response_json, "version");
      Put_Line("Version: " & version'Image);
      if (version = cab_version and version /= 0) then
         return cmd_queue;
      end if;
      Put_Line("ASKING");
      cab_version := version;
      Put_Line("Route length is: ");
      cmd_queue := new Cmd_Queue_p.Queue;
      Put_Line("Route length is: ");
      route_json := GNATCOLL.JSON.Get(response_json, "route");
      Put_Line("Route length is: " & GNATCOLL.JSON.Length(route_json)'Image);
      for I in 1 .. GNATCOLL.JSON.Length(route_json) loop
         command_json := GNATCOLL.JSON.Get(route_json, I);
         command := new Command_t;
         command.marker := GNATCOLL.JSON.get(command_json, "marker");
         action := GNATCOLL.JSON.get(command_json, "action");
         if To_String(action) = "wait" then
            command.action := WAIT_S;
         elsif To_String(action) = "turn" then
            direction := GNATCOLL.JSON.get(command_json, "direction");
            if To_String(direction) = "right" then
               command.action := NEXT_RIGHT_S;
            elsif To_String(direction) = "left" then
               command.action := NEXT_LEFT_S;
            else
               command.action := SYSTEM_ERROR_S;
            end if;
         elsif To_String(action) = "pickup" then
            command.customer_ID := GNATCOLL.JSON.Get(command_json, "customerId");
            command.action := PICK_UP_S;
         elsif To_String(action) = "dropoff" then
            command.customer_ID := GNATCOLL.JSON.Get(command_json, "customerId");
            command.action := DROP_OFF_S;
         else
            command.action := SYSTEM_ERROR_S;
         end if;
         Put_Line("Action : " & To_String(action));
         cmd_queue.Enqueue(command.all);
      end loop;
      return cmd_queue;
end request_route;

   ------------------
   -- register_cab --
   ------------------

   function register_cab(cabname : String; section : Road_Marker_Done_T) return Integer is
   cab_JSON : JSON_Value := Create_Object;
   id : Standard.Integer := -1;
   response_data_JSON : JSON_Value := Create_Object;
   status_code : Messages.Status_Code;
   begin
      cab_JSON.Set_Field ("cabName", cabname);
      cab_JSON.Set_Field ("section", Create (Integer(section)));
      status_code := post_JSON(connection, "/api/ec/registerCab", value => cab_JSON, response_data_JSON => response_data_JSON);
      if (status_code /= Messages.S200) then
         Put_Line("Register failed");
         return -1;
      end if;

      id := GNATCOLL.JSON.Get(response_data_JSON, "id");
      Put_Line("ID: " & id'Image);

      return id;
   end register_cab;

   ------------------------
   -- update_cabLocation --
   ------------------------

   procedure update_cabLocation(cab_id : Integer; cab_section : Road_Marker_Done_T) is
      section_JSON : JSON_Value := Create_Object;
      cab_id_str : String := Ada.Strings.Fixed.Trim(cab_id'Image, Ada.Strings.Left);
      parameters : param_map_p.Map;
      response_data_JSON : JSON_Value := Create_Object;
      status_code : Messages.Status_Code;
   begin
      section_JSON.Set_Field("section", Create(Integer(cab_section)));
      parameters.Insert("cabId", cab_id_str);
      Put_Line(section_JSON.Write);
      status_code := post_JSON(connection, "/api/ec/cabLocation", parameters, section_JSON, response_data_JSON);
      if (status_code /= Messages.S200) then
         Put_Line("Status Code: " & status_code'Image);
         Put_Line("Update Location failed");
         return;
      end if;
   end update_cabLocation;


   ------------------------
   --   request_pickup   --
   ------------------------
   procedure request_pickup(cab_id : Integer; customer_id : Integer) is
      cab_id_str : String := Ada.Strings.Fixed.Trim(cab_id'Image, Ada.Strings.Left);
      customer_id_str : String := Ada.Strings.Fixed.Trim(customer_id'Image, Ada.Strings.Left);
      response_data_JSON : JSON_Value := Create_Object;
      status_code : Messages.Status_Code;
      parameters : param_map_p.Map;
      begin
      parameters.Insert("cabId", cab_id_str);
      parameters.Insert("customerId", customer_id_str);
      status_code := post_JSON(connection, "/api/ec/requestPickup", parameters,response_data_JSON => response_data_JSON);
      if (status_code /= Messages.S200) then
         Put_Line("Status Code: " & status_code'Image);
         Put_Line("Update Location failed");
         return;
      end if;
   end request_pickup;
   ------------------------
   --   pickup_complete   --
   ------------------------
   function pickup_complete(cab_id : Integer) return Boolean is
      cab_id_str : String := Ada.Strings.Fixed.Trim(cab_id'Image, Ada.Strings.Left);
      status_code : Messages.Status_Code;
      response_JSON : JSON_Value := Create_Object;
      complete : Ada.Strings.Unbounded.Unbounded_String;
      parameters : param_map_p.Map;
   begin
      parameters.Insert("cabId", cab_id_str);
      status_code := get_JSON(connection, "/api/ec/pickupComplete", parameters, response_json);
      complete := GNATCOLL.JSON.Get(response_JSON, "complete");
      if (To_String(complete) = "true") then
         return True;
      else
         return False;
      end if;

   end pickup_complete;

   ------------------------
   --   request_dropoff  --
   ------------------------
   procedure request_dropoff(cab_id : Integer; customer_id : Integer) is
      cab_id_str : String := Ada.Strings.Fixed.Trim(cab_id'Image, Ada.Strings.Left);
      customer_id_str : String := Ada.Strings.Fixed.Trim(customer_id'Image, Ada.Strings.Left);
      response_data_JSON : JSON_Value := Create_Object;
      status_code : Messages.Status_Code;
      parameters : param_map_p.Map;
   begin
      parameters.Insert("cabId", cab_id_str);
      parameters.Insert("customerId", customer_id_str);
      status_code := post_JSON(connection, "/api/ec/requestDropoff", parameters,response_data_JSON => response_data_JSON);
      if (status_code /= Messages.S200) then
         Put_Line("Status Code: " & status_code'Image);
         Put_Line("Update Location failed");
         return;
      end if;
   end request_dropoff;

   ------------------------
   --  dropoff_complete  --
   ------------------------
   function dropoff_complete(cab_id : Integer) return Boolean is
      cab_id_str : String := Ada.Strings.Fixed.Trim(cab_id'Image, Ada.Strings.Left);
      status_code : Messages.Status_Code;
      response_JSON : JSON_Value := Create_Object;
      complete : Ada.Strings.Unbounded.Unbounded_String;
      parameters : param_map_p.Map;
   begin
      parameters.Insert("cabId", cab_id_str);
      status_code := get_JSON(connection, "/api/ec/dropoffComplete", parameters, response_json);
      complete := GNATCOLL.JSON.Get(response_JSON, "complete");
      if (To_String(complete) = "true") then
         return True;
      else
         return False;
      end if;
   end dropoff_complete;

end ec2b;
