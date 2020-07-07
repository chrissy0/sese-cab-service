with WC2EC;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body WC2EC is

   function get_ring_index(sensor_name : String) return Standard.Integer is
      ring_index : Standard.Integer;

      begin
      ring_index := Standard.Integer(sensor_map.Element(sensor_name));
      return ring_index;
   end;

   function get_distance_sensor_data(sensor_name : String) return Long_Float is
      ring_index : Standard.Integer;
      --head_index : RingIndex;
      --vec_output : distance_sensor_Ring.Element_Vector.Vector;
   begin

      ring_index := get_ring_index(sensor_name);
      --vec_output := sensor_ring(ring_index).get_elements;

      --return vec_output.Element(Index => vec_output.First_Index);
      return sensor_ring(ring_index).distance;
   end;

   procedure set_motor_sensor_data(sensor_name : String; velocity : Long_Float) is
      hdr : wc2ec_header_t;


   begin
      hdr.command     :=  CMD_CHANGE_SENSOR;
      hdr.sensor_type := SENSOR_TYPE_MOTOR;
      hdr.sensor_id   := sensor_map.Element(sensor_name);

      wc2ec_header_t'Write(Channel, hdr);
      Long_Float'Write(Channel, velocity);
   end;


   -- Private functions

   procedure register_sensor(Channel : Stream_Access; hdr : wc2ec_header_t) is
      num : Interfaces.Unsigned_8;
      Buffer : String (1 .. 256);

   begin
      -- TODO maybe store the type too
         Interfaces.Unsigned_8'Read(Channel, num);
         Ada.Text_IO.Put_Line(Interfaces.Unsigned_8'Image(num));
         for I in 1 .. num loop
            Character'Read(Channel, Buffer(Standard.Integer(I)));
            Ada.Text_IO.Put(Buffer(Standard.Integer(I)));
         end loop;
         Put_Line("!");
         sensor_map.Insert (Ada.Strings.Fixed.Head(Buffer,Standard.Integer(num)), hdr.sensor_id);
         --sensor_ring(Standard.Integer(hdr.sensor_id)) := new distance_sensor_Ring.RingBuffer;
         --sensor_ring(Standard.Integer(hdr.sensor_id)) := new distance_sensor_t;
      Put_Line("created map entry and sensor ring for " & Standard.Integer'Image(Standard.Integer(hdr.sensor_id)));
   end;

   procedure sensor_data(Channel : Stream_Access; hdr : wc2ec_header_t) is
      ds : distance_sensor_t;

   begin
      distance_sensor_t'Read (Channel, ds);
      --Ada.Text_IO.Put_Line (Long_Float'Image (ds.distance));
      --Put_Line("PRE");
      --sensor_ring(Standard.Integer(hdr.sensor_id)).print;
      --sensor_ring(Standard.Integer(hdr.sensor_id)).push(ds);
      sensor_ring(Standard.Integer(hdr.sensor_id)).distance := ds.distance;
      --Put_Line("Post");
      --sensor_ring(Standard.Integer(hdr.sensor_id)).print;
   end;

   procedure print_WC2EC_header(hdr : wc2ec_header_t) is
   begin
      Ada.Text_IO.Put("Command:");
      Ada.Text_IO.Put_Line(Interfaces.Unsigned_8'Image(hdr.command));
      Ada.Text_IO.Put("sensor_type:");
      Ada.Text_IO.Put_Line(Interfaces.Unsigned_8'Image(hdr.sensor_type));
      Ada.Text_IO.Put("sensor_id:");
      Ada.Text_IO.Put_Line(Interfaces.Unsigned_8'Image(hdr.sensor_id));
   end;

   function get_Stream(ip : String; port : Port_Type) return Stream_Access is
      Client  : Socket_Type; -- stores the socket
      Address : Sock_Addr_Type; -- stores the server address
      status : Selector_Status := Expired;
      Selector :  Selector_Access := null;
   begin

      GNAT.Sockets.Initialize;  -- initialize a new packet


      -- Set the server address:
      Address.Addr := Inet_Addr(ip); -- localhost
      Address.Port := port;

      while (status /= Completed) loop
         Create_Socket(Client); -- create a socket + store it as variable Client
         -- bind the address to the socket + connect
         Connect_Socket(Client, Address, Selector => Selector,Timeout => 1.0, Status => status);
         if (status /= Completed) then
            Close_Socket(Client);
         end if;

      end loop;
      Put_Line("Left loop");
      return Stream (Client); -- create a stream to access the socket
   end;

Task body wc2ec_thread_t is

   -- Define variables

      ip : Ada.Strings.Unbounded.Unbounded_String;
      port : Port_Type;
      hdr : wc2ec_header_t;
      running : Boolean := true;

   begin
      -- accept constructor call
      -- on timeout, close motor controller this is needed so that test cases
      -- termintate if an assert fails
      select
                 -- static, longer timeout for constructor
         delay 2.0;
         running := False;
      or
         accept Constructor(ip_arg :String; port_arg : Port_Type)
         do
            ip := To_Unbounded_String(ip_arg);
            port := port_arg;

         end Constructor;
      end select;

   -- TODO check return value!
   Channel := get_Stream(To_String(ip), port);

   while running loop
      wc2ec_header_t'Read (Channel, hdr);


       -- Todo replace with a switch
       if (hdr.command = CMD_REGISTER_SENSOR) then
            register_sensor(Channel, hdr);
       end if;

       if (hdr.command = CMD_SENSOR_DATA) then
            ready := true;
            sensor_data(Channel, hdr);
       end if;

   end loop;


   end wc2ec_thread_t;
end WC2EC;
