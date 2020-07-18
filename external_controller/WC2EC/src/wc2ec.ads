with Interfaces; use Interfaces;

with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
use Ada.Containers;
with Ada.Strings.Fixed;
with Ring_Buffer;
with distance_sensor_p; use distance_sensor_p;
with Ada.Strings.Unbounded;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
package WC2EC is
   type RingIndex is mod 2**4;
   --package distance_sensor_Ring is new Ring_Buffer(distance_sensor_t, RingIndex, distance_sensor_p.print_Distance_Sensor);

   --type sensor_ring_access_t is array (0..255) of access distance_sensor_Ring.RingBuffer;




   type sensor_ring_access_t is array (0..255) of distance_sensor_t;
   package sensor_map_p is new Indefinite_Hashed_Maps (Key_Type => String,
                                               Element_Type => Interfaces.Unsigned_8,
                                               Hash => Ada.Strings.Hash,
                                                              Equivalent_Keys => "=");
   use sensor_map_p;

   subtype whoosh_t is Integer range 1 .. 100;
   subtype whoosh_randomness_t is Integer range -100 .. 100;

   package random_whoosh_p is new Ada.Numerics.Discrete_Random (whoosh_randomness_t);

   type sensor_manipulation_t is record
      disabled : Boolean;
      whoosh : whoosh_t;
   end record;


   package sensor_manipulation_map_p is new Indefinite_Hashed_Maps(Key_Type        => String,
                                                                   Element_Type    => sensor_manipulation_t,
                                                                   Hash            => Ada.Strings.Hash,
                                                                   Equivalent_Keys => "=");

   type sensor_manipulation_map_access_t is access sensor_manipulation_map_p.Map;
   procedure update_sensor_manipulation_map(new_sensor_manipulation_map : sensor_manipulation_map_access_t);
   type wc2ec_header_t is record
      command : Interfaces.Unsigned_8;
      sensor_type : Interfaces.Unsigned_8;
      sensor_id : Interfaces.Unsigned_8;
   end record;

   -- Constants
   CMD_REGISTER_SENSOR  : constant := 0;
   CMD_SENSOR_DATA      : constant := 1;

   CMD_CHANGE_SENSOR : constant := 130;
   SENSOR_TYPE_MOTOR    : constant := 53;




   -- Thread definition
   Task Type wc2ec_thread_t is
       entry Constructor
        (ip_arg : Ada.Strings.Unbounded.Unbounded_String ; port_arg : Port_Type);

      end wc2ec_thread_t; -- Thread type
   type wc2ec_thread_access_t is access wc2ec_thread_t;


   -- Variables (maybe hide those)
   sensor_map :  sensor_map_p.Map;
   sensor_manipulation_map : sensor_manipulation_map_access_t;
   sensor_ring : sensor_ring_access_t;
   Channel : Stream_Access; -- socket I/O interface
   ready : Boolean := false;


   -- functions
   function get_distance_sensor_data(sensor_name : String) return Long_Float;

   procedure set_motor_sensor_data(sensor_name : String; velocity : Long_Float);

end WC2EC;
