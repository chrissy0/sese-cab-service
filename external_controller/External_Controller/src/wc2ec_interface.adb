pragma Ada_2012;
with Motor_Controller; use Motor_Controller;
with Front_Distance; use Front_Distance;
with Roadmarker; use Roadmarker;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Strings.Unbounded;
package body WC2EC_Interface is

   ---------------------
   -- set_motor_value --
   ---------------------

   procedure set_motor_value
     (ID : Motor_Controller.Motor_ID_T; Value : Long_Float)
   is
   begin
      case ID is
         when MOTOR_FRONT_LEFT =>
            WC2EC.set_motor_sensor_data("wheel1", Value);

         when MOTOR_FRONT_RIGHT =>
            WC2EC.set_motor_sensor_data("wheel2", Value);

         when MOTOR_BACK_LEFT =>
            WC2EC.set_motor_sensor_data("wheel3", Value);

         when MOTOR_BACK_RIGHT =>
            WC2EC.set_motor_sensor_data("wheel4", Value);
      end case;
   end set_motor_value;

   ------------------------------
   -- get_front_distance_value --
   ------------------------------

   function get_front_distance_value
   (
      typ : in Sensor_Type_T;
      pos : in Sensor_Position_T;
      num : in Sensor_Number_T
   ) return Long_Float is
      id_string : Unbounded_String;
   begin
      -- set up string
      case typ is
         when US =>
            id_string := To_Unbounded_String("dist_");
         when IR =>
            id_string := To_Unbounded_String("dist_ir_");
      end case;
      case pos is
         when CENTER =>
            Append(id_string, To_Unbounded_String("c"));
         when LEFT =>
            Append(id_string, To_Unbounded_String("l"));
         when RIGHT =>
            Append(id_string, To_Unbounded_String("r"));
      end case;
      case num is
         when 0 =>
            null;
         when 1 =>
            Append(id_string, To_Unbounded_String("2"));
      end case;

      -- get value:
      return WC2EC.get_distance_sensor_data(To_String(id_string));


   end get_front_distance_value;

   -------------------------
   -- get_rm_sensor_value --
   -------------------------

   function get_rm_sensor_value
     (ID : Roadmarker.Roadmarker_Sensor_ID_T)
   return Long_Float is
   begin
      case ID is
         when FRONT_LEFT =>
            return WC2EC.get_distance_sensor_data("inf_rm_fl_act");
         when FRONT_RIGHT =>
            return WC2EC.get_distance_sensor_data("inf_rm_fr_act");

         when BEHIND_LEFT =>
            return WC2EC.get_distance_sensor_data("inf_rm_bl_act");
         when BEHIND_RIGHT =>
            return WC2EC.get_distance_sensor_data("inf_rm_br_act");

         when RM_FL =>
            return WC2EC.get_distance_sensor_data("inf_rm_fl");
         when RM_FR =>
            return WC2EC.get_distance_sensor_data("inf_rm_fr");


         when RM_BL =>
            return WC2EC.get_distance_sensor_data("inf_rm_bl");
         when RM_BR =>
            return WC2EC.get_distance_sensor_data("inf_rm_br");
      end case;
   end get_rm_sensor_value;

end WC2EC_Interface;
