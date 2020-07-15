-- @summary
-- Interface providing abstract sensor getter and setter functions.
--
-- @author Julian Hartmer
-- @description
-- This package provides an abstract interface to the EC2WC driver. Each sensor
-- getter and setter function is referenced by sensor position, type and
-- similiar abstract properties.

with WC2EC;
with Motor_Controller;
with Front_Distance;
with Job_Executer;
with Roadmarker;
with Lane_Detection;
package WC2EC_Interface is

   -- Setter for wheel speed values
   -- @param vertical vertical position of wheel
   -- @param horizontal horiztonal position of wheel
   -- @param value Speed value to set the wheel to
   procedure set_motor_value
     (
      vertical   : Motor_Controller.Vertical_Position_T;
      horizontal : Motor_Controller.Horizontal_Position_T;
      value      : Long_Float
     );

   -- Getter for front distance sensor values
   -- @param typ sensor type
   -- @param pos sensor position
   -- @param num sensor number
   -- @return current sensor value
   function get_front_distance_value
     (
      typ : in Front_Distance.Sensor_Type_T;
      pos : in Front_Distance.Sensor_Position_T;
      num : in Front_Distance.Sensor_Number_T
     ) return Long_Float;


   -- Getter for roadmarker sensor values
   -- @param ID sensor ID
   -- @param is_backup True: reference Backup sensor, False: Refernce default sensor
   -- @return current sensor value
   function get_rm_sensor_value
     (
      ID        : in Roadmarker.Roadmarker_Sensor_ID_T;
      is_backup : in Boolean
     ) return Long_Float;


   -- Getter for line detection sensor values
   -- @param pos sensor position
   -- @param is_backup True: reference Backup sensor, False: Refernce default sensor
   -- @return current sensor value
   function get_line_detection_sensor_value
     (
      pos       : in Lane_Detection.Line_Sensor_Position_T;
      is_backup : in Boolean
     ) return Long_Float;


   -- Getter for curb sensor values
   -- @param pos sensor position
   -- @param orientation sensor orientaten
   -- @param is_backup True: reference Backup sensor, False: Refernce default sensor
   -- @return current sensor value
   function get_curb_detection_sensor_value
     (
      pos         : in Lane_Detection.Curb_Sensor_Position_T;
      orientation : in Lane_Detection.Sensor_Orientation_T;
      is_backup : in Boolean
     ) return Long_Float;

   -- Getter for wall detection sensor values
   -- @param orientation sensor orientaten
   -- @param is_backup True: reference Backup sensor, False: Refernce default sensor
   -- @return current sensor value
   function get_wall_detection_sensor_value
     (
      orientation : in Lane_Detection.Sensor_Orientation_T;
      is_backup   : in Boolean
     ) return Long_Float;

   -- Call to elevate the curb sensors
   procedure elevate_curb_sensor;

end WC2EC_Interface;
