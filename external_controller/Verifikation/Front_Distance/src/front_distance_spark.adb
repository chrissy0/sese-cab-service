pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Front_Distance;   use Front_Distance;
with Motor_Controller; use Motor_Controller;

package body Front_Distance_SPARK  with SPARK_Mode is
  
   procedure SPARK_1  is 
   -- type All_Sensor_Values_Array_T is array (Sensor_Type_T, Sensor_Position_T, Sensor_Number_T) of Long_Float; 
   -- type Sensor_Position_T : (CENTER, LEFT, RIGHT);
   -- subtype Sensor_Number_T is Integer range 0..1;
   -- type Sensor_Type_T is (IR, US);
  begin
      Output_V := calculate_output(all_sensor_values_V ,threshholds_V);
   end SPARK_1;
   
   

   
   
end Front_Distance_SPARK;
