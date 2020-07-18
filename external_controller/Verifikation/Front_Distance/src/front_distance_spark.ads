pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Front_Distance;   use Front_Distance;
with Motor_Controller; use Motor_Controller;

package Front_Distance_SPARK with SPARK_Mode 
is
 

Output_V                        : Front_Distance_Done_t;
all_sensor_values_V             : All_Sensor_Values_Array_T ;
threshholds_V                   : Threshhold_Array_T  := (300.0 ,200.0);


 
   procedure SPARK_1 with
    Post => (if Output_V = FD_FAULT_S then 
                ( for some typ in Sensor_Type_T =>
                   ( for some pos in Sensor_Position_T =>
                       ( for some num in Sensor_Number_T =>
                           all_sensor_values_V(typ, pos, num) < 0.0)))
              else (if Output_V = FRONT_BLOCKED_S then
                       ( for some typ in Sensor_Type_T =>
                           ( for some pos in Sensor_Position_T =>
                                ( for some num in Sensor_Number_T =>
                                       all_sensor_values_V(typ, pos, num) < threshholds_V(typ))))
              else (if Output_V = FRONT_CLEAR_S then
                       ( for all typ in Sensor_Type_T =>
                            ( for all pos in Sensor_Position_T =>
                                ( for all num in Sensor_Number_T =>
                                      all_sensor_values_V(typ, pos, num) >= threshholds_V(typ))))
              else Output_V = EMPTY_S)));
        
       
       
    
   


   

end Front_Distance_SPARK;
