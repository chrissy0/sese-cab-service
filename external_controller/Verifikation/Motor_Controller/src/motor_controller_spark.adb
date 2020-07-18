pragma Ada_2012;
with Motor_Controller; use Motor_Controller;


package body Motor_Controller_SPARK with SPARK_Mode is

   
   procedure SPARK_1  is 
   begin
      output_driving(state_V, motor_values_V);
      
   end SPARK_1;
   
   procedure SPARK_2   is 
   begin
      output_front_is_clear(state_V, motor_values_V);
     
   end SPARK_2;
   
   procedure SPARK_3  is 
   begin
       output_no_system_error(state_V, motor_values_V, JE_Next_Signal_V);
       
   end SPARK_3;
   
   procedure SPARK_4   is 
   begin
       output_final_safe_state(state_V, motor_values_V, JE_Next_Signal_V);
     
   end SPARK_4;
   
   procedure SPARK_5  is 
   begin
       output_system_error(state_V, motor_values_V, JE_Next_Signal_V);   
   
   end SPARK_5;
   
   procedure SPARK_6   is 
   begin
      calculate_output(state_V, motor_values_V,LD_Next_Signal_V, FD_Next_Signal_V, JE_Next_Signal_V );   
    
   end SPARK_6;
   
   
 
   
end Motor_Controller_SPARK;
