
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Roadmarker_Functions; use Roadmarker_Functions;
with Roadmarker;           use Roadmarker;

package body Roadmarker_SPARK with SPARK_Mode is
   

    --------------------------Roadmarker.adb/ads--------------------------
   
  procedure SPARK_1  is 
   begin
      is_backup_sensor_V := False;
      Output_V1 := check_error_sensor_array( all_sensor_values_V , is_backup_sensor_V) ;
   end SPARK_1;
   
   procedure SPARK_2  is 
   begin
      Output_V2 := read_road_marker( sensors_V , is_backup_sensor_V);
   end SPARK_2;
   
   
   procedure SPARK_3  is 
   begin
      empty_history(history_V);
   end SPARK_3;
   
   procedure SPARK_4  is 
   begin
      Output_V4 := calculate_output_from_history( history_V );
   end SPARK_4;
   
   procedure SPARK_5  is 
   begin
      Output_V5 := is_on_hotfix_rm( sensors_V,  is_backup_sensor_V );
   end SPARK_5;
   
   --------------------------Roadmarker_Function.adb/ads--------------------------
   
   procedure SPARK_6  is 
   begin
      Output_V6 := is_Active(inf_Value);
   end SPARK_6;
   
   procedure SPARK_7  is 
   begin
      Output_V7 := on_Road_Marker (ir_marker_out_fl , ir_marker_out_fr , ir_marker_out_bl , ir_marker_out_br );
   end SPARK_7;
   
   procedure SPARK_8   is 
   begin
       Output_V8 := rm_First_Bit (ir_marker_out_fl);
     
   end SPARK_8;
   
   procedure SPARK_9  is 
   begin
      Output_V9 := rm_Second_Bit (ir_marker_out_fr);
   end SPARK_9;
   
   procedure SPARK_10  is 
   begin
      Output_V10 := rm_Third_Bit (ir_marker_out_bl);
   end SPARK_10;
   
   procedure SPARK_11  is 
   begin
      Output_V11 := rm_Fourth_Bit (ir_marker_out_br);
   end SPARK_11;
   
  procedure SPARK_12  is 
   begin
      Output_V12 := get_Road_MarkerID (ir_marker_out_fl , ir_marker_out_fr , ir_marker_out_bl , ir_marker_out_br );
   end SPARK_12;
   

   
end Roadmarker_SPARK;
