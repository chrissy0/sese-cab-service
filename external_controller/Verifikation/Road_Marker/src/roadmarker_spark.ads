with Roadmarker_Functions; use Roadmarker_Functions;
with Roadmarker;           use Roadmarker;

package Roadmarker_SPARK with SPARK_Mode is
  
   all_sensor_values_V : All_Sensor_Values_Array_T;
   is_backup_sensor_V  : Boolean;
   sensors_V           : All_Sensor_Values_Array_T;
   history_V           : Road_Marker_History_T;
   
   inf_Value : Long_Float;
   ir_marker_out_fl : Long_Float; 
   ir_marker_out_fr : Long_Float;  
   ir_marker_out_bl : Long_Float;  
   ir_marker_out_br : Long_Float; 
   
   Output_V1           : Boolean ;
   Output_V2           : Road_Marker_Done_T;
   Output_V3           : Boolean ;
   Output_V4           : Road_Marker_Done_T;
   Output_V5           : Boolean;
   Output_V6           : Boolean;
   Output_V7           : Boolean;
   Output_V8           : Boolean;
   Output_V9           : Boolean;
   Output_V10          : Boolean;
   Output_V11          : Boolean;
   Output_V12          : Long_Float;
   
   --------------------------Roadmarker.adb/ads--------------------------
   
   procedure SPARK_1 with Post => (for some I in Roadmarker_Sensor_ID_T'Range =>(if all_sensor_values_V(I,is_backup_sensor_V) = -1.0 then Output_V1 = True  else Output_V1 = False));

   procedure SPARK_2 with Post => (if sensors_V(FRONT_LEFT ,is_backup_sensor_V ) <250.0 and sensors_V(FRONT_RIGHT ,is_backup_sensor_V ) <250.0 and sensors_V(BEHIND_LEFT ,is_backup_sensor_V ) <250.0 and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V ) <250.0 then
                                    
                                   (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )< 250.0  then Output_V2 = 15
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )< 250.0  then Output_V2 = 14 
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )< 250.0  then Output_V2 = 13 
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )< 250.0  then Output_V2 = 12 
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )< 250.0  then Output_V2 = 11 
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )< 250.0  then Output_V2 = 10 
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )< 250.0  then Output_V2 = 9  
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )< 250.0  then Output_V2 = 8  
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )>= 250.0 then Output_V2 = 7  
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )>= 250.0 then Output_V2 = 6  
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )>= 250.0 then Output_V2 = 5  
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )>= 250.0 then Output_V2 = 4  
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )>= 250.0 then Output_V2 = 3  
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )< 250.0  and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )>= 250.0 then Output_V2 = 2  
                                   else (if sensors_V(FRONT_LEFT ,is_backup_sensor_V )< 250.0  and sensors_V(FRONT_RIGHT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_LEFT ,is_backup_sensor_V )>= 250.0 and sensors_V(BEHIND_RIGHT ,is_backup_sensor_V )>= 250.0 then Output_V2 = 1  
                                   else  Output_V2 = 0 )))))))))))))))
                                    
                                   else Output_V2 = 17);
                                                                                   
     
   procedure SPARK_3  with Post => (for all I in Road_Marker_Done_T=> ( history_V(I) = 0 ));           

   procedure SPARK_4  with Post => (for all I in Road_Marker_Done_T=> (if Output_V4 /= I then history_V(Output_V4) > history_V(I)));  ---------problem
   
   procedure SPARK_5 with Post =>(for all I in Roadmarker_Sensor_ID_T'Range => (if sensors_V(I, is_backup_sensor_V) <240.0 and sensors_V(I, is_backup_sensor_V)>250.0 then Output_V5 =False  else Output_V5 =True));

   
   --------------------------Roadmarker_Function.adb/ads--------------------------
   
   procedure SPARK_6 with Pre => inf_Value >= 0.0 , Post => (if inf_Value < 250.0 then Output_V6 = True  else Output_V6 = False); 
       
   procedure SPARK_7 with Pre => (ir_marker_out_fl >= 0.0 and ir_marker_out_fr >= 0.0 and ir_marker_out_bl >= 0.0 and ir_marker_out_br >= 0.0),
                          Post=> (if ir_marker_out_fl < 250.0 and ir_marker_out_fr < 250.0 and ir_marker_out_bl < 250.0 and ir_marker_out_br < 250.0 then Output_V7 = True  else Output_V7 = False);  
 
   procedure SPARK_8   with Post=> (if ir_marker_out_fl >= 250.0 then Output_V8 = False   else Output_V8 = True);
                         
                         
                         
   
   procedure SPARK_9 with Pre => (ir_marker_out_fr >= 0.0 ),
                          Post=> (if ir_marker_out_fr >= 250.0 then Output_V9 = False  else Output_V9 = True);   
      
   procedure SPARK_10 with Pre => (ir_marker_out_bl >= 0.0 ),
                           Post=> (if ir_marker_out_bl >= 250.0 then Output_V10 = False  else Output_V10 = True);
   
   procedure SPARK_11 with Pre => (ir_marker_out_br >= 0.0 ),
                           Post=> (if ir_marker_out_br >= 250.0 then Output_V11 = False  else Output_V11 = True);  
      
   procedure SPARK_12 with Pre => (ir_marker_out_fl >= 0.0 and ir_marker_out_fr >= 0.0 and ir_marker_out_bl >= 0.0 and ir_marker_out_br >= 0.0),
                           Post=> (if ir_marker_out_fl< 250.0  and ir_marker_out_fr< 250.0  and ir_marker_out_bl< 250.0  and ir_marker_out_br< 250.0  then Output_V12 = 15.0 
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr< 250.0  and ir_marker_out_bl< 250.0  and ir_marker_out_br< 250.0  then Output_V12 = 14.0 
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr>= 250.0 and ir_marker_out_bl< 250.0  and ir_marker_out_br< 250.0  then Output_V12 = 13.0 
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr>= 250.0 and ir_marker_out_bl< 250.0  and ir_marker_out_br< 250.0  then Output_V12 = 12.0 
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr< 250.0  and ir_marker_out_bl>= 250.0 and ir_marker_out_br< 250.0  then Output_V12 = 11.0 
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr< 250.0  and ir_marker_out_bl>= 250.0 and ir_marker_out_br< 250.0  then Output_V12 = 10.0 
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr>= 250.0 and ir_marker_out_bl>= 250.0 and ir_marker_out_br< 250.0  then Output_V12 = 9.0  
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr>= 250.0 and ir_marker_out_bl>= 250.0 and ir_marker_out_br< 250.0  then Output_V12 = 8.0  
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr< 250.0  and ir_marker_out_bl< 250.0  and ir_marker_out_br>= 250.0 then Output_V12 = 7.0  
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr< 250.0  and ir_marker_out_bl< 250.0  and ir_marker_out_br>= 250.0 then Output_V12 = 6.0  
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr>= 250.0 and ir_marker_out_bl< 250.0  and ir_marker_out_br>= 250.0 then Output_V12 = 5.0  
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr>= 250.0 and ir_marker_out_bl< 250.0  and ir_marker_out_br>= 250.0 then Output_V12 = 4.0  
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr< 250.0  and ir_marker_out_bl>= 250.0 and ir_marker_out_br>= 250.0 then Output_V12 = 3.0  
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr< 250.0  and ir_marker_out_bl>= 250.0 and ir_marker_out_br>= 250.0 then Output_V12 = 2.0  
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr>= 250.0 and ir_marker_out_bl>= 250.0 and ir_marker_out_br>= 250.0 then Output_V12 = 1.0  
                                   else  Output_V12 = 0.0 )))))))))))))));
                                  
end Roadmarker_SPARK;
