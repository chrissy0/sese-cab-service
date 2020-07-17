package Roadmarker_Functions with SPARK_Mode  is

   function is_Active (inf_Value : Long_Float) return Boolean with
     Post => ( if inf_Value < 250.0 then is_Active'Result =True else  is_Active'Result =False);

   function on_Road_Marker (ir_marker_out_fl : Long_Float; ir_marker_out_fr : Long_Float; ir_marker_out_bl : Long_Float; ir_marker_out_br : Long_Float) return Boolean with
     Post => ( if ir_marker_out_fl < 250.0 and  ir_marker_out_fr < 250.0 and ir_marker_out_bl < 250.0 and ir_marker_out_br < 250.0  then on_Road_Marker'Result =True  else  on_Road_Marker'Result =False);


   function rm_First_Bit (ir_marker_out_fl : Long_Float) return Boolean with
   Post => ( if ir_marker_out_fl < 250.0 then rm_First_Bit'Result =True else  rm_First_Bit'Result =False);

   function rm_Second_Bit (ir_marker_out_fr : Long_Float) return Boolean with
     Post => ( if ir_marker_out_fr < 250.0 then rm_Second_Bit'Result =True else  rm_Second_Bit'Result =False);

   function rm_Third_Bit (ir_marker_out_bl : Long_Float) return Boolean with
     Post => ( if ir_marker_out_bl < 250.0 then rm_Third_Bit'Result =True else  rm_Third_Bit'Result =False);

   function rm_Fourth_Bit (ir_marker_out_br : Long_Float) return Boolean with
     Post => ( if ir_marker_out_br < 250.0 then rm_Fourth_Bit'Result =True else  rm_Fourth_Bit'Result =False);

   function get_Road_MarkerID (ir_marker_out_fl : Long_Float; ir_marker_out_fr : Long_Float; ir_marker_out_bl : Long_Float; ir_marker_out_br : Long_Float) return Long_Float with
      Pre => (ir_marker_out_fl >= 0.0 and ir_marker_out_fr >= 0.0 and ir_marker_out_bl >= 0.0 and ir_marker_out_br >= 0.0),
                            Post=> (if ir_marker_out_fl< 250.0  and ir_marker_out_fr< 250.0  and ir_marker_out_bl< 250.0  and ir_marker_out_br< 250.0  then get_Road_MarkerID'Result = 15.0
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr< 250.0  and ir_marker_out_bl< 250.0  and ir_marker_out_br< 250.0  then get_Road_MarkerID'Result = 14.0
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr>= 250.0 and ir_marker_out_bl< 250.0  and ir_marker_out_br< 250.0  then get_Road_MarkerID'Result = 13.0
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr>= 250.0 and ir_marker_out_bl< 250.0  and ir_marker_out_br< 250.0  then get_Road_MarkerID'Result = 12.0
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr< 250.0  and ir_marker_out_bl>= 250.0 and ir_marker_out_br< 250.0  then get_Road_MarkerID'Result = 11.0
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr< 250.0  and ir_marker_out_bl>= 250.0 and ir_marker_out_br< 250.0  then get_Road_MarkerID'Result = 10.0
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr>= 250.0 and ir_marker_out_bl>= 250.0 and ir_marker_out_br< 250.0  then get_Road_MarkerID'Result = 9.0
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr>= 250.0 and ir_marker_out_bl>= 250.0 and ir_marker_out_br< 250.0  then get_Road_MarkerID'Result = 8.0
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr< 250.0  and ir_marker_out_bl< 250.0  and ir_marker_out_br>= 250.0 then get_Road_MarkerID'Result = 7.0
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr< 250.0  and ir_marker_out_bl< 250.0  and ir_marker_out_br>= 250.0 then get_Road_MarkerID'Result = 6.0
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr>= 250.0 and ir_marker_out_bl< 250.0  and ir_marker_out_br>= 250.0 then get_Road_MarkerID'Result = 5.0
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr>= 250.0 and ir_marker_out_bl< 250.0  and ir_marker_out_br>= 250.0 then get_Road_MarkerID'Result = 4.0
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr< 250.0  and ir_marker_out_bl>= 250.0 and ir_marker_out_br>= 250.0 then get_Road_MarkerID'Result = 3.0
                                   else (if ir_marker_out_fl>= 250.0 and ir_marker_out_fr< 250.0  and ir_marker_out_bl>= 250.0 and ir_marker_out_br>= 250.0 then get_Road_MarkerID'Result = 2.0
                                   else (if ir_marker_out_fl< 250.0  and ir_marker_out_fr>= 250.0 and ir_marker_out_bl>= 250.0 and ir_marker_out_br>= 250.0 then get_Road_MarkerID'Result = 1.0
                                   else  get_Road_MarkerID'Result = 0.0 )))))))))))))));


end Roadmarker_Functions;
