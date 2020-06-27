package Roadmarker_Functions is

   function is_Active (inf_Value : Long_Float) return Boolean;

   function on_Road_Marker (ir_marker_out_fl : Long_Float; ir_marker_out_fr : Long_Float; ir_marker_out_bl : Long_Float; ir_marker_out_br : Long_Float) return Boolean ;


   function rm_First_Bit (ir_marker_out_fl : Long_Float) return Boolean ;


   function rm_Second_Bit (ir_marker_out_fr : Long_Float) return Boolean ;


   function rm_Third_Bit (ir_marker_out_bl : Long_Float) return Boolean ;


   function rm_Fourth_Bit (ir_marker_out_br : Long_Float) return Boolean;


   function get_Road_MarkerID (ir_marker_out_fl : Long_Float; ir_marker_out_fr : Long_Float; ir_marker_out_bl : Long_Float; ir_marker_out_br : Long_Float) return Long_Float;

end Roadmarker_Functions;
