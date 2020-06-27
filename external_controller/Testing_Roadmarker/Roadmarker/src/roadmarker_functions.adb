package body Roadmarker_Functions is
   
   function is_Active (inf_Value : Long_Float) return Boolean is
   begin
      return inf_Value <250.0;
   end is_Active;
   
 function on_Road_Marker (ir_marker_out_fl : Long_Float; ir_marker_out_fr : Long_Float; ir_marker_out_bl : Long_Float; ir_marker_out_br : Long_Float) return Boolean is
   begin
      return is_Active(ir_marker_out_fl) and is_Active(ir_marker_out_fr) and is_Active(ir_marker_out_bl) and is_Active(ir_marker_out_br);
   end on_Road_Marker;

   function rm_First_Bit (ir_marker_out_fl : Long_Float) return Boolean is
   begin
      return is_Active(ir_marker_out_fl);
   end rm_First_Bit;
      
   function rm_Second_Bit (ir_marker_out_fr : Long_Float) return Boolean is
   begin
      return is_Active(ir_marker_out_fr);
   end rm_Second_Bit;
   
   function rm_Third_Bit (ir_marker_out_bl : Long_Float) return Boolean is
   begin
      return is_Active(ir_marker_out_bl);
   end rm_Third_Bit;
   
   function rm_Fourth_Bit (ir_marker_out_br : Long_Float) return Boolean is
   begin
      return is_Active(ir_marker_out_br);
   end rm_Fourth_Bit;
   
   function get_Road_MarkerID (ir_marker_out_fl : Long_Float; ir_marker_out_fr : Long_Float; ir_marker_out_bl : Long_Float; ir_marker_out_br : Long_Float) return Long_Float is
   Sum_MarkerID : Long_Float := 0.0;
   begin  
      if(rm_First_Bit(ir_marker_out_fl) = True)then Sum_MarkerID := Sum_MarkerID+1.0; end if;
      if(rm_Second_Bit(ir_marker_out_fr) = True)then Sum_MarkerID := Sum_MarkerID+2.0; end if;
      if(rm_Third_Bit(ir_marker_out_bl) = True)then Sum_MarkerID := Sum_MarkerID+4.0; end if;
      if(rm_Fourth_Bit(ir_marker_out_br) = True)then Sum_MarkerID := Sum_MarkerID+8.0; end if;
         
      return  Sum_MarkerID;--rm_First_Bit(inf_rm_fl) + 2*rm_Second_Bit(inf_rm_fr) + 4*rm_Third_Bit(inf_rm_bl) +8*rm_Fourth_Bit(inf_rm_br);
   end get_Road_MarkerID;

end Roadmarker_Functions;
