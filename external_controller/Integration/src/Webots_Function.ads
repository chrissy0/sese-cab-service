package Webots_Function is

   function follow_Line (Speed : Integer; dist_l_Value : Integer; dist_c_Value : Integer; dist_r_Value : Integer; inf_l_Value : Integer; inf_r_Value : Integer) return Integer ;
   function is_Active (inf_Value : Integer) return Boolean;

   function rm_First_Bit (inf_rm_fl : Integer) return Boolean;
   function rm_Second_Bit (inf_rm_fr : Integer) return Boolean;
   function rm_Third_Bit (inf_rm_bl : Integer) return Boolean;
   function rm_Fourth_Bit (inf_rm_br : Integer) return Boolean;
   function get_Road_MarkerID (inf_rm_fl : Integer; inf_rm_fr : Integer; inf_rm_bl : Integer; inf_rm_br : Integer) return Integer;


   function on_Road_Marker (inf_rm_fr_act : Integer; inf_rm_fl_act : Integer; inf_rm_br_act : Integer; inf_rm_bl_act : Integer) return Boolean;

end Webots_Function;
