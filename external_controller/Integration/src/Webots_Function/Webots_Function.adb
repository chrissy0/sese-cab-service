package body Webots_Function is

   function follow_Line (Speed : Integer; dist_l_Value : Integer; dist_c_Value : Integer; dist_r_Value : Integer; inf_l_Value : Integer; inf_r_Value : Integer) return Integer is
   leftSpeed : Integer  := Speed;
   rightSpeed : Integer := Speed;
   minus : Integer := -1;
   begin


   if (dist_l_Value > 350 and dist_c_Value > 700 and dist_r_Value > 350) then
        if (inf_r_Value < 250 and inf_l_Value > 250)then  --right
           rightSpeed := 0;

        elsif (inf_l_Value < 250 and inf_r_Value > 250)then--left
           leftSpeed := 0;

      end if;
   else
      leftSpeed := 0;
      rightSpeed := 0;

   end if;

   if(rightSpeed = 0) then return leftSpeed; -- expected result : 10
   elsif( leftSpeed = 0) then return rightSpeed*minus; -- expected result : -10
   else return 0;
   end if;

   end follow_Line;



   function is_Active (inf_Value : Integer) return Boolean is
   begin
      return inf_Value <250;
   end is_Active;


   ----------------------------------------------------------------------------------------------------------------------------------------

   function rm_First_Bit (inf_rm_fl : Integer) return Boolean is
   begin
      return is_Active(inf_rm_fl);
   end rm_First_Bit;

   function rm_Second_Bit (inf_rm_fr : Integer) return Boolean is
   begin
      return is_Active(inf_rm_fr);
   end rm_Second_Bit;

   function rm_Third_Bit (inf_rm_bl : Integer) return Boolean is
   begin
      return is_Active(inf_rm_bl);
   end rm_Third_Bit;

   function rm_Fourth_Bit (inf_rm_br : Integer) return Boolean is
   begin
      return is_Active(inf_rm_br);
   end rm_Fourth_Bit;

   function get_Road_MarkerID (inf_rm_fl : Integer; inf_rm_fr : Integer; inf_rm_bl : Integer; inf_rm_br : Integer) return Integer is
   Sum_MarkerID : Integer := 0;
   begin
      if(rm_First_Bit(inf_rm_fl) = True)then Sum_MarkerID := Sum_MarkerID+1; end if;
      if(rm_Second_Bit(inf_rm_fr) = True)then Sum_MarkerID := Sum_MarkerID+2; end if;
      if(rm_Third_Bit(inf_rm_bl) = True)then Sum_MarkerID := Sum_MarkerID+4; end if;
      if(rm_Fourth_Bit(inf_rm_br) = True)then Sum_MarkerID := Sum_MarkerID+8; end if;

      return  Sum_MarkerID;--rm_First_Bit(inf_rm_fl) + 2*rm_Second_Bit(inf_rm_fr) + 4*rm_Third_Bit(inf_rm_bl) +8*rm_Fourth_Bit(inf_rm_br);
   end get_Road_MarkerID;


   ----------------------------------------------------------------------------------------------------------------------------------------


   function on_Road_Marker (inf_rm_fr_act : Integer; inf_rm_fl_act : Integer; inf_rm_br_act : Integer; inf_rm_bl_act : Integer) return Boolean is
   begin
      return is_Active(inf_rm_fr_act) and is_Active(inf_rm_fl_act) and is_Active(inf_rm_br_act) and is_Active(inf_rm_bl_act);
   end on_Road_Marker;




end Webots_Function;
