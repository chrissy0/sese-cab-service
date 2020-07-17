with WC2EC; use WC2EC;
with Ada.Text_IO; use Ada.Text_IO;
with distance_sensor_p; use distance_sensor_p;
package body Webots_Function is
   
   function follow_Line (Speed : Long_Float; leftSpeed :  out Long_Float; rightSpeed : out Long_Float) return Integer is
   dist_l_Value : Long_Float;
   dist_c_Value : Long_Float;
   dist_r_Value : Long_Float;
   inf_l_Value : Long_Float;
   inf_r_Value : Long_Float;
   --leftSpeed : Long_Float  := Speed;
   --rightSpeed : Long_Float := Speed;
      

   begin
      leftSpeed := Speed;
      rightSpeed := Speed;
      --Put_Line("follow_Line");
      
      dist_l_Value :=  WC2EC.get_distance_sensor_data("dist_l");
      
      dist_c_Value :=  WC2EC.get_distance_sensor_data("dist_c");  
      
      dist_r_Value :=  WC2EC.get_distance_sensor_data("dist_r");

      inf_r_Value :=  WC2EC.get_distance_sensor_data("inf_right");

      inf_l_Value :=  WC2EC.get_distance_sensor_data("inf_left");

      
   if (dist_l_Value > 350.0 and dist_c_Value > 700.0 and dist_r_Value > 350.0) then
        if (inf_r_Value < 200.0 and inf_l_Value > 200.0)then  --right
           rightSpeed := 0.0;
         
        elsif (inf_l_Value < 200.0 and inf_r_Value > 200.0)then--left
           leftSpeed := 0.0;
         
      end if;
   else 
      leftSpeed := 0.0;
      rightSpeed := 0.0;
   
      end if;
   if (leftSpeed = 0.0) then
      Put_Line("dist_l " & dist_l_Value'Image);
      Put_Line("dist_c " & dist_c_Value'Image);
      Put_Line("dist_r " & dist_r_Value'Image);
      Put_Line("inf_r " & inf_r_Value'Image);
      Put_Line("inf_l " & inf_l_Value'Image);
      
      
   end if ;
   
   --Put_Line("LeftSpeed: " & leftSpeed'Image);
   --Put_Line("rightspeed: " & rightSpeed'Image);
   
   return 0;
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
