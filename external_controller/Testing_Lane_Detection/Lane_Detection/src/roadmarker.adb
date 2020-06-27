pragma Ada_2012;
with Roadmarker_Functions; use Roadmarker_Functions;
with Lane_Detection; use Lane_Detection;
package body Roadmarker is


   task body Roadmarker_Task_T is



      Current_Marker             : Long_Float :=0.0;
      Target_Marker              : Long_Float;
      Intersection_Target_Marker : Long_Float;
      IR_Marker_Front_Left_Active       : Long_Float ;
      IR_Marker_Front_Right_Active      : Long_Float ;
      IR_Marker_Behind_Left_Active      : Long_Float ;
      IR_Marker_Behind_Right_Active    : Long_Float ;
      IR_Marker_Front_Left       : Long_Float ;
      IR_Marker_Front_Right      : Long_Float ;
      IR_Marker_Behind_Left      : Long_Float ;
      IR_Marker_Behind_Right     : Long_Float ;



      WC2EC_Driver               : wc2ec_thread_access_t;
      Lane_signal_task   : Lane_Detection.Lane_Detection_Taks_T;


   begin

      accept Construct
        (
         WC2EC_Driver_A         : in wc2ec_thread_access_t  )
      do
         WC2EC_Driver           := WC2EC_Driver_A;


      end Construct;

     while (true) loop
         -- Read sensor values

         IR_Marker_Front_Left_Active := WC2EC.get_distance_sensor_data ("inf_rm_fl_act");

         IR_Marker_Front_Right_Active := WC2EC.get_distance_sensor_data ("inf_rm_fr_act");

         IR_Marker_Behind_Left_Active := WC2EC.get_distance_sensor_data ("inf_rm_bl_act");

         IR_Marker_Behind_Right_Active := WC2EC.get_distance_sensor_data ("inf_rm_br_act");

         IR_Marker_Front_Left := WC2EC.get_distance_sensor_data ("inf_rm_fl");

         IR_Marker_Front_Right := WC2EC.get_distance_sensor_data ("inf_rm_fr");

         IR_Marker_Behind_Left := WC2EC.get_distance_sensor_data ("inf_rm_bl");

         IR_Marker_Behind_Right := WC2EC.get_distance_sensor_data ("inf_rm_br");




         Intersection_Target_Marker := 1.0;
       --  Put_Line
      --     (ASCII.HT & "Current_Marker := " & Integer(Current_Marker)'Image);


         if
           on_Road_Marker(IR_Marker_Front_Left_Active , IR_Marker_Front_Right_Active , IR_Marker_Behind_Left_Active , IR_Marker_Behind_Right_Active)
         then
             Current_Marker := get_Road_MarkerID( IR_Marker_Front_Left , IR_Marker_Front_Right , IR_Marker_Behind_Left , IR_Marker_Behind_Right);




         end if;


      end loop;



   end  Roadmarker_Task_T;

end Roadmarker;
