pragma Ada_2012;
with Roadmarker_Functions; use Roadmarker_Functions;

package body Roadmarker is

    function Get_Current_Marker(IR_Marker_Front_Left_Active : in Long_Float;
                                     IR_Marker_Front_Right_Active: in Long_Float;
                                     IR_Marker_Behind_Left_Active: in Long_Float;
                                     IR_Marker_Behind_Right_Active:in Long_Float;
                                     IR_Marker_Front_Left        : in Long_Float;
                                     IR_Marker_Front_Right       : in Long_Float;
                                     IR_Marker_Behind_Left       : in Long_Float;
                                     IR_Marker_Behind_Right      : in Long_Float)
                                return Integer is
      Current_Marker : Long_Float := -1.0;
         begin

            if
              on_Road_Marker(IR_Marker_Front_Left_Active , IR_Marker_Front_Right_Active , IR_Marker_Behind_Left_Active , IR_Marker_Behind_Right_Active)
            then
               Current_Marker := get_Road_MarkerID( IR_Marker_Front_Left , IR_Marker_Front_Right , IR_Marker_Behind_Left , IR_Marker_Behind_Right);
            end if;

            return Integer(Current_Marker);
    end Get_Current_Marker;


   task body Roadmarker_Task_T is



      Current_Marker             : Integer := 0;
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






   begin



     while (true) loop
         -- Read sensor values





         Intersection_Target_Marker := 1.0;
       --  Put_Line
      --     (ASCII.HT & "Current_Marker := " & Integer(Current_Marker)'Image);

         Current_Marker:= Get_Current_Marker(IR_Marker_Front_Left_Active ,
                                             IR_Marker_Front_Right_Active,
                                             IR_Marker_Behind_Left_Active,
                                             IR_Marker_Behind_Right_Active,
                                             IR_Marker_Front_Left        ,
                                             IR_Marker_Front_Right       ,
                                             IR_Marker_Behind_Left       ,
                                             IR_Marker_Behind_Right);








      end loop;



   end  Roadmarker_Task_T;

end Roadmarker;
