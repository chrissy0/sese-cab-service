
with Ada.Text_IO;      use Ada.Text_IO;

package Roadmarker is

   type Marker_ID_T is (Outer_Front_Left,Outer_Front_Right,Outer_Behind_Left,Outer_Behind_Right,
                        Inner_Front_Left,Inner_Front_Right,Inner_Behind_Left,Inner_Behind_Right);




    function Get_Current_Marker(     IR_Marker_Front_Left_Active : in Long_Float;
                                     IR_Marker_Front_Right_Active: in Long_Float;
                                     IR_Marker_Behind_Left_Active: in Long_Float;
                                     IR_Marker_Behind_Right_Active:in Long_Float;
                                     IR_Marker_Front_Left        : in Long_Float;
                                     IR_Marker_Front_Right       : in Long_Float;
                                     IR_Marker_Behind_Left       : in Long_Float;
                                     IR_Marker_Behind_Right      : in Long_Float    ) return Integer;

   task type Roadmarker_Task_T is


   end Roadmarker_Task_T;




end Roadmarker;
