pragma Ada_2012;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;
with Roadmarker;              use Roadmarker;

package body Roadmarker_Test is
   
   
   type Marker_Values_Long_Float_Array_T is array (Marker_ID_T) of Long_Float;

   protected type Marker_Values_T is

      procedure set(ID : Marker_ID_T; Value : Long_Float); --Assign the value of sensor
      function get (ID : Marker_ID_T) return Long_Float;

   private

      Marker_Values_Array : Marker_Values_Long_Float_Array_T; -- ex) (Lane_Mid_Value, Lane_Left_Value, Lane_Right_Value, Curb_Left_Value, Curb_Right_Value)

   end Marker_Values_T;


   protected body Marker_Values_T is

      procedure set (ID : Marker_ID_T; Value : Long_Float) is
      begin
         Marker_Values_Array (ID) := Value;

      end set;

      function get (ID : Marker_ID_T) return Long_Float is
      begin
         return Marker_Values_Array (ID);
      end get;
   end Marker_Values_T;

   
   Marker_Values : Marker_Values_T; ---new.




   ----------
   -- Name --
   ----------

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Math package");
   end Name;

   --------------
   -- Run_Test --
   --------------
   
   
   Procedure Output_Marker_Test ( Output_Marker : Integer) is
      Marker_Threshhold      : Long_Float := 250.0;
      begin
     
      if(Output_Marker = -1) then
            Put_line("System Error of Roadmarker");
            Assert(Marker_Values.get(Outer_Front_Left) > Marker_Threshhold or
                   Marker_Values.get(Outer_Front_Left) > Marker_Threshhold or
                   Marker_Values.get(Outer_Front_Left) > Marker_Threshhold or
                   Marker_Values.get(Outer_Front_Left) > Marker_Threshhold,
                   "At least one of the Outer_Marker is bigger than the Value of Marker_Threshhold");
                  
      elsif(Output_Marker = 0) then
                Put_line("Output_Marker = 0");
                Assert(Marker_Values.get(Inner_Front_Left) > Marker_Threshhold , "Expected Inner_Front_Left (1) is off");
                Assert(Marker_Values.get(Inner_Front_Right) > Marker_Threshhold , "Expected Inner_Front_Left (2) is off");
                Assert(Marker_Values.get(Inner_Behind_Left) > Marker_Threshhold , "Expected Inner_Front_Left (4) is off");
                Assert(Marker_Values.get(Inner_Behind_Right) > Marker_Threshhold , "Expected Inner_Front_Left (8) is off");
      
      elsif(Output_Marker = 1) then
                Put_line("Output_Marker = 1");
                Assert(Marker_Values.get(Inner_Front_Left) < Marker_Threshhold , "Expected Inner_Front_Left (1) is on");
                Assert(Marker_Values.get(Inner_Front_Right) > Marker_Threshhold , "Expected Inner_Front_Left (2) is off");
                Assert(Marker_Values.get(Inner_Behind_Left) > Marker_Threshhold , "Expected Inner_Front_Left (4) is off");
                Assert(Marker_Values.get(Inner_Behind_Right) > Marker_Threshhold , "Expected Inner_Front_Left (8) is off");
      
     
      elsif(Output_Marker = 2) then
                Put_line("Output_Marker = 2");
                Assert(Marker_Values.get(Inner_Front_Left) > Marker_Threshhold , "Expected Inner_Front_Left (1) is off");
                Assert(Marker_Values.get(Inner_Front_Right) < Marker_Threshhold , "Expected Inner_Front_Left (2) is on");
                Assert(Marker_Values.get(Inner_Behind_Left) > Marker_Threshhold , "Expected Inner_Front_Left (4) is off");
                Assert(Marker_Values.get(Inner_Behind_Right) > Marker_Threshhold , "Expected Inner_Front_Left (8) is off");
      
     
       elsif(Output_Marker = 3) then
                Put_line("Output_Marker = 3");
                Assert(Marker_Values.get(Inner_Front_Left) < Marker_Threshhold , "Expected Inner_Front_Left (1) is on");
                Assert(Marker_Values.get(Inner_Front_Right) < Marker_Threshhold , "Expected Inner_Front_Left (2) is on");
                Assert(Marker_Values.get(Inner_Behind_Left) > Marker_Threshhold , "Expected Inner_Front_Left (4) is off");
                Assert(Marker_Values.get(Inner_Behind_Right) > Marker_Threshhold , "Expected Inner_Front_Left (8) is off");
      
     
      elsif(Output_Marker = 4) then
                Put_line("Output_Marker = 4");
                Assert(Marker_Values.get(Inner_Front_Left) > Marker_Threshhold , "Expected Inner_Front_Left (1) is off");
                Assert(Marker_Values.get(Inner_Front_Right) > Marker_Threshhold , "Expected Inner_Front_Left (2) is off");
                Assert(Marker_Values.get(Inner_Behind_Left) < Marker_Threshhold , "Expected Inner_Front_Left (4) is on");
                Assert(Marker_Values.get(Inner_Behind_Right) > Marker_Threshhold , "Expected Inner_Front_Left (8) is off");
                
     
       elsif(Output_Marker = 5) then
                Put_line("Output_Marker = 5");
                Assert(Marker_Values.get(Inner_Front_Left) < Marker_Threshhold , "Expected Inner_Front_Left (1) is on");
                Assert(Marker_Values.get(Inner_Front_Right) > Marker_Threshhold , "Expected Inner_Front_Left (2) is off");
                Assert(Marker_Values.get(Inner_Behind_Left) < Marker_Threshhold , "Expected Inner_Front_Left (4) is on");
                Assert(Marker_Values.get(Inner_Behind_Right) > Marker_Threshhold , "Expected Inner_Front_Left (8) is off");
                
     
       elsif(Output_Marker = 6) then
                Put_line("Output_Marker = 6");
                Assert(Marker_Values.get(Inner_Front_Left) > Marker_Threshhold , "Expected Inner_Front_Left (1) is off");
                Assert(Marker_Values.get(Inner_Front_Right) < Marker_Threshhold , "Expected Inner_Front_Left (2) is on");
                Assert(Marker_Values.get(Inner_Behind_Left) < Marker_Threshhold , "Expected Inner_Front_Left (4) is on");
                Assert(Marker_Values.get(Inner_Behind_Right) > Marker_Threshhold , "Expected Inner_Front_Left (8) is off");
                
    
      elsif(Output_Marker = 7) then
                Put_line("Output_Marker = 7");
                Assert(Marker_Values.get(Inner_Front_Left) < Marker_Threshhold , "Expected Inner_Front_Left (1) is on");
                Assert(Marker_Values.get(Inner_Front_Right) < Marker_Threshhold , "Expected Inner_Front_Left (2) is on");
                Assert(Marker_Values.get(Inner_Behind_Left) < Marker_Threshhold , "Expected Inner_Front_Left (4) is on");
                Assert(Marker_Values.get(Inner_Behind_Right) > Marker_Threshhold , "Expected Inner_Front_Left (8) is off");
                
     
      elsif(Output_Marker = 8) then
                Put_line("Output_Marker = 8");
                Assert(Marker_Values.get(Inner_Front_Left) > Marker_Threshhold , "Expected Inner_Front_Left (1) is off");
                Assert(Marker_Values.get(Inner_Front_Right) > Marker_Threshhold , "Expected Inner_Front_Left (2) is off");
                Assert(Marker_Values.get(Inner_Behind_Left) > Marker_Threshhold , "Expected Inner_Front_Left (4) is off");
                Assert(Marker_Values.get(Inner_Behind_Right) < Marker_Threshhold , "Expected Inner_Front_Left (8) is on");
                
     
       elsif(Output_Marker = 9) then
                Put_line("Output_Marker = 9");
                Assert(Marker_Values.get(Inner_Front_Left) < Marker_Threshhold , "Expected Inner_Front_Left (1) is on");
                Assert(Marker_Values.get(Inner_Front_Right) > Marker_Threshhold , "Expected Inner_Front_Left (2) is off");
                Assert(Marker_Values.get(Inner_Behind_Left) > Marker_Threshhold , "Expected Inner_Front_Left (4) is off");
                Assert(Marker_Values.get(Inner_Behind_Right) < Marker_Threshhold , "Expected Inner_Front_Left (8) is on");
                
    
      elsif(Output_Marker = 10) then
                Put_line("Output_Marker = 10");
                Assert(Marker_Values.get(Inner_Front_Left) > Marker_Threshhold , "Expected Inner_Front_Left (1) is off");
                Assert(Marker_Values.get(Inner_Front_Right) < Marker_Threshhold , "Expected Inner_Front_Left (2) is on");
                Assert(Marker_Values.get(Inner_Behind_Left) > Marker_Threshhold , "Expected Inner_Front_Left (4) is off");
                Assert(Marker_Values.get(Inner_Behind_Right) < Marker_Threshhold , "Expected Inner_Front_Left (8) is on");
         
    
      elsif(Output_Marker = 11) then
                Put_line("Output_Marker = 11");
                Assert(Marker_Values.get(Inner_Front_Left) < Marker_Threshhold , "Expected Inner_Front_Left (1) is on");
                Assert(Marker_Values.get(Inner_Front_Right) < Marker_Threshhold , "Expected Inner_Front_Left (2) is on");
                Assert(Marker_Values.get(Inner_Behind_Left) > Marker_Threshhold , "Expected Inner_Front_Left (4) is off");
                Assert(Marker_Values.get(Inner_Behind_Right) < Marker_Threshhold , "Expected Inner_Front_Left (8) is on");
              
     
      elsif(Output_Marker = 12) then
                Put_line("Output_Marker = 12");
                Assert(Marker_Values.get(Inner_Front_Left) > Marker_Threshhold , "Expected Inner_Front_Left (1) is off");
                Assert(Marker_Values.get(Inner_Front_Right) > Marker_Threshhold , "Expected Inner_Front_Left (2) is off");
                Assert(Marker_Values.get(Inner_Behind_Left) < Marker_Threshhold , "Expected Inner_Front_Left (4) is on");
                Assert(Marker_Values.get(Inner_Behind_Right) < Marker_Threshhold , "Expected Inner_Front_Left (8) is on");
                
    
      elsif(Output_Marker = 13) then
                Put_line("Output_Marker = 13");
                Assert(Marker_Values.get(Inner_Front_Left) < Marker_Threshhold , "Expected Inner_Front_Left (1) is on");
                Assert(Marker_Values.get(Inner_Front_Right) > Marker_Threshhold , "Expected Inner_Front_Left (2) is off");
                Assert(Marker_Values.get(Inner_Behind_Left) < Marker_Threshhold , "Expected Inner_Front_Left (4) is on");
                Assert(Marker_Values.get(Inner_Behind_Right) < Marker_Threshhold , "Expected Inner_Front_Left (8) is on");
                
     
      elsif(Output_Marker =14) then
               Put_line("Output_Marker = 14");
                Assert(Marker_Values.get(Inner_Front_Left) > Marker_Threshhold , "Expected Inner_Front_Left (1) is off");
                Assert(Marker_Values.get(Inner_Front_Right) < Marker_Threshhold , "Expected Inner_Front_Left (2) is on");
                Assert(Marker_Values.get(Inner_Behind_Left) < Marker_Threshhold , "Expected Inner_Front_Left (4) is on");
                Assert(Marker_Values.get(Inner_Behind_Right) < Marker_Threshhold , "Expected Inner_Front_Left (8) is on");
               
    
     elsif(Output_Marker = 15) then
                Put_line("Output_Marker = 15");
                Assert(Marker_Values.get(Inner_Front_Left) < Marker_Threshhold , "Expected Inner_Front_Left (1) is on");
                Assert(Marker_Values.get(Inner_Front_Right) < Marker_Threshhold , "Expected Inner_Front_Left (2) is on");
                Assert(Marker_Values.get(Inner_Behind_Left) < Marker_Threshhold , "Expected Inner_Front_Left (4) is on");
                Assert(Marker_Values.get(Inner_Behind_Right) < Marker_Threshhold , "Expected Inner_Front_Left (8) is on");
         
   
     end if;
   
       
    end Output_Marker_Test;
   
   procedure test_roadmarker (T : in out Test) is
      pragma Unreferenced (T);
      Output_Marker          : Integer ;
      
         
   
   begin
       Put_Line ("Starting case " );
   
      select
         delay 2.0;
            Put_Line ("Time out!");
            Assert    (False, "Roadmarker timed out ");              -------(Outer_Front_Left,Outer_Front_Right,Outer_Behind_Left,Outer_Behind_Right,
                                                                     --------Inner_Front_Left,Inner_Front_Right,Inner_Behind_Left,Inner_Behind_Right);
      then abort
         Marker_Values.set(Outer_Front_Left ,200.0);
         Marker_Values.set(Outer_Front_Right ,200.0);
         Marker_Values.set(Outer_Behind_Left ,200.0);
         Marker_Values.set(Outer_Behind_Right ,200.0);
         Marker_Values.set(Inner_Front_Left ,300.0);
         Marker_Values.set(Inner_Front_Right ,300.0);
         Marker_Values.set(Inner_Behind_Left ,300.0);
         Marker_Values.set(Inner_Behind_Right ,200.0);
         
         Output_Marker := Roadmarker.Get_Current_Marker(Marker_Values.get(Outer_Front_Left) ,
                                                        Marker_Values.get(Outer_Front_Right) ,
                                                        Marker_Values.get(Outer_Behind_Left) ,
                                                        Marker_Values.get(Outer_Behind_Right) ,
                                                        Marker_Values.get(Inner_Front_Left) ,
                                                        Marker_Values.get(Inner_Front_Right) ,
                                                        Marker_Values.get(Inner_Behind_Left) ,
                                                        Marker_Values.get(Inner_Behind_Right) );
        
        Output_Marker_Test(Output_Marker);
         ------------------------------------------------------------------------------------------
         
         Marker_Values.set(Outer_Front_Left ,200.0);
         Marker_Values.set(Outer_Front_Right ,200.0);
         Marker_Values.set(Outer_Behind_Left ,200.0);
         Marker_Values.set(Outer_Behind_Right ,200.0);
         Marker_Values.set(Inner_Front_Left ,200.0);
         Marker_Values.set(Inner_Front_Right ,200.0);
         Marker_Values.set(Inner_Behind_Left ,200.0);
         Marker_Values.set(Inner_Behind_Right ,200.0);
         
         Output_Marker := Roadmarker.Get_Current_Marker(Marker_Values.get(Outer_Front_Left) ,
                                                        Marker_Values.get(Outer_Front_Right) ,
                                                        Marker_Values.get(Outer_Behind_Left) ,
                                                        Marker_Values.get(Outer_Behind_Right) ,
                                                        Marker_Values.get(Inner_Front_Left) ,
                                                        Marker_Values.get(Inner_Front_Right) ,
                                                        Marker_Values.get(Inner_Behind_Left) ,
                                                        Marker_Values.get(Inner_Behind_Right) );
         Output_Marker_Test(Output_Marker);
         
         
     end select;
      
      
    
            
   
   end test_roadmarker;
end Roadmarker_Test;
