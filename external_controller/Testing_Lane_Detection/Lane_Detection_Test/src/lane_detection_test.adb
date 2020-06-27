pragma Ada_2012;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;
with Lane_Detection;          use Lane_Detection;
package body Lane_Detection_Test is

   type Lane_Values_Long_Float_Array_T is array (Lane_ID_T) of Long_Float;

   protected type Lane_Values_T is

      procedure set_value_sensor (ID : Lane_ID_T; Value : Long_Float); --Assign the value of sensor
      function get (ID : Lane_ID_T) return Long_Float;

   private

      Lane_Values_Array : Lane_Values_Long_Float_Array_T; -- ex) (Lane_Mid_Value, Lane_Left_Value, Lane_Right_Value, Curb_Left_Value, Curb_Right_Value)

   end Lane_Values_T;


   protected body Lane_Values_T is

      procedure set_value_sensor (ID : Lane_ID_T; Value : Long_Float) is
      begin
         Lane_Values_Array (ID) := Value;

      end set_value_sensor;

      function get (ID : Lane_ID_T) return Long_Float is
      begin
         return Lane_Values_Array (ID);
      end get;
   end Lane_Values_T;


   Lane_Values : Lane_Values_T; ---new.




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


   procedure test_lane_detection (T : in out Test) is
      pragma Unreferenced (T);
     Output_T: Lane_Detection_Done_T ;
     Output_function                : Lane_Detection_Done_T;
     Lane_IR_Threshhold             : Long_Float := 250.0;
     Lane_UR_Threshhold             : Long_Float := 500.0;

   begin

       Put_Line ("Starting case " );


      select
         delay 2.0;
            Put_Line ("Time out!");
            Assert
              (False, "Lane_Detection timed out in lane_detection_done");
      then abort

            Lane_Values.set_value_sensor(Lane_Mid_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Left_Value,200.0);
            Lane_Values.set_value_sensor(Lane_Right_Value,300.0);
            Lane_Values.set_value_sensor(Curb_Left_Value,600.0);
            Lane_Values.set_value_sensor(Curb_Right_Value,600.0);

         Output_function:= Lane_Detection.Lane_Detection_Process( Output_T,
                                              Lane_Values.get(Lane_Left_Value)  ,
                                              Lane_Values.get(Lane_Right_Value)   ,
                                              Lane_Values.get(Lane_Mid_Value)  ,
                                              Lane_Values.get(Curb_Left_Value) ,
                                              Lane_Values.get(Curb_Right_Value)  ,
                                              Lane_IR_Threshhold,
                                              Lane_UR_Threshhold);


         --------------------------------------------------------------------------------------- Go left Infrared
            Lane_Values.set_value_sensor(Lane_Mid_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Left_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Right_Value,200.0);
            Lane_Values.set_value_sensor(Curb_Left_Value,600.0);
            Lane_Values.set_value_sensor(Curb_Right_Value,600.0);

         Output_function:= Lane_Detection.Lane_Detection_Process( Output_T,
                                              Lane_Values.get(Lane_Left_Value)  ,
                                              Lane_Values.get(Lane_Right_Value)   ,
                                              Lane_Values.get(Lane_Mid_Value)  ,
                                              Lane_Values.get(Curb_Left_Value) ,
                                              Lane_Values.get(Curb_Right_Value)  ,
                                              Lane_IR_Threshhold,
                                              Lane_UR_Threshhold);
         --------------------------------------------------------------------------------------- Go Right Infrared
            Lane_Values.set_value_sensor(Lane_Mid_Value,200.0);
            Lane_Values.set_value_sensor(Lane_Left_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Right_Value,300.0);
            Lane_Values.set_value_sensor(Curb_Left_Value,600.0);
            Lane_Values.set_value_sensor(Curb_Right_Value,600.0);

         Output_function:= Lane_Detection.Lane_Detection_Process( Output_T,
                                              Lane_Values.get(Lane_Left_Value)  ,
                                              Lane_Values.get(Lane_Right_Value)   ,
                                              Lane_Values.get(Lane_Mid_Value)  ,
                                              Lane_Values.get(Curb_Left_Value) ,
                                              Lane_Values.get(Curb_Right_Value)  ,
                                              Lane_IR_Threshhold,
                                              Lane_UR_Threshhold);

         --------------------------------------------------------------------------------------- Go Straight Infrared

           Lane_Values.set_value_sensor(Lane_Mid_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Left_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Right_Value,300.0);
            Lane_Values.set_value_sensor(Curb_Left_Value,400.0);
            Lane_Values.set_value_sensor(Curb_Right_Value,600.0);

         Output_function:= Lane_Detection.Lane_Detection_Process( Output_T,
                                              Lane_Values.get(Lane_Left_Value)  ,
                                              Lane_Values.get(Lane_Right_Value)   ,
                                              Lane_Values.get(Lane_Mid_Value)  ,
                                              Lane_Values.get(Curb_Left_Value) ,
                                              Lane_Values.get(Curb_Right_Value)  ,
                                              Lane_IR_Threshhold,
                                              Lane_UR_Threshhold);

          --------------------------------------------------------------------------------------- Go Right Curb

            Lane_Values.set_value_sensor(Lane_Mid_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Left_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Right_Value,300.0);
            Lane_Values.set_value_sensor(Curb_Left_Value,600.0);
            Lane_Values.set_value_sensor(Curb_Right_Value,400.0);

         Output_function:= Lane_Detection.Lane_Detection_Process( Output_T,
                                              Lane_Values.get(Lane_Left_Value)  ,
                                              Lane_Values.get(Lane_Right_Value)   ,
                                              Lane_Values.get(Lane_Mid_Value)  ,
                                              Lane_Values.get(Curb_Left_Value) ,
                                              Lane_Values.get(Curb_Right_Value)  ,
                                              Lane_IR_Threshhold,
                                              Lane_UR_Threshhold);

         --------------------------------------------------------------------------------------- Go Left Curb

            Lane_Values.set_value_sensor(Lane_Mid_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Left_Value,300.0);
            Lane_Values.set_value_sensor(Lane_Right_Value,300.0);
            Lane_Values.set_value_sensor(Curb_Left_Value,1000.0);
            Lane_Values.set_value_sensor(Curb_Right_Value,1000.0);

         Output_function:= Lane_Detection.Lane_Detection_Process( Output_T,
                                              Lane_Values.get(Lane_Left_Value)  ,
                                              Lane_Values.get(Lane_Right_Value)   ,
                                              Lane_Values.get(Lane_Mid_Value)  ,
                                              Lane_Values.get(Curb_Left_Value) ,
                                              Lane_Values.get(Curb_Right_Value)  ,
                                              Lane_IR_Threshhold,
                                              Lane_UR_Threshhold);

          --------------------------------------------------------------------------------------- System Error


         end select;


         case Output_function is
            -------------------------
         when SYSTEM_ERROR_S =>

             if(Lane_Values.get (Lane_Mid_Value) > Lane_IR_Threshhold and
             Lane_Values.get (Lane_Right_Value) > Lane_IR_Threshhold and
                  Lane_Values.get (Lane_Left_Value) > Lane_IR_Threshhold )

             then
               Assert(Lane_Values.get (Curb_Left_Value) = 1000.0 and
                      Lane_Values.get (Curb_Right_Value) = 1000.0,
                     "Setting System_Error: Expected Curb_Values > 1000 and Lane_Values  > 250");
            end if;
            null;

         when GO_STRAIGHT_S =>


              Assert
                 (Lane_Values.get (Lane_Mid_Value) < Lane_IR_Threshhold,
                  "Go_Straight: Expected Lane_Mid_Value < 250");
              Assert
                 (Lane_Values.get (Lane_Left_Value) > Lane_IR_Threshhold,
                  "Go_Straight: Expected Lane_Left_Value > 250");
              Assert
                 (Lane_Values.get (Lane_Right_Value) > Lane_IR_Threshhold,
                  "Go_Straight: Expected Lane_Right_Value > 250");


               null;


         when GO_LEFT_S =>

          if(Lane_Values.get (Lane_Mid_Value) > Lane_IR_Threshhold and
             Lane_Values.get (Lane_Right_Value) > Lane_IR_Threshhold and
             Lane_Values.get (Lane_Left_Value) > Lane_IR_Threshhold )
          then
                Put_Line("Receiving Go Left_Curb");
              Assert
                 (Lane_Values.get (Curb_Right_Value) < Lane_UR_Threshhold,
                  "Go_Left : Expected Curb_Right_Value < 500");

          else
            Put_Line("Receiving Go Left_Infrared");
            Assert
                 (Lane_Values.get (Lane_Mid_Value) > Lane_IR_Threshhold,
                  "Go_Straight: Expected Lane_Mid_Value > 250");
              Assert
                 (Lane_Values.get (Lane_Left_Value) < Lane_IR_Threshhold,
                  "Go_Straight: Expected Lane_Left_Value < 250");
               Assert
                 (Lane_Values.get (Lane_Right_Value) > Lane_IR_Threshhold,
                  "Go_Straight: Expected Lane_Right_Value > 250");
            end if;

            null;



         when GO_RIGHT_S =>
           if(Lane_Values.get (Lane_Mid_Value) > Lane_IR_Threshhold and
               Lane_Values.get (Lane_Right_Value) > Lane_IR_Threshhold and
               Lane_Values.get (Lane_Left_Value) > Lane_IR_Threshhold )
           then
               Put_Line("Receiving Go Right_Curb");
              Assert
                 (Lane_Values.get (Curb_Left_Value) < Lane_UR_Threshhold,
                  "Go_Right : Expected Curb_Left_Value < 500");

            else
              Put_Line("Receiving Go Right_Infrared");
                Assert
                 (Lane_Values.get (Lane_Mid_Value) > Lane_IR_Threshhold,
                  "Go_Straight: Expected Lane_Mid_Value > 250");
              Assert
                 (Lane_Values.get (Lane_Left_Value) > Lane_IR_Threshhold,
                  "Go_Straight: Expected Lane_Left_Value > 250");
               Assert
                 (Lane_Values.get (Lane_Right_Value) < Lane_IR_Threshhold,
                  "Go_Straight: Expected Lane_Right_Value < 250");

            end if;

            null;

            when EMPTY_S =>
               null;
         end case;
         Put_Line ("-----------------------------------Finishing case " );


   end test_lane_detection;
end Lane_Detection_Test;
