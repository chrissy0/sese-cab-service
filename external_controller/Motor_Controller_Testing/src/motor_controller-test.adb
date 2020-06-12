pragma Ada_2012;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;

package body Motor_Controller.Test is

   type Motor_Values_Long_Float_Array_T is array (Motor_ID_T) of Long_Float;

   protected type Motor_Values_T is
      procedure set (ID : Motor_ID_T; Value : Long_Float);
      function get (ID : Motor_ID_T) return Long_Float;
   private
      Motor_Values_Array : Motor_Values_Long_Float_Array_T;
   end Motor_Values_T;

   protected body Motor_Values_T is
      procedure set (ID : Motor_ID_T; Value : Long_Float) is
      begin
         Motor_Values_Array (ID) := Value;
      end set;

      function get (ID : Motor_ID_T) return Long_Float is
      begin
         return Motor_Values_Array (ID);
      end get;
   end Motor_Values_T;

   type Motor_Values_Acces_T is access Motor_Values_T;
   Motor_Values : Motor_Values_T;

   procedure dummy_set (ID : Motor_ID_T; Value : Long_Float) is
   begin
      Motor_Values.set (ID, Value);
   end dummy_set;

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

   procedure test_lane_detection_processing (T : in out Test) is
      pragma Unreferenced (T);
      type Motor_Controller_Task_Array_T is
        array
          (Motor_Controller.Lane_Detection_Done_T) of Motor_Controller
          .Motor_Controller_Task_T;
      Motor_Controller_Task_Array : Motor_Controller_Task_Array_T;
      Motor_Straight_Speed        : constant := 5.0;
      Motor_Turn_Speed            : constant := 3.0;
   begin
      -- check all transitions from init
      for I in Motor_Controller.Lane_Detection_Done_T loop
         Put_Line ("Starting case " & I'Image);

         select
            delay 1.0;
            Put_Line ("time out!");
            Assert
              (False, "Motor_Controller timed out in lane_detection_done");
         then abort
            Motor_Controller_Task_Array (I).Construct
              (MC_State => NORMAL_DRIVING, ND_State => FRONT_CLEAR,
               FC_State => DRIVE, D_State => INIT, SE_State => STOP,
               MS_Speed => Motor_Straight_Speed, MT_Speed => Motor_Turn_Speed,
               set_motor_value_access => dummy_set'Access, LD_State => INIT);
            Put_Line (I'Image & " constructor done");
            Motor_Controller_Task_Array (I).lane_detection_done (I);
            Put_Line (I'Image & " lane_detection_done done");
            Motor_Controller_Task_Array (I).lane_detection_next;
            Put_Line (I'Image & " lane_detection_next done");

            -- ouput of motor controller is delayed by one iteration -> wait an
            -- additional iteration
            Motor_Controller_Task_Array (I).lane_detection_done (EMPTY_S);
            Put_Line (I'Image & " lane_detection_done done");
            Motor_Controller_Task_Array (I).lane_detection_next;
            Put_Line (I'Image & " lane_detection_next done");
         end select;
         Put_Line (I'Image & " signals sent");

         case I is
            when SYSTEM_ERROR_S =>
               for J in Motor_ID_T loop
                  Assert
                    (Motor_Values.get (J) = 0.0,
                     "Setting System_Error: Expected motor speed 0, got non 0");
               end loop;
               null;
            when GO_STRAIGHT_S =>
               for J in Motor_ID_T loop
                  Assert
                    (Motor_Values.get (J) = Motor_Straight_Speed,
                     "Setting System_Error: Expected motor speed MOTOR_STRAIGHT, got non MOTOR_STRAIGHT");
               end loop;
               null;
            when GO_LEFT_S =>
               Assert
                 (Motor_Values.get (MOTOR_FRONT_LEFT) = 0.0,
                  "Go_Left: Expected motor_front_left = 0, got non 0");
               Assert
                 (Motor_Values.get (MOTOR_BACK_LEFT) = 0.0,
                  "Go_Left: Expected motor_back_left = 0, got non 0");
               Assert
                 (Motor_Values.get (MOTOR_FRONT_RIGHT) = Motor_Turn_Speed,
                  "Go_Left: Expected MOTOR_FRONT_RIGHT = Motor_Turn_Speed, got non Motor_Turn_Speed");
               Assert
                 (Motor_Values.get (MOTOR_BACK_RIGHT) = Motor_Turn_Speed,
                  "Go_Left: Expected MOTOR_BACK_RIGHT = Motor_Turn_Speed, got non Motor_Turn_Speed");
               null;
            when GO_RIGHT_S =>
               Assert
                 (Motor_Values.get (MOTOR_FRONT_LEFT) = Motor_Turn_Speed,
                  "Go_Left: Expected motor_front_left = Motor_Turn_Speed, got non Motor_Turn_Speed");
               Assert
                 (Motor_Values.get (MOTOR_BACK_LEFT) = Motor_Turn_Speed,
                  "Go_Left: Expected motor_back_left = Motor_Turn_Speed, got non Motor_Turn_Speed");
               Assert
                 (Motor_Values.get (MOTOR_FRONT_RIGHT) = 0.0,
                  "Go_Left: Expected MOTOR_FRONT_RIGHT = 0, got non 0");
               Assert
                 (Motor_Values.get (MOTOR_BACK_RIGHT) = 0.0,
                  "Go_Left: Expected MOTOR_BACK_RIGHT = 0, got non 0");
               null;
            when EMPTY_S =>
               null;
         end case;
         Put_Line ("Finishing case " & I'Image);
      end loop;
      Put_Line ("Finishing test case");

      -- check that init reacts to all Init Test
   end test_lane_detection_processing;

end Motor_Controller.Test;
