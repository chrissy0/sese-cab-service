pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Motor_Controller is

   procedure front_clear_state is
   begin
      Put_Line ("Entering front_clear");
      while True loop
         null;
      end loop;

   end front_clear_state;

   procedure front_blocked_state is
   begin
      Put_Line ("Entering front_blocked");
   end front_blocked_state;

   ---------------------------
   -- Motor_Controller_Task --
   ---------------------------

   task body Motor_Controller_Task is
      Front_Clear   : Boolean := False;
      System_Error  : Boolean := False;
      no_new_signal : Boolean := False;
   begin

      accept Construct;
      while True loop
         -- Output Signals depending on State
         case System_Error is
            when True =>
               loop
                  null;

               end loop;

            when False =>
               case Front_Clear is

                  when True =>
                     Put_Line ("front_blocked_State");
                  when False =>
                     Put_Line ("front_clear State");
               end case;

         end case;

         -- look for all signals -> order not set Break when every task raised
         -- one signal or raised fin
         select
            accept System_Error_State_R do
               System_Error := True;
               Put_Line ("Switch to System_Error");
            end System_Error_State_R;
         or
            accept Change_Front_State_R do
               Front_Clear := not Front_Clear;
               Put_Line ("Switch to front_clear = " & Front_Clear'Image);
            end Change_Front_State_R;
         or
            accept main_done do
               Put_Line ("no new signal from main");
               null;
            end main_done;
         end select;

         Put_Line ("Signaling main to start next iteration");
         -- Signal all tasks to unless it is system_error
         if not System_Error then
            accept main_next do
               null;
            end main_next;
         end if;

      end loop;

   end Motor_Controller_Task;

end Motor_Controller;
