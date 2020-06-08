pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Motor_Controller is

   procedure front_clear_state is
   begin
      Put_Line("Entering front_clear");
      while True loop
         null;
      end loop;

   end front_clear_state;

   procedure front_blocked_state is
   begin
      Put_Line("Entering front_blocked");
   end front_blocked_state;


   ---------------------------
   -- Motor_Controller_Task --
   ---------------------------

   task body Motor_Controller_Task is
      is_System_Error_Access : System_Error_Access := new System_Error.Synchronized_Data_T;
      is_System_Error_New_Value : Boolean;
      is_Front_Blocked_Access : Front_Blocked_Access := new Front_Blocked.Synchronized_Data_T;
      is_Front_Blocked_New_Value : Boolean;


      procedure no_system_error_state is
      begin
         loop
            select
               is_Front_Blocked_Access.wait_Change(is_Front_Blocked_New_Value);
               Put_Line("Got new value in motor thread for is_front_blocked");
            then abort
               Put_Line("Entering new front state...");
               case is_Front_Blocked_Access.peek is
                  when False => Put_Line("Entering front_clear");
                  when True => Put_Line("Entering front_blocked");
               end case;
               Put_Line("is_System_Error_Access.is_change_value: " & Boolean'Image(is_System_Error_Access.get_is_changed_value));
               Put_Line("is_System_Error_value: " &  Boolean'Image(is_System_Error_Access.peek));
               Put_Line("is_Front_Blocked_Access.is_change_value: " & Boolean'Image(is_Front_Blocked_Access.get_is_changed_value));
               Put_Line("is_Fron_Blocked_value: " &  Boolean'Image(is_Front_Blocked_Access.peek));
               loop
                  delay 10.0;
               end loop;
            end select;
      end loop;
      end no_system_error_state;

   begin
      is_System_Error_Access.init(False);
      is_Front_Blocked_Access.init(False);
      accept Construct (is_System_Error_Access_Copy : out System_Error_Access;
                     is_Front_Blocked_Access_Copy : out Front_Blocked_Access) do
         is_System_Error_Access_Copy := is_System_Error_Access;
         is_Front_Blocked_Access_Copy := is_Front_Blocked_Access;
      end Construct;

      select
         is_System_Error_Access.wait_Change(is_System_Error_New_Value);
         Put_Line("Got new value in motor thread for system_error");
         Put_Line("Entering system_error");
      then abort
         Put_Line("Entering no_system_error");
         no_system_error_state;
      end select;
      Put_Line("Entering system_error");
   end Motor_Controller_Task;

end Motor_Controller;
