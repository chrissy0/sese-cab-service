with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Asynchronous is
   task type T_T is
      entry ReadUserInput; -- Signal
   end T_T;

   task body T_T is
      Line : String (1 .. 1_000);
      Last : Natural;
   begin
      Get_Line (Line, Last);
      accept ReadUserInput do -- Sende ich das Signal
         null;
      end ReadUserInput;
   end T_T;

   t_1 : T_T;

begin
   --  Insert code here.
   select
      t_1.ReadUserInput; -- Warte ich auf das Signal
   then abort
      select
         delay 10.0;
         while True loop
            Put_Line ("Hallo Welt");
            delay 2.0;
      end loop;
      then abort
         while True loop
            Put_Line ("a");
            delay 1.0;
         end loop;
      end select;
   end select;
   -- hier beginnt final state
   Put_Line ("aborted");

   null;
end Test_Asynchronous;
