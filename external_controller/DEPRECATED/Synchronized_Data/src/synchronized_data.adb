pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Synchronized_Data is

   -------------------------
   -- Synchronized_Data_T --
   -------------------------

   protected body Synchronized_Data_T is

      -----------------
      -- wait_Change --
      -----------------

      entry wait_Change (value : out Data_T) when is_changed is
      begin
         value := data;
         Put_Line("consuming is_changed");
         is_changed := False;
      end wait_Change;

      ---------
      -- set --
      ---------

      procedure set (value : in Data_T) is
      begin
         if value /= data then
            Put_Line("a");
            data := value;
            is_changed := True;
         else
            Put_Line("b");
         end if;


      end set;

      ----------
      -- init --
      ----------

      procedure init (value : in Data_T) is
      begin
         data := value;
         is_changed := False;

      end init;

      function get_is_changed_value return Boolean is
      begin
         return is_changed;
      end get_is_changed_value;


      ----------
      -- peek --
      ----------

      function peek return Data_T is
         begin
            return data;
      end peek;

   end Synchronized_Data_T;

end Synchronized_Data;
