generic
   type Data_T is private;
package Synchronized_Data is
   protected type Synchronized_Data_T is

      entry wait_Change(value : out Data_T);

      procedure set (value : in Data_T);

      procedure init (value : in Data_T);

      function peek return Data_T;

      function get_is_changed_value return Boolean;

   private
      data : Data_T;
      is_changed : Boolean;
   end Synchronized_Data_T;
end Synchronized_Data;
