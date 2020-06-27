
package body Signal is

   --------------
   -- Signal_T --
   --------------

   protected body Signal_T is

      ---------
      -- Set --
      ---------

      procedure Set is
      begin
         is_set := True;
      end Set;

      -------------
      -- Consume --
      -------------

      procedure Consume (value : out Boolean) is
      begin
         value := is_set;
         is_set := False;
      end Consume;

   end Signal_T;

end Signal;
