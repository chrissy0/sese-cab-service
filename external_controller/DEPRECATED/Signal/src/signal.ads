
package Signal is

   protected type Signal_T is
      -- Set signal to True
      procedure Set;

      -- Returns the value of the signal. Sets the Signal to False.
      procedure Consume(value : out Boolean);
   private
      is_set : Boolean := False;
   end Signal_T;

end Signal;
