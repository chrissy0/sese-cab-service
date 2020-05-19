generic
   type Element is private;                 -- one has to specify these
   with procedure printElement(E: Element); -- when using this package
package RB is
   type RingIndex is mod 2**3;              -- unfortunatly, the mod range
                                            -- has to be static, thus
                                            -- we cannot make this generic :(
   type Memory is array (RingIndex) of Element;
   -- no Size attribute, because only type RingIndex is usable and
   -- Size needs to be from 0 to RingIndex'Last + 1. Since RingIndex is modular
   -- type, RingIndex'Last + 1 = 0 (Unsigned Integer Overflow). The unsigned
   -- integer overflow is usefull for RingBuffer



   protected type RingBuffer is
      procedure push (data : in Element);

      function get (idx  : in RingIndex) return Element;

      procedure reset;

      procedure removeLast;

      entry peek (elem : out Element);

      function getHead return RingIndex;

      function getTail return RingIndex;

      function isEmpty return Boolean;

      function isFull return Boolean;

      procedure print;

   private
      head   : RingIndex := 0;
      tail   : RingIndex := 0;
      buffer : Memory;
      full   : Boolean   := False;
   end RingBuffer;

   --function createRingBuffer return RingBuffer;
end RB;
