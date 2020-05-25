with Ada.Containers.Vectors;

generic
   type Element is private;                 -- one has to specify these
   type Index is mod <>;
   with procedure printElement (E : Element); -- when using this package
package Ring_Buffer is

   -- Size provided by Index (parameter of generic)
   type Memory is array (Index) of Element;

   package Element_Vector is new Ada.Containers.Vectors (Index_Type => Natural,
      Element_Type => Element);

   protected type RingBuffer is

      -- data: data to be pushed in RB
      -- Note: when full, data may override existing elements in RB
      procedure push (data : in Element);

      -- idx: index of RB element to be retrived
      -- Note: Behavior undefined when idx ahead of head and tail
      function get (idx : in Index) return Element;

      -- empties the RB
      procedure reset;

      -- removes last element of RB. If RB empty, this does nothing.
      procedure removeLast;

      -- elem: returned element
      -- Peek blocks until there is a element in RB
      entry peek_blocking (elem : out Element);

      -- elem: returned element
      -- Peek blocks until there is a element in RB
      function peek_non_blocking (value_on_empty : in Element) return Element;

      function getHead return Index;

      function getTail return Index;

      function isEmpty return Boolean;

      function isFull return Boolean;

      -- Disclaimer: This function is untested! Tests will follow soon (tm)
      -- Returns vector containing all valid elements in RB starting from tail
      -- to (head - 1).
      function get_elements return Element_Vector.Vector;

      procedure print;

   private
      head   : Index   := 0;
      tail   : Index   := 0;
      buffer : Memory;
      full   : Boolean := False;
   end RingBuffer;

   --function createRingBuffer return RingBuffer;
end Ring_Buffer;
