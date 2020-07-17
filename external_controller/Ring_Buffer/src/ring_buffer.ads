-- @summary
-- Generic ring buffer package specification.
--
-- @author Julian Hartmer
-- @description
-- This package is a generic ring buffer. You can set the element type
-- when including this package.

with Ada.Containers.Vectors;

generic
   type Element is private;                 -- one has to specify these
   type Index is mod <>;

   -- procedure to print one ring buffer element
   -- @param E ring buffer element
   with procedure printElement (E : Element); -- when using this package
package Ring_Buffer is

   -- Size provided by Index (parameter of generic)
   type Memory is array (Index) of Element;

   package Element_Vector is new Ada.Containers.Vectors (Index_Type => Natural,
      Element_Type => Element);

   protected type RingBuffer is
      -- push data to ring buffer
      -- Note: when full, data may override existing elements in RB
      -- @param data data to be pushed in RB
      procedure push (data : in Element);

      -- get element from ring buffer by index.
      -- Note: Behavior undefined when idx ahead of head and tail
      -- @param idx index of RB element to be retrived
      function get (idx : in Index) return Element;

      -- empties the RB
      -- sets head and tail to default values
      procedure reset;

      -- removes last element of RB. If RB empty, this does nothing.
      procedure removeLast;

      -- Get RB element at tail. Blocks when RB empty
      -- Peek blocks until there is a element in RB
      -- @param elem element at tail
      entry peek_blocking (elem : out Element);

      -- Get RB element at tail.
      -- @param value_on_empty return value if RB empty
      -- @return value_on_empty if RB empty, else last RB element
      function peek_non_blocking (value_on_empty : in Element) return Element;

      -- get index of RB head element
      -- @return index of RB head element
      function getHead return Index;

      -- get index of RB tail element
      -- @return index of RB tail element
      function getTail return Index;

      -- Checks if RB is empty
      -- @return true: empty, false: not empty
      function isEmpty return Boolean;

      -- Checks if RB is full
      -- @return true: full, false: not full
      function isFull return Boolean;

      -- Returns vector containing all valid elements in RB starting from tail
      -- to (head - 1).
      -- @return vector of RB elements
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
