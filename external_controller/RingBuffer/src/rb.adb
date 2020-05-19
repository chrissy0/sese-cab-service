with Ada.Text_IO; use Ada.Text_IO;
package body RB is
   -- interesting read: https://groups.google.com/forum/#!topic/comp.lang.ada/-0Xkjo3d6r0

   protected body RingBuffer is      -- protected: blocks every call of it's
                                     -- methods until there isnt an active
                                     -- call going on. According to google link:
                                     -- Protected functions cannot access the
                                     -- protected objecs while it is being
                                     -- accessed by a protected procedure.
                                     -- Protected procedues and protected
                                     -- entries may not access the protected
                                     -- object while it is being accessed
                                     -- by a protected function.

      procedure push                 -- procedure: no return value!
                                     -- protected procedure: may change
                                     -- object's state
        (data : in Element) is
      begin

         buffer(head) := data;
         if full then
            tail := tail + 1;
         end if;
         head := head + 1;
         full := (head = tail);

      end push;

      function get
        (idx  : in RingIndex) return Element is
      begin
         return buffer(idx);
      end get;

      procedure removeLast is        -- protected functions shouldnt change the
                                     -- object's state -> no pop, instead we
                                     -- have "removeLast" and "peek"
      begin
         tail := tail + 1;
         full := False;
      end;

      entry peek(elem : out Element) -- entry: Can have waiting condition
      when not isEmpty is            -- thread waits for this condition when called
      begin
         elem := buffer(tail);
      end;

      procedure reset is
      begin
         head := 0;
         tail := 0;
         full := False;
      end;

      function getHead return RingIndex is
      begin
         return head;
      end;

      function getTail return RingIndex is
      begin
         return tail;
      end;

      function isEmpty return Boolean is
      begin
         return (not full) and (head = tail);
      end;


      function isFull return Boolean is
      begin
         return full;
      end;

      procedure print is
      begin
         for I in RingIndex loop
            Put(I'Image & ": ");
            printElement(buffer(I));
            if (head = I) then
               Put(" <-- h");
            end if;
            if tail = I then
               Put(" <-- t");
            end if;
            New_Line;
         end loop;

      end print;
   end RingBuffer;



end RB;
