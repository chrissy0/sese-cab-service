pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Ring_Buffer is

   ----------------
   -- RingBuffer --
   ----------------

   protected body RingBuffer is

      ----------
      -- push --
      ----------

      procedure push (data : in Element) is
      begin
         buffer (head) := data;
         if full then
            tail := tail + 1;
         end if;
         head := head + 1;
         full := (head = tail);
      end push;

      ---------
      -- get --
      ---------

      function get (idx : in Index) return Element is
      begin
         return buffer (idx);
      end get;

      -----------
      -- reset --
      -----------

      procedure reset is
      begin
         head := 0;
         tail := 0;
         full := False;
      end reset;

      ----------------
      -- removeLast --
      ----------------

      procedure removeLast is
      begin
         tail := tail + 1;
         full := False;
      end removeLast;

      ----------
      -- peek --
      ----------

      entry peek (elem : out Element)
        when not isEmpty is
      begin
         elem := buffer (tail);
      end peek;

      -------------
      -- getHead --
      -------------

      function getHead return Index is
      begin
         return head;
      end getHead;

      -------------
      -- getTail --
      -------------

      function getTail return Index is
      begin
         return tail;
      end getTail;

      -------------
      -- isEmpty --
      -------------

      function isEmpty return Boolean is
      begin
         return (not full) and (head = tail);
      end isEmpty;

      ------------
      -- isFull --
      ------------

      function isFull return Boolean is
      begin
         return full;
      end isFull;

      -----------
      -- print --
      -----------

      procedure print is
      begin
         for I in Index loop
            Put (I'Image & ": ");
            printElement (buffer (I));
            if (head = I) then
               Put (" <-- h");
            end if;
            if tail = I then
               Put (" <-- t");
            end if;
            New_Line;
         end loop;
      end print;

   end RingBuffer;

end Ring_Buffer;
