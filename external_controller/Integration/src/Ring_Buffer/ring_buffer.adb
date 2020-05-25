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
         if not isEmpty then
            tail := tail + 1;
            full := False;
         end if;

      end removeLast;

      ----------
      -- peek --
      ----------

      entry peek_blocking (elem : out Element) when not isEmpty is
      begin
         elem := buffer (tail);
      end peek_blocking;

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

      -----------------------
      -- peek_non_blocking --
      -----------------------

      function peek_non_blocking (value_on_empty : in Element) return Element
      is
      begin
         if isEmpty then
            return value_on_empty;
         else
            return buffer (tail);
         end if;
      end peek_non_blocking;

      ------------------
      -- get_elements --
      ------------------

      function get_elements return Element_Vector.Vector is
         vec              : Element_Vector.Vector;
         head_tail_offset : Index;
      begin
         head_tail_offset := head - tail;
         if isFull then

            for I in Index loop
               vec.Append (New_Item => get (I + tail));
            end loop;

         elsif not isEmpty then

            -- check
            if head <= tail then
               for I in tail .. Index'Last loop
                  vec.Append (New_Item => get (I));
               end loop;

               for I in 0 .. (head - 1) loop
                  vec.Append (New_Item => get (I));
               end loop;
            else
               for I in tail .. (head - 1) loop
                  vec.Append (New_Item => get (I));
               end loop;
            end if;

         end if;

         return vec;
      end get_elements;

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
