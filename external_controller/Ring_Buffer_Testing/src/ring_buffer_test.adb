with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Ring_Buffer;

package body Ring_Buffer_Test is
   
   procedure printInt (a : Integer) is
   begin
      Put(a'Image);
   end printInt;
   
   type RingIndex is mod 2 ** 4;
   
   package Ring is new Ring_Buffer(Integer, RingIndex, printInt);
   type RingBuffer_Access is access Ring.RingBuffer;

   procedure Test_Push (T : in out Test) is
      pragma Unreferenced (T);
      r     : RingBuffer_Access := new Ring.RingBuffer;
      index : RingIndex := 0;
      value : Integer   := 0;
      value_nested : Integer := 0;
      value_tmp : Integer := 0;
   begin
      r.reset;
      
      -- fill up ring buffer
      for I in RingIndex loop
         Put_Line("Push #" & I'Image);
         r.push(value);
         
         if I = 15 then
            Assert (r.isFull, "Ring is not full, expected full");
         else
            Assert (not r.isFull, "Ring is full, expected not full");
         end if;
         
         Assert (r.getHead = I + 1, "Head wrong!");
         Assert (r.getTail = 0, "Tail wrong! Expected 0!");
         value_nested := 0;
         for J in 0 .. I loop
         
            Assert (r.get(J) = value_nested,
                    "Incorrect value in Ring_Buffer after first pushes");
            value_nested := value_nested + 1;
         end loop;
         value := value + 1;
         index := index + 1;
      end loop;
      
      for I in RingIndex loop
         Put_Line(value'Image);
         r.push(value);
         
         Assert (r.isFull, "Ring is not full, expected full");
         Assert (r.getHead = index + 1, "Head wrong!");
         Assert (r.getTail = index + 1, "Tail wrong! Expected 0!");
         
         -- set up value nested to max:
         value_tmp := 0;
         value_nested := 16;
         for J in RingIndex loop
            if J <= I then
               Assert (r.get (J) = value_nested, "Wrong value in Ring Buffer");
            else
               Assert (r.get (J) = value_tmp, "Wrong value in Ring Buffer");
            end if;
            value_nested := value_nested - 1;
            value_tmp := value_tmp + 1;
         end loop;
         value := value - 1;
         index := index + 1;
      end loop;
      --Assert (False, "This should fail!");
      
   end Test_Push;
end Ring_Buffer_Test;
