with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;      use Ada.Text_IO;
with Ring_Buffer;

package body Ring_Buffer_Test is

   procedure printInt (a : Integer) is
   begin
      Put (a'Image);
   end printInt;

   type RingIndex is mod 2**4;

   package Ring is new Ring_Buffer (Integer, RingIndex, printInt);
   type RingBuffer_Access is access Ring.RingBuffer;

   procedure Test_Push_Get (T : in out Test) is
      pragma Unreferenced (T);
      r            : RingBuffer_Access := new Ring.RingBuffer;
      index        : RingIndex         := 0;
      value        : Integer           := 0;
      value_nested : Integer           := 0;
      value_tmp    : Integer           := 0;
      vec          : Ring.Element_Vector.Vector;
   begin
      r.reset;

      Assert (r.isEmpty, "After reset: Ring is not empty, expected empty");
      Assert (not r.isFull, "After reset: Ring is full, expected not full");
      Assert
        (r.getHead = 0,
         "After reset: Head points to " & r.getHead'Image & ", expected 0");
      Assert
        (r.getTail = 0,
         "After reset: Tail points to " & r.getTail'Image & ", expected 0");

      -- fill up ring buffer
      for I in RingIndex loop
         r.push (value);
         r.peek_blocking (value_nested);
         Assert
           (value_nested = 0,
            "First Round Pushes: Push " & I'Image & ": peek returned " &
            value_nested'Image & ", expected 0");

         if I = 15 then
            Assert
              (r.isFull,
               "First Round Pushes: Push " & I'Image &
               ": Ring is not full, expected full");
         else
            Assert
              (not r.isFull,
               "First Round Pushes: Push " & I'Image &
               ": Ring is full, expected not full");
         end if;

         Assert
           (not r.isEmpty,
            "First Round Pushes: Push " & I'Image &
            ": Ring is empty, expected not empty");

         Assert
           (r.getHead = I + 1,
            "First Round Pushes: Push " & I'Image & ": Head points to " &
            r.getHead'Image & ", expected " & I'Image & " + 1");
         Assert
           (r.getTail = 0,
            "First Round Pushes: Push " & I'Image & ": Tail points to " &
            r.getTail'Image & ", expected 0");
         value_nested := 0;
         for J in 0 .. I loop

            Assert
              (r.get (J) = value_nested,
               "First Round Pushes: Push " & I'Image &
               ": Incorrect value at " & J'Image & ": Got " & r.get (J)'Image &
               ", expected " & value_nested'Image);
            value_nested := value_nested + 1;
         end loop;
         value := value + 1;
         index := index + 1;
      end loop;

      -- override values in ring buffer
      for I in RingIndex loop
         r.push (value);
         r.peek_blocking (value_nested);
         Assert
           (value_nested = 17 - value,
            "Second Round Pushes: Push " & I'Image & ": peek returned " &
            value_nested'Image & ", expected (" & value'Image & " - 17)");

         Assert
           (not r.isEmpty,
            "Second Round Pushes: Push " & I'Image &
            ": Ring is empty, expected not empty");

         Assert
           (r.isFull,
            "Second Round Pushes: Push " & I'Image &
            ": Ring is not full, expected full");
         Assert
           (r.getHead = index + 1,
            "Second Round Pushes: Push " & I'Image & ": Head points to " &
            r.getHead'Image & ", expected " & index'Image & " + 1");
         Assert
           (r.getTail = index + 1,
            "Second Round Pushes: Push " & I'Image & ": Tail points to " &
            r.getTail'Image & ", expected " & index'Image & " + 1");

         -- set up value nested to max:
         value_tmp    := 0;
         value_nested := 16;
         for J in RingIndex loop
            if J <= I then
               Assert
                 (r.get (J) = value_nested,
                  "Second Round Pushes: Push " & I'Image &
                  ": Incorrect value at " & J'Image & ": Got " &
                  r.get (J)'Image & ", expected " & value_nested'Image);
            else
               Assert
                 (r.get (J) = value_tmp,
                  "Second Round Pushes: Push " & I'Image &
                  ": Incorrect value at " & J'Image & ": Got " &
                  r.get (J)'Image & ", expected " & value_tmp'Image);
            end if;
            value_nested := value_nested - 1;
            value_tmp    := value_tmp + 1;
         end loop;
         value := value - 1;
         index := index + 1;
      end loop;

      -- empty ring buffer
      value := 15;
      for I in RingIndex loop
         r.removeLast;
         if (value /= 0) then
            r.peek_blocking (value_nested);
            Assert
              (value_nested = value,
               "Third Round: removeLast " & I'Image & ": peek returned " &
               value_nested'Image & ", expected (" & value'Image & " - 17)");
            Assert
              (not r.isEmpty,
               "Third Round: removeLast " & I'Image &
               ": Ring is empty, expected not empty");
         else
            Assert
              (r.isEmpty,
               "Third Round: removeLast " & I'Image &
               ": Ring is empty, expected not empty");
         end if;

         Assert
           (not r.isFull,
            "Third Round: removeLast " & I'Image &
            ": Ring is full, expected not full");
         Assert
           (r.getHead = 0,
            "Third Round: removeLast " & I'Image & ": Head points to " &
            r.getHead'Image & ", expected 0");
         Assert
           (r.getTail = index + 1,
            "Third Round: removeLast " & I'Image & ": Tail points to " &
            r.getTail'Image & ", expected " & index'Image & " + 1");

         -- set up value nested to max:
         value_tmp    := 0;
         value_nested := value;
         if (value /= 0) then
            for J in index + 1 .. RingIndex'Last loop
               Assert
                 (r.get (J) = value_nested,
                  "Third Round: removeLast " & I'Image &
                  ": Incorrect value at " & J'Image & ": Got " &
                  r.get (J)'Image & ", expected " & value_tmp'Image);
               value_nested := value_nested - 1;
            end loop;
         end if;
         value := value - 1;
         index := index + 1;
      end loop;

      --Assert (False, "This should fail!");
   end Test_Push_Get;

   type Element_Array is array (Natural) of Integer;
   procedure Test_Get_Elements (T : in out Test) is
      pragma Unreferenced (T);
      r          : RingBuffer_Access := new Ring.RingBuffer;
      vec_output : Ring.Element_Vector.Vector;
      tmp        : Integer;
   begin
      r.reset;
      for I in 0 .. 100 loop
         r.push (I);
         vec_output := r.get_elements;

         tmp := I;

         for J in reverse vec_output.First_Index .. vec_output.Last_Index loop
            Assert
              (vec_output.Element (Index => J) = tmp,
               "Push " & I'Image & ": vector returned by get_elements at " &
               J'Image & ": got " & vec_output.Element (Index => J)'Image &
               ", expected " & tmp'Image);
            tmp := tmp - 1;
         end loop;
      end loop;

      for I in 0 .. 15 loop
         r.removeLast;
         tmp        := 100;
         vec_output := r.get_elements;
--           for J in vec_output.First_Index .. vec_output.Last_Index loop
--              Put("(" & J'Image & ": " & vec_output.Element(J)'Image & ") ");
--           end loop;
--           New_Line;

         Assert
           ((vec_output.Last_Index + 1) = 15 - I,
            "removeLast " & I'Image &
            ": vector returned by get_elements: size is (" &
            vec_output.Last_Index'Image & " - 1), expected (15 - " & I'Image &
            ")");

         for J in reverse vec_output.First_Index .. vec_output.Last_Index loop
            Assert
              (vec_output.Element (Index => J) = tmp,
               "removeLast " & I'Image &
               ": vector returned by get_elements at " & J'Image & ": got " &
               vec_output.Element (Index => J)'Image & ", expected " &
               tmp'Image);

            tmp := tmp - 1;
         end loop;
      end loop;

   end Test_Get_Elements;

end Ring_Buffer_Test;
