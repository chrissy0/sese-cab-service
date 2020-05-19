-- NOTE: ADA is not case sensitive!!!!!!!!!!!!
--       That did cost me quite a lot of time....

with Ada.Text_IO; use Ada.Text_IO;
with RB;

procedure main is

   procedure printInt (a : Integer) is
   begin
      Put(a'Image);
   end printInt;


   package Ring is new RB(Integer, printInt);
   type RingBuffer_Access is access Ring.RingBuffer;

   Task Type Reader (q : RingBuffer_Access); -- Thread type
   task body Reader is
      tmp : Integer;
   begin
      while True loop
         q.peek(tmp);
         Put_Line("Reader: popped "&tmp'Image);
      Put_Line("-----------------------");
         q.print;
         q.removeLast;
         delay 2.0;
      end loop;
   end Reader;

   type Reader_access is access Reader;

   a : RingBuffer_Access;
   r : Reader_access;
begin
   a := new Ring.RingBuffer;  -- allocate RingBuffer
   r := new Reader(a);        -- allocate and start thread
   for I in 0..20 loop
      delay 1.0;
      a.push(I);
   end loop;
end main;
