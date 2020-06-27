with Roadmarker_Test; use Roadmarker_Test;
with AUnit.Test_Caller;

package body Roadmarker_Suite is

   package Caller is new AUnit.Test_Caller (Roadmarker_Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create
           ("-------------Test Roadmarker output-------------",
            Roadmarker_Test.test_roadmarker'Access));
      return Ret;
   end Suite;

end Roadmarker_Suite;
