with Roadmarker_Test; use Roadmarker_Test;
with Roadmarker_Functions.Test; use Roadmarker_Functions.Test;
with AUnit.Test_Caller;

package body Roadmarker_Suite is

   package Caller is new AUnit.Test_Caller
     (Test_Fixture => Roadmarker_Functions.Test.Test);


   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      --  Ret.Add_Test
      --    (Caller.Create
      --       ("-------------Test Roadmarker output-------------",
      --        Roadmarker_Test.test_roadmarker'Access));
      Ret.Add_Test
        (Caller.Create
           ("-------------Test Roadmarke Function's get_roadmarker-------------",
            Roadmarker_Functions.Test.test_roadmarker_functions'Access));
      Ret.Add_Test
        (Caller.Create
           ("-------------Test Roadmarke Function's on_roadmarker-------------",
            Roadmarker_Functions.Test.test_on_road_marker'Access));


      return Ret;
   end Suite;

end Roadmarker_Suite;
