
pragma Ada_2012;
with AUnit.Test_Caller;
with Job_Executer.Test; use Job_Executer.Test;
package body Job_Executer_Suite is

   package Caller is new AUnit.Test_Caller
     (Test_Fixture => Job_Executer.Test.Test);

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create("test_test", Job_Executer.Test.test_test'Access));
      return Ret;
   end Suite; 

end Job_Executer_Suite;
