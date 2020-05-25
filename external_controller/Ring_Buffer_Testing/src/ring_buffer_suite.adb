--
--  Copyright (C) 2008, AdaCore
--
with Ring_Buffer_Test;         use Ring_Buffer_Test;
with AUnit.Test_Caller;

package body Ring_Buffer_Suite is

   package Caller is new AUnit.Test_Caller (Ring_Buffer_Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create ("Test Ring Buffer", Test_Push_Get'Access));
      return Ret;
   end Suite;

end Ring_Buffer_Suite;
