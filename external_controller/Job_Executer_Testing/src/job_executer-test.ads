pragma Ada_2012;
with AUnit.Assertions;        use AUnit.Assertions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;             use Ada.Text_IO;
with AUnit.Test_Fixtures;
   
package Job_Executer.Test is
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;
   procedure test_test (T : in out Test);    
end Job_Executer.Test;
