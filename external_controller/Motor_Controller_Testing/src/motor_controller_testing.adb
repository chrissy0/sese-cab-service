-- @summary
-- Motor Controller test runner.
--
-- @author Julian Hartmer

with AUnit.Reporter.XML;
with AUnit.Run;
with Motor_Controller_Suite; use Motor_Controller_Suite;

procedure Motor_Controller_Testing is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.XML.XML_Reporter;
begin
   Runner (Reporter);
end Motor_Controller_Testing;
