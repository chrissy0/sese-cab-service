

with AUnit.Reporter.XML;
with AUnit.Run;
with Job_Executer_Suite; use Job_Executer_Suite;
procedure Job_Executer_Testing is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.XML.XML_Reporter;
begin
   Runner (Reporter);
end Job_Executer_Testing;
