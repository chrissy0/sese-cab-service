with AUnit.Reporter.XML;
with AUnit.Run;
with Lane_Detection_Suite; use Lane_Detection_Suite;
procedure Lane_Detection_Testing is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.XML.XML_Reporter;
begin
   Runner (Reporter);
end Lane_Detection_Testing;
