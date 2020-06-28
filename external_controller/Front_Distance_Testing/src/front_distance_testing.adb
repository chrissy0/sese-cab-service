with AUnit.Reporter.XML;
with AUnit.Run;
with Front_Distance_Suite; use Front_Distance_Suite;

procedure Front_Distance_Testing is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.XML.XML_Reporter;
begin
   Runner (Reporter);
end Front_Distance_Testing;
