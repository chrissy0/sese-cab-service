with AUnit.Reporter.XML;
with AUnit.Run;
with Roadmarker_Suite; use Roadmarker_Suite;

procedure Test_Roadmarker is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.XML.XML_Reporter;
   begin
   Runner (Reporter);
end Test_Roadmarker;
