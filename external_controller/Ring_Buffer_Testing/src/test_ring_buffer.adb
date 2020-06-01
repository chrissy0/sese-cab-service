with AUnit.Reporter.XML;
with AUnit.Run;
with Ring_Buffer_Suite; use Ring_Buffer_Suite;

procedure Test_Ring_Buffer is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.XML.XML_Reporter;
begin
   Runner (Reporter);
end Test_Ring_Buffer;
