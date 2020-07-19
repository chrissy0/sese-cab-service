
with AWS.Config.Set;
with AWS.Server;

with Job_Executer_Testing.Callbacks;

with AUnit.Reporter.XML;
with AUnit.Run;
with Job_Executer_Suite; use Job_Executer_Suite;
with ec2b; use ec2b;
with AWS.Client; use AWS.Client;
with Job_Executer_Suite;

procedure Job_Executer_Testing.Main is
   use AWS;
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.XML.XML_Reporter;
   Web_Server : Server.HTTP;
   Web_Config : Config.Object;
begin
   --  Setup

   Config.Set.Server_Host (Web_Config, Host);
   Config.Set.Server_Port (Web_Config, Port);
   ec2b.connection := Job_Executer_Suite.Client_con'Access;
   --  Start Mockup server

   Server.Start (Web_Server, Callbacks.Default'Access, Web_Config);
   Runner (Reporter);
   --  Stop Mockup server

   Server.Shutdown (Web_Server);
end Job_Executer_Testing.Main;
