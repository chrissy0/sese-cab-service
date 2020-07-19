with AUnit.Test_Suites; use AUnit.Test_Suites;
with AWS.Client; use AWS.Client;
package Job_Executer_Suite is

      function Suite return Access_Test_Suite;
   Client_con : aliased HTTP_Connection := Create("http://127.0.0.1:18081", Timeouts =>  Timeouts(Each => 0.1 ));
end Job_Executer_Suite;
