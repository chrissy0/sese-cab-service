
with AWS.Response;
with AWS.Status;

package Job_Executer_Testing.Callbacks is

   use AWS;
   type error_t is (E_Success, E_Failure, E_Timeout, E_incomplete_JSON, E_wrong_JSON);
   action : error_t := E_Success;
   completed : Boolean := False;
   function Default (Request : in Status.Data) return Response.Data;

end Job_Executer_Testing.Callbacks;
