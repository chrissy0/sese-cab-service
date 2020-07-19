
with AWS.Messages;
with AWS.MIME;
with Ada.Text_IO; use Ada.Text_IO;

package body Job_Executer_Testing.Callbacks is

   -------------
   -- Default --
   -------------

   function Default (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = "/" then
         return Response.Build
           (MIME.Text_HTML, "<p>Hello World!");

      elsif URI = "/api/ec/registerCab" then
         case(action) is
         when E_Success =>
               return Response.Build
              (MIME.Text_HTML, "{""id"": 1337}");
         when E_Failure =>
            return Response.Acknowledge(Messages.S409);
         when E_Timeout =>
            delay 1.0;
            return Response.Acknowledge(Messages.S408);
         when E_incomplete_JSON =>
            return Response.Build
              (MIME.Text_HTML, "{""id"": 1337");
         when E_wrong_JSON =>
            return Response.Build
              (MIME.Text_HTML, "{""WRONG"": 1337");
         end case;
      elsif URI = "/api/ec/cabLocation" then
         if (action = E_Success) then
            return Response.Acknowledge(Messages.S200);
         else
            return Response.Acknowledge(Messages.S409);
         end if;
      else
         return Response.Acknowledge (Messages.S404);
      end if;
   end Default;

end Job_Executer_Testing.Callbacks;
