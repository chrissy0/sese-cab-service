
with AWS.Messages;
with AWS.MIME;
with Ada.Text_IO; use Ada.Text_IO;
with AWS.Status; use AWS.Status;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Job_Executer; use Job_Executer;
with GNATCOLL.JSON; use GNATCOLL.JSON;
package body Job_Executer_Testing.Callbacks is

   -------------
   -- Default --
   -------------

   function Default (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
      version : Integer := -1;
      id : Integer := -1;
      cab_name :  Ada.Strings.Unbounded.Unbounded_String;
      section : Integer := -1;
      data : Ada.Strings.Unbounded.Unbounded_String;
      data_JSON :  JSON_Value;
      customer_id : Integer := -1;
   begin
      if URI = "/" then
         return Response.Build
           (MIME.Text_HTML, "<p>Hello World!");

      elsif URI = "/api/ec/registerCab" then
         case(action) is
         when E_Success =>
            data := Binary_Data(Request);
            data_JSON :=   Read(data);
            section := GNATCOLL.JSON.Get(data_JSON, "section");
            cab_name := GNATCOLL.JSON.Get(data_JSON, "cabName");

            if (section not in Road_Marker_ID_T) then
               return Response.Acknowledge(Messages.S409);
            end if;
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
            id := Integer'Value(Parameter(Request, "cabId"));
            if (id < 0) then
               return Response.Acknowledge(Messages.S409);
            end if;
            data := Binary_Data(Request);
            data_JSON :=   Read(data);
            section := GNATCOLL.JSON.Get(data_JSON, "section");
            if (section not in Road_Marker_ID_T) then
               return Response.Acknowledge(Messages.S409);
            end if;

            return Response.Acknowledge(Messages.S200);
         else
            return Response.Acknowledge(Messages.S409);
         end if;
      elsif URI = "/api/ec/requestRoute" then
         case (action) is
         when E_Success =>
            version := Integer'Value(Parameter(Request, "version"));
            id := Integer'Value(Parameter(Request, "id"));
            if (id < 0) then
               return Response.Acknowledge(Messages.S409);
            end if;
            if (version < 1) then
               return Response.Build
                 (MIME.Text_HTML, "{""version"": 1,""route"":" &
                    "[{""action"": ""turn"",""direction"": ""right"", ""marker"": 1}, " &
                    "{""action"": ""pickup"", ""customerId"": 0, ""marker"": 2}," &
                    "{""action"": ""pickup"", ""customerId"": 1, ""marker"": 2}," &
                    "{""action"" : ""turn"", ""direction"": ""right"", ""marker"": 4}," &
                    "{""action"": ""dropoff"",""customerId"": 0, ""marker"": 5}," &
                    "{""action"": ""pickup"", ""customerId"": 2, ""marker"": 5}," &
                    "{""action"": ""turn"", ""direction"": ""right"", ""marker"": 7}, " &
                    "{""action"": ""dropoff"", ""customerId"": 1, ""marker"": 8}," &
                    "{""action"": ""dropoff"", ""customerId"": 2, ""marker"": 8}," &
                    "{""action"": ""turn"", ""direction"": ""left"", ""marker"": 10 }," &
                    "{""action"": ""turn"", ""direction"": ""left"", ""marker"": 12 }," &
                    "{""action"": ""turn"", ""direction"": ""right"", ""marker"": 14 }," &
                    "{""action"": ""wait"", ""marker"": 0} ]}");
            else
                return Response.Build
                 (MIME.Text_HTML, "{""version"": 1}");
            end if;
         when E_Failure =>
            return Response.Acknowledge(Messages.S409);
         when E_Timeout =>
            delay 1.0;
            return Response.Acknowledge(Messages.S408);
         when E_incomplete_JSON =>
            return Response.Build
              (MIME.Text_HTML, "{""version"": 1,""route"":" &
                 "[{""action"": ""turn"",""direction"": ""right"", ""marker"": 1}, " &
                 "{""action"": ""pickup"", ""customerId"": 0, ""marker"": 2}," &
                 "{""action"": ""pickup"", ""customerId"": 1, ""marker"": 2}," &
                 "{""action"" : ""turn"", ""direction"": ""right"", ""marker"": 4}," &
                 "{""action"": ""dropoff"",""customerId"": 0, ""marker"": 5}," &
                 "{""action"": ""pickup"", ""customerId"": 2, ""marker"": 5}," &
                 "{""action"": ""turn"", ""direction"": ""right"", ""marker"": 7}, " &
                 "{""action"": ""dropoff"", ""customerId"": 1, ""marker"": 8}," &
                 "{""action"": ""dropoff"", ""customerId"": 2, ""marker"": 8}," &
                 "{""action"": ""turn"", ""direction"": ""left"", ""marker"": 10 }," &
                 "{""action"":  ""direction"": ""left"", ""marker"": 12 }," &
                 "{""action"": ""turn"", ""direction"": ""right"", ""marker"": 14 }," &
                 "{""action"": ""wait"", ""marker"": 0} ]}");
         when E_wrong_JSON =>
            return Response.Build
              (MIME.Text_HTML, "{""version"": 1,""route"":" &
                 "[""action"": ""turn"",""direction"": ""right"", ""marker"": 1}, " &
                 "{""action"": ""pickup"", ""customerId"": 0, ""marker"": 2}," &
                 "{""action"": ""pickup"", ""customerId"": 1, ""marker"": 2}," &
                 "{""action"" : ""turn"", ""direction"": ""right"", ""marker"": 4}," &
                 "{""action"": ""dropoff"",""customerId"": 0, ""marker"": 5}," &
                 "{""action"": ""pickup"", ""customerId"": 2, ""marker"": 5}," &
                 "{""action"": ""turn"", ""direction"": ""right"", ""marker"": 7}, " &
                 "{""action"": ""dropoff"", ""customerId"": 1, ""marker"": 8}," &
                 "{""action"": ""dropoff"", ""customerId"": 2, ""marker"": 8}," &
                 "{""action"": ""turn"", ""direction"": ""left"", ""marker"": 10 }," &
                 "{""action"":  ""direction"": ""left"", ""marker"": 12 }," &
                 "{""action"": ""turn"", ""direction"": ""right"", ""marker"": 14 }," &
                 "{""action"": ""wait"", ""marker"": 0} ]}");
         end case;
      elsif URI = "/api/ec/requestPickup" then
         if (action = E_Success) then
            id := Integer'Value(Parameter(Request, "cabId"));
            customer_id := Integer'Value(Parameter(Request, "customerId"));
            if (id < 0 or customer_id < 0) then
               return Response.Acknowledge(Messages.S409);
            end if;
            return Response.Acknowledge(Messages.S200);
         else
            return Response.Acknowledge(Messages.S409);
         end if;
      elsif URI = "/api/ec/pickupsComplete" then
         case (action) is
            when E_Success =>
               id := Integer'Value(Parameter(Request, "cabId"));
               if (id < 0) then
                  return Response.Acknowledge(Messages.S409);
               end if;
               if (completed = true) then
                  return Response.Build
                    (MIME.Text_HTML, "{""completed"": true}");
               else
                  return Response.Build
                    (MIME.Text_HTML, "{""completed"": false}");
               end if;
            when E_Failure =>
               return Response.Acknowledge(Messages.S409);
            when E_Timeout =>
               delay 1.0;
               return Response.Acknowledge(Messages.S408);
            when E_incomplete_JSON =>
            return Response.Build
                 (MIME.Text_HTML, "{""completed"" false}");
         when E_wrong_JSON =>
            return Response.Build
               (MIME.Text_HTML, "{""complated"": false}");

         end case;
      elsif URI = "/api/ec/requestDropoff" then
         if (action = E_Success) then
            id := Integer'Value(Parameter(Request, "cabId"));
            customer_id := Integer'Value(Parameter(Request, "customerId"));
            if (id < 0 or customer_id < 0) then
               return Response.Acknowledge(Messages.S409);
            end if;
            return Response.Acknowledge(Messages.S200);
         else
            return Response.Acknowledge(Messages.S409);
         end if;
      elsif URI = "/api/ec/dropoffsComplete" then
        case (action) is
            when E_Success =>
               id := Integer'Value(Parameter(Request, "cabId"));
               if (id < 0) then
                  return Response.Acknowledge(Messages.S409);
               end if;
               if (completed = true) then
                  return Response.Build
                    (MIME.Text_HTML, "{""completed"": true}");
               else
                  return Response.Build
                    (MIME.Text_HTML, "{""completed"": false}");
               end if;
            when E_Failure =>
               return Response.Acknowledge(Messages.S409);
            when E_Timeout =>
               delay 1.0;
               return Response.Acknowledge(Messages.S408);
            when E_incomplete_JSON =>
            return Response.Build
                 (MIME.Text_HTML, "{""completed"" false}");
         when E_wrong_JSON =>
            return Response.Build
               (MIME.Text_HTML, "{""complated"": false}");

         end case;
      else
         return Response.Acknowledge (Messages.S404);
      end if;


   end Default;

end Job_Executer_Testing.Callbacks;
