pragma Ada_2012;
with AWS.Client; use AWS;
with AWS.Response;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with AWS.Messages;
with AWS.Response; use AWS.Response;
use type AWS.Messages.Status_Code;
with Ada.Streams; use Ada.Streams;
with System.Strings.Stream_Ops; use System.Strings.Stream_Ops;

package body JSON_Wrappers is

   --------------
   -- get_JSON --
   --------------

   procedure build_url(url : in out Unbounded_String; Host : String; port : String;
                       path : String; parameters : param_map_p.Map) is
      para_concat : String := "?";
   begin
      Ada.Strings.Unbounded.Append(url, "http://" & host & ":" & port & path);
      for container in parameters.Iterate loop
         Ada.Strings.Unbounded.Append(url, para_concat & Key(container) & "=" & Element(container));
         para_concat := "&";
      end loop;
   end build_url;

    procedure build_uri(uri : in out Unbounded_String;
                       path : String; parameters : param_map_p.Map) is
      para_concat : String := "?";
   begin
      Ada.Strings.Unbounded.Append(uri, path);
      for container in parameters.Iterate loop
         Ada.Strings.Unbounded.Append(uri, para_concat & Key(container) & "=" & Element(container));
         para_concat := "&";
      end loop;
   end build_uri;

   function get_JSON
     (Host       : String; port : String; path : String;
      parameters : param_map_p.Map;
      response_data_JSON : out JSON_Value) return Messages.Status_Code
   is
      response : AWS.Response.Data;
      response_data_raw : Ada.Strings.Unbounded.Unbounded_String;
      url : Ada.Strings.Unbounded.Unbounded_String;
      status_code : Messages.Status_Code;
   begin
      response_data_JSON := Create_Object;
      build_url(url,  Host,  port, path, parameters);
      --Put_Line("URL: " & To_String(url));
      response := Client.Get(URL          => To_String(url));
      status_code := AWS.Response.Status_Code(response);
      if (status_code /= Messages.S200) then
         return status_code;
      end if;
      response_data_raw := AWS.Response.Message_Body (response);
      --Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(response_data_raw));
      response_data_JSON := Read(response_data_raw);
      return status_code;
   end get_JSON;


   function get_JSON
     (con : in out HTTP_Connection; path : String;
      parameters : param_map_p.Map;
      response_data_JSON : out JSON_Value) return Messages.Status_Code
   is
      response : AWS.Response.Data;
      response_data_raw : Ada.Strings.Unbounded.Unbounded_String;
      uri : Ada.Strings.Unbounded.Unbounded_String;
      status_code : Messages.Status_Code;
   begin
      response_data_JSON := Create_Object;
      build_uri(uri, path, parameters);
      --Put_Line("URL: " & To_String(url));
      Client.Get(con, URI => To_String(uri), Result => response);
      status_code := AWS.Response.Status_Code(response);
      if (status_code /= Messages.S200) then
         return status_code;
      end if;
      response_data_raw := AWS.Response.Message_Body (response);
      --Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(response_data_raw));
      response_data_JSON := Read(response_data_raw);
      return status_code;
   end get_JSON;


    function post_JSON
     (Host       : String; port : String; path : String;
      parameters : param_map_p.Map := No_Params;
      value : JSON_Value := Create_Object;
      response_data_JSON : out JSON_Value
     ) return Messages.Status_Code is
      response : AWS.Response.Data;
      response_data_raw : Ada.Strings.Unbounded.Unbounded_String;
      url : Ada.Strings.Unbounded.Unbounded_String;
      status_code : Messages.Status_Code;
   begin
      response_data_JSON := Create_Object;
      build_url(url,  Host,  port, path, parameters);
      --Put_Line("URL: " & To_String(url));
      response := Client.Post(URL          => To_String(url),
                        Data         => value.Write,
                              Content_Type =>  "application/json; charset=utf8");
      status_code := AWS.Response.Status_Code(response);
      if (status_code /= Messages.S200) then
         return status_code;
      end if;

       if (AWS.Response.Content_Length(response) /= 0) then
         response_data_raw := AWS.Response.Message_Body(response);
         --Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(response_data_raw));
         response_data_JSON := Read(response_data_raw);
      end if;
      return status_code;


   end post_JSON;

    function post_JSON
     (con : in out HTTP_Connection; path : String; parameters : param_map_p.Map := No_Params;
      value : JSON_Value := Create_Object; response_data_JSON : out JSON_Value
     ) return Messages.Status_Code is

      response : AWS.Response.Data;
      response_data_raw : Ada.Strings.Unbounded.Unbounded_String;
      uri : Ada.Strings.Unbounded.Unbounded_String;
      status_code : Messages.Status_Code;
      --data : Stream_Element_Array (1 .. 1);

   begin
      response_data_JSON := Create_Object;
      build_uri(uri, path, parameters);
      --Put_Line("URL: " & To_String(url));

      --Stream_Element_Array_Read(value.Write, data);
      Client.Post(con, response, value.Write, Content_Type =>  "application/json; charset=utf8", URI => To_String(uri));
      status_code := AWS.Response.Status_Code(response);
      if (status_code /= Messages.S200) then
         return status_code;
      end if;

       if (AWS.Response.Content_Length(response) /= 0) then
         response_data_raw := AWS.Response.Message_Body(response);
         --Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(response_data_raw));
         response_data_JSON := Read(response_data_raw);
      end if;
      return status_code;


      end post_JSON;
end JSON_Wrappers;
