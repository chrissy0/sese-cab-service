with Ada.Containers.Indefinite_Hashed_Maps;
use Ada.Containers;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Ada.Strings;
with Ada.Strings.Hash;
with AWS.Messages;
use AWS;
with Ada.Containers;
with AWS.Client; use AWS.Client;


use type AWS.Messages.Status_Code;
package JSON_Wrappers is


   package param_map_p is new Indefinite_Hashed_Maps (Key_Type => String,
                                               Element_Type => String,
                                               Hash => Ada.Strings.Hash,
                                                              Equivalent_Keys => "=");
   use param_map_p;

   No_Params : param_map_p.Map;
   function get_JSON(Host: String ; port : String ; path : String; parameters : param_map_p.Map; response_data_JSON : out JSON_Value) return Messages.Status_Code;
   function get_JSON(con : in out HTTP_Connection; path : String; parameters : param_map_p.Map; response_data_JSON : out JSON_Value) return Messages.Status_Code;
   function post_JSON(Host       : String; port : String; path : String;
      parameters : param_map_p.Map := No_Params; value : JSON_Value := Create_Object; response_data_JSON : out JSON_Value) return Messages.Status_Code;
    function post_JSON(con : in out HTTP_Connection; path : String; parameters : param_map_p.Map := No_Params; value : JSON_Value := Create_Object; response_data_JSON : out JSON_Value
     ) return Messages.Status_Code;
end JSON_Wrappers;
