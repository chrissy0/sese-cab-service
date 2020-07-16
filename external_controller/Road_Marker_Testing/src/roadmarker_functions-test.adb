-- @summary
-- Road Marker Function child unit test package body.
--
-- @author Chanki Hong

pragma Ada_2012;
with AUnit.Assertions; use AUnit.Assertions;
package body Roadmarker_Functions.Test is

   ----------
   -- Name --
   ----------

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Math package");
   end Name;

   -------------------------------
   -- test_roadmarker_functions --
   -------------------------------

   -- Reimplement get_Road_MarkerID and compare return values for all combinations
   -- of sensors on and off.
   procedure test_roadmarker_functions (T : in out Test) is
      on : constant := 240.0;
      off : constant := 260.0;
      subtype idx is Integer range 0..3;
      type sensor_array is array (idx) of Long_Float;

      values : sensor_array;

      function rm_value(sensor_value : Long_Float; weight : Long_Float) return Long_Float
      is
      begin
         if sensor_value < 250.0 then
            return weight;
         else
            return 0.0;
         end if;
      end rm_value;

      function dummy_get_Road_MarkerID (ir_marker_out_fl : Long_Float; ir_marker_out_fr : Long_Float; ir_marker_out_bl : Long_Float; ir_marker_out_br : Long_Float) return Long_Float is
      begin
         return rm_value(ir_marker_out_fl, 1.0) + rm_value(ir_marker_out_fr, 2.0) + rm_value(ir_marker_out_bl, 4.0) + rm_value(ir_marker_out_br, 8.0);

      end dummy_get_Road_MarkerID;
   begin
      for A in Boolean loop
         case A is
            when True =>
               values(0) := on;
            when False =>
               values(0) := off;
         end case;

         for B in Boolean loop
            case B is
            when True =>
               values(1) := on;
            when False =>
               values(1) := off;
            end case;
            for C in Boolean loop
               case C is
               when True =>
                  values(2) := on;
               when False =>
                  values(2) := off;
               end case;
               for D in Boolean loop
                  case D is
                  when True =>
                     values(3) := on;
                  when False =>
                     values(3) := off;
                  end case;

                  Assert(dummy_get_Road_MarkerID(values(0), values(1), values(2), values(3))
                         =     get_Road_MarkerID(values(0), values(1), values(2), values(3)),
                         "road marker function doesnt follow the formula");
               end loop;
            end loop;
         end loop;
      end loop;


   end test_roadmarker_functions;

   -------------------------
   -- test_on_road_marker --
   -------------------------

   -- Test on Road marker. True, when all outer RM on, else False.
   procedure test_on_road_marker (T : in out Test) is
      on : constant := 240.0;
      off : constant := 260.0;
      subtype idx is Integer range 0..3;
      type sensor_array is array (idx) of Long_Float;

      values : sensor_array;

      function rm_value(sensor_value : Long_Float; weight : Long_Float) return Long_Float
      is
      begin
         if sensor_value < 250.0 then
            return weight;
         else
            return 0.0;
         end if;
      end rm_value;

      function dummy_get_Road_MarkerID (ir_marker_out_fl : Long_Float; ir_marker_out_fr : Long_Float; ir_marker_out_bl : Long_Float; ir_marker_out_br : Long_Float) return Long_Float is
      begin
         return rm_value(ir_marker_out_fl, 1.0) + rm_value(ir_marker_out_fr, 2.0) + rm_value(ir_marker_out_bl, 4.0) + rm_value(ir_marker_out_br, 8.0);

      end dummy_get_Road_MarkerID;
   begin
      for A in Boolean loop
         case A is
            when True =>
               values(0) := on;
            when False =>
               values(0) := off;
         end case;

         for B in Boolean loop
            case B is
            when True =>
               values(1) := on;
            when False =>
               values(1) := off;
            end case;
            for C in Boolean loop
               case C is
               when True =>
                  values(2) := on;
               when False =>
                  values(2) := off;
               end case;
               for D in Boolean loop
                  case D is
                  when True =>
                     values(3) := on;
                  when False =>
                     values(3) := off;
                  end case;

                  Assert(on_Road_Marker(values(0), values(1), values(2), values(3))
                         =     ((values(0) < 250.0) and (values(1) < 250.0) and (values(2) < 250.0) and (values(3) < 250.0)),
                         "road marker function doesnt follow the formula");
               end loop;
            end loop;
         end loop;
      end loop;
   end test_on_road_marker;



end Roadmarker_Functions.Test;
