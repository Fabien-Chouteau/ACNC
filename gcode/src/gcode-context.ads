with Ada.Unchecked_Conversion;
with Gcode.Parameters; use Gcode.Parameters;

package Gcode.Context is

   type GContext is abstract tagged record
      Params : Parameters_Set;
      Unit : Coord_Unit := Millimeters;
      Positioning : Positioning_Mode := Absolute_Positioning;
      B : Block;
      Fast_Feed_Rate   : Step_Speed := 2.0;
      Current_Feed_Rate : Step_Speed := 1.0;
      Error_Flag : Boolean := False;
--        Real_Position : Step_Position;
      Virt_Position : Float_Position;
   end record;

   function Home (Ctx : in out GContext; Axis : Axis_Name) return Boolean;

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart, EEnd : Natural);
   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart : Natural);

   procedure Raise_Error (Ctx : in out GContext; Msg : String);
   procedure Clear_Error (Ctx : in out GContext);
   function Error_Raised (Ctx : in out GContext) return Boolean;

   type Log_Level is (Info, Warning, Error, Board);
   procedure Log (Ctx : in out GContext; Lvl : Log_Level; Str : String);
   procedure Put (Ctx : in out GContext; C : Character) is abstract;
   procedure Put (Ctx : in out GContext; Str : String);
   procedure Put_Line (Ctx : in out GContext; Str : String);
   procedure New_Line (Ctx : in out GContext);
end Gcode.Context;

