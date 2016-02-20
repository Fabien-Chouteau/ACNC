with Ada.Unchecked_Conversion;
with Gcode.Parameters; use Gcode.Parameters;

package Gcode.Context is

   type GContext is tagged record
      Params : Parameters_Set;
      Unit : Coord_Unit := Millimeters;
      B : Block;
      Fast_Feed_Rate   : Step_Speed := 2.0;
      Current_Feed_Rate : Step_Speed := 1.0;
      Error_Flag : Boolean := False;
      Step_Per_Millimeter : Float_Position := (100.0, 100.0, 100.0);
--        Real_Position : Step_Position;
      Virt_Position : Float_Position;
   end record;

   function Home (Ctx : in out GContext; Axis : Axis_Name) return Boolean;

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart, EEnd : Natural);

   function Step_To_Milli (Ctx : in out GContext; S : Step_Position)
                           return Float_Position;
   function Milli_To_Step (Ctx : in out GContext; S : Float_Position)
                           return Step_Position;
   function Inch_To_Milli (S : Float_Position) return Float_Position;
   function Inch_To_Milli (S : Float_Value) return Float_Value;

   procedure Raise_Error (Ctx : in out GContext; Msg : String);
   procedure Clear_Error (Ctx : in out GContext);
   function Error_Raised (Ctx : in out GContext) return Boolean;

   type Log_Level is (Info, Warning, Error, Board);
   procedure Log (Ctx : in out GContext; Lvl : Log_Level; Str : String);
   procedure Put (Ctx : in out GContext; C : Character);
   procedure Put (Ctx : in out GContext; Str : String);
   procedure Put_Line (Ctx : in out GContext; Str : String);
   procedure New_Line (Ctx : in out GContext);
end Gcode.Context;

