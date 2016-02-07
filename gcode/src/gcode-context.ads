with Ada.Unchecked_Conversion;
with Gcode.Parameters; use Gcode.Parameters;
with Coords;

package Gcode.Context is

   type Word is record
      Value  : Long_Float;
      Is_Set : Boolean;
   end record;

   subtype Word_Letter is Character range 'A' .. 'Z';
   type Block is array (Word_Letter) of Word;

   type Coord_Unit is (Inches, Millimeters, Step);

   package Float_Coords is new Coords (Long_Float, 0.0);
   subtype Float_Position is Float_Coords.Position;

   subtype Steps is Integer;
   package Steps_Coords is new Coords (Steps, 0);
   subtype Step_Position is Steps_Coords.Position;

   type Direction is (Forward, Backward);
   type Axis_Name is (X_Axis, Y_Axis, Z_Axis);

   type GContext is tagged record
      Params : Parameters_Set;
      Unit : Coord_Unit := Millimeters;
      B : Block;
      Fast_Feed_Rate   : Long_Float := 2.0;
      Current_Feed_Rate : Long_Float := 1.0;
      Error_Flag : Boolean := False;
      Step_Per_Millimeter : Float_Position := (100.0, 100.0, 100.0);
      Real_Position : Step_Position;
      Virt_Position : Step_Position;
   end record;

   procedure Step (Ctx : in out GContext; Axis : Axis_Name; Dir : Direction);
   function Home (Ctx : in out GContext; Axis : Axis_Name) return Boolean;

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : string;
                           EStart, EEnd : Natural);

   function Step_To_Milli (Ctx : in out GContext; S : Step_Position)
                           return Float_Position;
   function Milli_To_Step (Ctx : in out GContext; S : Float_Position)
                           return Step_Position;
   function Inch_To_Milli (S : Float_Position) return Float_Position;
   function Inch_To_Milli (S : Long_Float) return Long_Float;

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

