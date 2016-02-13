with Ada.Numerics.Generic_Elementary_Functions;
with Coords;

package Gcode is

   subtype Float_Value is Float;
   subtype Line_Value is Natural;

   package Float_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Float_Value);
   use Float_Functions;

   type Word is record
      Value  : Float_Value;
      Is_Set : Boolean;
   end record;

   subtype Word_Letter is Character range 'A' .. 'Z';
   type Block is array (Word_Letter) of Word;

   type Coord_Unit is (Inches, Millimeters, Step);
   type Axis_Name is (X_Axis, Y_Axis, Z_Axis);
   type Direction is (Forward, Backward);
   type Axis_Directions is array (Axis_Name) of Direction;

   package Float_Coords is new Coords (Float_Value, Axis_Name);
   subtype Float_Position is Float_Coords.Position;

   subtype Steps is Integer;
   package Steps_Coords is new Coords (Steps, Axis_Name);
   subtype Step_Position is Steps_Coords.Position;


   type Step_Speed is new Float_Value;
   type Step_Acceleration is new Float_Value;

   function Distance (A, B : Float_Position) return Float_Value;
   function Image (Val : Float_Value) return String;
   function Image (Pos : Float_Position) return String;
   function Image (Pos : Step_Position) return String;
end Gcode;
