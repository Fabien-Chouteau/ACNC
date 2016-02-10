with Coords;

package Gcode is

   subtype Float_Value is Long_Float;
   subtype Line_Value is Natural;

   type Word is record
      Value  : Float_Value;
      Is_Set : Boolean;
   end record;

   subtype Word_Letter is Character range 'A' .. 'Z';
   type Block is array (Word_Letter) of Word;

   type Coord_Unit is (Inches, Millimeters, Step);

   package Float_Coords is new Coords (Float_Value, 0.0);
   subtype Float_Position is Float_Coords.Position;

   subtype Steps is Integer;
   package Steps_Coords is new Coords (Steps, 0);
   subtype Step_Position is Steps_Coords.Position;

   type Direction is (Forward, Backward);
   type Axis_Name is (X_Axis, Y_Axis, Z_Axis);
end Gcode;
