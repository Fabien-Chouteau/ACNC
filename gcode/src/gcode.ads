-------------------------------------------------------------------------------
--                                                                           --
--                                   ACNC                                    --
--                                                                           --
--         Copyright (C) 2016 Fabien Chouteau (chouteau@adacore.com)         --
--                                                                           --
--                                                                           --
--    ACNC is free software: you can redistribute it and/or modify it        --
--    under the terms of the GNU General Public License as published by      --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    ACNC is distributed in the hope that it will be useful, but WITHOUT    --
--    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY     --
--    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public        --
--    License for more details.                                              --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with ACNC. If not, see <http://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_Functions;
with Coords;

package Gcode is

   subtype Float_Value is Float;
   subtype Line_Value is Natural;
   subtype Frequency_Value is Duration;

   package Float_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Float_Value);

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
   type Positioning_Mode is (Absolute_Positioning, Relative_Positioning);

   function To_Letter (Axis : Axis_Name) return Character;

   procedure Reverse_Dir (Dir : in out Direction);

   package Float_Coords is new Coords (Float_Value, Axis_Name);
   subtype Float_Position is Float_Coords.Position;

   subtype Steps is Integer;
   package Steps_Coords is new Coords (Steps, Axis_Name);
   subtype Step_Position is Steps_Coords.Position;

   function Step_To_Milli (S : Step_Position) return Float_Position;
   function Milli_To_Step (S : Float_Position) return Step_Position;
   function Inch_To_Milli (S : Float_Position) return Float_Position;
   function Inch_To_Milli (S : Float_Value) return Float_Value;

   type Step_Speed is new Float_Value;
   type Step_Acceleration is new Float_Value;

   type Motor_Enable_Array is array (Axis_Name) of Boolean;

   function Distance (A, B : Float_Position) return Float_Value;
   function Image (Val : Float_Value) return String;
   function Image (Pos : Float_Position) return String;
   function Image (Pos : Step_Position) return String;
end Gcode;
