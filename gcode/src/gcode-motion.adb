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

with Ada.Numerics; use Ada.Numerics;
with Gcode.Planner;

package body Gcode.Motion is

   use type Float_Position;
   use Float_Functions;

   ---------------
   -- Move_Line --
   ---------------

   procedure Move_Line
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Step_Speed)
   is
   begin
      --  Update virtual position
      Ctx.Virt_Position := Target;

      Gcode.Planner.Planner_Add_Motion (Ctx, Target, Feed_Rate);
   end Move_Line;

   -----------------
   -- Move_Circle --
   -----------------

   procedure Move_Circle
     (Ctx         : in out GContext'Class;
      Start_Point : Float_Position;
      End_Point   : Float_Position;
      Offset      : Float_Position;
      Dir         : Circular_Interpolation_Direction;
      Feed_Rate   : Step_Speed)
   is
      Target : Float_Position;
      Radius : Float_Value;
      Travel, Angle, Cos_A, Sin_A : Float_Value;
      Divisions : Natural;
   begin

      Radius := Distance (Offset, End_Point);

      Travel := Arctan (End_Point (X_Axis) - Offset (X_Axis),
                        End_Point (Y_Axis) - Offset (Y_Axis))
        - Arctan (Start_Point (X_Axis) - Offset (X_Axis),
                  Start_Point (Y_Axis) - Offset (Y_Axis));

      if Dir = Clockwise then
         if Travel >= 0.0 then
            Travel := Travel - 1.0 * Pi;
         else
            Travel := Travel + 1.0 * Pi;
         end if;
      end if;

      declare
         From : constant Float_Position := Start_Point - Offset;
         To   : constant Float_Position := End_Point - Offset;
      begin
         Travel := Arctan
           (From (X_Axis) * To (Y_Axis) - From (Y_Axis) * To (X_Axis),
            From (X_Axis) * To (X_Axis) + From (Y_Axis) * To (Y_Axis));
         if Travel < 0.0 then
            Travel := Travel + 2.0 * Pi;
         end if;
         if Dir = Clockwise then
            Travel := Travel - 2.0 * Pi;
         end if;
         if Travel = 0.0 then
            if Dir = Clockwise then
               Travel := Travel - 2.0 * Pi;
            else
               Travel := Travel + 2.0 * Pi;
            end if;
         end if;
      end;

      Divisions := Natural (abs Travel * Radius);

      --  Make sure we always have at least one division
      Divisions := Divisions + 1;

      Angle := Travel / Float_Value (Divisions);
      Cos_A := Cos (Angle);
      Sin_A := Sin (Angle);
      Target := Start_Point;
      for Count in 1 .. Divisions loop
         declare
            Vect : constant Float_Position := Target - Offset;
            Rot : Float_Position;
         begin
            --  Rotate direction vector
            Rot (X_Axis) := Vect (X_Axis) * Cos_A - Vect (Y_Axis) * Sin_A;
            Rot (Y_Axis) := Vect (X_Axis) * Sin_A + Vect (Y_Axis) * Cos_A;
            Rot (Z_Axis) := 0.0;

            Target := Offset + Rot;
            Move_Line (Ctx, Target, Feed_Rate);
         end;
      end loop;
      Move_Line (Ctx, End_Point, Feed_Rate);
   end Move_Circle;

end Gcode.Motion;
