with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;

with Gcode.Planner;

package body Gcode.Motion is

   use type Step_Position;
   use type Float_Position;

   package Float_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Float_Value);
   use Float_Functions;

   ---------------
   -- Move_Line --
   ---------------

   procedure Move_Line
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Float_Value)
   is
   begin
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
      Feed_Rate   : Float_Value)
   is
      Target : Float_Position;
      Radius, Radius2 : Float_Value;
      Travel, Angle, Cos_A, Sin_A : Float_Value;
      Divisions : Natural;
   begin

      Ctx.Put_Line ("Circle_From " & Image (Start_Point));
      Ctx.Put_Line ("Circle_To " & Image (End_Point));

      Radius := Distance (Offset, End_Point);
      Ctx.Put_Line ("Radius: " & Image (Radius));
      Radius2 := Distance (Offset, Start_Point);
      Ctx.Put_Line ("Radius2: " & Image (Radius2));

      Travel := Arctan (End_Point.X - Offset.X, End_Point.Y - Offset.Y)
        - Arctan (Start_Point.X - Offset.X, Start_Point.Y - Offset.Y);
      if Dir = Clockwise then
         if Travel >= 0.0 then
            Travel := Travel - 1.0 * Pi;
         else
            Travel := Travel + 1.0 * Pi;
         end if;
      end if;

      Ctx.Put_Line ("Travel: " & Image (Travel));
      declare
         From : constant Float_Position := Start_Point - Offset;
         To   : constant Float_Position := End_Point - Offset;
      begin
         Travel := Arctan (From.X * To.Y - From.Y * To.X,
                           From.X * To.X + From.Y * To.Y);
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

      Divisions := Natural (abs Travel * Radius) * 5;

      --  Make sure we always have at least one division
      Divisions := Divisions + 1;

      Ctx.Put_Line ("Division:" & Divisions'Img);

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
            Rot.X := Vect.X * Cos_A - Vect.Y * Sin_A;
            Rot.Y := Vect.X * Sin_A + Vect.Y * Cos_A;
            Rot.Z := 0.0;

            Target := Offset + Rot;
            Move_Line (Ctx, Target, Feed_Rate);
         end;
      end loop;
      Move_Line (Ctx, End_Point, Feed_Rate);
   end Move_Circle;

end Gcode.Motion;
