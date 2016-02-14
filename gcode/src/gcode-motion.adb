with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Gcode.Planner;

package body Gcode.Motion is

   use type Step_Position;
   use type Float_Position;

   ---------------
   -- Move_Line --
   ---------------

   procedure Move_Line
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Step_Speed)
   is
   begin
      Put_Line ("Move_Line to " & Image (Target));
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

      Ctx.Put_Line ("Travel: " & Image (Travel));
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

      Divisions := Natural (abs Travel * Radius) * 2;

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
