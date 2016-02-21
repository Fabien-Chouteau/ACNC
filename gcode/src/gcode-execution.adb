with Gcode.Motion;
with Gcode.Planner;

package body Gcode.Execution is

   use type Step_Position;
   use type Float_Position;

   procedure Line_Command (Ctx : in out GContext'Class;
                           Feed_Rate : Step_Speed);
   procedure Circle_Command
     (Ctx : in out GContext'Class;
      Dir : Gcode.Motion.Circular_Interpolation_Direction;
      Feed_Rate : Step_Speed);
   procedure Return_To_Home (Ctx : in out GContext'Class;
                             Feed_Rate : Step_Speed);
   procedure Dwell_Command (Ctx : in out GContext'Class);

   ------------------
   -- Line_Command --
   ------------------

   procedure Line_Command (Ctx : in out GContext'Class;
                           Feed_Rate : Step_Speed)
   is
      Target : Float_Position := Ctx.Virt_Position;
   begin
      if Ctx.B ('X').Is_Set then
         Target (X_Axis) := Ctx.B ('X').Value;
      end if;
      if Ctx.B ('Y').Is_Set then
         Target (Y_Axis) := Ctx.B ('Y').Value;
      end if;
      if Ctx.B ('Z').Is_Set then
         Target (Z_Axis) := Ctx.B ('Z').Value;
      end if;

      Gcode.Motion.Move_Line (Ctx, Target, Feed_Rate);
   end Line_Command;

   --------------------
   -- Circle_Command --
   --------------------

   procedure Circle_Command
     (Ctx : in out GContext'Class;
      Dir : Gcode.Motion.Circular_Interpolation_Direction;
      Feed_Rate : Step_Speed)
   is
      Start_Point, End_Point, Center : Float_Position;
   begin
      --  Ctx.Log (Info, "Move_Circle");
      if not (Ctx.B ('X').Is_Set or else Ctx.B ('Y').Is_Set) then
         Ctx.Report_Error ("", "X or Y required for circular motion",
                           0, 0);
         return;
      end if;

      if not (Ctx.B ('I').Is_Set or else Ctx.B ('J').Is_Set) then
         Ctx.Report_Error ("", "I or J required for circular motion",
                           0, 0);
         return;
      end if;

      if Ctx.B ('R').Is_Set then
         Ctx.Report_Error ("", "R not supported in circular motion", 0, 0);
         return;
      end if;

      Start_Point := Ctx.Virt_Position;
      Center      := Start_Point;
      End_Point   := Start_Point;

      if Ctx.B ('X').Is_Set then
         End_Point (X_Axis) := Ctx.B ('X').Value;
      end if;
      if Ctx.B ('Y').Is_Set then
         End_Point (Y_Axis) := Ctx.B ('Y').Value;
      end if;
      if Ctx.B ('Z').Is_Set then
         End_Point (Z_Axis) := Ctx.B ('Z').Value;
      end if;

      if Start_Point (Z_Axis) /= End_Point (Z_Axis) then
         Ctx.Log (Warning, "Z movement not supported in circular motion");
      end if;

      if Ctx.B ('I').Is_Set then
         Center (X_Axis) := Start_Point (X_Axis) + Ctx.B ('I').Value;
      end if;
      if Ctx.B ('J').Is_Set then
         Center (Y_Axis) := Start_Point (Y_Axis) + Ctx.B ('J').Value;
      end if;

      Gcode.Motion.Move_Circle (Ctx         => Ctx,
                                Start_Point => Start_Point,
                                End_Point   => End_Point,
                                Offset      => Center,
                                Dir         => Dir,
                                Feed_Rate   => Feed_Rate);
   end Circle_Command;

   --------------------
   -- Return_To_Home --
   --------------------

   procedure Return_To_Home (Ctx : in out GContext'Class;
                             Feed_Rate : Step_Speed)
   is
   begin
      --  First, go to the intermediate position
      if Ctx.B ('X').Is_Set
        or else
          Ctx.B ('Y').Is_Set
        or else
          Ctx.B ('Z').Is_Set
      then
         Line_Command (Ctx, Feed_Rate);
      end if;

      Gcode.Planner.Planner_Add_Homing (Ctx       => Ctx,
                                        Feed_Rate => Feed_Rate);
   end Return_To_Home;

   -------------------
   -- Dwell_Command --
   -------------------

   procedure Dwell_Command (Ctx : in out GContext'Class) is
   begin
      if not Ctx.B ('P').Is_Set then
         Ctx.Report_Error ("", "P required in dwell command", 0, 0);
         return;
      end if;

      Planner.Planner_Add_Dwell (Ctx, Duration (Ctx.B ('P').Value));
   end Dwell_Command;

   -------------
   -- Execute --
   -------------

   function Execute (Line : String; Ctx : in out GContext'Class)
                     return Boolean
   is
      Int_Part, Frac_Part : Integer;
      pragma Unreferenced (Frac_Part);
      Feed : Step_Speed;
   begin
      if Ctx.B ('M').Is_Set then
         Int_Part := Integer (Float_Value'Floor (Ctx.B ('M').Value));
         Frac_Part := Integer (Float_Value'Floor (Ctx.B ('M').Value * 100.0));
         case Int_Part is
            when others =>
               Ctx.Report_Error (Line, "Unknown M code " & Int_Part'Img, 0, 0);
               return False;
         end case;
      end if;

      if Ctx.B ('F').Is_Set then
         Ctx.Current_Feed_Rate := Step_Speed (Ctx.B ('F').Value);
      end if;

      Feed := Ctx.Current_Feed_Rate;
      if Ctx.Unit = Inches then
         Ctx.B ('X').Value := Inch_To_Milli (Ctx.B ('X').Value);
         Ctx.B ('Y').Value := Inch_To_Milli (Ctx.B ('Y').Value);
         Ctx.B ('Z').Value := Inch_To_Milli (Ctx.B ('Z').Value);
         Ctx.B ('I').Value := Inch_To_Milli (Ctx.B ('I').Value);
         Ctx.B ('J').Value := Inch_To_Milli (Ctx.B ('J').Value);
         Ctx.B ('R').Value := Inch_To_Milli (Ctx.B ('R').Value);
      end if;

      if Ctx.B ('G').Is_Set then
         Int_Part := Integer (Float_Value'Floor (Ctx.B ('G').Value));
         Frac_Part := Integer (Float_Value'Floor (Ctx.B ('G').Value * 100.0));
         case Int_Part is
            when 0 =>
               Line_Command (Ctx, Ctx.Fast_Feed_Rate);
            when 1 =>
               Line_Command (Ctx, Feed);
            when 2 =>
               Circle_Command (Ctx, Gcode.Motion.Clockwise, Feed);
            when 3 =>
               Circle_Command (Ctx, Gcode.Motion.Counter_Clockwise, Feed);
            when 4 =>
               Dwell_Command (Ctx);
            when 20 => Ctx.Unit := Inches;
            when 21 => Ctx.Unit := Millimeters;
            when 28 => Return_To_Home (Ctx, Feed);
            when others =>
               Ctx.Report_Error (Line, "Unknown G code " & Int_Part'Img, 0, 0);
               return False;
         end case;
      end if;
      return True;
   end Execute;
end Gcode.Execution;
