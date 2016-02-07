with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;

package body Gcode.Execution is

   use type Step_Position;
   use type Float_Position;

   package Float_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Long_Float);
   use Float_Functions;

   type Circular_Interpolation_Direction is (Clockwise, Counter_Clockwise);

   -----------
   -- Image --
   -----------

   function Image (Val : Long_Float) return String is
      Int_Part : constant Integer := Integer (Long_Float'Floor (Val));
      Frac_Part : constant Integer := Integer (Long_Float'Floor (Val * 10000.0));
   begin
      return Int_Part'Img & "." & Frac_Part'Img;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Pos : Float_Position) return String is
   begin
      return ("X:" & Image (Pos.X) & " Y:" & Image (Pos.Y)
              & " Z:" & Image (Pos.Z));
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Pos : Step_Position) return String is
   begin
      return ("X:" & Pos.X'Img & " Y:" & Pos.Y'Img & " Z:" & Pos.Z'Img);
   end Image;

   -------------
   -- Move_To --
   -------------

   function Move_To (Ctx       : in out GContext'Class;
                     Target    : Float_Position;
                     Feed_Rate : Long_Float) return Boolean
   is
      pragma Unreferenced (Feed_Rate);
      D_Step, Cmd : Step_Position;
      D, Absolute : Float_Position;
      Err1, Err2 : Long_Float;
      Dir_X, Dir_Y, Dir_Z : Direction;
   begin
      Ctx.Log (Info, "Move_To " & Image (Target));
      Cmd := Milli_To_Step (Ctx, Target);

      D_Step := Cmd - Ctx.Real_Position;
      D := (Long_Float (D_Step.X),
            Long_Float (D_Step.Y),
            Long_Float (D_Step.Z));
      Absolute := abs D;

      Ctx.Put_Line ("Move_To steps" & Image (D_Step));
      if D_Step = (0, 0, 0) then
         --  Nothing to do...
         return True;
      end if;

      Dir_X := (if D_Step.X >= 0 then Forward else Backward);
      Dir_Y := (if D_Step.Y >= 0 then Forward else Backward);
      Dir_Z := (if D_Step.Z >= 0 then Forward else Backward);

      if Absolute.X >= Absolute.Z and then Absolute.X >= Absolute.Y then
         Err1 := Absolute.X / 2.0;
         Err2 := Absolute.X / 2.0;

         for Cnt in 0 .. Integer (Absolute.X) - 1 loop
            Err1 := Err1 - Absolute.Y;
            Err2 := Err2 - Absolute.Z;
            Ctx.Step (X_Axis, Dir_X);
            if Err1 < 0.0 then
               Ctx.Step (Y_Axis, Dir_Y);
               Err1 := Err1 + Absolute.X;
            end if;
            if Err2 < 0.0 then
               Ctx.Step (Z_Axis, Dir_Z);
               Err2 := Err2 + Absolute.X;
            end if;
         end loop;
      elsif Absolute.Y >= Absolute.Z and then Absolute.Y >=  Absolute.X then
         Err1 := Absolute.Y / 2.0;
         Err2 := Absolute.Y / 2.0;

         for Cnt in 0 .. Integer (Absolute.Y) - 1 loop
            Err1 := Err1 - Absolute.X;
            Err2 := Err2 - Absolute.Z;
            Ctx.Step (Y_Axis, Dir_Y);
            if Err1 < 0.0 then
               Ctx.Step (X_Axis, Dir_X);
               Err1 := Err1 + Absolute.Y;
            end if;
            if Err2 < 0.0 then
               Ctx.Step (Z_Axis, Dir_Z);
               Err2 := Err2 + Absolute.Y;
            end if;
         end loop;
      else
         Err1 := Absolute.Z / 2.0;
         Err2 := Absolute.Z / 2.0;

         for Cnt in 0 .. Integer (Absolute.Z) - 1 loop
            Err1 := Err1 - Absolute.X;
            Err2 := Err2 - Absolute.Y;
            Ctx.Step (Z_Axis, Dir_Z);
            if Err1 < 0.0 then
               Ctx.Step (X_Axis, Dir_X);
               Err1 := Err1 + Absolute.Z;
            end if;
            if Err2 < 0.0 then
               Ctx.Step (Y_Axis, Dir_Y);
               Err2 := Err2 + Absolute.Z;
            end if;
         end loop;
      end if;

      --  Make sure that we really go to the exact target position
      while Ctx.Real_Position /= Cmd loop
         Ctx.Put_Line ("Ctx.Real_Position " & Image (Ctx.Real_Position));
         Ctx.Put_Line ("Cmd " & Image (Cmd));

         Ctx.Put_Line ("Correction loop");
         if Ctx.Real_Position.X < Cmd.X then
            Ctx.Put_Line ("X Fwd");
            Ctx.Step (X_Axis, Forward);
         elsif Ctx.Real_Position.X > Cmd.X then
            Ctx.Put_Line ("X Bwd");
            Ctx.Step (X_Axis, Backward);
         end if;
         if Ctx.Real_Position.Y < Cmd.Y then
            Ctx.Put_Line ("Y Fwd");
            Ctx.Step (Y_Axis, Forward);
         elsif Ctx.Real_Position.Y > Cmd.Y then
            Ctx.Put_Line ("Y Bwd");
            Ctx.Step (Y_Axis, Backward);
         end if;
         if Ctx.Real_Position.Z < Cmd.Z then
            Ctx.Put_Line ("Z Fwd");
            Ctx.Step (Z_Axis, Forward);
         elsif Ctx.Real_Position.Z > Cmd.Z then
            Ctx.Put_Line ("Z Bwd");
            Ctx.Step (Z_Axis, Backward);
         end if;
      end loop;
      Ctx.Put_Line ("End Of Move_To");
      return True;
   end Move_To;

   ---------------
   -- Move_Line --
   ---------------

   function Move_Line (Ctx : in out GContext'Class;
                       Feed_Rate : Long_Float) return Boolean is
      Target : Float_Position := Step_To_Milli (Ctx, Ctx.Real_Position);
   begin
      if Ctx.B ('X').Is_Set then
         Target.X := Ctx.B ('X').Value;
      end if;
      if Ctx.B ('Y').Is_Set then
         Target.Y := Ctx.B ('Y').Value;
      end if;
      if Ctx.B ('Z').Is_Set then
         Target.Z := Ctx.B ('Z').Value;
      end if;

      return Move_To (Ctx, Target, Feed_Rate);
   end Move_Line;

   --------------
   -- Distance --
   --------------

   function Distance (A, B : Float_Position) return Long_Float is
      Tmp : constant Long_Float :=
        (A.X - B.X)**2 + (A.Y - B.Y)**2 + (A.Z - B.Z)**2;

      ----------
      -- Sqrt --
      ----------

      function Sqrt (X : Long_Float) return Long_Float is
         U     : Long_Float := X;
         New_U : Long_Float;
      begin
         if X < 0.0 then
            raise Program_Error;
         end if;
         if X = 0.0 then
            return 0.0;
         end if;
         loop
            New_U := (U + X/U)/2.0;
            exit when New_U >= U;
            U := New_U;
         end loop;
         return U;
      end Sqrt;   begin
      return Sqrt (Tmp);
   exception
      when others =>
         return 0.0;
   end;

   -----------------
   -- Move_Circle --
   -----------------

   function Move_Circle (Ctx : in out GContext'Class;
                         Dir : Circular_Interpolation_Direction;
                         Feed_Rate : Long_Float) return Boolean is
      Start_Point, End_Point, Center, Target : Float_Position;
      Radius, Radius2 : Long_Float;
      Travel, Angle, Cos_A, Sin_A : Long_Float;
      Divisions : Natural;
   begin
      --  Ctx.Log (Info, "Move_Circle");
      if not (Ctx.B ('X').Is_Set or else Ctx.B ('Y').Is_Set) then
         Ctx.Report_Error ("", "X or Y is required for circular motion",
                           0, 0);
         Ctx.Put_Line ("X or Y is required for circular motion");
         return False;
      end if;

      if not (Ctx.B ('I').Is_Set or else Ctx.B ('J').Is_Set) then
         Ctx.Report_Error ("", "I or J is required for circular motion",
                           0, 0);
         Ctx.Put_Line ("I or J is required for circular motion");
         return False;
      end if;

      if Ctx.B ('R').Is_Set then
         Ctx.Report_Error ("", "R not supported in circular motion", 0, 0);
         Ctx.Put_Line ("R not supported in circular motion");
         return False;
      end if;

      Start_Point := Step_To_Milli (Ctx, Ctx.Real_Position);
      Center      := Start_Point;
      End_Point   := Start_Point;

      if Ctx.B ('X').Is_Set then
         End_Point.X := Ctx.B ('X').Value;
      end if;
      if Ctx.B ('Y').Is_Set then
         End_Point.Y := Ctx.B ('Y').Value;
      end if;
      if Ctx.B ('Z').Is_Set then
         End_Point.Z := Ctx.B ('Z').Value;
      end if;

      if Start_Point.Z /= End_Point.Z then
         Ctx.Log (Warning, "Z movement not supported in circular motion");
      end if;

      if Ctx.B ('I').Is_Set then
         Center.X := Start_Point.X + Ctx.B ('I').Value;
      end if;
      if Ctx.B ('J').Is_Set then
         Center.Y := Start_Point.Y + Ctx.B ('J').Value;
      end if;

      Ctx.Put_Line ("Circle_From " & Image (Start_Point));
      Ctx.Put_Line ("Circle_To " & Image (End_Point));


      Radius := Distance (Center, End_Point);
      Ctx.Put_Line ("Radius: " & Image (Radius));
      Radius2 := Distance (Center, Start_Point);
      Ctx.Put_Line ("Radius2: " & Image (Radius2));

      Travel := Arctan (End_Point.X - Center.X, End_Point.Y - Center.Y)
        - Arctan (Start_Point.X - Center.X, Start_Point.Y - Center.Y);
      if Dir = Clockwise then
         if Travel >= 0.0 then
            Travel := Travel - 1.0 * Pi;
         else
            Travel := Travel + 1.0 * Pi;
         end if;
      end if;

      Ctx.Put_Line ("Travel: " & Image (Travel));
      declare
         From : constant Float_Position := Start_Point - Center;
         To   : constant Float_Position := End_Point - Center;
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

      Angle := Travel / Long_Float (Divisions);
      Cos_A := Cos (Angle);
      Sin_A := Sin (Angle);
      Target := Start_Point;
      for Count in 1 .. Divisions loop
         declare
            Vect : constant Float_Position := Target - Center;
            Rot : Float_Position;
         begin
            --  Rotate direction vector
            Rot.X := Vect.X * Cos_A - Vect.Y * Sin_A;
            Rot.Y := Vect.X * Sin_A + Vect.Y * Cos_A;
            Rot.Z := 0.0;

            Target := Center + Rot;
            if not Move_To (Ctx, Target, Feed_Rate) then
               return False;
            end if;
         end;
      end loop;
      return Move_To (Ctx, End_Point, Feed_Rate);
   end Move_Circle;

--     function Move_Circle_2 (Ctx : in out GContext;
--                             Dir : Circular_Interpolation_Direction;
--                             Feed_Rate : Long_Float) return Boolean is
--        End_Point_F, Center_F : Float_Position;
--        End_Point, Center : Step_Position;
--        --  Dir_X, Dir_Y, Dir_Z : Direction;
--        Radius, Radius2, Err, Dx, Dy, Check : Steps;
--        Sel : Integer;
--        Cnt : Integer := 0;
--     begin
--
--        if not (Ctx.B ('X').Is_Set or else Ctx.B ('Y').Is_Set) then
--           Ctx.Set_Error (Ctx, "", "X or Y is required for circular motion",
--                          0, 0);
--           return False;
--        end if;
--
--        if not (Ctx.B ('I').Is_Set or else Ctx.B ('J').Is_Set) then
--           Ctx.Set_Error (Ctx, "", "I or J is required for circular motion",
--                          0, 0);
--           return False;
--        end if;
--
--        if Ctx.B ('R').Is_Set then
--           Ctx.Set_Error (Ctx, "", "R not supported in circular motion",
--                          0, 0);
--           return False;
--        end if;
--
--        if Ctx.B ('X').Is_Set then
--           End_Point_F.X := Ctx.B ('X').Value;
--        end if;
--        if Ctx.B ('Y').Is_Set then
--           End_Point_F.Y := Ctx.B ('Y').Value;
--        end if;
--        if Ctx.B ('Z').Is_Set then
--           End_Point_F.Z := Ctx.B ('Z').Value;
--        end if;
--
--        if Ctx.B ('I').Is_Set then
--           Center_F.X := Ctx.B ('I').Value;
--        end if;
--        if Ctx.B ('J').Is_Set then
--           Center_F.Y := Ctx.B ('J').Value;
--        end if;
--
--        Center := Ctx.Real_Position + Milli_To_Step (Ctx, Center_F);
--        End_Point := Milli_To_Step (Ctx, End_Point_F);
--        Radius := Distance (Center, End_Point);
--        Radius2 := (Center.X - End_Point.X)**2 + (Center.Y - End_Point.Y)**2;
--
--        if Ctx.Real_Position.Z /= End_Point.Z then
--           Ctx.Set_Error (Ctx, "", "Z movement not supported in circular motion",
--                          0, 0);
--           return False;
--        end if;
--
--
--        --  (Ctx.Real_Position.X, Ctx.Real_Position.Y, 0)
--        Check := Distance (Center, Ctx.Real_Position);
--        Put_Line ("Circular interpolation: Radius (" &
--                    Check'Img & " vs " & Radius'Img & ")");
--  --        Put_Line ("Circular interpolation: Radius (" &
--  --                    Distance (Center_F, End_Point_F)'Img);
--        Put_Line ("Center.X:" & Center.X'Img & " Center.Y:" & Center.Y'Img & " Center.Z:" & Center.Z'Img);
--        Put_Line ("Ctx.Real_Position.X:" & Ctx.Real_Position.X'Img & " Ctx.Real_Position.Y:" & Ctx.Real_Position.Y'Img & " Ctx.Real_Position.Z:" & Ctx.Real_Position.Z'Img);
--        Put_Line ("End_Point.X: " & End_Point.X'Img & " End_Point.Y:" & End_Point.Y'Img & " End_Point.Z:" & End_Point.Z'Img);
--        if Check /= Radius then
--                    Ctx.Set_Error (Ctx, "", "Circular interpolation: Radius missmatch (" &
--                            Check'Img & " vs " & Radius'Img & ")", 0, 0);
--           return False;
--        end if;
--
--        loop
--  --           Put_Line ("Move Circle EndX:" & End_Point.X'Img & " EndY:" & End_Point.Y'Img);
--  --           Put_Line ("            CurX:" & Ctx.Real_Position.X'Img & " CurY:" & Ctx.Real_Position.Y'Img);
--  --           Put_Line ("Distance to end_point: " &
--  --                       Distance (Ctx.Real_Position, End_Point)'Img);
--
--           Err := (Ctx.Real_Position.X - Center.X)**2 +
--             (Ctx.Real_Position.Y - Center.Y)**2 - Radius2;
--           Dx := 2 * (Ctx.Real_Position.X - Center.X);
--           Dy := 2 * (Ctx.Real_Position.Y - Center.Y);
--           Sel := 0;
--           Sel := Sel + (if Dir = Counter_Clockwise then 0 else 2#1000#);
--           Sel := Sel + (if Err < 0 then 0 else 2#0100#);
--           Sel := Sel + (if Dx < 0 then 0 else 2#0010#);
--           Sel := Sel + (if Dy < 0 then 0 else 2#0001#);
--           case Sel is
--           when 2#0000# => Ctx.Do_Step_Y (Ctx, Backward);
--           when 2#0001# => Ctx.Do_Step_X (Ctx, Backward);
--           when 2#0010# => Ctx.Do_Step_X (Ctx, Forward);
--           when 2#0011# => Ctx.Do_Step_Y (Ctx, Forward);
--           when 2#0100# => Ctx.Do_Step_X (Ctx, Forward);
--           when 2#0101# => Ctx.Do_Step_Y (Ctx, Backward);
--           when 2#0110# => Ctx.Do_Step_Y (Ctx, Forward);
--           when 2#0111# => Ctx.Do_Step_X (Ctx, Backward);
--           when 2#1000# => Ctx.Do_Step_X (Ctx, Backward);
--           when 2#1001# => Ctx.Do_Step_Y (Ctx, Forward);
--           when 2#1010# => Ctx.Do_Step_Y (Ctx, Backward);
--           when 2#1011# => Ctx.Do_Step_X (Ctx, Forward);
--           when 2#1100# => Ctx.Do_Step_Y (Ctx, Forward);
--           when 2#1101# => Ctx.Do_Step_X (Ctx, Forward);
--           when 2#1110# => Ctx.Do_Step_X (Ctx, Backward);
--           when 2#1111# => Ctx.Do_Step_Y (Ctx, Backward);
--           when others =>
--              Ctx.Set_Error (Ctx, "", "Unknown Action in circular interpolation",
--                             0, 0);
--              return False;
--           end case;
--           Cnt := Cnt + 1;
--           exit when (Ctx.Real_Position.X = End_Point.X
--                      and then Ctx.Real_Position.Y = End_Point.Y)
--             or else Cnt > 10000;
--        end loop;
--        Put_Line ("Ctx.Real_Position.X:" & Ctx.Real_Position.X'Img & " Ctx.Real_Position.Y:" & Ctx.Real_Position.Y'Img & " Ctx.Real_Position.Z:" & Ctx.Real_Position.Z'Img);
--        return True;
--     end Move_Circle_2;

   --------------------
   -- Return_To_Home --
   --------------------

   function Return_To_Home (Ctx : in out GContext'Class;
                            Feed_Rate : Long_Float) return Boolean is
      X_At_Home, Y_At_Home, Z_At_Home : Boolean := False;
      X_Cnt, Y_Cnt, Z_Cnt : Natural := 0;
   begin
      --  First, go to the intermediate position
      if Ctx.B ('X').Is_Set
        or else
          Ctx.B ('Y').Is_Set
        or else
          Ctx.B ('Z').Is_Set
      then
         if not Move_Line (Ctx, Feed_Rate) then
            return False;
         end if;
      end if;

      while not X_At_Home or else not Y_At_Home or else not Z_At_Home loop
         if not X_At_Home and then not Ctx.Home (X_Axis) then
            Ctx.Step (X_Axis, Backward);
            X_Cnt := X_Cnt + 1;
         else
            X_At_Home := True;
         end if;
         if not Y_At_Home and then not Ctx.Home (Y_Axis) then
            Ctx.Step (Y_Axis, Backward);
            Y_Cnt := Y_Cnt + 1;
         else
            Y_At_Home := True;
         end if;
         if not Z_At_Home and then not Ctx.Home (Z_Axis) then
            Ctx.Step (Z_Axis, Forward);
            Z_Cnt := Z_Cnt + 1;
         else
            Z_At_Home := True;
         end if;
      end loop;
--        while not Ctx.At_Home (Ctx, X_Axis) loop
--           Ctx.Do_Step_X (Ctx, Backward);
--        end loop;
--        while not Ctx.At_Home (Ctx, Y_Axis) loop
--           Ctx.Do_Step_Y (Ctx, Backward);
--        end loop;
--        while not Ctx.At_Home (Ctx, Z_Axis) loop
--           Ctx.Do_Step_Z (Ctx, Forward);
--        end loop;

      --  Set known home position
      Ctx.Step_Per_Millimeter.X := Long_Float (X_Cnt) / 38.5;
      Ctx.Step_Per_Millimeter.Y := Long_Float (Y_Cnt) / 37.0;
      Ctx.Step_Per_Millimeter.Z := Long_Float (Z_Cnt) / 15.0;
      Ctx.Real_Position := (0, 0, Z_Cnt);
      return True;
   end Return_To_Home;

   -------------
   -- Execute --
   -------------

   function Execute (Line : String; Ctx : in out GContext'Class)
                     return Boolean
   is
      Int_Part, Frac_Part : Integer;
      pragma Unreferenced (Frac_Part);
      Feed : Long_Float;
   begin
      if Ctx.B ('M').Is_Set then
         Int_Part := Integer (Long_Float'Floor (Ctx.B ('M').Value));
         Frac_Part := Integer (Long_Float'Floor (Ctx.B ('M').Value * 100.0));
         case Int_Part is
            when others =>
               Ctx.Report_Error (Line, "Unknown M code " & Int_Part'Img, 0, 0);
               return False;
         end case;
      end if;

      if Ctx.B ('F').Is_Set then
         Ctx.Current_Feed_Rate := Ctx.B ('F').Value;
      end if;

      Feed := Ctx.Current_Feed_Rate;
      if Ctx.Unit = Inches then
         Ctx.B('X').Value := Inch_To_Milli (Ctx.B('X').Value);
         Ctx.B('Y').Value := Inch_To_Milli (Ctx.B('Y').Value);
         Ctx.B('Z').Value := Inch_To_Milli (Ctx.B('Z').Value);
         Ctx.B('I').Value := Inch_To_Milli (Ctx.B('I').Value);
         Ctx.B('J').Value := Inch_To_Milli (Ctx.B('J').Value);
         Ctx.B('R').Value := Inch_To_Milli (Ctx.B('R').Value);
      end if;

      if Ctx.B ('G').Is_Set then
         Int_Part := Integer (Long_Float'Floor (Ctx.B ('G').Value));
         Frac_Part := Integer (Long_Float'Floor (Ctx.B ('G').Value * 100.0));
         case Int_Part is
            when 0 =>
               return Move_Line (Ctx, Ctx.Fast_Feed_Rate);
            when 1 =>
               return Move_Line (Ctx, Feed);
            when 2 =>
               return Move_Circle (Ctx, Clockwise, Feed);
            when 3 =>
               return Move_Circle (Ctx, Counter_Clockwise, Feed);
            when 20 => Ctx.Unit := Inches;
            when 21 => Ctx.Unit := Millimeters;
            when 28 => return Return_To_Home (Ctx, Feed);
            when others =>
               Ctx.Report_Error (Line, "Unknown G code " & Int_Part'Img, 0, 0);
               return False;
         end case;
      end if;
      return True;
   end;
end Gcode.Execution;
