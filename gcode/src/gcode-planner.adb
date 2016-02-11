--  with Bounded_Buffers;

package body Gcode.Planner is

   use type Float_Position;
   use type Step_Position;

--     type Motion_Block is record
--        Target    : Float_Position;
--        Feed_Rate : Float_Value;
--     end record;

--     package Buffer_Package is new Bounded_Buffers (Motion_Block);
--     use Buffer_Package;
--
--     Motion_Block_Buffer : Bounded_Buffer (16, Default_Ceiling);

   ------------------------
   -- Planner_Add_Motion --
   ------------------------

   procedure Planner_Add_Motion
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Float_Value)
   is
      pragma Unreferenced (Feed_Rate);
      D_Step, Cmd : Step_Position;
      D, Absolute : Float_Position;
      Err1, Err2 : Float_Value;
      Dir_X, Dir_Y, Dir_Z : Direction;
   begin
--        Motion_Block_Buffer.Insert ((Target    => Target,
--                                     Feed_Rate => Feed_Rate));
      Ctx.Log (Info, "Move_To " & Image (Target));
      Cmd := Milli_To_Step (Ctx, Target);

      D_Step := Cmd - Ctx.Real_Position;
      D := (Float_Value (D_Step.X),
            Float_Value (D_Step.Y),
            Float_Value (D_Step.Z));
      Absolute := abs D;

      Ctx.Put_Line ("Move_To steps" & Image (D_Step));
      if D_Step = (0, 0, 0) then
         --  Nothing to do...
         return;
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
      return;
   end Planner_Add_Motion;

end Gcode.Planner;
