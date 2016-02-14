with Bounded_Buffers_Blocking_Consumer;
with Bounded_Buffers_Blocking_Producer;
with Ada.Text_IO; use Ada.Text_IO;

package body Gcode.Planner is

   Acceleration_Ticks_Per_Second : constant := 100;
   Max_Segment_Time_Second : constant := 1.0 / Acceleration_Ticks_Per_Second;

   use type Float_Position;
   use type Step_Position;

   type Motion_Kind is (Motion_Line, Motion_Dwell, Motion_Homing);

   type Motion_Block is record
      Kind             : Motion_Kind;
      Target           : Float_Position;
      Dwell_Duration   : Float_Value;

      Relative_Steps   : Step_Position;
      --  Steps for each axis

      Directions       : Axis_Directions := (others => Forward);
      --  Step direction for each axis

      Step_Event_Count : Steps := 0;
      --  Number of steps required to complete this block

      Entry_Speed   : Step_Speed := 0.0;
      Nominal_Speed : Step_Speed := 0.0;
      Acceleration  : Step_Acceleration := Step_Acceleration'Last;
      Remaining_Distance : Float_Value := 0.0;
   end record;

   package Motion_Buffer_Package is
     new Bounded_Buffers_Blocking_Consumer (Motion_Block);
   package Segment_Buffer_Package is
     new Bounded_Buffers_Blocking_Producer (Segment_Block);

   Motion_Block_Buffer : Motion_Buffer_Package.Bounded_Buffer
     (128, Motion_Buffer_Package.Default_Ceiling);
   Segment_Block_Buffer : Segment_Buffer_Package.Bounded_Buffer
     (64, Segment_Buffer_Package.Default_Ceiling);

   Planner_Position : Step_Position;

   procedure Wait_And_Add_Motion (M_Block : Motion_Block);

   -------------------------
   -- Wait_And_Add_Motion --
   -------------------------

   procedure Wait_And_Add_Motion (M_Block : Motion_Block) is
   begin
      while Motion_Block_Buffer.Full loop
         null;
      end loop;
      Motion_Block_Buffer.Insert (M_Block);
   end Wait_And_Add_Motion;

   ------------------------
   -- Planner_Add_Motion --
   ------------------------

   procedure Planner_Add_Motion
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Step_Speed)
   is
      M_Block : Motion_Block;
      Target_Steps : constant Step_Position := Milli_To_Step (Ctx, Target);
      Delta_MM : Float_Value;

      Unit_Vect : Float_Position;
      pragma Unreferenced (Unit_Vect);
      --  Unit vector of the block used for junction angle computation
   begin
      M_Block.Kind := Motion_Line;

      M_Block.Relative_Steps := abs (Target_Steps - Planner_Position);

      for Axis in Axis_Name loop
         M_Block.Step_Event_Count :=
           Steps'Max (M_Block.Step_Event_Count,
                      M_Block.Relative_Steps (Axis));

         --  Distance traveled in milimeters
         Delta_MM :=
           Float_Value (Target_Steps (Axis) - Planner_Position (Axis))
             / Ctx.Step_Per_Millimeter (Axis);

         --  Direction of movement for this axis
         M_Block.Directions (Axis) :=
           (if Delta_MM < 0.0 then Backward else Forward);

         --  Add square of each axis to compute distance
         M_Block.Remaining_Distance :=
           M_Block.Remaining_Distance + Delta_MM**2;

         Unit_Vect (Axis) := Delta_MM;
      end loop;

      --  Compute distance
      M_Block.Remaining_Distance := Sqrt (M_Block.Remaining_Distance);

      if M_Block.Step_Event_Count = 0 then
         --  zero-length block
         return;
      end if;

      --  Someday I'll do something very clever here...
      M_Block.Entry_Speed := 0.0;
      M_Block.Nominal_Speed := Feed_Rate;

      Put_Line ("Insert new block relative absolute steps:" &
                  Image (M_Block.Relative_Steps));
      Wait_And_Add_Motion (M_Block);

      --  Update planner position
      Planner_Position := Target_Steps;
   end Planner_Add_Motion;

   --     ------------------------
--     -- Planner_Add_Motion --
--     ------------------------
--
--     procedure Planner_Add_Motion
--       (Ctx       : in out GContext'Class;
--        Target    : Float_Position;
--        Feed_Rate : Float_Value)
--     is
--        D_Step, Cmd : Step_Position;
--        D, Absolute : Float_Position;
--        Err1, Err2 : Float_Value;
--        Dir_X, Dir_Y, Dir_Z : Direction;
--     begin
--        Wait_And_Add_Motion ((Kind      => Motion_Line,
--                             Target     => Target,
--                             Feed_Rate  => Feed_Rate));
--
--        Ctx.Log (Info, "Move_To " & Image (Target));
--        Cmd := Milli_To_Step (Ctx, Target);
--
--        D_Step := Cmd - Ctx.Real_Position;
--        D := (Float_Value (D_Step.X),
--              Float_Value (D_Step.Y),
--              Float_Value (D_Step.Z));
--        Absolute := abs D;
--
--        Ctx.Put_Line ("Move_To steps" & Image (D_Step));
--        if D_Step = (0, 0, 0) then
--           --  Nothing to do...
--           return;
--        end if;
--
--        Dir_X := (if D_Step.X >= 0 then Forward else Backward);
--        Dir_Y := (if D_Step.Y >= 0 then Forward else Backward);
--        Dir_Z := (if D_Step.Z >= 0 then Forward else Backward);
--
--        if Absolute.X >= Absolute.Z and then Absolute.X >= Absolute.Y then
--           Err1 := Absolute.X / 2.0;
--           Err2 := Absolute.X / 2.0;
--
--           for Cnt in 0 .. Integer (Absolute.X) - 1 loop
--              Err1 := Err1 - Absolute.Y;
--              Err2 := Err2 - Absolute.Z;
--              Ctx.Step (X_Axis, Dir_X);
--              if Err1 < 0.0 then
--                 Ctx.Step (Y_Axis, Dir_Y);
--                 Err1 := Err1 + Absolute.X;
--              end if;
--              if Err2 < 0.0 then
--                 Ctx.Step (Z_Axis, Dir_Z);
--                 Err2 := Err2 + Absolute.X;
--              end if;
--           end loop;
--        elsif Absolute.Y >= Absolute.Z and then Absolute.Y >=  Absolute.X
--        then
--           Err1 := Absolute.Y / 2.0;
--           Err2 := Absolute.Y / 2.0;
--
--           for Cnt in 0 .. Integer (Absolute.Y) - 1 loop
--              Err1 := Err1 - Absolute.X;
--              Err2 := Err2 - Absolute.Z;
--              Ctx.Step (Y_Axis, Dir_Y);
--              if Err1 < 0.0 then
--                 Ctx.Step (X_Axis, Dir_X);
--                 Err1 := Err1 + Absolute.Y;
--              end if;
--              if Err2 < 0.0 then
--                 Ctx.Step (Z_Axis, Dir_Z);
--                 Err2 := Err2 + Absolute.Y;
--              end if;
--           end loop;
--        else
--           Err1 := Absolute.Z / 2.0;
--           Err2 := Absolute.Z / 2.0;
--
--           for Cnt in 0 .. Integer (Absolute.Z) - 1 loop
--              Err1 := Err1 - Absolute.X;
--              Err2 := Err2 - Absolute.Y;
--              Ctx.Step (Z_Axis, Dir_Z);
--              if Err1 < 0.0 then
--                 Ctx.Step (X_Axis, Dir_X);
--                 Err1 := Err1 + Absolute.Z;
--              end if;
--              if Err2 < 0.0 then
--                 Ctx.Step (Y_Axis, Dir_Y);
--                 Err2 := Err2 + Absolute.Z;
--              end if;
--           end loop;
--        end if;
--
--        --  Make sure that we really go to the exact target position
--        while Ctx.Real_Position /= Cmd loop
--           Ctx.Put_Line ("Ctx.Real_Position " & Image (Ctx.Real_Position));
--           Ctx.Put_Line ("Cmd " & Image (Cmd));
--
--           Ctx.Put_Line ("Correction loop");
--           if Ctx.Real_Position.X < Cmd.X then
--              Ctx.Put_Line ("X Fwd");
--              Ctx.Step (X_Axis, Forward);
--           elsif Ctx.Real_Position.X > Cmd.X then
--              Ctx.Put_Line ("X Bwd");
--              Ctx.Step (X_Axis, Backward);
--           end if;
--           if Ctx.Real_Position.Y < Cmd.Y then
--              Ctx.Put_Line ("Y Fwd");
--              Ctx.Step (Y_Axis, Forward);
--           elsif Ctx.Real_Position.Y > Cmd.Y then
--              Ctx.Put_Line ("Y Bwd");
--              Ctx.Step (Y_Axis, Backward);
--           end if;
--           if Ctx.Real_Position.Z < Cmd.Z then
--              Ctx.Put_Line ("Z Fwd");
--              Ctx.Step (Z_Axis, Forward);
--           elsif Ctx.Real_Position.Z > Cmd.Z then
--              Ctx.Put_Line ("Z Bwd");
--              Ctx.Step (Z_Axis, Backward);
--           end if;
--        end loop;
--        Ctx.Put_Line ("End Of Move_To");
--        return;
--     end Planner_Add_Motion;

   -----------------------
   -- Planner_Add_Dwell --
   -----------------------

   procedure Planner_Add_Dwell
     (Ctx       : in out GContext'Class;
      Duration  : Float_Value)
   is
      pragma Unreferenced (Ctx);
      M_Block : Motion_Block;
   begin
      M_Block.Kind := Motion_Dwell;
      M_Block.Dwell_Duration := Duration;
      Wait_And_Add_Motion (M_Block);
   end Planner_Add_Dwell;

   ------------------------
   -- Planner_Add_Homing --
   ------------------------

   procedure Planner_Add_Homing
     (Ctx       : in out GContext'Class;
      Feed_Rate : Step_Speed)
   is
      pragma Unreferenced (Ctx, Feed_Rate);
      M_Block : Motion_Block;
   begin
      M_Block.Kind := Motion_Homing;
      Wait_And_Add_Motion (M_Block);
   end Planner_Add_Homing;

   ----------------------
   -- Get_Next_Segment --
   ----------------------

   function Get_Next_Segment (Segment : out Segment_Block) return Boolean is
   begin
      if not Segment_Block_Buffer.Empty then
         Segment_Block_Buffer.Remove (Segment);
         return True;
      else
         return False;
      end if;
   end Get_Next_Segment;

   task Planner_Task is
   end Planner_Task;

   task body Planner_Task is
      Motion : Motion_Block;
      Segment : Segment_Block;
      Remaining_Steps : Steps;
      Step_Per_Milli : Float_Value;
      pragma Unreferenced (Step_Per_Milli);
--
--        Current_Speed : Step_Speed;
--        Cruise_Speed  : Step_Speed;
--        Exit_Speed    : Step_Speed;
--
      Seg_Time : Float_Value;
      pragma Unreferenced (Seg_Time);
   begin
      loop

         Put_Line ("Planner: Trying to get a new block");
         --  Blocking call
         Motion_Block_Buffer.Remove (Motion);

         Put_Line ("Planner: Take new motion block (Step event:" &
                     Motion.Step_Event_Count'Img & ")");
         case Motion.Kind is
            when Motion_Dwell =>
               --  Not implemented
               raise Program_Error;
            when Motion_Homing =>
               --  Not implemented
               raise Program_Error;
            when Motion_Line =>
               Remaining_Steps := Motion.Step_Event_Count;

               Step_Per_Milli :=
                 Float_Value (Remaining_Steps) / Motion.Remaining_Distance;

               --  Signal this segment as first of the new block
               Segment.New_Block := True;
               while Remaining_Steps > 0 loop
                  Seg_Time := Max_Segment_Time_Second;

                  --  Dummy block spliting until cleaver speed profile is
                  --  implemented...
                  Segment.Step_Count := Steps'Min (Remaining_Steps, 500);
                  Remaining_Steps := Remaining_Steps - Segment.Step_Count;

                  Segment.Directions := Motion.Directions;

                  Put_Line ("Planner: Insert new segment");
                  --  Blocking call
                  Segment_Block_Buffer.Insert (Segment);

                  --  Next segements are not first of the block
                  Segment.New_Block := False;

               end loop;
               Put_Line ("Planner: End of Block procesing");
         end case;
      end loop;
   end Planner_Task;
end Gcode.Planner;
