with Bounded_Buffers_Blocking_Consumer;
with Bounded_Buffers_Blocking_Producer;
with System;
with Settings;

package body Gcode.Planner is

   Acceleration_Ticks_Per_Second : constant := 100;
   Max_Segment_Time_Second : constant := 1.0 / Acceleration_Ticks_Per_Second;

   use type Float_Position;
   use type Step_Position;

   type Motion_Kind is (Motion_Line, Motion_Dwell, Motion_Homing);

   type Motion_Block is record
      Kind             : Motion_Kind;
      Target           : Float_Position;
      Dwell_Duration   : Duration;

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
     new Bounded_Buffers_Blocking_Consumer (Motion_Block, 128,
                                            System.Default_Priority + 1);
   package Segment_Buffer_Package is
     new Bounded_Buffers_Blocking_Producer (Segment, 64,
                                            System.Default_Priority + 2);

   Motion_Block_Buffer : Motion_Buffer_Package.Bounded_Buffer;
   Segment_Block_Buffer : Segment_Buffer_Package.Bounded_Buffer;

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
             / Settings.Step_Per_Millimeter (Axis);

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

      Wait_And_Add_Motion (M_Block);

      --  Update planner position
      Planner_Position := Target_Steps;
   end Planner_Add_Motion;

   -----------------------
   -- Planner_Add_Dwell --
   -----------------------

   procedure Planner_Add_Dwell
     (Ctx            : in out GContext'Class;
      Dwell_Duration : Duration)
   is
      pragma Unreferenced (Ctx);
      M_Block : Motion_Block;
   begin
      M_Block.Kind := Motion_Dwell;
      M_Block.Dwell_Duration := Dwell_Duration;
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

   function Get_Next_Segment (Seg : out Segment) return Boolean is
   begin
      if not Segment_Block_Buffer.Empty then
         Segment_Block_Buffer.Remove (Seg);
         return True;
      else
         return False;
      end if;
   end Get_Next_Segment;

   task Planner_Task is
      pragma Priority (System.Default_Priority + 1);
   end Planner_Task;

   task body Planner_Task is
      Motion : Motion_Block;
      Seg : Segment;
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

         --  Blocking call
         Motion_Block_Buffer.Remove (Motion);


         case Motion.Kind is
            when Motion_Dwell =>
               Seg.New_Block := True;
               Seg.Homing := False;

               --  100Hz gives us a 10ms precision
               Seg.Frequency :=
                 Duration'Max (100.0, Settings.Idle_Stepper_Frequency);

               Seg.Step_Count :=
                 Steps (Motion.Dwell_Duration * Seg.Frequency);

               --  No axis is moving
               Seg.Block_Steps := (others => 0);
               Seg.Block_Event_Count := Steps'Last;

               --  Blocking call
               Segment_Block_Buffer.Insert (Seg);

            when Motion_Homing =>
               Seg.Homing := True;
               Seg.New_Block := True;

               --  Blocking call
               Segment_Block_Buffer.Insert (Seg);

            when Motion_Line =>
               Remaining_Steps := Motion.Step_Event_Count;

               Step_Per_Milli :=
                 Float_Value (Remaining_Steps) / Motion.Remaining_Distance;

               --  Signal this segment as first of the new block
               Seg.New_Block := True;
               Seg.Homing := False;

               --  Segment values that will remain for the entire block
               Seg.Block_Steps := Motion.Relative_Steps;
               Seg.Block_Event_Count := Motion.Step_Event_Count;
               Seg.Directions := Motion.Directions;
               while Remaining_Steps > 0 loop
                  Seg_Time := Max_Segment_Time_Second;

                  --  Dummy block spliting until cleaver speed profile is
                  --  implemented...
                  Seg.Frequency   := 1_000.0;
                  Seg.Step_Count  := Steps'Min (Remaining_Steps, 500);
                  Remaining_Steps := Remaining_Steps - Seg.Step_Count;

                  --  Blocking call
                  Segment_Block_Buffer.Insert (Seg);

                  --  Next segements are not first of the block
                  Seg.New_Block := False;

               end loop;
         end case;
      end loop;
   end Planner_Task;
end Gcode.Planner;
