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

with Bounded_Buffers_Blocking_Consumer;
with Bounded_Buffers_Blocking_Producer;
with System;
with Settings; use Settings;

package body Gcode.Planner is

   use type Float_Position;
   use type Step_Position;

   type Motion_Kind is (Motion_Line, Motion_Dwell, Motion_Homing,
                        Motion_Enable_Motors);

   type Motion_Block (Kind : Motion_Kind := Motion_Line) is record
      case Kind is
         when Motion_Line =>
            Target             : Float_Position;

            Abs_Relative_Steps : Step_Position;
            --  Absolute number of steps for each axis

            Directions         : Axis_Directions := (others => Forward);
            --  Step direction for each axis

            Step_Event_Count   : Steps := 0;
            --  Number of steps required to complete this block

            Entry_Speed        : Step_Speed := 0.0;
            Nominal_Speed      : Step_Speed := 0.0;
            Acceleration       : Step_Acceleration := Step_Acceleration'Last;
         when Motion_Dwell =>
            Dwell_Duration     : Float_Value;
         when Motion_Homing =>
            null;
         when Motion_Enable_Motors =>
            Enable : Motor_Enable_Array;
      end case;
   end record;

   package Motion_Buffer_Package is
     new Bounded_Buffers_Blocking_Consumer (Motion_Block,
                                            Settings.Block_Buffer_Size,
                                            System.Default_Priority + 1);
   package Segment_Buffer_Package is
     new Bounded_Buffers_Blocking_Producer (Segment,
                                            Settings.Segment_Buffer_Size,
                                            System.Default_Priority + 2);

   Motion_Block_Buffer : Motion_Buffer_Package.Bounded_Buffer;
   Segment_Block_Buffer : Segment_Buffer_Package.Bounded_Buffer;

   Planner_Position : Step_Position :=
     Milli_To_Step (Settings.Home_Coordinate);

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
      pragma Unreferenced (Ctx);
      M_Block : Motion_Block (Kind => Motion_Line);
      Target_Steps : constant Step_Position := Milli_To_Step (Target);
      Relative_Steps : Step_Position := Target_Steps - Planner_Position;
      Delta_MM : Float_Value;

      Unit_Vect : Float_Position;
      pragma Unreferenced (Unit_Vect);
      --  Unit vector of the block used for junction angle computation
   begin

      if Settings.Use_CoreXY_Motion then
         declare
            DX, DY, DA, DB : Steps;
         begin
            DX := Relative_Steps (X_Axis);
            DY := Relative_Steps (Y_Axis);

            DA := DX + DY;
            DB := DX - DY;

            Relative_Steps (X_Axis) := DA;
            Relative_Steps (Y_Axis) := DB;
         end;
      end if;

      M_Block.Abs_Relative_Steps := abs Relative_Steps;

      for Axis in Axis_Name loop
         M_Block.Step_Event_Count :=
           Steps'Max (M_Block.Step_Event_Count,
                      M_Block.Abs_Relative_Steps (Axis));

         --  Distance traveled in milimeters
         Delta_MM :=
           Float_Value (Relative_Steps (Axis))
             / Settings.Step_Per_Millimeter (Axis);

         --  Direction of movement for this axis
         M_Block.Directions (Axis) :=
           (if Delta_MM < 0.0 then Backward else Forward);

         Unit_Vect (Axis) := Delta_MM;
      end loop;

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
      Dwell_Duration : Float_Value)
   is
      pragma Unreferenced (Ctx);
      M_Block : Motion_Block (Kind => Motion_Dwell);
   begin
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
      M_Block : Motion_Block (Kind => Motion_Homing);
   begin
      Planner_Position := Milli_To_Step (Settings.Home_Coordinate);
      Wait_And_Add_Motion (M_Block);
   end Planner_Add_Homing;

   ---------------------------
   -- Planner_Enable_Motors --
   ---------------------------

   procedure Planner_Enable_Motors
     (Ctx    : in out GContext'Class;
      Enable : Motor_Enable_Array)
   is
      pragma Unreferenced (Ctx);
      M_Block : Motion_Block (Kind => Motion_Enable_Motors);
   begin
      M_Block.Enable := Enable;
      Wait_And_Add_Motion (M_Block);
   end Planner_Enable_Motors;

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
      Motion               : Motion_Block;
      Seg                  : Segment;
      Remaining_Steps      : Steps;
      Remaining_Seg        : Natural;
      Seg_Required_To_Stop : Integer;
   begin
      loop

         --  Blocking call
         Motion_Block_Buffer.Remove (Motion);


         case Motion.Kind is
            when Motion_Dwell =>
               Seg.New_Block := True;

               --  100Hz gives us a 10ms precision
               Seg.Frequency :=
                 Frequency_Value'Max (100.0, Settings.Idle_Stepper_Frequency);

               Seg.Step_Count :=
                 Steps (Motion.Dwell_Duration * Seg.Frequency);

               --  No axis is moving
               Seg.Abs_Block_Steps := (others => 0);
               Seg.Block_Event_Count := Steps'Last;

               --  Blocking call
               Segment_Block_Buffer.Insert
                 ((Kind => Dwell_Segment,
                   Dwell_Duration => Motion.Dwell_Duration));

            when Motion_Homing =>

               --  Blocking call
               Segment_Block_Buffer.Insert ((Kind => Homing_Segment));

            when Motion_Enable_Motors =>
               Segment_Block_Buffer.Insert ((Kind => Enable_Motors_Segment,
                                             Enable => Motion.Enable));


            when Motion_Line =>
               Remaining_Steps := Motion.Step_Event_Count;

               --  Signal this segment as first of the new block
               Seg.New_Block := True;

               --  Segment values that will remain for the entire block
               Seg.Abs_Block_Steps := Motion.Abs_Relative_Steps;
               Seg.Block_Event_Count := Motion.Step_Event_Count;
               Seg.Directions := Motion.Directions;


               --  Start speed
               Seg.Frequency := Stepper_Min_Frequency;

               while Remaining_Steps > 0 loop

                  --  Remaining number of segement in the block
                  Remaining_Seg := Remaining_Steps / Max_Step_Per_Segment;


                  --  Given the current speed, number of segments required to
                  --  stop.
                  Seg_Required_To_Stop :=
                    Natural (Seg.Frequency / Acceleration_Freq);


                  if Remaining_Seg < Seg_Required_To_Stop then

                     --  Deceleration
                     Seg.Frequency := Seg.Frequency - Acceleration_Freq;

                  elsif Seg.Frequency < Stepper_Max_Frequency then

                     --  Acceleration
                     Seg.Frequency := Seg.Frequency + Acceleration_Freq;

                  end if;

                  --  Check that we don't completely stop the stepper
                  if Seg.Frequency < Stepper_Min_Frequency then
                     Seg.Frequency := Stepper_Min_Frequency;
                  end if;

                  Seg.Step_Count  := Steps'Min (Remaining_Steps,
                                                Max_Step_Per_Segment);
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
