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

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;
with Gcode.Planner; use Gcode.Planner;
with System;
with Settings; use Settings;

package body Stepper is

   --  Translation to CoreXY limits
   Stepper_Max_Limit : constant Gcode.Step_Position :=
     (if Settings.Use_CoreXY_Motion then
        (X_Axis => Tool_Max_Limit_Cartesian (X_Axis) + Tool_Max_Limit_Cartesian (Y_Axis),
         Y_Axis => Tool_Max_Limit_Cartesian (X_Axis) - Tool_Min_Limit_Cartesian (Y_Axis),
         Z_Axis => Tool_Max_Limit_Cartesian (Z_Axis))
      else
         Tool_Max_Limit_Cartesian);

   Stepper_Min_Limit : constant Gcode.Step_Position :=
     (if Settings.Use_CoreXY_Motion then
        (X_Axis => Tool_Min_Limit_Cartesian (X_Axis) + Tool_Min_Limit_Cartesian (Y_Axis),
         Y_Axis => Tool_Min_Limit_Cartesian (X_Axis) - Tool_Max_Limit_Cartesian (Y_Axis),
         Z_Axis => Tool_Min_Limit_Cartesian (Z_Axis))
      else Tool_Min_Limit_Cartesian);

   type Homing_Cycle_State is (Homing_Approach,
                               Homing_Back,
                               Homing_Precision);

   type Do_Step_Array is array (Axis_Name) of Boolean;

   procedure Dummy_Set_Step_Pin (Axis : Axis_Name) is null;
   procedure Dummy_Clear_Step_Pin  (Axis : Axis_Name) is null;
   procedure Dummy_Set_Direction_Pin (Axis : Axis_Name;
                                      Dir : Direction) is null;
   procedure Dummy_Set_Stepper_Frequency (Freq_Hz : Frequency_Value) is null;
   function Dummy_Home_Test (Unused : Axis_Name) return Boolean is (False);
   procedure Dummy_Motor_Enable  (Axis : Axis_Name; Enable : Boolean) is null;

   procedure Setup_Homing;
   function Homing return Boolean;
   procedure Reset_To_Home_Coordinates;

   type Stepper_Data_Type is record
      Has_Segment : Boolean := False;
      --  Does the stepper still has a segment to execute

      Seg : Segment;

      Step_Count : Steps;
      Counter : Step_Position;

      Do_Step : Do_Step_Array := (others => False);
      --  Tells which axis has to do a step at the next iteration

      Directions       : Axis_Directions := (others => Forward);
      --  Direction of steps for eaxh axis

      Abs_Block_Steps : Step_Position;
      --  Absolute number of steps for the current Motion block each axis
      Block_Event_Count : Steps;
      --  Step count for the current block

      Set_Step_Callback : Set_Step_Pin_Proc :=
        Dummy_Set_Step_Pin'Access;

      Clear_Step_Callback : Clear_Step_Pin_Proc :=
        Dummy_Clear_Step_Pin'Access;

      Set_Direction_Callback : Set_Direction_Pin_Proc :=
        Dummy_Set_Direction_Pin'Access;

      Set_Stepper_Frequency_Callback : Set_Stepper_Frequency_Proc :=
        Dummy_Set_Stepper_Frequency'Access;

      Home_Test_Callback : Home_Test_Proc :=
        Dummy_Home_Test'Access;

      Motor_Enable_Callback : Motor_Enable_Proc :=
        Dummy_Motor_Enable'Access;

      Current_Position : Step_Position := (others => 0);
      --  Keep track of the actuall position of the machine

      Homing_State : Homing_Cycle_State;
      --  Curent state in homing procedure

      Homing_Axis : Axis_Name;
      --  Current homing axis

      Homing_Order_Index : Settings.Homing_Order_Range;
      --  Index of current homing axis in the Homing_Order array

      Dwell_Timeout : Time := Time_First;
      --  Expiration time of the dwell command
   end record;

   St_Data : Stepper_Data_Type;

   type Step_Timing_Event is new Timing_Event with record
      Do_Step : Do_Step_Array;
   end record;

   ----------------
   -- Step_Pulse --
   ----------------

   protected Step_Pulse is
      pragma Priority (System.Interrupt_Priority'Last);

      procedure Start_Step_Cycle (Do_Step : Do_Step_Array;
                                  Directions  : Axis_Directions);
      procedure Set_Step_Pins (Event : in out Timing_Event);
      procedure Clear_Step_Pins (Event : in out Timing_Event);
   private
      Set_Event   : Step_Timing_Event;
      Clear_Event : Step_Timing_Event;
   end Step_Pulse;

   protected body Step_Pulse is
      ----------------------
      -- Start_Step_Cycle --
      ----------------------

      procedure Start_Step_Cycle (Do_Step     : Do_Step_Array;
                                  Directions  : Axis_Directions)
      is
         Now : Time;
         Direction_Delay : constant Time_Span :=
           Settings.Direction_Pulse_Delay;

         Step_Delay      : constant Time_Span :=
           Settings.Step_Pulse_Duration;
      begin
         Step_Pulse.Set_Event.Do_Step := Do_Step;
         Step_Pulse.Clear_Event.Do_Step := Do_Step;

         --  Set direction pins now
         for Axis in Axis_Name loop
            --  Set_Direction pin
            St_Data.Set_Direction_Callback (Axis, Directions (Axis));
         end loop;

         Now := Clock;

         if Direction_Delay = Microseconds (0) then
            --  Set step pins imediatly
            Set_Step_Pins (Timing_Event (Set_Event));
         else
            --  Schedule the timming evnet that will set the step pins
            Set_Handler (Set_Event, Now + Direction_Delay,
                         Set_Step_Pins'Access);
         end if;

         if Direction_Delay = Microseconds (0)
           and then
             Step_Delay = Microseconds (0)
         then
            --  Clear step pins imediatly
            Clear_Step_Pins (Timing_Event (Clear_Event));
         else
            --  Schedule the timming evnet that will clear the step pins
            Set_Handler (Clear_Event,  Now + Direction_Delay + Step_Delay,
                         Clear_Step_Pins'Access);
         end if;
      end Start_Step_Cycle;

      -------------------
      -- Set_Step_Pins --
      -------------------

      procedure Set_Step_Pins (Event : in out Timing_Event) is
         Do_Step : constant Do_Step_Array :=
           Step_Timing_Event (Timing_Event'Class (Event)).Do_Step;
      begin
         for Axis in Axis_Name loop
            if Do_Step (Axis) then
               St_Data.Set_Step_Callback (Axis);
            end if;
         end loop;
      end Set_Step_Pins;

      ---------------------
      -- Clear_Step_Pins --
      ---------------------

      procedure Clear_Step_Pins (Event : in out Timing_Event) is
         Do_Step : constant Do_Step_Array :=
           Step_Timing_Event (Timing_Event'Class (Event)).Do_Step;
      begin
         for Axis in Axis_Name loop
            if Do_Step (Axis) then
               St_Data.Clear_Step_Callback (Axis);
            end if;
         end loop;
      end Clear_Step_Pins;
   end Step_Pulse;

   ------------------
   -- Setup_Homing --
   ------------------

   procedure Setup_Homing is
   begin
      St_Data.Homing_Axis  :=
        Settings.Homing_Order (St_Data.Homing_Order_Index);

      St_Data.Set_Stepper_Frequency_Callback
        (Frequency_Value (Homing_Approach_Feed_Rate *
             Step_Per_Millimeter (St_Data.Homing_Axis)));

      St_Data.Homing_State := Homing_Approach;
   end Setup_Homing;

   ------------
   -- Homing --
   ------------

   function Homing return Boolean is
   begin
      case St_Data.Homing_State is
         when Homing_Approach =>
            if St_Data.Home_Test_Callback (St_Data.Homing_Axis) then
               St_Data.Homing_State := Homing_Back;
            end if;

         when Homing_Back =>
            if not St_Data.Home_Test_Callback (St_Data.Homing_Axis) then
               St_Data.Homing_State := Homing_Precision;

               --  Switch to precission feed rate

               St_Data.Set_Stepper_Frequency_Callback
                 (Frequency_Value (Homing_Precision_Feed_Rate *
                      Step_Per_Millimeter (St_Data.Homing_Axis)));

            end if;

         when Homing_Precision =>
            if St_Data.Home_Test_Callback (St_Data.Homing_Axis) then

               --  Reset step instruction
               St_Data.Do_Step := (others => False);

               if
                 St_Data.Homing_Order_Index = Settings.Homing_Order_Range'Last
               then
                  --  End of homing
                  St_Data.Has_Segment := False;

                  Reset_To_Home_Coordinates;
               else
                  --  Start homing cycle for next axis
                  St_Data.Homing_Order_Index := St_Data.Homing_Order_Index + 1;
                  Setup_Homing;
               end if;
            end if;
      end case;

      --  Compute the next steps/directions for homming

      St_Data.Do_Step := (others => False);

      if Settings.Use_CoreXY_Motion
        and then
         St_Data.Homing_Axis /= Z_Axis
      then

         pragma Warnings (Off, "condition can only be");
         pragma Warnings (Off, "condition is always");
         case St_Data.Homing_Axis is
            when X_Axis =>
               St_Data.Do_Step (X_Axis) := True;
               St_Data.Do_Step (Y_Axis) := True;

               if (Settings.Homing_Directions (X_Axis) = Forward
                   and then
                   St_Data.Homing_State /= Homing_Back)
                 or else
                  (Settings.Homing_Directions (X_Axis) = Backward
                   and then
                   St_Data.Homing_State = Homing_Back)
               then
                  St_Data.Directions (X_Axis) := Forward;
                  St_Data.Directions (Y_Axis) := Forward;
               else
                  St_Data.Directions (X_Axis) := Backward;
                  St_Data.Directions (Y_Axis) := Backward;
               end if;

            when Y_Axis =>
               St_Data.Do_Step (X_Axis) := True;
               St_Data.Do_Step (Y_Axis) := True;

               if (Settings.Homing_Directions (Y_Axis) = Forward
                   and then
                   St_Data.Homing_State /= Homing_Back)
                 or else
                  (Settings.Homing_Directions (Y_Axis) = Backward
                   and then
                   St_Data.Homing_State = Homing_Back)
               then
                  St_Data.Directions (X_Axis) := Forward;
                  St_Data.Directions (Y_Axis) := Backward;
               else
                  St_Data.Directions (X_Axis) := Backward;
                  St_Data.Directions (Y_Axis) := Forward;
               end if;

            when Z_Axis => raise Program_Error; -- Unreachable
         end case;
         pragma Warnings (On, "condition can only be");
         pragma Warnings (On, "condition is always");

      else

         St_Data.Do_Step (St_Data.Homing_Axis) := True;

         St_Data.Directions (St_Data.Homing_Axis) :=
           Settings.Homing_Directions (St_Data.Homing_Axis);

         if St_Data.Homing_State = Homing_Back then
            Reverse_Dir (St_Data.Directions (St_Data.Homing_Axis));
         end if;

      end if;

      return True;
   end Homing;

   -------------------------------
   -- Reset_To_Home_Coordinates --
   -------------------------------

   procedure Reset_To_Home_Coordinates is
   begin
      if Settings.Use_CoreXY_Motion then
         declare
            X : constant Steps :=
              Milli_To_Step (Settings.Home_Coordinate (X_Axis),
                             X_Axis);

            Y : constant Steps :=
              Milli_To_Step (Settings.Home_Coordinate (Y_Axis),
                             Y_Axis);
         begin
            St_Data.Current_Position :=
              (X + Y,
               X - Y,
               Milli_To_Step (Settings.Home_Coordinate (Z_Axis),
                 Z_Axis));
         end;
      else
         St_Data.Current_Position :=
           Milli_To_Step (Settings.Home_Coordinate);
      end if;
   end Reset_To_Home_Coordinates;

   ------------------------
   -- Execute_Step_Event --
   ------------------------

   function Execute_Step_Event return Boolean is
   begin

      Step_Pulse.Start_Step_Cycle (St_Data.Do_Step,
                                   St_Data.Directions);

      for Axis in Axis_Name loop
         if St_Data.Do_Step (Axis) then
            if St_Data.Directions (Axis) = Forward then
               St_Data.Current_Position (Axis) :=
                 St_Data.Current_Position (Axis) + 1;
            else
               St_Data.Current_Position (Axis) :=
                 St_Data.Current_Position (Axis) - 1;
            end if;

            --  Check soft limits if not in homing
            if St_Data.Has_Segment and then St_Data.Seg.Kind /= Homing_Segment
            then
               if St_Data.Current_Position (Axis) > Stepper_Max_Limit (Axis)
               then
                  raise Program_Error;
               elsif St_Data.Current_Position (Axis) < Stepper_Min_Limit (Axis)
               then
                  raise Program_Error;
               end if;
            end if;
         end if;
      end loop;

      --  Reset step instruction
      St_Data.Do_Step := (others => False);

      if not St_Data.Has_Segment then
         if Get_Next_Segment (St_Data.Seg) then

            St_Data.Has_Segment := True;
            --  Process new segment

            case St_Data.Seg.Kind is
               when Homing_Segment =>
                  --  Start homing

                  St_Data.Homing_Order_Index := Settings.Homing_Order'First;
                  Setup_Homing;

               when Dwell_Segment =>
                  St_Data.Dwell_Timeout :=
                    Clock + To_Time_Span (Duration (St_Data.Seg.Dwell_Duration));
                  St_Data.Set_Stepper_Frequency_Callback
                    (Settings.Dwell_Stepper_Frequency);
               when Motion_Segment =>
                  --  Motion segment

                  St_Data.Step_Count := St_Data.Seg.Step_Count;
                  St_Data.Directions := St_Data.Seg.Directions;

                  --  Set frequency for this segment
                  St_Data.Set_Stepper_Frequency_Callback
                    (St_Data.Seg.Frequency);

                  if St_Data.Seg.New_Block then
                     --  This is the first segment of a new block

                     --  Prep data for bresenham algorithm
                     St_Data.Counter := (others => 0);
                     St_Data.Abs_Block_Steps := St_Data.Seg.Abs_Block_Steps;
                     St_Data.Block_Event_Count :=
                       St_Data.Seg.Block_Event_Count;
                  end if;
               when Enable_Motors_Segment =>
                  for Axis in Axis_Name loop
                     St_Data.Motor_Enable_Callback
                       (Axis, St_Data.Seg.Enable (Axis));
                  end loop;
                  St_Data.Has_Segment := False;
            end case;
         else
            --  No segment to exectute
            St_Data.Set_Stepper_Frequency_Callback
              (Settings.Idle_Stepper_Frequency);
            return False;
         end if;
      end if;

      case St_Data.Seg.Kind is
         when Homing_Segment =>
            return Homing;
         when Dwell_Segment =>
            if Clock >= St_Data.Dwell_Timeout then
               St_Data.Has_Segment := False;
            end if;
         when Motion_Segment =>
            --  Bresenham for each axis
            for Axis in Axis_Name loop

               if St_Data.Abs_Block_Steps (Axis) /= 0 then
                  St_Data.Counter (Axis) :=
                    St_Data.Counter (Axis) + St_Data.Abs_Block_Steps (Axis);
                  if St_Data.Counter (Axis) >= St_Data.Block_Event_Count then
                     St_Data.Do_Step (Axis) := True;
                     St_Data.Counter (Axis) :=
                       St_Data.Counter (Axis) - St_Data.Block_Event_Count;
                  else
                     St_Data.Do_Step (Axis) := False;
                  end if;
               end if;
            end loop;

            St_Data.Step_Count := St_Data.Step_Count - 1;
            --  Check end of segement
            if St_Data.Step_Count = 0 then
               St_Data.Has_Segment := False;
            end if;
         when Enable_Motors_Segment =>
            null;
      end case;

      return True;
   end Execute_Step_Event;

   ---------------------------
   -- Set_Stepper_Callbacks --
   ---------------------------

   procedure Set_Stepper_Callbacks
     (Set_Step              : Set_Step_Pin_Proc;
      Clear_Step            : Clear_Step_Pin_Proc;
      Set_Direcetion        : Set_Direction_Pin_Proc;
      Set_Stepper_Frequency : Set_Stepper_Frequency_Proc;
      Home_Test             : Home_Test_Proc;
      Motor_Enable          : Motor_Enable_Proc)
   is
   begin
      St_Data.Set_Step_Callback := Set_Step;
      St_Data.Clear_Step_Callback := Clear_Step;
      St_Data.Set_Direction_Callback := Set_Direcetion;
      St_Data.Set_Stepper_Frequency_Callback := Set_Stepper_Frequency;
      St_Data.Home_Test_Callback := Home_Test;
      St_Data.Motor_Enable_Callback := Motor_Enable;

      Reset_To_Home_Coordinates;
   end Set_Stepper_Callbacks;

end Stepper;
