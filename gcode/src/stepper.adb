with Ada.Real_Time; use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;
with Gcode.Planner; use Gcode.Planner;
with System;
with Settings; use Settings;
with Gcode.Context; use Gcode.Context;

package body Stepper is

   type Homing_Cycle_State is (Homing_Approach,
                               Homing_Back,
                               Homing_Precision);

   type Do_Step_Array is array (Axis_Name) of Boolean;

   procedure Dummy_Set_Step_Pin (Axis : Axis_Name) is null;
   procedure Dummy_Clear_Step_Pin  (Axis : Axis_Name) is null;
   procedure Dummy_Set_Direction_Pin (Axis : Axis_Name;
                                      Dir : Direction) is null;
   procedure Dummy_Set_Stepper_Frequency (Freq_Hz : Frequency_Value) is null;
   function Dummy_Home_Test (Axis : Axis_Name) return Boolean is (False);
   procedure Setup_Homing;
   function Homing return Boolean;

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

      Block_Steps : Step_Position;
      --  Steps for the current Motion block each axis
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

      St_Data.Directions   := Settings.Homing_Directions;
   end Setup_Homing;

   ------------
   -- Homing --
   ------------

   function Homing return Boolean is
   begin
      St_Data.Do_Step := (others => False);
      St_Data.Do_Step (St_Data.Homing_Axis) := True;

      case St_Data.Homing_State is
         when Homing_Approach =>
            if St_Data.Home_Test_Callback (St_Data.Homing_Axis) then
               St_Data.Homing_State := Homing_Back;
               Reverse_Dir (St_Data.Directions (St_Data.Homing_Axis));
            end if;

         when Homing_Back =>
            if not St_Data.Home_Test_Callback (St_Data.Homing_Axis) then
               St_Data.Homing_State := Homing_Precision;
               Reverse_Dir (St_Data.Directions (St_Data.Homing_Axis));

               --  Switch to precission feed rate
               St_Data.Set_Stepper_Frequency_Callback
                 (Frequency_Value (Homing_Precision_Feed_Rate *
                      Step_Per_Millimeter (St_Data.Homing_Axis)));

            end if;

         when Homing_Precision =>
            if St_Data.Home_Test_Callback (St_Data.Homing_Axis) then
               if St_Data.Homing_Order_Index = Settings.Homing_Order_Range'Last
               then
                  --  End of homing
                  St_Data.Has_Segment := False;
                  St_Data.Current_Position :=
                    Milli_To_Step (Settings.Home_Coordinate);
               else
                  --  Start homing cycle for next axis
                  St_Data.Homing_Order_Index := St_Data.Homing_Order_Index + 1;
                  Setup_Homing;
               end if;
            end if;
      end case;
      return True;
   end Homing;

   ------------------------
   -- Execute_Step_Event --
   ------------------------

   function Execute_Step_Event return Boolean is
   begin

      Step_Pulse.Start_Step_Cycle (St_Data.Do_Step,
                                   St_Data.Directions);

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
                    Clock + To_Time_Span (St_Data.Seg.Dwell_Duration);
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
                     St_Data.Block_Steps := St_Data.Seg.Block_Steps;
                     St_Data.Block_Event_Count :=
                       St_Data.Seg.Block_Event_Count;
                  end if;
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
               St_Data.Counter (Axis) :=
                 St_Data.Counter (Axis) + St_Data.Block_Steps (Axis);

               if St_Data.Counter (Axis) > St_Data.Block_Event_Count then
                  St_Data.Do_Step (Axis) := True;
                  St_Data.Counter (Axis) :=
                    St_Data.Counter (Axis) - St_Data.Block_Event_Count;
                  if St_Data.Directions (Axis) = Forward then
                     St_Data.Current_Position (Axis) :=
                       St_Data.Current_Position (Axis) + 1;
                  else
                     St_Data.Current_Position (Axis) :=
                       St_Data.Current_Position (Axis) + 1;
                  end if;
               else
                  St_Data.Do_Step (Axis) := False;
               end if;
            end loop;

            St_Data.Step_Count := St_Data.Step_Count - 1;
            --  Check end of segement
            if St_Data.Step_Count = 0 then
               St_Data.Has_Segment := False;
            end if;
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
      Home_Test             : Home_Test_Proc)
   is
   begin
      St_Data.Set_Step_Callback := Set_Step;
      St_Data.Clear_Step_Callback := Clear_Step;
      St_Data.Set_Direction_Callback := Set_Direcetion;
      St_Data.Set_Stepper_Frequency_Callback := Set_Stepper_Frequency;
      St_Data.Home_Test_Callback := Home_Test;
   end Set_Stepper_Callbacks;

end Stepper;
