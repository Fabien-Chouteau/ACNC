with Ada.Real_Time; use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;
with Gcode.Planner; use Gcode.Planner;
with Ada.Text_IO; use Ada.Text_IO;

-------------
-- Stepper --
-------------

package body Stepper is

   type Do_Step_Array is array (Axis_Name) of Boolean;

   type Stepper_Data_Type is record
      Has_Segment : Boolean := False;
      --  Does the stepper still has a segment to execute

      Segment : Segment_Block;

      Step_Count : Steps;
      Counter : Step_Position;

      Do_Step : Do_Step_Array := (others => False);
      --  Tells which axis has to do a step at the next iteration

      Directions       : Axis_Directions := (others => Forward);
      --  Direction of steps for eaxh axis


      Set_Step_Callback : Set_Step_Pin_Proc := null;
      Clear_Step_Callback : Clear_Step_Pin_Proc := null;
      Set_Direction_Callback : Set_Direction_Pin_Proc := null;

   end record;

   Stepper_Data : Stepper_Data_Type;

   type Step_Timing_Event is new Timing_Event with record
      Do_Step : Do_Step_Array;
   end record;

   ----------------
   -- Step_Pulse --
   ----------------

   protected Step_Pulse is
      procedure Start_Step_Cycle (Do_Step : Do_Step_Array;
                                  Directions  : Axis_Directions);
      procedure Set_Step_Pins (Event : in out Timing_Event);
      procedure Clear_Step_Pins (Event : in out Timing_Event);
   private
      Set_Event   : Step_Timing_Event;
      Clear_Event : Step_Timing_Event;

      --  Step pins timming
      --    Direction delay   Step delay
      --  |-----------------|------------|
      --  ^                 ^            ^
      --  Set  direction    Set step     Reset step

      Direction_Delay : Time_Span := Microseconds (0);
      Step_Delay      : Time_Span := Microseconds (0);
   end Step_Pulse;

   protected body Step_Pulse is
      ----------------------
      -- Start_Step_Cycle --
      ----------------------

      procedure Start_Step_Cycle (Do_Step     : Do_Step_Array;
                                  Directions  : Axis_Directions)
      is
         Now : Time;
      begin
         Step_Pulse.Set_Event.Do_Step := Do_Step;
         Step_Pulse.Clear_Event.Do_Step := Do_Step;

         if Stepper_Data.Set_Direction_Callback /= null then
            --  Set direction pins now
            for Axis in Axis_Name loop
               --  Set_Direction pin
               Stepper_Data.Set_Direction_Callback (Axis, Directions (Axis));
            end loop;
         end if;

         Now := Clock;

         if Direction_Delay = Microseconds (0) then
            --  Set and clear step pins imediatly
            Set_Step_Pins (Timing_Event (Set_Event));
            Clear_Step_Pins (Timing_Event (Clear_Event));
         else
            --  Schedule the timming evnet that will set the step pins
            Set_Handler (Set_Event, Now + Direction_Delay,
                         Set_Step_Pins'Access);

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
         if Stepper_Data.Set_Step_Callback /= null then
            for Axis in Axis_Name loop
               if Do_Step (Axis) then
                  Stepper_Data.Set_Step_Callback (Axis);
               end if;
            end loop;
         end if;
      end Set_Step_Pins;

      ---------------------
      -- Clear_Step_Pins --
      ---------------------

      procedure Clear_Step_Pins (Event : in out Timing_Event) is
         Do_Step : constant Do_Step_Array :=
           Step_Timing_Event (Timing_Event'Class (Event)).Do_Step;
      begin
         if Stepper_Data.Set_Step_Callback /= null then
            for Axis in Axis_Name loop
               if Do_Step (Axis) then
                  Stepper_Data.Clear_Step_Callback (Axis);
               end if;
            end loop;
         end if;
      end Clear_Step_Pins;
   end Step_Pulse;

   procedure Execute_Step_Event is
   begin

      Step_Pulse.Start_Step_Cycle (Stepper_Data.Do_Step,
                                   Stepper_Data.Directions);

      --  Reset step instruction
      Stepper_Data.Do_Step := (others => False);

      if not Stepper_Data.Has_Segment then
         if Get_Next_Segment (Stepper_Data.Segment) then
            Put_Line ("Taking new segment");

            Stepper_Data.Has_Segment := True;
            --  Process new segment

            Stepper_Data.Step_Count := Stepper_Data.Segment.Step_Count;
            Stepper_Data.Directions := Stepper_Data.Segment.Directions;

            Put_Line ("Segment Step count:" & Stepper_Data.Step_Count'Img);

            if Stepper_Data.Segment.New_Block then
               --  This is the first segment of a new block
               Put_Line ("This is the first segment of a new block");
               --  Prep data for bresenham algorithm
               for Axis in Axis_Name loop
                  Stepper_Data.Counter (Axis) := 0;
               end loop;

            end if;
         else
            --  No segment to exectute
            return;
         end if;
      end if;

      --  TODO: Insert bresenham algorithm here...
      Stepper_Data.Do_Step := (others => True);

      Stepper_Data.Step_Count := Stepper_Data.Step_Count - 1;
      --  Check end of segement
      if Stepper_Data.Step_Count = 0 then
         Stepper_Data.Has_Segment := False;
      end if;
   end Execute_Step_Event;

   ---------------------------
   -- Set_Stepper_Callbacks --
   ---------------------------

   procedure Set_Stepper_Callbacks (Set_Step       : Set_Step_Pin_Proc;
                                    Clear_Step     : Clear_Step_Pin_Proc;
                                    Set_Direcetion : Set_Direction_Pin_Proc)
   is
   begin
      Stepper_Data.Set_Step_Callback := Set_Step;
      Stepper_Data.Clear_Step_Callback := Clear_Step;
      Stepper_Data.Set_Direction_Callback := Set_Direcetion;

   end Set_Stepper_Callbacks;

end Stepper;
