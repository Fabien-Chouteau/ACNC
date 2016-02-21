with Ada.Synchronous_Task_Control;
with Ada.Real_Time; use Ada.Real_Time;
with Stepper;
with System;
with Settings;

package body Step_Control is

   Task_Sync   : Ada.Synchronous_Task_Control.Suspension_Object;
   Current_Pos : Step_Position := (others => 0);
   Current_Dir : Axis_Directions := (others => Forward);
   Task_Period : Time_Span := Milliseconds (500);

   procedure Set_Step_Direction (Axis : Axis_Name;
                                 Dir : Direction);
   procedure Clear_Step_Pin (Axis : Axis_Name);
   procedure Set_Step_Pin (Axis : Axis_Name);
   procedure Set_Stepper_Frequency (Freq_Hz : Frequency_Value);

   ---------------
   -- Initalize --
   ---------------

   procedure Initalize is
      Configuration : GPIO_Port_Configuration;
   begin
      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Pull_Down;

      for Axis in Axis_Name loop
         Enable_Clock (Step_GPIO (Axis).Port.all);
         Enable_Clock (Dir_GPIO (Axis).Port.all);
         Enable_Clock (Not_Enable_GPIO (Axis).Port.all);

         Configure_IO (Step_GPIO (Axis),
                       Config => Configuration);
         Configure_IO (Dir_GPIO (Axis),
                       Config => Configuration);
         Configure_IO (Not_Enable_GPIO (Axis),
                       Config => Configuration);

         --  Init value
         Clear (Step_GPIO (Axis));
         Clear (Not_Enable_GPIO (Axis));
         Set_Step_Direction (Axis, Forward);
      end loop;

      Enable_Clock (Analysis_Point.Port.all);
      Configure_IO (Analysis_Point, Config => Configuration);
      Clear (Analysis_Point);

      Set_Stepper_Frequency (Settings.Idle_Stepper_Frequency);

      --  Release stepper task
      Ada.Synchronous_Task_Control.Set_True (Task_Sync);
   end Initalize;

   ------------------------
   -- Set_Step_Direction --
   ------------------------

   procedure Set_Step_Direction (Axis : Axis_Name;
                                 Dir : Direction)
   is
   begin
      Current_Dir (Axis) := Dir;
      if Dir = Forward then
         Set (Dir_GPIO (Axis));
      else
         Clear (Dir_GPIO (Axis));
      end if;
   end Set_Step_Direction;

   --------------------
   -- Clear_Step_Pin --
   --------------------

   procedure Clear_Step_Pin (Axis : Axis_Name) is
   begin
      Clear (Step_GPIO (Axis));
   end Clear_Step_Pin;

   ------------------
   -- Set_Step_Pin --
   ------------------

   procedure Set_Step_Pin (Axis : Axis_Name) is
   begin
      Set (Step_GPIO (Axis));
   end Set_Step_Pin;

   ---------------------------
   -- Set_Stepper_Frequency --
   ---------------------------

   procedure Set_Stepper_Frequency (Freq_Hz : Frequency_Value) is
   begin
      Task_Period := To_Time_Span (1.0 / Freq_Hz);
   end Set_Stepper_Frequency;

   ---------------
   -- Step_Task --
   ---------------

   task Step_Task is
      pragma Priority (System.Default_Priority + 2);
   end Step_Task;

   task body Step_Task is
      Next_Period : Time := Clock;
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True (Task_Sync);

      Stepper.Set_Stepper_Callbacks
        (Set_Step       => Set_Step_Pin'Access,
         Clear_Step     => Clear_Step_Pin'Access,
         Set_Direcetion => Set_Step_Direction'Access,
         Set_Stepper_Frequency => Set_Stepper_Frequency'Access);

      loop
         Next_Period := Next_Period + Task_Period;

         Set (Analysis_Point);
         if Stepper.Execute_Step_Event then
            null;
         end if;
         Clear (Analysis_Point);
         delay until Next_Period;
      end loop;
   end Step_Task;

end Step_Control;
