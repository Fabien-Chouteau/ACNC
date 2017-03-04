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

with Ada.Synchronous_Task_Control;
with Ada.Real_Time; use Ada.Real_Time;
with Stepper;
with System;
with Settings;

package body Step_Control is

   Task_Sync   : Ada.Synchronous_Task_Control.Suspension_Object;
   Current_Dir : Axis_Directions := (others => Forward);
   pragma Unreferenced (Current_Dir);
   Task_Period : Time_Span := Milliseconds (500);

   procedure Set_Step_Direction (Axis : Axis_Name;
                                 Dir : Direction);
   procedure Clear_Step_Pin (Axis : Axis_Name);
   procedure Set_Step_Pin (Axis : Axis_Name);
   procedure Set_Stepper_Frequency (Freq_Hz : Frequency_Value);
   function Home_Test (Axis : Axis_Name) return Boolean;
   procedure Motor_Enable (Axis : Axis_Name;
                           Enable : Boolean);

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
         Enable_Clock (Step_GPIO (Axis));
         Enable_Clock (Dir_GPIO (Axis));
         Enable_Clock (Not_Enable_GPIO (Axis));

         Configure_IO (Step_GPIO (Axis),
                       Config => Configuration);
         Configure_IO (Dir_GPIO (Axis),
                       Config => Configuration);
         Configure_IO (Not_Enable_GPIO (Axis),
                       Config => Configuration);

         --  Init value
         Step_GPIO (Axis).Clear;

         --  Motors are disabled at init
         Not_Enable_GPIO (Axis).Set;

         Set_Step_Direction (Axis, Forward);
      end loop;

      --  Home switches
      Configuration.Mode        := Mode_In;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Pull_Up;

      for Axis in Axis_Name loop
         Enable_Clock (Home_GPIO (Axis));
         Configure_IO (Home_GPIO (Axis), Configuration);
      end loop;

      Enable_Clock (Analysis_Point);
      Configuration.Mode        := Mode_Out;
      Configuration.Speed       := Speed_100MHz;
      Configure_IO (Analysis_Point, Config => Configuration);
      Analysis_Point.Clear;

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
   -- Home_Test --
   ---------------

   function Home_Test (Axis : Axis_Name) return Boolean is
   begin
      return Set (Home_GPIO (Axis)) = Home_Switch_Polarity (Axis);
   end Home_Test;

   ------------------
   -- Motor_Enable --
   ------------------

   procedure Motor_Enable (Axis : Axis_Name;
                           Enable : Boolean) is
   begin
      if Enable then
         Clear (Not_Enable_GPIO (Axis));
      else
         Set (Not_Enable_GPIO (Axis));
      end if;
   end Motor_Enable;

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
        (Set_Step              => Set_Step_Pin'Access,
         Clear_Step            => Clear_Step_Pin'Access,
         Set_Direcetion        => Set_Step_Direction'Access,
         Set_Stepper_Frequency => Set_Stepper_Frequency'Access,
         Home_Test             => Home_Test'Access,
         Motor_Enable          => Motor_Enable'Access);

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
