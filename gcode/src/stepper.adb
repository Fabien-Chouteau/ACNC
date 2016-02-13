with Ada.Real_Time; use Ada.Real_Time;
with System;

package body Stepper is

   Task_Min_Period : constant Time_Span := Microseconds (33); --  ~30_000 kHz
   Task_Max_Period : constant Time_Span  := Milliseconds (100); --  10Hz
   Task_Period : Time_Span := Task_Max_Period;

   task Stepper_Task is
      pragma Priority (System.Priority'Last);
   end Stepper_Task;

   task body Stepper_Task is
      Next_Time : Time := Clock;
   begin
      --  The stepper task is always running. When there is no segement to
      --  execute, its periode can be increased to lower cpu load.
      loop
         Next_Time := Next_Time + Task_Period;

         delay until Next_Time;
      end loop;
   end Stepper_Task;

   procedure Set_Frequency (D : Duration) is
      Period : constant Time_Span := To_Time_Span (1.0 / D);
   begin
      if Period < Task_Min_Period then
         Task_Period := Task_Min_Period;
      elsif Period > Task_Max_Period then
         Task_Period := Task_Max_Period;
      else
         Task_Period := Period;
      end if;
   end Set_Frequency;
end Stepper;
