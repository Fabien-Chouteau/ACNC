with Ada.Real_Time;
with Gcode;

package Settings is

   Idle_Stepper_Frequency : constant Gcode.Frequency_Value := 100.0;
   --  Frequency of the stepper when the machine is idle (Hz)


   ------------------------
   --  Step pins timming --
   ------------------------

   --    Direction delay   Step delay
   --  |-----------------|------------|
   --  ^                 ^            ^
   --  Set  direction    Set step     Clear step


   Direction_Pulse_Delay : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (0);
   --  Minimum delay between direction pin change and step pulse

   Step_Pulse_Duration : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (0);
   --  Minimum duration of step pulse

end Settings;
