with Ada.Real_Time;
with Gcode_Controller;
with Step_Control;
with Coms;

procedure Main is
begin
   Gcode_Controller.Initalize;
   Step_Control.Initalize;
   Coms.Initalize;
   Gcode_Controller.Start;

   --  Sleep forever...
   delay until Ada.Real_Time.Time_Last;
end Main;
