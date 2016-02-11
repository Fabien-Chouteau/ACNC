with Gcode.Context; use Gcode.Context;
package Gcode.Motion is
   --  Add motion lines to the planner from absolute millimeter coordinates

   type Circular_Interpolation_Direction is (Clockwise, Counter_Clockwise);

   procedure Move_Line
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Float_Value);

   procedure Move_Circle
     (Ctx         : in out GContext'Class;
      Start_Point : Float_Position;
      End_Point   : Float_Position;
      Offset      : Float_Position;
      Dir         : Circular_Interpolation_Direction;
      Feed_Rate   : Float_Value);
end Gcode.Motion;
