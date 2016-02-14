with Gcode.Context; use Gcode.Context;

package Gcode.Planner is

   --  TODO: the planner should hold a buffer of motions to be completed

   procedure Planner_Add_Motion
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Step_Speed);

   procedure Planner_Add_Dwell
     (Ctx       : in out GContext'Class;
      Duration  : Float_Value);

   procedure Planner_Add_Homing
     (Ctx       : in out GContext'Class;
      Feed_Rate : Step_Speed);

   type Segment_Block is record
      New_Block  : Boolean;
      Step_Count : Steps;

      Directions       : Axis_Directions := (others => Forward);
      --  Step direction for each axis

   end record;

   function Get_Next_Segment (Segment : out Segment_Block) return Boolean;

end Gcode.Planner;
