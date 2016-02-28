with Gcode.Context; use Gcode.Context;

package Gcode.Planner is

   --  TODO: the planner should hold a buffer of motions to be completed

   procedure Planner_Add_Motion
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Step_Speed);

   procedure Planner_Add_Dwell
     (Ctx             : in out GContext'Class;
      Dwell_Duration  : Duration);

   procedure Planner_Add_Homing
     (Ctx       : in out GContext'Class;
      Feed_Rate : Step_Speed);

   type Segment is record
      New_Block  : Boolean;
      --  This is the first segment of a new block

      Homing : Boolean;
      --  This is a homing segment

      Step_Count : Steps;
      --  Number of steps in this segment

      Frequency : Frequency_Value;
      --  Requested stepper frequency for this segment

      Directions : Axis_Directions := (others => Forward);
      --  Step direction for each axis

      Block_Steps : Step_Position;
      --  Steps for the current Motion block each axis

      Block_Event_Count : Steps;
      --  Step count for the current block

   end record;

   function Get_Next_Segment (Seg : out Segment) return Boolean;

end Gcode.Planner;
