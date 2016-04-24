with Gcode.Context; use Gcode.Context;

package Gcode.Planner is

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

   procedure Planner_Enable_Motors
     (Ctx    : in out GContext'Class;
      Enable : Motor_Enable_Array);

   type Segment_Kind is (Motion_Segment, Homing_Segment, Dwell_Segment,
                         Enable_Motors_Segment);

   type Segment (Kind : Segment_Kind := Motion_Segment) is record
      case Kind is
         when Motion_Segment =>
            New_Block  : Boolean;
            --  This is the first segment of a new block

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
         when Homing_Segment =>
            null;
         when Dwell_Segment =>
            Dwell_Duration : Duration;
         when Enable_Motors_Segment =>
            Enable : Motor_Enable_Array;
      end case;
   end record;

   function Get_Next_Segment (Seg : out Segment) return Boolean;

end Gcode.Planner;
