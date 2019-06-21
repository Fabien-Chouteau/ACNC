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

with Gcode.Context; use Gcode.Context;

package Gcode.Planner is

   procedure Planner_Add_Motion
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Step_Speed);

   procedure Planner_Add_Dwell
     (Ctx             : in out GContext'Class;
      Dwell_Duration  : Float_Value);

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

            Abs_Block_Steps : Step_Position;
            --  Absolute number of steps for the current Motion block each axis

            Block_Event_Count : Steps;
            --  Step count for the current block
         when Homing_Segment =>
            null;
         when Dwell_Segment =>
            Dwell_Duration : Float_Value;
         when Enable_Motors_Segment =>
            Enable : Motor_Enable_Array;
      end case;
   end record;

   function Get_Next_Segment (Seg : out Segment) return Boolean;

end Gcode.Planner;
