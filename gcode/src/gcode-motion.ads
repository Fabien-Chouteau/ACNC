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
package Gcode.Motion is
   --  Add motion lines to the planner from absolute millimeter coordinates

   type Circular_Interpolation_Direction is (Clockwise, Counter_Clockwise);

   procedure Move_Line
     (Ctx       : in out GContext'Class;
      Target    : Float_Position;
      Feed_Rate : Step_Speed);

   procedure Move_Circle
     (Ctx         : in out GContext'Class;
      Start_Point : Float_Position;
      End_Point   : Float_Position;
      Offset      : Float_Position;
      Dir         : Circular_Interpolation_Direction;
      Feed_Rate   : Step_Speed);
end Gcode.Motion;
