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

with Gcode.Parameters; use Gcode.Parameters;
with Settings;

package Gcode.Context is

   type GContext is abstract tagged record
      Params : Parameters_Set;
      Unit : Coord_Unit := Millimeters;
      Positioning : Positioning_Mode := Absolute_Positioning;
      B : Block;
      Fast_Feed_Rate   : Step_Speed := 2.0;
      Current_Feed_Rate : Step_Speed := 1.0;
      Error_Flag : Boolean := False;


      Virt_Position : Float_Position := Settings.Home_Coordinate;
      --  Initial virtual posistion is set to the home coordinate
   end record;

   function Home (Ctx : in out GContext; Axis : Axis_Name) return Boolean;

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart, EEnd : Natural);
   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart : Natural);

   procedure Raise_Error (Ctx : in out GContext; Msg : String);
   procedure Clear_Error (Ctx : in out GContext);
   function Error_Raised (Ctx : in out GContext) return Boolean;

   type Log_Level is (Info, Warning, Error, Board);
   procedure Log (Ctx : in out GContext; Lvl : Log_Level; Str : String);
   procedure Put (Ctx : in out GContext; C : Character) is abstract;
   procedure Put (Ctx : in out GContext; Str : String);
   procedure Put_Line (Ctx : in out GContext; Str : String);
   procedure New_Line (Ctx : in out GContext);
end Gcode.Context;

