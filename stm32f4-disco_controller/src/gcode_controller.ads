-------------------------------------------------------------------------------
--                                                                           --
--                                   ACNC                                    --
--                                                                           --
--       Copyright (C) 2016-2017 Fabien Chouteau (chouteau@adacore.com)      --
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
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;


package Gcode_Controller is

   procedure Initalize;
   procedure Execute (Str : String);

   type CNC_Context is new GContext with private;

   overriding
   procedure Put (Ctx : in out CNC_Context; C : Character);

   procedure Start;

private
   subtype Buffer_Range is Positive range 1 .. 256;

   type CNC_Context is new GContext with record
      Output_Buffer : String (Buffer_Range);
      Output_Index  : Buffer_Range := Buffer_Range'First;
   end record;

   Ctx : CNC_Context;

   Demo_Mode_Button : GPIO_Point renames PE11;
   Demo_Mode_LED    : GPIO_Point renames PD13;

end Gcode_Controller;
