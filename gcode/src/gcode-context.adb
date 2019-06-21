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

with Gcode.Error; use Gcode.Error;

package body Gcode.Context is

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Ctx : in out GContext; Msg : String) is
   begin
      Ctx.Error_Flag := True;
      Ctx.Put_Line (Msg);
      raise Gcode_Exception with Msg;
   end Raise_Error;

   -----------------
   -- Clear_Error --
   -----------------

   procedure Clear_Error (Ctx : in out GContext) is
   begin
      Ctx.Error_Flag := False;
   end Clear_Error;

   ------------------
   -- Error_Raised --
   ------------------

   function Error_Raised (Ctx : in out GContext) return Boolean is
   begin
      return Ctx.Error_Flag;
   end Error_Raised;

   ----------
   -- Home --
   ----------

   function Home (Ctx : in out GContext; Axis : Axis_Name)
                  return Boolean is
      pragma Unreferenced (Axis, Ctx);
   begin
      return False;
   end Home;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart, EEnd : Natural) is
   begin
      Ctx.Error_Flag := True;
      Ctx.Put_Line (Line);
      if EStart /= 0 and then EEnd /= 0 then
         for Index in Line'First .. EStart - 1 loop
            Put (GContext'Class (Ctx), ' ');
         end loop;
         Put (GContext'Class (Ctx), '^');
         if EStart /= EEnd then
            for Index in EStart + 1 .. EEnd - 1 loop
               Put (GContext'Class (Ctx), '-');
            end loop;
            Put (GContext'Class (Ctx), '^');
         end if;
         Ctx.New_Line;
      end if;
      Ctx.Raise_Error ("Error: " & Msg);
   end Report_Error;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Ctx : in out GContext;
                           Line, Msg : String;
                           EStart : Natural) is
   begin
      Report_Error (Ctx, Line, Msg, EStart, EStart);
   end Report_Error;

   ---------
   -- Log --
   ---------

   procedure Log (Ctx : in out GContext; Lvl : Log_Level; Str : String) is
   begin
      Ctx.Put((case Lvl is
              when Info    => "Info: ",
              when Warning => "Warning: ",
              when Error   => "Error: ",
              when Board   => "Board: ") & Str);
      Ctx.New_Line;
   end Log;

   ---------
   -- Put --
   ---------

   procedure Put (Ctx : in out GContext; Str : String) is
   begin
      for C of Str loop
         Put (GContext'Class (Ctx), C);
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Ctx : in out GContext; Str : String) is
   begin
      Ctx.Put (Str);
      Ctx.New_Line;
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Ctx : in out GContext) is
   begin
      Ctx.Put (ASCII.CR & ASCII.LF);
   end New_Line;

end Gcode.Context;
