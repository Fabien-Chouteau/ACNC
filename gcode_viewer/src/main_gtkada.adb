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

with Glib; use Glib;
with Station_Gtk; use Station_Gtk;
with Gtk; use Gtk;
with Gtk.Main;
with Gtkada.Builder; use Gtkada.Builder;
with Glib.Error; use Glib.Error;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Widget; use Gtk.Widget;

procedure Main_GtkAda is

   Glade_Filename : constant String := "gcode_viewer.glade";
   Builder : Gtkada_Builder;
   Error : aliased GError;
begin

   Gtk.Main.Init;
   Gtk_New (Builder);

   if Add_From_File (Builder, Glade_Filename, Error'Access) = 0 then
      Ada.Text_IO.Put_Line ("Error : " & Get_Message (Error));
      Error_Free (Error);
      return;
   end if;

   Init_Gtk (Builder);

   Gtk.Main.Main;
end Main_GtkAda;
