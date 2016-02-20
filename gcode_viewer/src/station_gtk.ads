
with Glib; use Glib;
with Glib.Main; use Glib.Main;
with Gtk.Button; use Gtk.Button;
with Gtkada.Builder; use Gtkada.Builder;
with Gcode.Context; use Gcode.Context;
with Gcode; use Gcode;

package Station_Gtk is

   type GTK_CNC is new GContext with record
      null;
   end record;

   overriding
   function Home (Ctx : in out GTK_CNC; Axis : Axis_Name) return Boolean;
   overriding
   procedure Report_Error (Ctx : in out GTK_CNC;
                           Line, Msg : String;
                           EStart, EEnd : Natural);
   overriding
   procedure Log (Ctx : in out GTK_CNC; Lvl : Log_Level; Str : String);
   overriding
   procedure Put (Ctx : in out GTK_CNC; Str : String);
   overriding
   procedure Put_Line (Ctx : in out GTK_CNC; Str : String);
   overriding
   procedure New_Line (Ctx : in out GTK_CNC);
   overriding
   procedure Put (Ctx : in out GTK_CNC; C : Character);

   type Station_Gtk_Record is record
      Fwd          : Gtk_Button;
      Left         : Gtk_Button;
      Right        : Gtk_Button;
      Timeout      : G_Source_Id;
   end record;

   type Station_Gtk_Ref is access all Station_Gtk_Record;

   procedure Init_Gtk (Builder : Gtkada_Builder);

end Station_Gtk;
