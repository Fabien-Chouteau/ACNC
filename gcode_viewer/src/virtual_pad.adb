with Gtk.Window; use Gtk.Window;
with Gtk.Grid; use Gtk.Grid;
with Glib; use Glib;
with Gtk.Button; use Gtk.Button;
with Glib.Main; use Glib.Main;

package body Virtual_Pad is

   Buttons : array (Width_Range, Height_Range) of Gtk_Button;

   Cnt : Natural := 0;

   function Window_Idle return Boolean is
   begin
      for X in Width_Range loop
         for Y in Height_Range loop
            Buttons (X, Y).Set_Label (Cnt'Img);
         end loop;
      end loop;
      Cnt := Cnt + 1;
      return True;
   end Window_Idle;

   procedure Create_Windows is
      Win    : Gtk_Window;
      Grid   : Gtk_Grid;
      Src_Id : G_Source_Id;
      pragma Unreferenced (Src_Id);
   begin

      --  Create a window with a size of 400x400
      Gtk_New (Win);
      Win.Set_Default_Size (400, 400);


      Gtk_New (Grid);
      Win.Add (Grid);
      Grid.Set_Column_Homogeneous (True);
      Grid.Set_Row_Homogeneous (True);

      for X in Width_Range loop
         for Y in Height_Range loop
            Gtk_New (Buttons (X, Y), "Cell" & X'Img & Y'Img);
            Grid.Attach (Buttons (X, Y), Gint (X), Gint (Y));
         end loop;
      end loop;
      Show_All (Win);

      Src_Id := Timeout_Add (1000, Window_Idle'Access);
   end Create_Windows;

--     procedure Set_Light (X : Width_Range; Y : Height_Range) is
--     begin
--        null;
--     end;
end Virtual_Pad;
