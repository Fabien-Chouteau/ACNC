with Gtk; use Gtk;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Handlers;
with Gtk.Window;
with Cairo; use Cairo;
with Gtk.Widget; use Gtk.Widget;
with Glib.Object; use Glib.Object;
with Gdk.Window; use Gdk.Window;
with Gtk.Enums; use Gtk.Enums;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.File_Chooser_Button; use Gtk.File_Chooser_Button;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Glib.Values; use Glib.Values;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View; use Gtk.Text_View;
with Gcode.Parser;
with Gcode.Execution;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Gdk.RGBA; use Gdk.RGBA;
with Glib.Properties;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;
with Gtk.Dialog; use Gtk.Dialog;
with Machine_Motion_history; use Machine_Motion_history;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Control_Window;

package body Station_Gtk is

   Tool_Size   : constant Gdouble := 9.0;
   Darea       : Gtk_Drawing_Area := null;
   File_Button : Gtk_File_Chooser_Button := null;
   Text        : Gtk_Text_Buffer := null;
   Log_Buffer  : Gtk_Text_Buffer := null;
   Text_View   : Gtk_Text_View := null;
   Log_View    : Gtk_Text_View := null;
   pragma Unreferenced (Log_View);
   Zoom        : constant Gdouble := 0.19;
   View_X : constant Gdouble := -2000.0;
   View_Y : constant Gdouble := -2000.0;
   Serial_Dialog : Gtk_Dialog;
   Serial_Name, Serial_Baud : Gtk_Combo_Box_Text;
   Execute_Menu : Gtk_Image_Menu_Item;
   The_Builder : Gtkada_Builder;

   Serial : aliased Serial_Port;

   Log_Error_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
   Log_Warning_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
   Log_Info_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
   Log_Board_Tag : Gtk.Text_Tag.Gtk_Text_Tag;

   Error_Msg : Unbounded_String;
   Error_Line : Gint;

   Ctx : aliased GTK_CNC;

   use type Step_Position;
   use type Gdk.Gdk_Window;

   overriding
   procedure Report_Error (Ctx : in out GTK_CNC;
                           Line, Msg : String;
                           EStart, EEnd : Natural) is
      pragma Unreferenced (Line, EEnd, EStart);
   begin
      Ctx.Error_Flag := True;
      Error_Msg := To_Unbounded_String (Msg);
   end Report_Error;

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Drawing_Area_Record, Boolean);

   function Window_Idle return Boolean;
   function Stepper_Tick return Boolean;

   function Redraw (Area  : access Gtk_Drawing_Area_Record'Class;
                    Cr    : Cairo_Context) return Boolean;
   procedure Open_Handler (User_Data : access Gtkada_Builder_Record'Class);
   function Travel_Gcode (Line : String) return Boolean;
   function Simulate_Handler (User_Data : access Gtkada_Builder_Record'Class)
                             return Boolean;
   function Connect_Handler (User_Data : access Gtkada_Builder_Record'Class)
                          return Boolean;
   function Connect_Apply_Handler
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Connect_Cancel_Handler
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Execute_Handler
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   procedure Create_Tags;
   function Text_Tooltip
     (Self          : access Gtk_Widget_Record'Class;
      X             : Gint;
      Y             : Gint;
      Keyboard_Mode : Boolean;
      Tooltip       : not null access Glib.Object.GObject_Record'Class)
   return Boolean;

   -----------------
   -- Window_Idle --
   -----------------

   function Window_Idle return Boolean
   is
      W, H  : Gint;
      Main_W : Gdk.Gdk_Window := null;
   begin
      if Darea = null then
         return True;
      end if;
      W := Darea.Get_Allocated_Width;
      H := Darea.Get_Allocated_Height;

      Main_W := Get_Window (Darea);
      if Main_W = null then
         return True;
      end if;
      Invalidate_Rect (Main_W, (0, 0, W, H), Invalidate_Children => True);
      return True;
   end Window_Idle;

   ------------
   -- Redraw --
   ------------

   function Redraw (Area  : access Gtk_Drawing_Area_Record'Class;
                    Cr    : Cairo_Context) return Boolean is
      pragma Unreferenced (Area);
   begin
      Cairo.Save (Cr);
      --  Set the origin at the bottom left corner
      Cairo.Scale (Cr, 1.0, -1.0);
      Cairo.Translate (Cr, 0.0, Gdouble (-Darea.Get_Allocated_Height));

      Cairo.Translate (Cr, Gdouble (Darea.Get_Allocated_Width) / 2.0,
                       Gdouble (Darea.Get_Allocated_Height) / 2.0);
      Cairo.Scale (Cr, Zoom, Zoom);
      Cairo.Translate (Cr, View_X, View_Y);

      for GX in 0 .. 40 loop
         if GX mod 10 = 0 then
            Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
         else
            Set_Source_Rgb (Cr, 0.5, 0.5, 0.5);
         end if;
         Move_To (Cr, Gdouble (GX) * 100.0, 0.0);
         Line_To (Cr, Gdouble (GX) * 100.0, 40.0 * 100.0);
         Cairo.Stroke (Cr);
         Move_To (Cr, 0.0, Gdouble (GX) * 100.0);
         Line_To (Cr, 40.0 * 100.0, Gdouble (GX) * 100.0);
         Cairo.Stroke (Cr);
      end loop;
      Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
      Rectangle (Cr     => Cr,
                 X      => -Tool_Size / 2.0,
                 Y      => -Tool_Size / 2.0,
                 Width  => Tool_Size,
                 Height => Tool_Size);
      Cairo.Fill (Cr);

      Draw_History (Cr, Zoom);

      Cairo.Restore (Cr);

      return False;
   end Redraw;

   -------------------
   --  Open_Handler --
   -------------------

   procedure Open_Handler (User_Data : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (User_Data);
      File       : Ada.Text_IO.File_Type;
      Line_Count : Natural := 0;
   begin
      Put_Line ("Open file: " & File_Button.Get_Filename);
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Button.Get_Filename);
      Text.Set_Text ("");
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
            Val  : GValue;
            Iter : Gtk.Text_Iter.Gtk_Text_Iter;
         begin
            Init (Val, GType_String);
            Set_String (Val, Line);
            Line_Count := Line_Count + 1;
            Text.Get_End_Iter (Iter);
            Text.Insert (Iter, Line & ASCII.CR);
         end;
      end loop;
      Ada.Text_IO.Close (File);
   end Open_Handler;

   ------------------
   -- Travel_Gcode --
   ------------------

   function Travel_Gcode (Line : String) return Boolean is
   begin
      if Gcode.Parser.Parse (Line, Ctx) then
         return Gcode.Execution.Execute (Line, Ctx);
      end if;
      return False;
   end Travel_Gcode;

   -----------------------
   --  Simulate_Handler --
   -----------------------

   function Simulate_Handler (User_Data : access Gtkada_Builder_Record'Class)
                             return Boolean
   is
      pragma Unreferenced (User_Data);
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Line_Cnt : Gint := 0;
   begin
      Ctx.Log (Error, "Test error");
      Ctx.Log (Warning, "Test Warning");
      Ctx.Log (Board, "Test Board");
      Ctx.Virt_Position := (0.0, 0.0, 0.0);
      Clear_History;
      loop
         Text.Get_Iter_At_Line (Start_Iter, Line_Cnt);
         Text.Get_Iter_At_Line (End_Iter, Line_Cnt + 1);
         exit when Is_End (Start_Iter);
         declare
            Line : constant UTF8_String :=
              Text.Get_Text (Start_Iter, End_Iter);
         begin
            if not Travel_Gcode (Line) then
               Put_Line ("FAILED");
               Text.Apply_Tag_By_Name ("error", Start_Iter, End_Iter);
               Error_Line := Line_Cnt;
               exit;
            else
               Text.Apply_Tag_By_Name ("noedit", Start_Iter, End_Iter);
            end if;
         exception
            when others =>
               Put_Line ("Exception");
               exit;
         end;
         Line_Cnt := Line_Cnt + 1;
      end loop;
      return True;
   end Simulate_Handler;

   ---------------------
   -- Connect_Handler --
   ---------------------

   function Connect_Handler (User_Data : access Gtkada_Builder_Record'Class)
                          return Boolean
   is
      pragma Unreferenced (User_Data);
      Serial_Found : Boolean := False;
   begin
      Serial_Name.Remove_All;
      for K in 1 .. 255 loop
         declare
            Port : Serial_Port;
            Name : constant Port_Name := GNAT.Serial_Communications.Name (K);
         begin
            --  Test if serial port exists
            Open (Port, Port_Name (Name));
            Close (Port);
            Serial_Name.Append_Text (String (Name));
            Serial_Found := True;
         exception
            when Serial_Error =>
               --  Not a valid serial port
               null;
         end;
      end loop;

      if not Serial_Found then
         Serial_Name.Append_Text ("No serial port detected");
      end if;

      Serial_Name.Set_Active (0);

      if Serial_Dialog.Run = Gtk_Response_Apply then
         if Serial_Found then
            Put_Line ("Selected serial port:" & Serial_Name.Get_Active_Text &
                        ":" & Serial_Baud.Get_Active_Text);
            declare
            begin
               Open (Serial, Port_Name (Serial_Name.Get_Active_Text));
               Set (Serial, Data_Rate'Value (Serial_Baud.Get_Active_Text));
               Execute_Menu.Set_Sensitive (True);
            exception
               when Serial_Error =>
                  Ctx.Log (Error, "Cannot open serial port" &
                             Serial_Name.Get_Active_Text &
                             ":" & Serial_Baud.Get_Active_Text);
            end;
         end if;
      end if;
      Serial_Dialog.Destroy;
      return True;
   end Connect_Handler;

   ---------------------------
   -- Connect_Apply_Handler --
   ---------------------------

   function Connect_Apply_Handler
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      Serial_Dialog.Response (Gtk.Dialog.Gtk_Response_Apply);
      return True;
   end Connect_Apply_Handler;

   ----------------------------
   -- Connect_Cancel_Handler --
   ----------------------------

   function Connect_Cancel_Handler
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      Serial_Dialog.Response (Gtk.Dialog.Gtk_Response_Cancel);
      return True;
   end Connect_Cancel_Handler;

   ---------------------
   -- Execute_Handler --
   ---------------------

   function Execute_Handler
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean
   is
      pragma Unreferenced (User_Data);
      Data : String (1 .. 10);
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Line_Cnt : Gint := 0;
   begin
      loop
         Text.Get_Iter_At_Line (Start_Iter, Line_Cnt);
         Text.Get_Iter_At_Line (End_Iter, Line_Cnt + 1);
         exit when Is_End (Start_Iter);
         declare
            Line : constant UTF8_String :=
              Text.Get_Text (Start_Iter, End_Iter);
         begin
            UTF8_String'Write (Serial'Access, Line & ASCII.LF & ASCII.CR);
            String'Read (Serial'Access, Data);
            Put_Line ("Len :" & Data'Length'Img);
            Put_Line ("Data : '" & Data & "'");
         exception
            when others =>
               Put_Line ("Exception");
               exit;
         end;
         Line_Cnt := Line_Cnt + 1;
      end loop;

      return True;
   end Execute_Handler;

   -----------------
   -- Create_Tags --
   -----------------

   procedure Create_Tags is
      Tag     : Gtk.Text_Tag.Gtk_Text_Tag;
      Color   : Gdk_RGBA;
   begin
      Tag := Text.Create_Tag ("error");
      Color := (1.0, 0.0, 0.0, 0.5);
      Set_Property (Tag,  Paragraph_Background_Rgba_Property, Color);

      Tag := Text.Create_Tag ("noedit");
      Glib.Properties.Set_Property (Tag, Gtk.Text_Tag.Editable_Property,
                                    False);

      Log_Error_Tag := Log_Buffer.Create_Tag ("err_tag");
      Color := (1.0, 0.0, 0.0, 0.5);
      Set_Property (Log_Error_Tag, Paragraph_Background_Rgba_Property, Color);
      Log_Warning_Tag := Log_Buffer.Create_Tag ("warn_tag");
      Color := (1.0, 0.4, 0.0, 0.5);
      Set_Property (Log_Warning_Tag, Paragraph_Background_Rgba_Property,
                    Color);
      Log_Info_Tag := Log_Buffer.Create_Tag ("info_tag");
      Color := (1.0, 1.0, 1.0, 1.0);
      Set_Property (Log_Info_Tag, Paragraph_Background_Rgba_Property, Color);
      Log_Board_Tag := Log_Buffer.Create_Tag ("board_tag");
      Color := (0.0, 3.0, 2.0, 0.5);
      Set_Property (Log_Board_Tag, Paragraph_Background_Rgba_Property, Color);
   end Create_Tags;

   ------------------
   -- Text_Tooltip --
   ------------------

   function Text_Tooltip
     (Self          : access Gtk_Widget_Record'Class;
      X             : Gint;
      Y             : Gint;
      Keyboard_Mode : Boolean;
      Tooltip       : not null access Glib.Object.GObject_Record'Class)
      return Boolean is
      pragma Unreferenced (Keyboard_Mode, Tooltip);
      Iter : Gtk_Text_Iter;
      Line : Gint;
   begin
      if Ctx.Error_Flag then
         Text_View.Get_Iter_At_Location (Iter, X, Y);
         Line := Get_Line (Iter);
         if Error_Line = Line then
            Set_Tooltip_Text (Self, To_String (Error_Msg));
            return False;
         else
            return True;
         end if;
      else
         return True;
      end if;
   end Text_Tooltip;

   --------------
   -- Init_Gtk --
   --------------

   procedure Init_Gtk (Builder : Gtkada_Builder) is
      Src_Id : G_Source_Id;
      Main_W : Gtk.Window.Gtk_Window;
      pragma Unreferenced (Src_Id);

   begin
      The_Builder := Builder;
      Register_Handler (Builder, "open_handler", Open_Handler'Access);
      Register_Handler (Builder, "simulate_handler", Simulate_Handler'Access);
      Register_Handler (Builder, "connect_handler", Connect_Handler'Access);
      Register_Handler (Builder, "serial_cancel_handler",
                        Connect_Cancel_Handler'Access);
      Register_Handler (Builder, "serial_save_handler",
                        Connect_Apply_Handler'Access);
      Register_Handler (Builder, "execute_handler",
                        Execute_Handler'Access);
      Control_Window.Register_Handlers (Builder, Ctx'Access);

      File_Button := Gtk_File_Chooser_Button (Builder.Get_Object ("open"));
      Darea := Gtk_Drawing_Area (Builder.Get_Object ("drawingarea"));
      Text := Gtk_Text_Buffer (Builder.Get_Object ("textbuffer"));
      Text_View := Gtk_Text_View (Builder.Get_Object ("textview"));
      Text_View.On_Query_Tooltip (Text_Tooltip'Access);

      Log_Buffer := Gtk_Text_Buffer (Builder.Get_Object ("logbuffer"));
      Log_View := Gtk_Text_View (Builder.Get_Object ("logview"));

      Serial_Dialog := Gtk_Dialog (The_Builder.Get_Object ("serial_dialog"));

      Serial_Name := Gtk_Combo_Box_Text
        (The_Builder.Get_Object ("serial_name_comboboxtext"));
      Serial_Baud := Gtk_Combo_Box_Text
        (The_Builder.Get_Object ("serial_baud_comboboxtext"));

      Serial_Baud.Remove_All;
      for Baud in Data_Rate loop
         Serial_Baud.Append_Text (Baud'Img);
      end loop;
      Serial_Baud.Set_Active (12);

      Execute_Menu := Gtk_Image_Menu_Item
        (The_Builder.Get_Object ("execute_menuitem"));

      Execute_Menu.Set_Sensitive (False);

      Create_Tags;

      Event_Cb.Connect (Darea, Signal_Draw,
                        Event_Cb.To_Marshaller (Redraw'Unrestricted_Access));

      Main_W := Gtk.Window.Gtk_Window (Get_Object (Builder, "window1"));

      Do_Connect (Builder);
      Main_W.Show_All;
      Main_W.Maximize;

      Src_Id := Timeout_Add (100, Window_Idle'Access);
      Src_Id := Timeout_Add (1, Stepper_Tick'Access);
   end Init_Gtk;

   ---------
   -- Log --
   ---------

   overriding
   procedure Log (Ctx : in out GTK_CNC; Lvl : Log_Level; Str : String) is
      pragma Unreferenced (Ctx);
      Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Log_Buffer.Get_End_Iter (Iter);
      Log_Buffer.Insert_With_Tags (Iter => Iter,
                                   Text => Str & ASCII.CR,
                                   Tag  => (case Lvl is
                                               when Info => Log_Info_Tag,
                                               when Warning => Log_Warning_Tag,
                                               when Error => Log_Error_Tag,
                                               when Board => Log_Board_Tag));
--        if Scroll_To_Iter (Log_View, Iter, 1.0, False, 0.0, 0.0) then
--           null;
--        end if;
   end Log;

   ---------
   -- Put --
   ---------

   overriding
   procedure Put (Ctx : in out GTK_CNC; Str : String) is
      pragma Unreferenced (Ctx);
      Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Log_Buffer.Get_End_Iter (Iter);
      Log_Buffer.Insert (Iter, Str);
      Log_Buffer.Get_End_Iter (Iter);
--        if Scroll_To_Iter (Log_View, Iter, 0.0, False, 0.0, 0.0) then
--           null;
--        end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   overriding
   procedure Put_Line (Ctx : in out GTK_CNC; Str : String) is
   begin
      Ctx.Put (Str & ASCII.CR);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   overriding
   procedure New_Line (Ctx : in out GTK_CNC) is
      Str : String (1 .. 1);
   begin
      Str (1) := ASCII.CR;
      Ctx.Put (Str);
   end New_Line;

   ---------
   -- Put --
   ---------

   overriding
   procedure Put (Ctx : in out GTK_CNC; C : Character) is
      Str : String (1 .. 1);
   begin
      Str (1) := C;
      Ctx.Put (Str);
   end Put;

   ------------------
   -- Stepper_Tick --
   ------------------

   function Stepper_Tick return Boolean is
   begin
      return True;
   end Stepper_Tick;

end Station_Gtk;
