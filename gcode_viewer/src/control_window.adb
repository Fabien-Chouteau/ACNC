with Gcode.Parser;
with Gcode.Execution;
with Gcode; use Gcode;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Gtk.Radio_Button; use Gtk.Radio_Button;

--------------------
-- Control_Window --
--------------------

package body Control_Window is

   procedure Simulate_Gcode_Line (Line : String);
   function Home_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Left_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Rigth_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Fwd_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Bwd_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Up_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Down_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Plus_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Minus_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   function Stop_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class) return Boolean;
   procedure Jog_Command (Axis : Axis_Name;
                          Distance : Float_Value);
   function Get_Jog_Distance return Float_Value;

   My_Ctx : Gcode_Context_Ref := null;
   Jog_Small : Gtk_Radio_Button := null;
   Jog_Medium : Gtk_Radio_Button := null;
   Jog_Big : Gtk_Radio_Button := null;

   -----------------
   -- Jog_Command --
   -----------------

   procedure Jog_Command (Axis : Axis_Name;
                          Distance : Float_Value)
   is
      Dist : String (1 .. 15);
   begin
      Put (Dist, Distance,
           Aft  => 5,
           Exp  => 0);
      Simulate_Gcode_Line ("G91");
      Simulate_Gcode_Line ("G01 " & To_Letter (Axis) & Dist);
      Simulate_Gcode_Line ("G90");
   end Jog_Command;

   ----------------------
   -- Get_Jog_Distance --
   ----------------------

   function Get_Jog_Distance return Float_Value is
   begin
      if Jog_Small /= null and then Jog_Small.Get_Active then
         return 0.1;
      elsif Jog_Medium /= null and then Jog_Medium.Get_Active then
         return 1.0;
      elsif Jog_Big /= null and then Jog_Big.Get_Active then
         return 10.0;
      else
         return 0.0;
      end if;
   end Get_Jog_Distance;

   -------------------------
   -- Simulate_Gcode_Line --
   -------------------------

   procedure Simulate_Gcode_Line (Line : String) is
   begin
      if Gcode.Parser.Parse (Line, My_Ctx.all) then
         if Gcode.Execution.Execute (Line, My_Ctx.all) then
            null;
         else
            null;
         end if;
      end if;
   end Simulate_Gcode_Line;

   --------------------
   -- Home_Btn_Press --
   --------------------

   function Home_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      Simulate_Gcode_Line ("G28 ; Homing");
      return False;
   end Home_Btn_Press;

   --------------------
   -- Left_Btn_Press --
   --------------------

   function Left_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      Jog_Command (X_Axis, -Get_Jog_Distance);
      return False;
   end Left_Btn_Press;

   ---------------------
   -- Rigth_Btn_Press --
   ---------------------

   function Rigth_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      Jog_Command (X_Axis, Get_Jog_Distance);
      return False;
   end Rigth_Btn_Press;

   -------------------
   -- Fwd_Btn_Press --
   -------------------

   function Fwd_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      Jog_Command (Y_Axis, Get_Jog_Distance);
      return False;
   end Fwd_Btn_Press;

   -------------------
   -- Bwd_Btn_Press --
   -------------------

   function Bwd_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      Jog_Command (Y_Axis, -Get_Jog_Distance);
      return False;
   end Bwd_Btn_Press;

   ------------------
   -- Up_Btn_Press --
   ------------------

   function Up_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      Jog_Command (Z_Axis, Get_Jog_Distance);
      return False;
   end Up_Btn_Press;

   --------------------
   -- Down_Btn_Press --
   --------------------

   function Down_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      Jog_Command (Z_Axis, -Get_Jog_Distance);
      return False;
   end Down_Btn_Press;

   --------------------
   -- Plus_Btn_Press --
   --------------------

   function Plus_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      return False;
   end Plus_Btn_Press;

   ---------------------
   -- Minus_Btn_Press --
   ---------------------

   function Minus_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      return False;
   end Minus_Btn_Press;

   --------------------
   -- Stop_Btn_Press --
   --------------------

   function Stop_Btn_Press
     (User_Data : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      return False;
   end Stop_Btn_Press;

   -----------------------
   -- Register_Handlers --
   -----------------------

   procedure Register_Handlers (Builder : Gtkada_Builder;
                                Ctx : Gcode_Context_Ref)
   is
   begin
      My_Ctx := Ctx;
      Register_Handler (Builder, "home_btn_press", Home_Btn_Press'Access);
      Register_Handler (Builder, "left_btn_press", Left_Btn_Press'Access);
      Register_Handler (Builder, "right_btn_press", Rigth_Btn_Press'Access);
      Register_Handler (Builder, "fwd_btn_press", Fwd_Btn_Press'Access);
      Register_Handler (Builder, "bwd_btn_press", Bwd_Btn_Press'Access);
      Register_Handler (Builder, "up_btn_press", Up_Btn_Press'Access);
      Register_Handler (Builder, "down_btn_press", Down_Btn_Press'Access);
      Register_Handler (Builder, "plus_btn_press", Plus_Btn_Press'Access);
      Register_Handler (Builder, "minus_btn_press", Minus_Btn_Press'Access);
      Register_Handler (Builder, "stop_btn_press", Stop_Btn_Press'Access);

      Jog_Small := Gtk_Radio_Button (Builder.Get_Object ("jog_01mm"));
      Jog_Medium := Gtk_Radio_Button (Builder.Get_Object ("jog_1mm"));
      Jog_Big := Gtk_Radio_Button (Builder.Get_Object ("jog_10mm"));

   end Register_Handlers;

end Control_Window;
