with Gcode; use Gcode;
with Cairo; use Cairo;
with Glib; use Glib;

package Machine_Motion_history is
   function Current_Position return Gcode.Step_Position;
   procedure Draw_History (Cr : Cairo_Context; Zoom : Gdouble);
   procedure Set_Time_Factor (Factor : Integer);
   procedure Clear_History;
end Machine_Motion_history;
