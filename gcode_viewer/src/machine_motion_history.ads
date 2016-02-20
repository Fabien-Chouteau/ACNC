with Gcode; use Gcode;
with Cairo; use Cairo;
with Glib; use Glib;

package Machine_Motion_history is
   procedure Set_Step_Direction (Axis : Axis_Name;
                                 Dir : Direction);
   procedure Clear_Step_Pin (Axis : Axis_Name);
   procedure Set_Step_Pin (Axis : Axis_Name);

   function Current_Position return Gcode.Step_Position;
   procedure Draw_History (Cr : Cairo_Context; Zoom : Gdouble);
   procedure Clear_History;
private

end Machine_Motion_history;
