with Gtkada.Builder; use Gtkada.Builder;
with Gcode.Context;

package Control_Window is
   type Gcode_Context_Ref is access all Gcode.Context.GContext'Class;

   procedure Register_Handlers (Builder : Gtkada_Builder;
                                Ctx : Gcode_Context_Ref);
end Control_Window;
