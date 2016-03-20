with Gtkada.Builder; use Gtkada.Builder;
with Gcode.Context;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;

package Control_Window is
   type Gcode_Context_Ref is access all Gcode.Context.GContext'Class;
   type Serial_Port_Ref is access all GNAT.Serial_Communications.Serial_Port;
   procedure Register_Handlers (Builder : Gtkada_Builder;
                                Ctx : Gcode_Context_Ref);

   procedure Set_Serial (Serial : Serial_Port_Ref);
end Control_Window;
