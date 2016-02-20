with Gcode.Parser;
with Gcode.Execution;
with Gcode.Error;
with Coms;

----------------------
-- Gcode_Controller --
----------------------

package body Gcode_Controller is

   ---------------
   -- Initalize --
   ---------------

   procedure Initalize is
   begin
      null;
   end Initalize;

   -------------
   -- Execute --
   -------------

   procedure Execute (Str : String) is
   begin
      if not Gcode.Parser.Parse (Str, Ctx) then
         Ctx.Put_Line ("Gcode parsing failed");
         return;
      end if;

      if not Gcode.Execution.Execute (Str, Ctx) then
         Ctx.Put_Line ("Gcode execution failed");
      end if;
   exception
      when E : Gcode.Error.Gcode_Exception =>
         Ctx.Put_Line ("Gcode exception");
      when others =>
         Ctx.Put_Line ("other exception");
   end Execute;

   ---------
   -- Put --
   ---------

   procedure Put (Ctx : in out CNC_Context; C : Character) is
   begin
      Ctx.Output_Buffer (Ctx.Output_Index) := C;
      if Ctx.Output_Index = Ctx.Output_Buffer'Last or else C = ASCII.LF then
         Coms.UART_Send_DMA_Data_Blocking
           (Ctx.Output_Buffer (Ctx.Output_Buffer'First .. Ctx.Output_Index));
         Ctx.Output_Index := 1;
      else
         Ctx.Output_Index := Ctx.Output_Index + 1;
      end if;
   end Put;

end Gcode_Controller;
