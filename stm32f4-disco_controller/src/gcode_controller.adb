with Gcode.Parser;
with Gcode.Execution;
with Gcode.Error;
with Coms;
with Ada.Synchronous_Task_Control;
with System;

package body Gcode_Controller is

   Task_Sync : Ada.Synchronous_Task_Control.Suspension_Object;

   task Execution_Task is
      pragma Priority (System.Default_Priority);
      pragma Storage_Size (40 * 1024);
   end Execution_Task;

   ---------------
   -- Initalize --
   ---------------

   procedure Initalize is
   begin
      Ctx.Output_Index := Ctx.Output_Buffer'First;
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
      when Gcode.Error.Gcode_Exception =>
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

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Ada.Synchronous_Task_Control.Set_True (Task_Sync);
   end Start;

   --------------------
   -- Execution_Task --
   --------------------

   task body Execution_Task is
      C : Character;
      subtype Buffer_Range is Positive range 1 .. 1024;
      Buffer : String (Buffer_Range);
      Index : Buffer_Range := Buffer_Range'First;
   begin
      Ada.Synchronous_Task_Control.Suspend_Until_True (Task_Sync);
      Coms.UART_Send_DMA_Data_Blocking
        ("--- ACNC ---" &
           ASCII.CR & ASCII.LF);
      loop
         Coms.UART_Get_Data_Blocking (C);
         Buffer (Index) := C;
         if C = ASCII.CR then
            Coms.UART_Send_DMA_Data_Blocking
              ("executing: '" & Buffer (1 .. Index - 1) & "'" &
                 ASCII.CR & ASCII.LF);
            Gcode_Controller.Execute (Buffer (1 .. Index - 1));
            Index := Buffer_Range'First;
         elsif Index = Buffer_Range'Last then
            Coms.UART_Send_DMA_Data_Blocking
              ("Error: Receive buffer full" & ASCII.CR & ASCII.LF);
            Index := Buffer_Range'First;
         else
            Index := Index + 1;
         end if;
      end loop;
   end Execution_Task;

end Gcode_Controller;
