-------------------------------------------------------------------------------
--                                                                           --
--                                   ACNC                                    --
--                                                                           --
--       Copyright (C) 2016-2017 Fabien Chouteau (chouteau@adacore.com)      --
--                                                                           --
--                                                                           --
--    ACNC is free software: you can redistribute it and/or modify it        --
--    under the terms of the GNU General Public License as published by      --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    ACNC is distributed in the hope that it will be useful, but WITHOUT    --
--    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY     --
--    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public        --
--    License for more details.                                              --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with ACNC. If not, see <http://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Gcode.Parser;
with Gcode.Execution;
with Gcode.Error;
with Coms;
with Ada.Synchronous_Task_Control;
with System;
with Make_With_Ada_Gcode;

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
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (Demo_Mode_Button);
      Configuration := (Mode        => Mode_In,
                        Resistors   => Pull_Up);
      Demo_Mode_Button.Configure_IO (Configuration);

      Enable_Clock (Demo_Mode_LED);
      Configuration := (Mode        => Mode_Out,
                        Resistors   => Pull_Up,
                        Output_Type => Open_Drain,
                        Speed       => Speed_50MHz);

      Demo_Mode_LED.Configure_IO (Configuration);
      Demo_Mode_LED.Clear;

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

      for Str_Ptr of Make_With_Ada_Gcode.Gcode loop
         Gcode_Controller.Execute (Str_Ptr.all);
      end loop;

      if not Demo_Mode_Button.Set then
         loop
            Demo_Mode_LED.Set;
            while not Demo_Mode_Button.Set loop
               --  Waiting for button release...
               null;
            end loop;

            while Demo_Mode_Button.Set loop
               --  Waiting for button press...
               null;
            end loop;
            Demo_Mode_LED.Clear;

            for Str_Ptr of Make_With_Ada_Gcode.Gcode loop
               Gcode_Controller.Execute (Str_Ptr.all);
            end loop;
         end loop;
      end if;

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
