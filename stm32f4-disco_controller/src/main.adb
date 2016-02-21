with Gcode_Controller;
with Step_Control;
with Coms;
with Interfaces.C;

procedure Main is
   C : Character;
   subtype Buffer_Range is Positive range 1 .. 256;
   Buffer : String (Buffer_Range);
   Index : Buffer_Range := Buffer_Range'First;
begin
   Gcode_Controller.Initalize;
   Step_Control.Initalize;
   Coms.Initalize;
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
end Main;
