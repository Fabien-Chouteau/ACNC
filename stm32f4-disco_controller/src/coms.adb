with Ada.Unchecked_Conversion;
with STM32; use STM32;
with Interfaces.Bit_Types; use Interfaces.Bit_Types;

package body Coms is

   type DMA_Data is array (Integer range <>) of Interfaces.Unsigned_8;

   procedure Ready_To_Receive (Enable : Boolean);

   ----------------------
   -- Ready_To_Receive --
   ----------------------

   procedure Ready_To_Receive (Enable : Boolean) is
   begin
      if Enable then
         RTS_Pin.Clear;
      else
         RTS_Pin.Set;
      end if;
   end Ready_To_Receive;

   -------------------------------
   -- Initialize_GPIO_Port_Pins --
   -------------------------------

   procedure Initialize_GPIO_Port_Pins is
      Configuration : GPIO_Port_Configuration;
      Points : constant GPIO_Points := Rx_Pin & Tx_Pin & CTS_Pin;
   begin
      Enable_Clock (Points);

      Configuration.Mode := Mode_AF;
      Configuration.Speed := Speed_50MHz;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors := Pull_Up;

      Configure_IO
        (Points, Configuration);

      Configure_Alternate_Function
        (Points, Transceiver_AF);

      --  Manual RTS controll
      Configuration.Mode := Mode_Out;
      Configuration.Speed := Speed_50MHz;
      RTS_Pin.Configure_IO (Configuration);
      Ready_To_Receive (False);
   end Initialize_GPIO_Port_Pins;

   ----------------------
   -- Initialize_USART --
   ----------------------

   procedure Initialize_USART is
   begin
      Enable_Clock (Transceiver);

      Enable (Transceiver);

      Set_Baud_Rate    (Transceiver, 115_200);
      Set_Mode         (Transceiver, Tx_Rx_Mode);
      Set_Stop_Bits    (Transceiver, Stopbits_1);
      Set_Word_Length  (Transceiver, Word_Length_8);
      Set_Parity       (Transceiver, No_Parity);
      Set_Flow_Control (Transceiver, CTS_Flow_Control);
      --  Because of FTDI's potential 3 character overrun
      --  (http://www.ftdichip.com/Support/FAQs.htm#HwGen3)
      --  RTS controll is manuall.
   end Initialize_USART;

   --------------------
   -- Initialize_DMA --
   --------------------

   procedure Initialize_DMA is
      Configuration : DMA_Stream_Configuration;
   begin
      Enable_Clock (Controller);

      Configuration.Channel                      := Tx_Channel;
      Configuration.Direction                    := Memory_To_Peripheral;
      Configuration.Increment_Peripheral_Address := False;
      Configuration.Increment_Memory_Address     := True;
      Configuration.Peripheral_Data_Format       := Bytes;
      Configuration.Memory_Data_Format           := Bytes;
      Configuration.Operation_Mode               := Normal_Mode;
      Configuration.Priority                     := Priority_Very_High;
      Configuration.FIFO_Enabled                 := True;
      Configuration.FIFO_Threshold               := FIFO_Threshold_Full_Configuration;
      Configuration.Memory_Burst_Size            := Memory_Burst_Inc4;
      Configuration.Peripheral_Burst_Size        := Peripheral_Burst_Inc4;

      Configure (Controller, Tx_Stream, Configuration);
      --  note the controller is disabled by the call to Configure
   end Initialize_DMA;

   ---------------
   -- Initalize --
   ---------------

   procedure Initalize is
   begin
      Initialize_GPIO_Port_Pins;
      Initialize_USART;
      Initialize_DMA;
      Enable (Transceiver);
      Enable_Interrupts (Transceiver, Source => Received_Data_Not_Empty);
      Ready_To_Receive (True);
   end Initalize;

   ----------------------------
   -- UART_Get_Data_Blocking --
   ----------------------------

   procedure UART_Get_Data_Blocking (C : out Character) is
      function Data_To_Character is new Ada.Unchecked_Conversion
        (Interfaces.Unsigned_8, Character);
      Rx_Byte : Interfaces.Unsigned_8;
   begin
      if Status (Transceiver, Overrun_Error_Indicated) then
         Clear_Status (Transceiver, Overrun_Error_Indicated);
         raise Program_Error with "USART overrun";
      end if;

      Rx_IRQ_Handler.Await_Byte_Reception (Rx_Byte);
      C := Data_To_Character (Rx_Byte);
   end UART_Get_Data_Blocking;

   ---------------------------------
   -- UART_Send_DMA_Data_Blocking --
   ---------------------------------

   procedure UART_Send_DMA_Data_Blocking
     (Data      : String)
   is
      function Character_To_Data is new Ada.Unchecked_Conversion
        (Character, Interfaces.Unsigned_8);
      Source_Block : DMA_Data (Data'Range);
   begin
      for Index in Source_Block'Range loop
         Source_Block (Index) := Character_To_Data (Data (Index));
      end loop;

      Start_Transfer_with_Interrupts
        (Controller,
         Tx_Stream,
         Source      => Source_Block'Address,
         Destination => Data_Register_Address (Transceiver),
         Data_Count  => Short (Source_Block'Length));
      --  also enables the stream

      Enable_DMA_Transmit_Requests (Transceiver);

      Tx_IRQ_Handler.Await_Transfer_Complete;
   end UART_Send_DMA_Data_Blocking;
   -------------------------------
   -- Finalize_DMA_Transmission --
   -------------------------------

   procedure Finalize_DMA_Transmission (Transceiver : in out USART) is
      --  see static void USART_DMATransmitCplt
   begin
      loop
         exit when Status (Transceiver, Transmission_Complete_Indicated);
      end loop;
      Clear_Status (Transceiver, Transmission_Complete_Indicated);
      Disable_DMA_Transmit_Requests (Transceiver);
   end Finalize_DMA_Transmission;

   --------------------
   -- Tx_IRQ_Handler --
   --------------------

   protected body Tx_IRQ_Handler is

      -----------------------------
      -- Await_Transfer_Complete --
      -----------------------------

      entry Await_Transfer_Complete when Transfer_Complete is
      begin
         Event_Occurred := False;
         Transfer_Complete := False;
      end Await_Transfer_Complete;

      -----------------
      -- IRQ_Handler --
      -----------------

      procedure IRQ_Handler is
      begin
         --  Transfer Error Interrupt management
         if Status (Controller, Tx_Stream, Transfer_Error_Indicated) then
            if Interrupt_Enabled
              (Controller, Tx_Stream, Transfer_Error_Interrupt)
            then
               Disable_Interrupt
                 (Controller, Tx_Stream, Transfer_Error_Interrupt);
               Clear_Status (Controller, Tx_Stream, Transfer_Error_Indicated);
               Event_Kind := Transfer_Error_Interrupt;
               Event_Occurred := True;
               return;
            end if;
         end if;

         --  FIFO Error Interrupt management.
         if Status (Controller, Tx_Stream, FIFO_Error_Indicated) then
            if Interrupt_Enabled
              (Controller, Tx_Stream, FIFO_Error_Interrupt)
            then
               Disable_Interrupt (Controller, Tx_Stream, FIFO_Error_Interrupt);
               Clear_Status (Controller, Tx_Stream, FIFO_Error_Indicated);
               Event_Kind := FIFO_Error_Interrupt;
               Event_Occurred := True;
               return;
            end if;
         end if;

         --  Direct Mode Error Interrupt management
         if Status (Controller, Tx_Stream, Direct_Mode_Error_Indicated) then
            if Interrupt_Enabled
              (Controller, Tx_Stream, Direct_Mode_Error_Interrupt)
            then
               Disable_Interrupt
                 (Controller, Tx_Stream, Direct_Mode_Error_Interrupt);
               Clear_Status
                 (Controller, Tx_Stream, Direct_Mode_Error_Indicated);
               Event_Kind := Direct_Mode_Error_Interrupt;
               Event_Occurred := True;
               return;
            end if;
         end if;

         --  Half Transfer Complete Interrupt management
         if Status
           (Controller, Tx_Stream, Half_Transfer_Complete_Indicated)
         then
            if Interrupt_Enabled
              (Controller, Tx_Stream, Half_Transfer_Complete_Interrupt)
            then
               if Double_Buffered (Controller, Tx_Stream) then
                  Clear_Status
                    (Controller, Tx_Stream, Half_Transfer_Complete_Indicated);
               else -- not double buffered
                  if not Circular_Mode (Controller, Tx_Stream) then
                     Disable_Interrupt
                       (Controller,
                        Tx_Stream,
                        Half_Transfer_Complete_Interrupt);
                  end if;
                  Clear_Status
                    (Controller, Tx_Stream, Half_Transfer_Complete_Indicated);
               end if;
               Event_Kind := Half_Transfer_Complete_Interrupt;
               Event_Occurred := True;
            end if;
         end if;

         --  Transfer Complete Interrupt management
         if Status (Controller, Tx_Stream, Transfer_Complete_Indicated) then
            if Interrupt_Enabled
              (Controller, Tx_Stream, Transfer_Complete_Interrupt)
            then
               if Double_Buffered
                 (Controller, Tx_Stream)
               then
                  Clear_Status
                    (Controller, Tx_Stream, Transfer_Complete_Indicated);
                  --  TODO: handle the difference between M0 and M1 callbacks
               else
                  if not Circular_Mode (Controller, Tx_Stream) then
                     Disable_Interrupt
                       (Controller, Tx_Stream, Transfer_Complete_Interrupt);
                  end if;
                  Clear_Status
                    (Controller, Tx_Stream, Transfer_Complete_Indicated);
               end if;
               Finalize_DMA_Transmission (Transceiver);
               Event_Kind := Transfer_Complete_Interrupt;
               Event_Occurred := True;
               Transfer_Complete := True;
            end if;
         end if;
      end IRQ_Handler;

   end Tx_IRQ_Handler;
   --------------------
   -- Rx_IRQ_Handler --
   --------------------

   protected body Rx_IRQ_Handler is

      --------------------------
      -- Await_Byte_Reception --
      --------------------------

      entry Await_Byte_Reception (Rx_Byte : out Interfaces.Unsigned_8)
        when Not_Empty is
      begin
         Remove (Rx_Byte);
         --  Manual RTS controll to handle FTDI 0-3 char overrun
         if Extent <= 3 then
            Ready_To_Receive (True);
         end if;
      end Await_Byte_Reception;

      -----------------
      -- IRQ_Handler --
      -----------------

      procedure IRQ_Handler is
         Received_Byte : Interfaces.Unsigned_8;
      begin
         while Status (Transceiver, Read_Data_Register_Not_Empty) loop
            Received_Byte :=
              Interfaces.Unsigned_8 (Current_Input (Transceiver) and 16#FF#);
            Clear_Status (Transceiver, Read_Data_Register_Not_Empty);
            Insert (Received_Byte);

            --  Manual RTS controll to handle FTDI 0-3 char overrun
            if Extent >= Buffer_Capacity - 4 then
               Ready_To_Receive (False);
            end if;
         end loop;
      end IRQ_Handler;

      ------------
      -- Insert --
      ------------

      procedure Insert (Item : Interfaces.Unsigned_8) is
      begin
         Values (Next_In) := Item;
         Next_In := (Next_In mod Buffer_Capacity) + 1;
         Count := Count + 1;
         Not_Empty := True;
      end Insert;

      ------------
      -- Remove --
      ------------

      procedure Remove (Item : out Interfaces.Unsigned_8) is
      begin
         Item := Values (Next_Out);
         Next_Out := (Next_Out mod Buffer_Capacity) + 1;
         Count := Count - 1;
         Not_Empty := Count /= 0;
      end Remove;

      -----------
      -- Empty --
      -----------

      function Empty return Boolean is
      begin
         return Count = 0;
      end Empty;

      ----------
      -- Full --
      ----------

      function Full return Boolean is
      begin
         return Count = Buffer_Capacity;
      end Full;

      ------------
      -- Extent --
      ------------

      function Extent return Natural is
      begin
         return Count;
      end Extent;
   end Rx_IRQ_Handler;
end Coms;
