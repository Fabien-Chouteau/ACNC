with STM32.Device; use STM32.Device;
with STM32.GPIO; use STM32.GPIO;
with STM32.USARTs; use STM32.USARTs;
with STM32.DMA; use STM32.DMA;
with Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Interfaces;

package Coms is

   procedure Initalize;

   procedure UART_Send_DMA_Data_Blocking (Data : String);

   procedure UART_Get_Data_Blocking (C : out Character);

private

   Buffer_Capacity : constant := 10;

   Transceiver : USART renames USART_2;

   Transceiver_AF : constant GPIO_Alternate_Function := GPIO_AF_USART2;

   TX_Pin  : GPIO_Point := PA2;
   RX_Pin  : GPIO_Point := PA3;
   CTS_Pin : GPIO_Point := PA0;
   RTS_Pin : GPIO_Point := PA1;

   Controller : STM32.Device.DMA_Controller renames STM32.Device.DMA_1;

   Tx_Channel : constant DMA_Channel_Selector := Channel_4;

   Tx_Stream : constant DMA_Stream_Selector := Stream_6;

   DMA_Tx_IRQ : constant Ada.Interrupts.Interrupt_Id := DMA1_Stream6_Interrupt;
   -- must match that of the selected controller and stream number!!!!

   --  DMA Interrupt Handler for transmission.
   protected Tx_IRQ_Handler is
      pragma Interrupt_Priority;

      entry Await_Transfer_Complete;

   private

      Event_Occurred    : Boolean := False;
      Transfer_Complete : Boolean := False;
      Event_Kind        : DMA_Interrupt;

      procedure IRQ_Handler;
      pragma Attach_Handler (IRQ_Handler, DMA_Tx_IRQ);

   end Tx_IRQ_Handler;

   type Content is array (Positive range <>) of Interfaces.Unsigned_8;

   --  Interrupt Handler for reception (DMA not used here).
   protected Rx_IRQ_Handler is
      pragma Interrupt_Priority;

      entry Await_Byte_Reception (Rx_Byte : out Interfaces.Unsigned_8);

   private


      Byte_Avalaible  : Boolean := False;
      Data : Interfaces.Unsigned_8;
      --  Rx_Queue        : T_Queue (UART_RX_QUEUE_SIZE);

      procedure IRQ_Handler;
      pragma Attach_Handler (IRQ_Handler, USART2_Interrupt);

      ------------
      -- Buffer --
      ------------

      procedure Insert (Item : Interfaces.Unsigned_8);
      --  Insert Item into the buffer, blocks caller until space is available

      procedure Remove (Item : out Interfaces.Unsigned_8);
      --  Remove next available Element from buffer.

      function Empty return Boolean;
      --  Returns whether the instance contains any Elements.
      --  Note: State may change immediately after call returns.

      function Full return Boolean;
      --  Returns whether any space remains within the instance.
      --  Note: State may change immediately after call returns.

      function Extent return Natural;
      --  Returns the number of Element values currently held
      --  within the instance.
      --  Note: State may change immediately after call returns.

      Values   : Content (1 .. Buffer_Capacity);
      --  The container for the values held by the buffer instance

      Next_In  : Positive := 1;
      --  The index of the next Element inserted. Wraps around

      Next_Out : Positive := 1;
      --  The index of the next Element removed. Wraps around

      Count    : Natural  := 0;
      --  The number of Elements currently held

      Not_Empty : Boolean := False;
      --  For Ravenscar entry barrier
   end Rx_IRQ_Handler;
end Coms;
