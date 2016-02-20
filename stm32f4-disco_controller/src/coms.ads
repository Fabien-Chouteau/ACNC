with STM32.Device; use STM32.Device;
with STM32.GPIO; use STM32.GPIO;
with STM32.USARTs; use STM32.USARTs;
with STM32.Timers; use STM32.Timers;
with STM32.DMA; use STM32.DMA;
with Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with Interfaces;

package Coms is
   procedure Initalize;
   procedure UART_Send_DMA_Data_Blocking (Data : String);
   procedure UART_Get_Data_Blocking (C : out Character);
private
   IO_Port : GPIO_Port renames GPIO_A;

   Transceiver : USART renames USART_2;

   Transceiver_AF : constant GPIO_Alternate_Function := GPIO_AF_USART2;

   TX_Pin : constant GPIO_Pin := Pin_2;
   RX_Pin : constant GPIO_Pin := Pin_3;

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
   end Rx_IRQ_Handler;
end Coms;
