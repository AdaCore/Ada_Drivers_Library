------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Interrupts;
with Ada.Interrupts.Names;
with Ada.Real_Time;
with System;

with STM32.DMA;      use STM32.DMA;
with STM32.GPIO;     use STM32.GPIO;
with STM32.USARTs;   use STM32.USARTs;
with STM32.Device;
with HAL.UART;       use HAL.UART;
with HM11;           use HM11;

package Drivers is

   TX     : GPIO_Point renames STM32.Device.PC12; --  UART5_TX
   --  Connect to HM-11 RX (4)
   RX     : GPIO_Point renames STM32.Device.PD2;  --  UART5_RX
   --  Connect to HM-11 TX (2)

   -- DMA --
   Controller : DMA_Controller renames STM32.Device.DMA_1;
   Rx_Channel : constant DMA_Channel_Selector := Channel_4;
   Rx_Stream  : constant DMA_Stream_Selector := Stream_0;
   DMA_Tx_IRQ : constant Ada.Interrupts.Interrupt_ID :=
     Ada.Interrupts.Names.DMA1_Stream0_Interrupt;

   --  UART --
   UART     : STM32.USARTs.USART renames STM32.Device.UART_5;
   UART_IRQ : constant Ada.Interrupts.Interrupt_ID :=
     Ada.Interrupts.Names.UART5_Interrupt;

   procedure DMA_Receive_Handler
     (Port      : HAL.UART.Any_UART_Port;
      Received  : System.Address;
      Length    : Natural;
      Status    : out UART_Status;
      Timeout   : Natural := 1000;
      As_Stream : Boolean := False);
   --  Reads UART5 RX with DMA

   procedure Last_Read_Handler
     (Closed : out Boolean;
      Zero   : out Positive);

   --  HM-11 Driver
   Driver : HM11_Driver
     (UART'Access, DMA_Receive_Handler'Access, Last_Read_Handler'Access);

   procedure Init_UART;
   --  Initialize GPIO/UART for HM-11/cc2541

   procedure Initialize_DMA;
   --  Initialize DMA for UART5 RX

private

   -- Watchdog --
   protected Watchdog is
      pragma Interrupt_Priority;

      procedure Start
        (Received : System.Address;
         Length   : Natural;
         Timeout  : Ada.Real_Time.Time_Span);

      procedure Stop;

      entry Await_Event (Status : out UART_Status);

      procedure Release (Status : UART_Status);

      procedure Readed
        (Closed : out Boolean;
         Zero   : out Positive);

   private
      Started        : Boolean := False;
      Event_Occurred : Boolean := False;

      Read_Status    : UART_Status := Ok;
   end Watchdog;

   -- IRQ_UART_Handler --
   protected IRQ_UART_Handler is
      pragma Interrupt_Priority;

   private
      procedure On_UART_IRQ;
      pragma Attach_Handler (On_UART_IRQ, UART_IRQ);

   end IRQ_UART_Handler;

   -- IRQ_DMA_Handler --
   protected IRQ_DMA_Handler is
      pragma Interrupt_Priority;

   private

      procedure On_DMA_IRQ;
      pragma Attach_Handler (On_DMA_IRQ, DMA_Tx_IRQ);
   end IRQ_DMA_Handler;

end Drivers;
