------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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

with Ada.Interrupts.Names; use Ada.Interrupts.Names;

with STM32.DMA;    use STM32.DMA;
with STM32.GPIO;   use STM32.GPIO;
with STM32.USARTs; use STM32.USARTs;

with STM32.Device; use STM32.Device;

package Peripherals is

   Transceiver : USART renames USART_2;

   Transceiver_AF : constant STM32.GPIO_Alternate_Function := GPIO_AF_USART2_7;

   TX_Pin : constant GPIO_Point := PA2;
   RX_Pin : constant GPIO_Point := PA3;

   Controller : DMA_Controller renames DMA_1;

   Tx_Channel : constant DMA_Channel_Selector := Channel_4;

   Tx_Stream : constant DMA_Stream_Selector := Stream_6;

   --  See RM0090, section 10.3.3, for the DMA channel request mapping tables
   --  that say which controllers, and which channels and streams on those
   --  controllers, can connect to which devices. For example, it is channel
   --  four and stream six that connect DMA1 to the transmitter of USART2, so
   --  we specify those values above.

   DMA_Tx_IRQ : constant Ada.Interrupts.Interrupt_ID := DMA1_Stream6_Interrupt;
   --  must match that of the selected controller and stream number!!!!

end Peripherals;
