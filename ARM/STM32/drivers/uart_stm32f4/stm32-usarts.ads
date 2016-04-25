------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_usart.h                                         --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of USARTS HAL module.                             --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides register definitions for the STM32F4 (ARM Cortex M4F)
--  USART from ST Microelectronics.

--  Note that there are board implementation assumptions represented by the
--  private function APB_Clock.

pragma Restrictions (No_Elaboration_Code);

with System;
private with STM32_SVD.USART;

package STM32.USARTs is

   type USART is limited private;

   procedure Enable (This : in out USART)
     with
       Post => Enabled (This),
       Inline;

   procedure Disable (This : in out USART)
     with
       Post => not Enabled (This),
       Inline;

   function Enabled (This : USART) return Boolean with Inline;

   procedure Receive (This : USART;  Data : out UInt9) with Inline;
   --  reads Device.DR into Data

   function Current_Input (This : USART) return UInt9 with Inline;
   --  returns Device.DR

   procedure Transmit (This : in out USART;  Data : UInt9) with Inline;

   function Tx_Ready (This : USART) return Boolean with Inline;

   function Rx_Ready (This : USART) return Boolean with Inline;

   type Stop_Bits is (Stopbits_1, Stopbits_2)
     with Size => 2;
   for Stop_Bits use (Stopbits_1 => 0, Stopbits_2 => 2#10#);

   procedure Set_Stop_Bits (This : in out USART;  To : Stop_Bits);

   type Word_Lengths is (Word_Length_8, Word_Length_9);

   procedure Set_Word_Length (This : in out USART;  To : Word_Lengths);

   type Parities is (No_Parity, Even_Parity, Odd_Parity);

   procedure Set_Parity (This : in out USART;  To : Parities);

   subtype Baud_Rates is Word;

   procedure Set_Baud_Rate (This : in out USART;  To : Baud_Rates);

   type Oversampling_Modes is (Oversampling_By_8, Oversampling_By_16);
   --  oversampling by 16 is the default

   procedure Set_Oversampling_Mode
     (This : in out USART;
      To   : Oversampling_Modes);

   type UART_Modes is (Rx_Mode, Tx_Mode, Tx_Rx_Mode);

   procedure Set_Mode (This : in out USART;  To : UART_Modes);

   type Flow_Control is
     (No_Flow_Control,
      RTS_Flow_Control,
      CTS_Flow_Control,
      RTS_CTS_Flow_Control);
   procedure Set_Flow_Control (This : in out USART;  To : Flow_Control);

   type USART_Interrupt is
     (Parity_Error,
      Transmit_Data_Register_Empty,
      Transmission_Complete,
      Received_Data_Not_Empty,
      Idle_Line_Detection,
      Line_Break_Detection,
      Clear_To_Send,
      Error);

   procedure Enable_Interrupts
     (This   : in out USART;
      Source : USART_Interrupt)
     with
       Post => Interrupt_Enabled (This, Source),
       Inline;

   procedure Disable_Interrupts
     (This   : in out USART;
      Source : USART_Interrupt)
     with
       Post => not Interrupt_Enabled (This, Source),
        Inline;

   function Interrupt_Enabled
     (This   : USART;
      Source : USART_Interrupt)
      return Boolean
     with Inline;

   type USART_Status_Flag is
     (Parity_Error_Indicated,
      Framing_Error_Indicated,
      USART_Noise_Error_Indicated,
      Overrun_Error_Indicated,
      Idle_Line_Detection_Indicated,
      Read_Data_Register_Not_Empty,
      Transmission_Complete_Indicated,
      Transmit_Data_Register_Empty,
      Line_Break_Detection_Indicated,
      Clear_To_Send_Indicated);

   function Status (This : USART; Flag : USART_Status_Flag) return Boolean
     with Inline;

   procedure Clear_Status (This : in out USART; Flag : USART_Status_Flag)
     with Inline;

   procedure Enable_DMA_Transmit_Requests (This : in out USART)
     with
       Inline,
       Post => DMA_Transmit_Requests_Enabled (This);

   procedure Disable_DMA_Transmit_Requests (This : in out USART)
     with
       Inline,
       Post => not DMA_Transmit_Requests_Enabled (This);

   function DMA_Transmit_Requests_Enabled  (This : USART) return Boolean
     with Inline;


   procedure Enable_DMA_Receive_Requests (This : in out USART)
     with
       Inline,
       Post => DMA_Receive_Requests_Enabled (This);

   procedure Disable_DMA_Receive_Requests (This : in out USART)
     with
       Inline,
       Post => not DMA_Receive_Requests_Enabled (This);

   function DMA_Receive_Requests_Enabled  (This : USART) return Boolean
     with Inline;

   procedure Pause_DMA_Transmission (This : in out USART)
     renames Disable_DMA_Transmit_Requests;

   procedure Resume_DMA_Transmission (This : in out USART)
     with
       Inline,
       Post => DMA_Transmit_Requests_Enabled (This) and
               Enabled (This);

   procedure Pause_DMA_Reception (This : in out USART)
     renames Disable_DMA_Receive_Requests;

   procedure Resume_DMA_Reception (This : in out USART)
     with
       Inline,
       Post => DMA_Receive_Requests_Enabled (This) and
               Enabled (This);

   function Data_Register_Address (This : USART) return System.Address
     with Inline;
   --  Returns the address of the USART Data Register. This is exported
   --  STRICTLY for the sake of clients driving a USART via DMA. All other
   --  clients of this package should use the procedural interfaces Transmit
   --  and Receive instead of directly accessing the Data Register!
   --  Seriously, don't use this function otherwise.

private

   function APB_Clock (This : USART) return Word with Inline;
   --  Returns either APB1 or APB2 clock rate, in Hertz, depending on the
   --  USART. For the sake of not making this package board-specific, we assume
   --  that we are given a valid USART object at a valid address, AND that the
   --  USART devices really are configured such that only 1 and 6 are on APB2.
   --  Therefore, if a board has additional USARTs beyond USART6, eg USART8 on
   --  the F429I Discovery board, they better conform to that assumption.
   --  See Note # 2 in each of Tables 139-141 of the RM on pages 970 - 972.

   type USART is new STM32_SVD.USART.USART2_Peripheral;

end STM32.USARTs;
