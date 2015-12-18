------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--   @file    stm32f4xx_hal_usart.c                                         --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   USARTS HAL module driver.                                     --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32.RCC;

with STM32_SVD.USART; use STM32_SVD.USART;

package body STM32.USARTs is

   ---------------
   -- APB_Clock --
   ---------------

   function APB_Clock (This : USART) return Word is
      use STM32.RCC;
      Clocks : constant RCC_System_Clocks := System_Clock_Frequencies;
   begin
      if This'Address = USART1_Base or This'Address = USART6_Base then
         return Clocks.PCLK2;
      else
         return Clocks.PCLK1;
      end if;
   end APB_Clock;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out USART) is
   begin
      This.CR1.UE := 1;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out USART) is
   begin
      This.CR1.UE := 0;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : USART) return Boolean is
     (This.CR1.UE = 1);

   -------------------
   -- Set_Stop_Bits --
   -------------------

   procedure Set_Stop_Bits (This : in out USART; To : Stop_Bits)
   is
   begin
      This.CR2.STOP := Stop_Bits'Enum_Rep (To);
   end Set_Stop_Bits;

   ---------------------
   -- Set_Word_Length --
   ---------------------

   procedure Set_Word_Length
     (This : in out USART;
      To : Word_Lengths)
   is
   begin
      This.CR1.M := (if To = Word_Length_8 then 0 else 1);
   end Set_Word_Length;

   ----------------
   -- Set_Parity --
   ----------------

   procedure Set_Parity (This : in out USART; To : Parities) is
   begin
      case To is
         when No_Parity =>
            This.CR1.PCE := 0;
            This.CR1.PS  := 0;
         when Even_Parity =>
            This.CR1.PCE := 1;
            This.CR1.PS  := 0;
         when Odd_Parity =>
            This.CR1.PCE := 1;
            This.CR1.PS  := 1;
      end case;
   end Set_Parity;

   -------------------
   -- Set_Baud_Rate --
   -------------------

   procedure Set_Baud_Rate (This : in out USART; To : Baud_Rates) is
      Clock        : constant Word := APB_Clock (This);
      Int_Divider  : constant Word := (25 * Clock) / (4 * To);
      Frac_Divider : constant Word := Int_Divider rem 100;
   begin
      This.BRR.DIV_Fraction :=
        BRR_DIV_Fraction_Field (((Frac_Divider * 16) + 50) / 100 mod 16);
      This.BRR.DIV_Mantissa :=
        BRR_DIV_Mantissa_Field (Int_Divider / 100);
   end Set_Baud_Rate;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (This : in out USART;  To : UART_Modes) is
   begin
      This.CR1.RE := (if To = Tx_Mode then 0 else 1);
      This.CR1.TE := (if To = Rx_Mode then 0 else 1);
   end Set_Mode;

   ----------------------
   -- Set_Flow_Control --
   ----------------------

   procedure Set_Flow_Control (This : in out USART;  To : Flow_Control) is
   begin
      case To is
         when No_Flow_Control =>
            This.CR3.RTSE := 0;
            This.CR3.CTSE := 0;
         when RTS_Flow_Control =>
            This.CR3.RTSE := 1;
            This.CR3.CTSE := 0;
         when CTS_Flow_Control =>
            This.CR3.RTSE := 0;
            This.CR3.CTSE := 1;
         when RTS_CTS_Flow_Control =>
            This.CR3.RTSE := 1;
            This.CR3.CTSE := 1;
      end case;
   end Set_Flow_Control;

   ---------
   -- Put --
   ---------

   procedure Transmit (This : in out USART;  Data : Bits_9) is
   begin
      This.DR.DR := Data;
   end Transmit;

   ---------
   -- Get --
   ---------

   procedure Receive (This : USART;  Data : out Bits_9) is
   begin
      Data := Current_Input (This);
   end Receive;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input (This : USART) return Bits_9 is (This.DR.DR);

   --------------
   -- Tx_Ready --
   --------------

   function Tx_Ready (This : USART) return Boolean is
   begin
      return This.SR.TXE = 1;
   end Tx_Ready;

   --------------
   -- Rx_Ready --
   --------------

   function Rx_Ready (This : USART) return Boolean is
   begin
      return This.SR.RXNE = 1;
   end Rx_Ready;

   ------------
   -- Status --
   ------------

   function Status (This : USART; Flag : USART_Status_Flag) return Boolean is
   begin
      case Flag is
         when Parity_Error_Indicated =>
            return This.SR.PE = 1;
         when Framing_Error_Indicated =>
            return This.SR.FE = 1;
         when USART_Noise_Error_Indicated =>
            return This.SR.NF = 1;
         when Overrun_Error_Indicated =>
            return This.SR.ORE = 1;
         when Idle_Line_Detection_Indicated =>
            return This.SR.IDLE = 1;
         when Read_Data_Register_Not_Empty =>
            return This.SR.RXNE = 1;
         when Transmission_Complete_Indicated =>
            return This.SR.TC = 1;
         when Transmit_Data_Register_Empty =>
            return This.SR.TXE = 1;
         when Line_Break_Detection_Indicated =>
            return This.SR.LBD = 1;
         when Clear_To_Send_Indicated =>
            return This.SR.CTS = 1;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status (This : in out USART;  Flag : USART_Status_Flag) is
   begin
      case Flag is
         when Parity_Error_Indicated =>
            This.SR.PE := 0;
         when Framing_Error_Indicated =>
            This.SR.FE := 0;
         when USART_Noise_Error_Indicated =>
            This.SR.NF := 0;
         when Overrun_Error_Indicated =>
            This.SR.ORE := 0;
         when Idle_Line_Detection_Indicated =>
            This.SR.IDLE := 0;
         when Read_Data_Register_Not_Empty =>
            This.SR.RXNE := 0;
         when Transmission_Complete_Indicated =>
            This.SR.TC := 0;
         when Transmit_Data_Register_Empty =>
            This.SR.TXE := 0;
         when Line_Break_Detection_Indicated =>
            This.SR.LBD := 0;
         when Clear_To_Send_Indicated =>
            This.SR.CTS := 0;
      end case;
   end Clear_Status;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (This   : in out USART;
      Source : USART_Interrupt)
   is
   begin
      case Source is
         when Parity_Error =>
            This.CR1.PEIE := 1;
         when Transmit_Data_Register_Empty =>
            This.CR1.TXEIE := 1;
         when Transmission_Complete =>
            This.CR1.TCIE := 1;
         when Received_Data_Not_Empty =>
            This.CR1.RXNEIE := 1;
         when Idle_Line_Detection =>
            This.CR1.IDLEIE := 1;
         when Line_Break_Detection =>
            This.CR2.LBDIE := 1;
         when Clear_To_Send =>
            This.CR3.CTSIE := 1;
         when Error =>
            This.CR3.EIE := 1;
      end case;
   end Enable_Interrupts;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out USART;
      Source : USART_Interrupt)
   is
   begin
      case Source is
         when Parity_Error =>
            This.CR1.PEIE := 0;
         when Transmit_Data_Register_Empty =>
            This.CR1.TXEIE := 0;
         when Transmission_Complete =>
            This.CR1.TCIE := 0;
         when Received_Data_Not_Empty =>
            This.CR1.RXNEIE := 0;
         when Idle_Line_Detection =>
            This.CR1.IDLEIE := 0;
         when Line_Break_Detection =>
            This.CR2.LBDIE := 0;
         when Clear_To_Send =>
            This.CR3.CTSIE := 0;
         when Error =>
            This.CR3.EIE := 0;
      end case;
   end Disable_Interrupts;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : USART;
      Source : USART_Interrupt)
      return Boolean
   is
   begin
      case Source is
         when Parity_Error =>
            return This.CR1.PEIE = 1;
         when Transmit_Data_Register_Empty =>
            return This.CR1.TXEIE = 1;
         when Transmission_Complete =>
            return This.CR1.TCIE = 1;
         when Received_Data_Not_Empty =>
            return This.CR1.RXNEIE = 1;
         when Idle_Line_Detection =>
            return This.CR1.IDLEIE = 1;
         when Line_Break_Detection =>
            return This.CR2.LBDIE = 1;
         when Clear_To_Send =>
            return This.CR3.CTSIE = 1;
         when Error =>
            return This.CR3.EIE = 1;
      end case;
   end Interrupt_Enabled;

   ----------------------------------
   -- Enable_DMA_Transmit_Requests --
   ----------------------------------

   procedure Enable_DMA_Transmit_Requests (This : in out USART) is
   begin
      This.CR3.DMAT := 1;
   end Enable_DMA_Transmit_Requests;

   ---------------------------------
   -- Enable_DMA_Receive_Requests --
   ---------------------------------

   procedure Enable_DMA_Receive_Requests (This : in out USART) is
   begin
      This.CR3.DMAR := 1;
   end Enable_DMA_Receive_Requests;

   -----------------------------------
   -- Disable_DMA_Transmit_Requests --
   -----------------------------------

   procedure Disable_DMA_Transmit_Requests (This : in out USART) is
   begin
      This.CR3.DMAT := 0;
   end Disable_DMA_Transmit_Requests;

   ----------------------------------
   -- Disable_DMA_Receive_Requests --
   ----------------------------------

   procedure Disable_DMA_Receive_Requests (This : in out USART) is
   begin
      This.CR3.DMAR := 1;
   end Disable_DMA_Receive_Requests;

   -----------------------------------
   -- DMA_Transmit_Requests_Enabled --
   -----------------------------------

   function DMA_Transmit_Requests_Enabled  (This : USART) return Boolean is
      (This.CR3.DMAT = 1);

   ----------------------------------
   -- DMA_Receive_Requests_Enabled --
   ----------------------------------

   function DMA_Receive_Requests_Enabled  (This : USART) return Boolean is
      (This.CR3.DMAR = 1);

   -----------------------------
   -- Resume_DMA_Transmission --
   -----------------------------

   procedure Resume_DMA_Transmission (This : in out USART) is
   begin
      Enable_DMA_Transmit_Requests (This);
      if not Enabled (This) then
         Enable (This);
      end if;
   end Resume_DMA_Transmission;

   --------------------------
   -- Resume_DMA_Reception --
   --------------------------

   procedure Resume_DMA_Reception (This : in out USART) is
   begin
      Enable_DMA_Receive_Requests (This);
      if not Enabled (This) then
         Enable (This);
      end if;
   end Resume_DMA_Reception;

   ---------------------------
   -- Data_Register_Address --
   ---------------------------

   function Data_Register_Address (This : USART) return System.Address is
         (This.DR'Address);

end STM32.USARTs;
