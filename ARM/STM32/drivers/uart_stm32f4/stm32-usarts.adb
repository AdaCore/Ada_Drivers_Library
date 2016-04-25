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
--   @file    stm32f4xx_hal_usart.c                                         --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   USARTS HAL module driver.                                     --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with System;          use System;
with STM32_SVD.USART; use STM32_SVD, STM32_SVD.USART;

with STM32.Device;    use STM32.Device;

package body STM32.USARTs is

   ---------------
   -- APB_Clock --
   ---------------

   function APB_Clock (This : USART) return Word is
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
      This.CR1.UE := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out USART) is
   begin
      This.CR1.UE := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : USART) return Boolean is
     (This.CR1.UE);

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
      This.CR1.M := To = Word_Length_9;
   end Set_Word_Length;

   ----------------
   -- Set_Parity --
   ----------------

   procedure Set_Parity (This : in out USART; To : Parities) is
   begin
      case To is
         when No_Parity =>
            This.CR1.PCE := False;
            This.CR1.PS  := False;
         when Even_Parity =>
            This.CR1.PCE := True;
            This.CR1.PS  := False;
         when Odd_Parity =>
            This.CR1.PCE := True;
            This.CR1.PS  := True;
      end case;
   end Set_Parity;

   -------------------
   -- Set_Baud_Rate --
   -------------------

   procedure Set_Baud_Rate (This : in out USART; To : Baud_Rates) is
      Clock        : constant Word := APB_Clock (This);
      Over_By_8    : constant Boolean := This.CR1.OVER8;
      Int_Scale    : constant Word := (if Over_By_8 then 2 else 4);
      Int_Divider  : constant Word := (25 * Clock) / (Int_Scale * To);
      Frac_Divider : constant Word := Int_Divider rem 100;
   begin
      --  the integer part of the divi
      if Over_By_8 then
         This.BRR.DIV_Fraction :=
           BRR_DIV_Fraction_Field (((Frac_Divider * 8) + 50) / 100 mod 8);
      else
         This.BRR.DIV_Fraction :=
           BRR_DIV_Fraction_Field (((Frac_Divider * 16) + 50) / 100 mod 16);
      end if;

      This.BRR.DIV_Mantissa :=
        BRR_DIV_Mantissa_Field (Int_Divider / 100);
   end Set_Baud_Rate;

   ---------------------------
   -- Set_Oversampling_Mode --
   ---------------------------

   procedure Set_Oversampling_Mode
     (This : in out USART;
      To   : Oversampling_Modes)
   is
   begin
      This.CR1.OVER8 := To = Oversampling_By_8;
   end Set_Oversampling_Mode;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (This : in out USART;  To : UART_Modes) is
   begin
      This.CR1.RE := To /= Tx_Mode;
      This.CR1.TE := To /= Rx_Mode;
   end Set_Mode;

   ----------------------
   -- Set_Flow_Control --
   ----------------------

   procedure Set_Flow_Control (This : in out USART;  To : Flow_Control) is
   begin
      case To is
         when No_Flow_Control =>
            This.CR3.RTSE := False;
            This.CR3.CTSE := False;
         when RTS_Flow_Control =>
            This.CR3.RTSE := True;
            This.CR3.CTSE := False;
         when CTS_Flow_Control =>
            This.CR3.RTSE := False;
            This.CR3.CTSE := True;
         when RTS_CTS_Flow_Control =>
            This.CR3.RTSE := True;
            This.CR3.CTSE := True;
      end case;
   end Set_Flow_Control;

   ---------
   -- Put --
   ---------

   procedure Transmit (This : in out USART;  Data : UInt9) is
   begin
      This.DR.DR := Data;
   end Transmit;

   ---------
   -- Get --
   ---------

   procedure Receive (This : USART;  Data : out UInt9) is
   begin
      Data := Current_Input (This);
   end Receive;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input (This : USART) return UInt9 is (This.DR.DR);

   --------------
   -- Tx_Ready --
   --------------

   function Tx_Ready (This : USART) return Boolean is
   begin
      return This.SR.TXE;
   end Tx_Ready;

   --------------
   -- Rx_Ready --
   --------------

   function Rx_Ready (This : USART) return Boolean is
   begin
      return This.SR.RXNE;
   end Rx_Ready;

   ------------
   -- Status --
   ------------

   function Status (This : USART; Flag : USART_Status_Flag) return Boolean is
   begin
      case Flag is
         when Parity_Error_Indicated =>
            return This.SR.PE;
         when Framing_Error_Indicated =>
            return This.SR.FE;
         when USART_Noise_Error_Indicated =>
            return This.SR.NF;
         when Overrun_Error_Indicated =>
            return This.SR.ORE;
         when Idle_Line_Detection_Indicated =>
            return This.SR.IDLE;
         when Read_Data_Register_Not_Empty =>
            return This.SR.RXNE;
         when Transmission_Complete_Indicated =>
            return This.SR.TC;
         when Transmit_Data_Register_Empty =>
            return This.SR.TXE;
         when Line_Break_Detection_Indicated =>
            return This.SR.LBD;
         when Clear_To_Send_Indicated =>
            return This.SR.CTS;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status (This : in out USART;  Flag : USART_Status_Flag) is
   begin
      case Flag is
         when Parity_Error_Indicated =>
            This.SR.PE := False;
         when Framing_Error_Indicated =>
            This.SR.FE := False;
         when USART_Noise_Error_Indicated =>
            This.SR.NF := False;
         when Overrun_Error_Indicated =>
            This.SR.ORE := False;
         when Idle_Line_Detection_Indicated =>
            This.SR.IDLE := False;
         when Read_Data_Register_Not_Empty =>
            This.SR.RXNE := False;
         when Transmission_Complete_Indicated =>
            This.SR.TC := False;
         when Transmit_Data_Register_Empty =>
            This.SR.TXE := False;
         when Line_Break_Detection_Indicated =>
            This.SR.LBD := False;
         when Clear_To_Send_Indicated =>
            This.SR.CTS := False;
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
            This.CR1.PEIE := True;
         when Transmit_Data_Register_Empty =>
            This.CR1.TXEIE := True;
         when Transmission_Complete =>
            This.CR1.TCIE := True;
         when Received_Data_Not_Empty =>
            This.CR1.RXNEIE := True;
         when Idle_Line_Detection =>
            This.CR1.IDLEIE := True;
         when Line_Break_Detection =>
            This.CR2.LBDIE := True;
         when Clear_To_Send =>
            This.CR3.CTSIE := True;
         when Error =>
            This.CR3.EIE := True;
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
            This.CR1.PEIE := False;
         when Transmit_Data_Register_Empty =>
            This.CR1.TXEIE := False;
         when Transmission_Complete =>
            This.CR1.TCIE := False;
         when Received_Data_Not_Empty =>
            This.CR1.RXNEIE := False;
         when Idle_Line_Detection =>
            This.CR1.IDLEIE := False;
         when Line_Break_Detection =>
            This.CR2.LBDIE := False;
         when Clear_To_Send =>
            This.CR3.CTSIE := False;
         when Error =>
            This.CR3.EIE := False;
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
            return This.CR1.PEIE;
         when Transmit_Data_Register_Empty =>
            return This.CR1.TXEIE;
         when Transmission_Complete =>
            return This.CR1.TCIE;
         when Received_Data_Not_Empty =>
            return This.CR1.RXNEIE;
         when Idle_Line_Detection =>
            return This.CR1.IDLEIE;
         when Line_Break_Detection =>
            return This.CR2.LBDIE;
         when Clear_To_Send =>
            return This.CR3.CTSIE;
         when Error =>
            return This.CR3.EIE;
      end case;
   end Interrupt_Enabled;

   ----------------------------------
   -- Enable_DMA_Transmit_Requests --
   ----------------------------------

   procedure Enable_DMA_Transmit_Requests (This : in out USART) is
   begin
      This.CR3.DMAT := True;
   end Enable_DMA_Transmit_Requests;

   ---------------------------------
   -- Enable_DMA_Receive_Requests --
   ---------------------------------

   procedure Enable_DMA_Receive_Requests (This : in out USART) is
   begin
      This.CR3.DMAR := True;
   end Enable_DMA_Receive_Requests;

   -----------------------------------
   -- Disable_DMA_Transmit_Requests --
   -----------------------------------

   procedure Disable_DMA_Transmit_Requests (This : in out USART) is
   begin
      This.CR3.DMAT := False;
   end Disable_DMA_Transmit_Requests;

   ----------------------------------
   -- Disable_DMA_Receive_Requests --
   ----------------------------------

   procedure Disable_DMA_Receive_Requests (This : in out USART) is
   begin
      This.CR3.DMAR := True;
   end Disable_DMA_Receive_Requests;

   -----------------------------------
   -- DMA_Transmit_Requests_Enabled --
   -----------------------------------

   function DMA_Transmit_Requests_Enabled  (This : USART) return Boolean is
      (This.CR3.DMAT);

   ----------------------------------
   -- DMA_Receive_Requests_Enabled --
   ----------------------------------

   function DMA_Receive_Requests_Enabled  (This : USART) return Boolean is
      (This.CR3.DMAR);

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
