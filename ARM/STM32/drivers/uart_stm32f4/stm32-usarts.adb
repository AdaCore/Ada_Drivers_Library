------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2017, AdaCore                        --
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

   function APB_Clock (This : USART) return UInt32 is
      Clocks : constant RCC_System_Clocks := System_Clock_Frequencies;
   begin
      if This.Periph.all'Address = USART1_Base
        or
         This.Periph.all'Address = USART6_Base
      then
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
      This.Periph.CR1.UE := True;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out USART) is
   begin
      This.Periph.CR1.UE := False;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : USART) return Boolean is
     (This.Periph.CR1.UE);

   -------------------
   -- Set_Stop_Bits --
   -------------------

   procedure Set_Stop_Bits (This : in out USART; To : Stop_Bits)
   is
   begin
      This.Periph.CR2.STOP := Stop_Bits'Enum_Rep (To);
   end Set_Stop_Bits;

   ---------------------
   -- Set_Word_Length --
   ---------------------

   procedure Set_Word_Length
     (This : in out USART;
      To : Word_Lengths)
   is
   begin
      This.Periph.CR1.M := To = Word_Length_9;
   end Set_Word_Length;

   ----------------
   -- Set_Parity --
   ----------------

   procedure Set_Parity (This : in out USART; To : Parities) is
   begin
      case To is
         when No_Parity =>
            This.Periph.CR1.PCE := False;
            This.Periph.CR1.PS  := False;
         when Even_Parity =>
            This.Periph.CR1.PCE := True;
            This.Periph.CR1.PS  := False;
         when Odd_Parity =>
            This.Periph.CR1.PCE := True;
            This.Periph.CR1.PS  := True;
      end case;
   end Set_Parity;

   -------------------
   -- Set_Baud_Rate --
   -------------------

   procedure Set_Baud_Rate (This : in out USART; To : Baud_Rates)
   is
      Clock        : constant UInt32 := APB_Clock (This);
      Over_By_8    : constant Boolean := This.Periph.CR1.OVER8;
      Int_Scale    : constant UInt32 := (if Over_By_8 then 2 else 4);
      Int_Divider  : constant UInt32 := (25 * Clock) / (Int_Scale * To);
      Frac_Divider : constant UInt32 := Int_Divider rem 100;
   begin
      --  the integer part of the divi
      if Over_By_8 then
         This.Periph.BRR.DIV_Fraction :=
           BRR_DIV_Fraction_Field (((Frac_Divider * 8) + 50) / 100 mod 8);
      else
         This.Periph.BRR.DIV_Fraction :=
           BRR_DIV_Fraction_Field (((Frac_Divider * 16) + 50) / 100 mod 16);
      end if;

      This.Periph.BRR.DIV_Mantissa :=
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
      This.Periph.CR1.OVER8 := To = Oversampling_By_8;
   end Set_Oversampling_Mode;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (This : in out USART;  To : UART_Modes) is
   begin
      This.Periph.CR1.RE := To /= Tx_Mode;
      This.Periph.CR1.TE := To /= Rx_Mode;
   end Set_Mode;

   ----------------------
   -- Set_Flow_Control --
   ----------------------

   procedure Set_Flow_Control (This : in out USART;  To : Flow_Control) is
   begin
      case To is
         when No_Flow_Control =>
            This.Periph.CR3.RTSE := False;
            This.Periph.CR3.CTSE := False;
         when RTS_Flow_Control =>
            This.Periph.CR3.RTSE := True;
            This.Periph.CR3.CTSE := False;
         when CTS_Flow_Control =>
            This.Periph.CR3.RTSE := False;
            This.Periph.CR3.CTSE := True;
         when RTS_CTS_Flow_Control =>
            This.Periph.CR3.RTSE := True;
            This.Periph.CR3.CTSE := True;
      end case;
   end Set_Flow_Control;

   ---------
   -- Put --
   ---------

   procedure Transmit (This : in out USART;  Data : UInt9) is
   begin
      This.Periph.DR.DR := Data;
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

   function Current_Input (This : USART) return UInt9 is (This.Periph.DR.DR);

   --------------
   -- Tx_Ready --
   --------------

   function Tx_Ready (This : USART) return Boolean is
   begin
      return This.Periph.SR.TXE;
   end Tx_Ready;

   --------------
   -- Rx_Ready --
   --------------

   function Rx_Ready (This : USART) return Boolean is
   begin
      return This.Periph.SR.RXNE;
   end Rx_Ready;

   ------------
   -- Status --
   ------------

   function Status (This : USART; Flag : USART_Status_Flag) return Boolean is
   begin
      case Flag is
         when Parity_Error_Indicated =>
            return This.Periph.SR.PE;
         when Framing_Error_Indicated =>
            return This.Periph.SR.FE;
         when USART_Noise_Error_Indicated =>
            return This.Periph.SR.NF;
         when Overrun_Error_Indicated =>
            return This.Periph.SR.ORE;
         when Idle_Line_Detection_Indicated =>
            return This.Periph.SR.IDLE;
         when Read_Data_Register_Not_Empty =>
            return This.Periph.SR.RXNE;
         when Transmission_Complete_Indicated =>
            return This.Periph.SR.TC;
         when Transmit_Data_Register_Empty =>
            return This.Periph.SR.TXE;
         when Line_Break_Detection_Indicated =>
            return This.Periph.SR.LBD;
         when Clear_To_Send_Indicated =>
            return This.Periph.SR.CTS;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status (This : in out USART;  Flag : USART_Status_Flag) is
   begin
      case Flag is
         when Parity_Error_Indicated =>
            This.Periph.SR.PE := False;
         when Framing_Error_Indicated =>
            This.Periph.SR.FE := False;
         when USART_Noise_Error_Indicated =>
            This.Periph.SR.NF := False;
         when Overrun_Error_Indicated =>
            This.Periph.SR.ORE := False;
         when Idle_Line_Detection_Indicated =>
            This.Periph.SR.IDLE := False;
         when Read_Data_Register_Not_Empty =>
            This.Periph.SR.RXNE := False;
         when Transmission_Complete_Indicated =>
            This.Periph.SR.TC := False;
         when Transmit_Data_Register_Empty =>
            This.Periph.SR.TXE := False;
         when Line_Break_Detection_Indicated =>
            This.Periph.SR.LBD := False;
         when Clear_To_Send_Indicated =>
            This.Periph.SR.CTS := False;
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
            This.Periph.CR1.PEIE := True;
         when Transmit_Data_Register_Empty =>
            This.Periph.CR1.TXEIE := True;
         when Transmission_Complete =>
            This.Periph.CR1.TCIE := True;
         when Received_Data_Not_Empty =>
            This.Periph.CR1.RXNEIE := True;
         when Idle_Line_Detection =>
            This.Periph.CR1.IDLEIE := True;
         when Line_Break_Detection =>
            This.Periph.CR2.LBDIE := True;
         when Clear_To_Send =>
            This.Periph.CR3.CTSIE := True;
         when Error =>
            This.Periph.CR3.EIE := True;
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
            This.Periph.CR1.PEIE := False;
         when Transmit_Data_Register_Empty =>
            This.Periph.CR1.TXEIE := False;
         when Transmission_Complete =>
            This.Periph.CR1.TCIE := False;
         when Received_Data_Not_Empty =>
            This.Periph.CR1.RXNEIE := False;
         when Idle_Line_Detection =>
            This.Periph.CR1.IDLEIE := False;
         when Line_Break_Detection =>
            This.Periph.CR2.LBDIE := False;
         when Clear_To_Send =>
            This.Periph.CR3.CTSIE := False;
         when Error =>
            This.Periph.CR3.EIE := False;
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
            return This.Periph.CR1.PEIE;
         when Transmit_Data_Register_Empty =>
            return This.Periph.CR1.TXEIE;
         when Transmission_Complete =>
            return This.Periph.CR1.TCIE;
         when Received_Data_Not_Empty =>
            return This.Periph.CR1.RXNEIE;
         when Idle_Line_Detection =>
            return This.Periph.CR1.IDLEIE;
         when Line_Break_Detection =>
            return This.Periph.CR2.LBDIE;
         when Clear_To_Send =>
            return This.Periph.CR3.CTSIE;
         when Error =>
            return This.Periph.CR3.EIE;
      end case;
   end Interrupt_Enabled;

   ----------------------------------
   -- Enable_DMA_Transmit_Requests --
   ----------------------------------

   procedure Enable_DMA_Transmit_Requests (This : in out USART) is
   begin
      This.Periph.CR3.DMAT := True;
   end Enable_DMA_Transmit_Requests;

   ---------------------------------
   -- Enable_DMA_Receive_Requests --
   ---------------------------------

   procedure Enable_DMA_Receive_Requests (This : in out USART) is
   begin
      This.Periph.CR3.DMAR := True;
   end Enable_DMA_Receive_Requests;

   -----------------------------------
   -- Disable_DMA_Transmit_Requests --
   -----------------------------------

   procedure Disable_DMA_Transmit_Requests (This : in out USART) is
   begin
      This.Periph.CR3.DMAT := False;
   end Disable_DMA_Transmit_Requests;

   ----------------------------------
   -- Disable_DMA_Receive_Requests --
   ----------------------------------

   procedure Disable_DMA_Receive_Requests (This : in out USART) is
   begin
      This.Periph.CR3.DMAR := False;
   end Disable_DMA_Receive_Requests;

   -----------------------------------
   -- DMA_Transmit_Requests_Enabled --
   -----------------------------------

   function DMA_Transmit_Requests_Enabled  (This : USART) return Boolean is
      (This.Periph.CR3.DMAT);

   ----------------------------------
   -- DMA_Receive_Requests_Enabled --
   ----------------------------------

   function DMA_Receive_Requests_Enabled  (This : USART) return Boolean is
      (This.Periph.CR3.DMAR);

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
         (This.Periph.DR'Address);

   ---------------
   -- Data_Size --
   ---------------

   overriding
   function Data_Size (This : USART) return HAL.UART.UART_Data_Size
   is
   begin
      if This.Periph.CR1.M then
         return Data_Size_9b;
      else
         return Data_Size_8b;
      end if;
   end Data_Size;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out USART;
      Data    : UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Status, Timeout);
   begin
      for Elt of Data loop
         loop
            exit when This.Tx_Ready;
         end loop;

         This.Transmit (UInt9 (Elt));
      end loop;
      Status := Ok;
   end Transmit;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out USART;
      Data    : UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Status, Timeout);
   begin
      for Elt of Data loop
         loop
            exit when This.Tx_Ready;
         end loop;

         This.Transmit (Elt);
      end loop;
      Status := Ok;
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out USART;
      Data    : out UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Status, Timeout);
   begin
      for Elt of Data loop
         loop
            exit when This.Rx_Ready;
         end loop;

         This.Receive (UInt9 (Elt));
      end loop;
      Status := Ok;
   end Receive;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out USART;
      Data    : out UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Status, Timeout);
   begin
      for Elt of Data loop
         loop
            exit when This.Rx_Ready;
         end loop;

         This.Receive (Elt);
      end loop;
      Status := Ok;
   end Receive;

end STM32.USARTs;
