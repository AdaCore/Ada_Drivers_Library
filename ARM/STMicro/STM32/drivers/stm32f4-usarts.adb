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

with STM32F4.RCC;

package body STM32F4.USARTs is

   ---------------
   -- APB_Clock --
   ---------------

   function APB_Clock (This : USART) return Word is
      use STM32F4.RCC;
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
      This.CR1 := This.CR1 or CR1_UE;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out USART) is
   begin
      This.CR1 := This.CR1 and (not CR1_UE);
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : USART) return Boolean is
     ((This.CR1 and CR1_UE) = CR1_UE);

   -------------------
   -- Set_Stop_Bits --
   -------------------

   procedure Set_Stop_Bits (This : in out USART; To : Stop_Bits) is
      Temp : Half_Word;
      USART_CR2_STOPBITS_MASK : constant Half_Word := 16#3000#;
   begin
      Temp := This.CR2;
      Temp := (Temp and (not USART_CR2_STOPBITS_MASK)) or To'Enum_Rep;
      This.CR2 := Temp;
   end Set_Stop_Bits;

   ---------------------
   -- Set_Word_Length --
   ---------------------

   procedure Set_Word_Length
     (This : in out USART;
      To : Word_Lengths)
   is
   begin
      if To = Word_Length_8 then
         This.CR1 := This.CR1 and (not CR1_M);
      else
         This.CR1 := This.CR1 or CR1_M;
      end if;
   end Set_Word_Length;

   ----------------
   -- Set_Parity --
   ----------------

   procedure Set_Parity (This : in out USART; To : Parities) is
      Temp : Half_Word;
      USART_PARITY_MASK : constant Half_Word := CR1_PS or CR1_PCE;
   begin
      Temp := This.CR1;
      Temp := (Temp and (not USART_PARITY_MASK)) or To'Enum_Rep;
      This.CR1 := Temp;
   end Set_Parity;

   -------------------
   -- Set_Baud_Rate --
   -------------------

   procedure Set_Baud_Rate (This : in out USART; To : Baud_Rates) is
      Clock        : constant Word := APB_Clock (This);
      Over_By_8    : constant Boolean := (This.CR1 and CR1_OVER8) = CR1_OVER8;
      Int_Scale    : constant Word := (if Over_By_8 then 2 else 4);
      Int_Divider  : constant Word := (25 * Clock) / (Int_Scale * To);
      Frac_Divider : constant Word := Int_Divider rem 100;
      BRR          : Half_Word;
   begin
      --  the integer part of the divi
      if Over_By_8 then
         BRR := (Half_Word (Frac_Divider * 8) + 50) / 100 mod 8
              or Half_Word (Int_Divider / 100 * 16);
      else
         BRR := (Half_Word (Frac_Divider * 16) + 50) / 100 mod 16
              or Half_Word (Int_Divider / 100 * 16);
      end if;
      This.BRR := BRR;
   end Set_Baud_Rate;

   ---------------------------
   -- Set_Oversampling_Mode --
   ---------------------------

   procedure Set_Oversampling_Mode (This : in out USART; To : Oversampling_Modes) is
      CR1 : Half_Word := This.CR1;
   begin
      if To = Oversampling_By_8 then
         CR1 := CR1 or CR1_OVER8;
      else
         CR1 := CR1 and not CR1_OVER8;
      end if;
      This.CR1 := CR1;
   end Set_Oversampling_Mode;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (This : in out USART;  To : UART_Modes) is
      Temp : Half_Word;
      USART_MODE_MASK : constant Half_Word := CR1_RE or CR1_TE;
   begin
      Temp := This.CR1;
      Temp := (Temp and (not USART_MODE_MASK)) or To'Enum_Rep;
      This.CR1 := Temp;
   end Set_Mode;

   ----------------------
   -- Set_Flow_Control --
   ----------------------

   procedure Set_Flow_Control (This : in out USART;  To : Flow_Control) is
      Temp : Half_Word;
      USART_FLOWCONTROL_MASK : constant Half_Word := CR3_RTSE or CR3_CTSE;
   begin
      Temp := This.CR3;
      Temp := (Temp and (not USART_FLOWCONTROL_MASK)) or To'Enum_Rep;
      This.CR3 := Temp;
   end Set_Flow_Control;

   ---------
   -- Put --
   ---------

   procedure Transmit (This : in out USART;  Data : Half_Word) is
   begin
      This.DR := Data and USART_DR_MASK;
   end Transmit;

   ---------
   -- Get --
   ---------

   procedure Receive (This : USART;  Data : out Half_Word) is
   begin
      Data := Current_Input (This);
   end Receive;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input (This : USART) return Half_Word is (This.DR);

   --------------
   -- Tx_Ready --
   --------------

   function Tx_Ready (This : USART) return Boolean is
      Result : constant Status_Register := This.SR;
   begin
      return Result.Transmit_Data_Register_Empty_Flag;
   end Tx_Ready;

   --------------
   -- Rx_Ready --
   --------------

   function Rx_Ready (This : USART) return Boolean is
      Result : constant Status_Register := This.SR;
   begin
      return Result.Read_Data_Register_Not_Empty_Flag;
   end Rx_Ready;

   ------------
   -- Status --
   ------------

   function Status (This : USART; Flag : USART_Status_Flag) return Boolean is
      Result : constant Status_Register := This.SR;
      --  TODO: if this proves too expensive, change the type back to an
      --  unsigned integer and use an enum rep clause in combination with
      --  'Enum_Rep to do bit masking
   begin
      case Flag is
         when Parity_Error_Indicated =>
            return Result.Parity_Error_Flag;
         when Framing_Error_Indicated =>
            return Result.Framing_Error_Flag;
         when USART_Noise_Error_Indicated =>
            return Result.Noise_Error_Flag;
         when Overrun_Error_Indicated =>
            return Result.OverRun_Error_Flag;
         when Idle_Line_Detection_Indicated =>
            return Result.IDLE_Line_Detected_Flag;
         when Read_Data_Register_Not_Empty =>
            return Result.Read_Data_Register_Not_Empty_Flag;
         when Transmission_Complete_Indicated =>
            return Result.Transmission_Complete_Flag;
         when Transmit_Data_Register_Empty =>
            return Result.Transmit_Data_Register_Empty_Flag;
         when Line_Break_Detection_Indicated =>
            return Result.IDLE_Line_Detected_Flag;
         when Clear_To_Send_Indicated =>
            return Result.Clear_To_Send_Flag;
      end case;
   end Status;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status (This : in out USART;  Flag : USART_Status_Flag) is
      --  TODO: Have a look at the code generated for this. If this proves too
      --  expensive, change the type back to an unsigned integer and use an
      --  enum rep clause in combination with 'Enum_Rep to do bit masking.

      --  A temporary used so that we access the SR as a whole, since we cannot
      --  directly set/clear a bit.
      Flag_Cleared : Status_Register;
   begin
      --  We do not, and must not, use the read-modify-write pattern because
      --  it leaves a window of vulnerability open to changes to the state
      --  after the read but before the write. The hardware for this register
      --  is designed so that writing other bits will not change them. This is
      --  indicated by the "rc_w0" notation in the status register definition.
      --  See the RM, page 57 for that notation explanation. However, we still
      --  must access the register as a whole, hence the use of Flag_Cleared.

      Flag_Cleared := (Reserved0 => 0, Reserved1 => 0, others => True);
      --  By setting the others to True (ie 1), the bits in the SR will not be
      --  changed, by hardware design, so any changes to the device's status
      --  will not be lost. Remember that the SR reflects the state of the
      --  device and so can change outside the flow of control of the CPU.

      case Flag is
         when Parity_Error_Indicated =>
            Flag_Cleared.Parity_Error_Flag                 := False;
         when Framing_Error_Indicated =>
            Flag_Cleared.Framing_Error_Flag                := False;
         when USART_Noise_Error_Indicated =>
            Flag_Cleared.Noise_Error_Flag                  := False;
         when Overrun_Error_Indicated =>
            Flag_Cleared.OverRun_Error_Flag                := False;
         when Idle_Line_Detection_Indicated =>
            Flag_Cleared.IDLE_Line_Detected_Flag           := False;
         when Read_Data_Register_Not_Empty =>
            Flag_Cleared.Read_Data_Register_Not_Empty_Flag := False;
         when Transmission_Complete_Indicated =>
            Flag_Cleared.Transmission_Complete_Flag        := False;
         when Transmit_Data_Register_Empty =>
            Flag_Cleared.Transmit_Data_Register_Empty_Flag := False;
         when Line_Break_Detection_Indicated =>
            Flag_Cleared.IDLE_Line_Detected_Flag           := False;
         when Clear_To_Send_Indicated =>
            Flag_Cleared.Clear_To_Send_Flag                := False;
      end case;

      This.SR := Flag_Cleared;
   end Clear_Status;


   Interrupt_Enablers : constant array (USART_Interrupt) of Half_Word :=
     (Parity_Error                 => CR1_PEIE,
      Transmit_Data_Register_Empty => CR1_TXEIE,
      Transmission_Complete        => CR1_TCIE,
      Received_Data_Not_Empty      => CR1_RXNEIE,
      Idle_Line_Detection          => CR1_IDLEIE,
      Line_Break_Detection         => CR2_LBDIE,
      Clear_To_Send                => CR3_CTSIE,
      Error                        => CR3_EIE);

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (This   : in out USART;
      Source : USART_Interrupt)
   is
      Mask : Half_Word renames Interrupt_Enablers (Source);
   begin
      case Source is
         when Parity_Error .. Idle_Line_Detection =>
            This.CR1 := This.CR1 or Mask;
         when Line_Break_Detection =>
            This.CR2 := This.CR2 or Mask;
         when Clear_To_Send | Error =>
            This.CR3 := This.CR3 or Mask;
      end case;
   end Enable_Interrupts;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out USART;
      Source : USART_Interrupt)
   is
      Mask : Half_Word renames Interrupt_Enablers (Source);
   begin
      case Source is
         when Parity_Error .. Idle_Line_Detection =>
            This.CR1 := This.CR1 and (not Mask);
         when Line_Break_Detection =>
            This.CR2 := This.CR2 and (not Mask);
         when Clear_To_Send | Error =>
            This.CR3 := This.CR3 and (not Mask);
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
      Mask : Half_Word renames Interrupt_Enablers (Source);
   begin
      case Source is
         when Parity_Error .. Idle_Line_Detection =>
            return (This.CR1 and Mask) = Mask;
         when Line_Break_Detection =>
            return (This.CR2 and Mask) = Mask;
         when Clear_To_Send | Error =>
            return (This.CR3 and Mask) = Mask;
      end case;
   end Interrupt_Enabled;

   ----------------------------------
   -- Enable_DMA_Transmit_Requests --
   ----------------------------------

   procedure Enable_DMA_Transmit_Requests (This : in out USART) is
   begin
      This.CR3 := This.CR3 or CR3_DMAT;
   end Enable_DMA_Transmit_Requests;

   ---------------------------------
   -- Enable_DMA_Receive_Requests --
   ---------------------------------

   procedure Enable_DMA_Receive_Requests (This : in out USART) is
   begin
      This.CR3 := This.CR3 or CR3_DMAR;
   end Enable_DMA_Receive_Requests;

   -----------------------------------
   -- Disable_DMA_Transmit_Requests --
   -----------------------------------

   procedure Disable_DMA_Transmit_Requests (This : in out USART) is
   begin
      This.CR3 := This.CR3 and (not CR3_DMAT);
   end Disable_DMA_Transmit_Requests;

   ----------------------------------
   -- Disable_DMA_Receive_Requests --
   ----------------------------------

   procedure Disable_DMA_Receive_Requests (This : in out USART) is
   begin
      This.CR3 := This.CR3 and (not CR3_DMAR);
   end Disable_DMA_Receive_Requests;

   -----------------------------------
   -- DMA_Transmit_Requests_Enabled --
   -----------------------------------

   function DMA_Transmit_Requests_Enabled  (This : USART) return Boolean is
     ((This.CR3 and CR3_DMAT) = CR3_DMAT);

   ----------------------------------
   -- DMA_Receive_Requests_Enabled --
   ----------------------------------

   function DMA_Receive_Requests_Enabled  (This : USART) return Boolean is
     ((This.CR3 and CR3_DMAR) = CR3_DMAR);

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

end STM32F4.USARTs;
