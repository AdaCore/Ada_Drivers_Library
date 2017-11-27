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
--   @file    stm32f4xx_hal_spi.h                                           --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of SPI HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides definitions for the STM32F4 (ARM Cortex M4F
--  from ST Microelectronics) Serial Peripheral Interface (SPI) facility.

private with STM32_SVD.SPI;
with HAL.SPI;
with System;

package STM32.SPI is

   type Internal_SPI_Port is private;

   type SPI_Port (Periph : not null access Internal_SPI_Port) is
      limited new HAL.SPI.SPI_Port with private;

   type SPI_Data_Direction is
     (D2Lines_FullDuplex,
      D2Lines_RxOnly,
      D1Line_Rx,
      D1Line_Tx);

   type SPI_Mode is (Master, Slave);

   type SPI_Clock_Polarity is (High, Low);

   type SPI_Clock_Phase is (P1Edge, P2Edge);

   type SPI_Slave_Management is (Software_Managed, Hardware_Managed);

   type SPI_Baud_Rate_Prescaler is
     (BRP_2, BRP_4, BRP_8, BRP_16, BRP_32, BRP_64, BRP_128, BRP_256);

   type SPI_First_Bit is (MSB, LSB);

   type SPI_Configuration is record
      Direction           : SPI_Data_Direction;
      Mode                : SPI_Mode;
      Data_Size           : HAL.SPI.SPI_Data_Size;
      Clock_Polarity      : SPI_Clock_Polarity;
      Clock_Phase         : SPI_Clock_Phase;
      Slave_Management    : SPI_Slave_Management;
      Baud_Rate_Prescaler : SPI_Baud_Rate_Prescaler;
      First_Bit           : SPI_First_Bit;
      CRC_Poly            : UInt16;
   end record;

   procedure Configure (This : in out SPI_Port; Conf : SPI_Configuration);

   procedure Enable (This : in out SPI_Port);

   procedure Disable (This : in out SPI_Port);

   function Enabled (This : SPI_Port) return Boolean;

   procedure Send (This : in out SPI_Port; Data : UInt16);

   function Data (This : SPI_Port) return UInt16
     with Inline;

   procedure Send (This : in out SPI_Port; Data : UInt8);

   function Data (This : SPI_Port) return UInt8
     with Inline;

   function Is_Busy (This : SPI_Port) return Boolean
     with Inline;

   function Rx_Is_Empty (This : SPI_Port) return Boolean
     with Inline;

   function Tx_Is_Empty (This : SPI_Port) return Boolean
     with Inline;

   function Busy (This : SPI_Port) return Boolean
     with Inline;

   function Channel_Side_Indicated (This : SPI_Port) return Boolean
     with Inline;

   function Underrun_Indicated (This : SPI_Port) return Boolean
     with Inline;

   function CRC_Error_Indicated (This : SPI_Port) return Boolean
     with Inline;

   function Mode_Fault_Indicated (This : SPI_Port) return Boolean
     with Inline;

   function Overrun_Indicated (This : SPI_Port) return Boolean
     with Inline;

   function Frame_Fmt_Error_Indicated (This : SPI_Port) return Boolean
     with Inline;

   procedure Clear_Overrun (This : SPI_Port);

   procedure Reset_CRC (This : in out SPI_Port);

   function CRC_Enabled (This : SPI_Port) return Boolean;

   function Is_Data_Frame_16bit (This : SPI_Port) return Boolean;

   function Current_Mode (This : SPI_Port) return SPI_Mode;

   function Current_Data_Direction (This : SPI_Port) return SPI_Data_Direction;

   --  The following I/O routines implement the higher level functionality for
   --  CRC and data direction, among others.

   type UInt8_Buffer is array (Natural range <>) of UInt8
     with Alignment => 2;
   --  The alignment is set to 2 because we treat component pairs as half_word
   --  values when sending/receiving in 16-bit mode.

   --  Blocking

   overriding
   function Data_Size (This : SPI_Port) return HAL.SPI.SPI_Data_Size;

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_8b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_16b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

   procedure Transmit
     (This     : in out SPI_Port;
      Outgoing : UInt8);

   overriding
   procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

   procedure Receive
     (This     : in out SPI_Port;
      Incoming : out UInt8);

   procedure Transmit_Receive
     (This      : in out SPI_Port;
      Outgoing  : UInt8_Buffer;
      Incoming  : out UInt8_Buffer;
      Size      : Positive);

   procedure Transmit_Receive
     (This      : in out SPI_Port;
      Outgoing  : UInt8;
      Incoming  : out UInt8);

   --  TODO: add the other higher-level HAL routines for interrupts and DMA

   function Data_Register_Address
     (This : SPI_Port)
      return System.Address;
   --  For DMA transfer

private

   type Internal_SPI_Port is new STM32_SVD.SPI.SPI_Peripheral;

   type SPI_Port (Periph : not null access Internal_SPI_Port) is
     limited new HAL.SPI.SPI_Port with null record;

   procedure Send_Receive_16bit_Mode
     (This     : in out SPI_Port;
      Outgoing : UInt8_Buffer;
      Incoming : out UInt8_Buffer;
      Size     : Positive);

   procedure Send_Receive_8bit_Mode
     (This     : in out SPI_Port;
      Outgoing : UInt8_Buffer;
      Incoming : out UInt8_Buffer;
      Size     : Positive);

   procedure Send_16bit_Mode
     (This     : in out SPI_Port;
      Outgoing : HAL.SPI.SPI_Data_16b);

   procedure Send_8bit_Mode
     (This     : in out SPI_Port;
      Outgoing : HAL.SPI.SPI_Data_8b);

   procedure Receive_16bit_Mode
     (This     : in out SPI_Port;
      Incoming : out HAL.SPI.SPI_Data_16b);

   procedure Receive_8bit_Mode
     (This     : in out SPI_Port;
      Incoming : out HAL.SPI.SPI_Data_8b);

end STM32.SPI;
