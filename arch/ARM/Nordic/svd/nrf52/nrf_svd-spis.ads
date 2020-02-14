--  Copyright (c) 2010 - 2018, Nordic Semiconductor ASA
--
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without modification,
--  are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form, except as embedded into a Nordic
--  Semiconductor ASA integrated circuit in a product or a software update for
--  such product, must reproduce the above copyright notice, this list of
--  conditions and the following disclaimer in the documentation and/or other
--  materials provided with the distribution.
--
--  3. Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from this
--  software without specific prior written permission.
--
--  4. This software, with or without modification, must only be used with a
--  Nordic Semiconductor ASA integrated circuit.
--
--  5. Any software provided in binary form under this license must not be reverse
--  engineered, decompiled, modified and/or disassembled.
--
--  THIS SOFTWARE IS PROVIDED BY NORDIC SEMICONDUCTOR ASA "AS IS" AND ANY EXPRESS
--  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL NORDIC SEMICONDUCTOR ASA OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
--  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
--  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf52.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF_SVD.SPIS is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between END event and ACQUIRE task
   type SHORTS_END_ACQUIRE_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_END_ACQUIRE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut register
   type SHORTS_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Shortcut between END event and ACQUIRE task
      END_ACQUIRE   : SHORTS_END_ACQUIRE_Field := NRF_SVD.SPIS.Disabled;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      END_ACQUIRE   at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Write '1' to Enable interrupt for END event
   type INTENSET_END_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_END_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for END event
   type INTENSET_END_Field_1 is
     (--  Reset value for the field
      Intenset_End_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_END_Field_1 use
     (Intenset_End_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for ENDRX event
   type INTENSET_ENDRX_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ENDRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ENDRX event
   type INTENSET_ENDRX_Field_1 is
     (--  Reset value for the field
      Intenset_Endrx_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ENDRX_Field_1 use
     (Intenset_Endrx_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for ACQUIRED event
   type INTENSET_ACQUIRED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ACQUIRED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ACQUIRED event
   type INTENSET_ACQUIRED_Field_1 is
     (--  Reset value for the field
      Intenset_Acquired_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ACQUIRED_Field_1 use
     (Intenset_Acquired_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Write '1' to Enable interrupt for END event
      END_k          : INTENSET_END_Field_1 := Intenset_End_Field_Reset;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Write '1' to Enable interrupt for ENDRX event
      ENDRX          : INTENSET_ENDRX_Field_1 := Intenset_Endrx_Field_Reset;
      --  unspecified
      Reserved_5_9   : HAL.UInt5 := 16#0#;
      --  Write '1' to Enable interrupt for ACQUIRED event
      ACQUIRED       : INTENSET_ACQUIRED_Field_1 :=
                        Intenset_Acquired_Field_Reset;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      END_k          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      ENDRX          at 0 range 4 .. 4;
      Reserved_5_9   at 0 range 5 .. 9;
      ACQUIRED       at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  Write '1' to Disable interrupt for END event
   type INTENCLR_END_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_END_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for END event
   type INTENCLR_END_Field_1 is
     (--  Reset value for the field
      Intenclr_End_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_END_Field_1 use
     (Intenclr_End_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for ENDRX event
   type INTENCLR_ENDRX_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ENDRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ENDRX event
   type INTENCLR_ENDRX_Field_1 is
     (--  Reset value for the field
      Intenclr_Endrx_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ENDRX_Field_1 use
     (Intenclr_Endrx_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for ACQUIRED event
   type INTENCLR_ACQUIRED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ACQUIRED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ACQUIRED event
   type INTENCLR_ACQUIRED_Field_1 is
     (--  Reset value for the field
      Intenclr_Acquired_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ACQUIRED_Field_1 use
     (Intenclr_Acquired_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Write '1' to Disable interrupt for END event
      END_k          : INTENCLR_END_Field_1 := Intenclr_End_Field_Reset;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Write '1' to Disable interrupt for ENDRX event
      ENDRX          : INTENCLR_ENDRX_Field_1 := Intenclr_Endrx_Field_Reset;
      --  unspecified
      Reserved_5_9   : HAL.UInt5 := 16#0#;
      --  Write '1' to Disable interrupt for ACQUIRED event
      ACQUIRED       : INTENCLR_ACQUIRED_Field_1 :=
                        Intenclr_Acquired_Field_Reset;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      END_k          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      ENDRX          at 0 range 4 .. 4;
      Reserved_5_9   at 0 range 5 .. 9;
      ACQUIRED       at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  Semaphore status
   type SEMSTAT_SEMSTAT_Field is
     (--  Semaphore is free
      Free,
      --  Semaphore is assigned to CPU
      Cpu,
      --  Semaphore is assigned to SPI slave
      Spis,
      --  Semaphore is assigned to SPI but a handover to the CPU is pending
      Cpupending)
     with Size => 2;
   for SEMSTAT_SEMSTAT_Field use
     (Free => 0,
      Cpu => 1,
      Spis => 2,
      Cpupending => 3);

   --  Semaphore status register
   type SEMSTAT_Register is record
      --  Read-only. Semaphore status
      SEMSTAT       : SEMSTAT_SEMSTAT_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEMSTAT_Register use record
      SEMSTAT       at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  TX buffer over-read detected, and prevented
   type STATUS_OVERREAD_Field is
     (--  Read: error not present
      Notpresent,
      --  Read: error present
      Present)
     with Size => 1;
   for STATUS_OVERREAD_Field use
     (Notpresent => 0,
      Present => 1);

   --  TX buffer over-read detected, and prevented
   type STATUS_OVERREAD_Field_1 is
     (--  Reset value for the field
      Status_Overread_Field_Reset,
      --  Write: clear error on writing '1'
      Clear)
     with Size => 1;
   for STATUS_OVERREAD_Field_1 use
     (Status_Overread_Field_Reset => 0,
      Clear => 1);

   --  RX buffer overflow detected, and prevented
   type STATUS_OVERFLOW_Field is
     (--  Read: error not present
      Notpresent,
      --  Read: error present
      Present)
     with Size => 1;
   for STATUS_OVERFLOW_Field use
     (Notpresent => 0,
      Present => 1);

   --  RX buffer overflow detected, and prevented
   type STATUS_OVERFLOW_Field_1 is
     (--  Reset value for the field
      Status_Overflow_Field_Reset,
      --  Write: clear error on writing '1'
      Clear)
     with Size => 1;
   for STATUS_OVERFLOW_Field_1 use
     (Status_Overflow_Field_Reset => 0,
      Clear => 1);

   --  Status from last transaction
   type STATUS_Register is record
      --  TX buffer over-read detected, and prevented
      OVERREAD      : STATUS_OVERREAD_Field_1 := Status_Overread_Field_Reset;
      --  RX buffer overflow detected, and prevented
      OVERFLOW      : STATUS_OVERFLOW_Field_1 := Status_Overflow_Field_Reset;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      OVERREAD      at 0 range 0 .. 0;
      OVERFLOW      at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Enable or disable SPI slave
   type ENABLE_ENABLE_Field is
     (--  Disable SPI slave
      Disabled,
      --  Enable SPI slave
      Enabled)
     with Size => 4;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 2);

   --  Enable SPI slave
   type ENABLE_Register is record
      --  Enable or disable SPI slave
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.SPIS.Disabled;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------------------------
   -- SPIS_PSEL cluster's Registers --
   -----------------------------------

   subtype SCK_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type SCK_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for SCK_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for SCK
   type SCK_PSEL_Register is record
      --  Pin number
      PIN           : SCK_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : SCK_CONNECT_Field := NRF_SVD.SPIS.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCK_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype MISO_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type MISO_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for MISO_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for MISO signal
   type MISO_PSEL_Register is record
      --  Pin number
      PIN           : MISO_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : MISO_CONNECT_Field := NRF_SVD.SPIS.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISO_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype MOSI_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type MOSI_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for MOSI_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for MOSI signal
   type MOSI_PSEL_Register is record
      --  Pin number
      PIN           : MOSI_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : MOSI_CONNECT_Field := NRF_SVD.SPIS.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MOSI_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   subtype CSN_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type CSN_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for CSN_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Pin select for CSN signal
   type CSN_PSEL_Register is record
      --  Pin number
      PIN           : CSN_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : CSN_CONNECT_Field := NRF_SVD.SPIS.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSN_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   --  Unspecified
   type SPIS_PSEL_Cluster is record
      --  Pin select for SCK
      SCK  : aliased SCK_PSEL_Register;
      --  Pin select for MISO signal
      MISO : aliased MISO_PSEL_Register;
      --  Pin select for MOSI signal
      MOSI : aliased MOSI_PSEL_Register;
      --  Pin select for CSN signal
      CSN  : aliased CSN_PSEL_Register;
   end record
     with Size => 128;

   for SPIS_PSEL_Cluster use record
      SCK  at 16#0# range 0 .. 31;
      MISO at 16#4# range 0 .. 31;
      MOSI at 16#8# range 0 .. 31;
      CSN  at 16#C# range 0 .. 31;
   end record;

   ----------------------------------
   -- SPIS_RXD cluster's Registers --
   ----------------------------------

   subtype MAXCNT_RXD_MAXCNT_Field is HAL.UInt8;

   --  Maximum number of bytes in receive buffer
   type MAXCNT_RXD_Register is record
      --  Maximum number of bytes in receive buffer
      MAXCNT        : MAXCNT_RXD_MAXCNT_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAXCNT_RXD_Register use record
      MAXCNT        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AMOUNT_RXD_AMOUNT_Field is HAL.UInt8;

   --  Number of bytes received in last granted transaction
   type AMOUNT_RXD_Register is record
      --  Read-only. Number of bytes received in the last granted transaction
      AMOUNT        : AMOUNT_RXD_AMOUNT_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMOUNT_RXD_Register use record
      AMOUNT        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Unspecified
   type SPIS_RXD_Cluster is record
      --  RXD data pointer
      PTR    : aliased HAL.UInt32;
      --  Maximum number of bytes in receive buffer
      MAXCNT : aliased MAXCNT_RXD_Register;
      --  Number of bytes received in last granted transaction
      AMOUNT : aliased AMOUNT_RXD_Register;
   end record
     with Size => 96;

   for SPIS_RXD_Cluster use record
      PTR    at 16#0# range 0 .. 31;
      MAXCNT at 16#4# range 0 .. 31;
      AMOUNT at 16#8# range 0 .. 31;
   end record;

   ----------------------------------
   -- SPIS_TXD cluster's Registers --
   ----------------------------------

   subtype MAXCNT_TXD_MAXCNT_Field is HAL.UInt8;

   --  Maximum number of bytes in transmit buffer
   type MAXCNT_TXD_Register is record
      --  Maximum number of bytes in transmit buffer
      MAXCNT        : MAXCNT_TXD_MAXCNT_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAXCNT_TXD_Register use record
      MAXCNT        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AMOUNT_TXD_AMOUNT_Field is HAL.UInt8;

   --  Number of bytes transmitted in last granted transaction
   type AMOUNT_TXD_Register is record
      --  Read-only. Number of bytes transmitted in last granted transaction
      AMOUNT        : AMOUNT_TXD_AMOUNT_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMOUNT_TXD_Register use record
      AMOUNT        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Unspecified
   type SPIS_TXD_Cluster is record
      --  TXD data pointer
      PTR    : aliased HAL.UInt32;
      --  Maximum number of bytes in transmit buffer
      MAXCNT : aliased MAXCNT_TXD_Register;
      --  Number of bytes transmitted in last granted transaction
      AMOUNT : aliased AMOUNT_TXD_Register;
   end record
     with Size => 96;

   for SPIS_TXD_Cluster use record
      PTR    at 16#0# range 0 .. 31;
      MAXCNT at 16#4# range 0 .. 31;
      AMOUNT at 16#8# range 0 .. 31;
   end record;

   --  Bit order
   type CONFIG_ORDER_Field is
     (--  Most significant bit shifted out first
      Msbfirst,
      --  Least significant bit shifted out first
      Lsbfirst)
     with Size => 1;
   for CONFIG_ORDER_Field use
     (Msbfirst => 0,
      Lsbfirst => 1);

   --  Serial clock (SCK) phase
   type CONFIG_CPHA_Field is
     (--  Sample on leading edge of clock, shift serial data on trailing edge
      Leading,
      --  Sample on trailing edge of clock, shift serial data on leading edge
      Trailing)
     with Size => 1;
   for CONFIG_CPHA_Field use
     (Leading => 0,
      Trailing => 1);

   --  Serial clock (SCK) polarity
   type CONFIG_CPOL_Field is
     (--  Active high
      Activehigh,
      --  Active low
      Activelow)
     with Size => 1;
   for CONFIG_CPOL_Field use
     (Activehigh => 0,
      Activelow => 1);

   --  Configuration register
   type CONFIG_Register is record
      --  Bit order
      ORDER         : CONFIG_ORDER_Field := NRF_SVD.SPIS.Msbfirst;
      --  Serial clock (SCK) phase
      CPHA          : CONFIG_CPHA_Field := NRF_SVD.SPIS.Leading;
      --  Serial clock (SCK) polarity
      CPOL          : CONFIG_CPOL_Field := NRF_SVD.SPIS.Activehigh;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      ORDER         at 0 range 0 .. 0;
      CPHA          at 0 range 1 .. 1;
      CPOL          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype DEF_DEF_Field is HAL.UInt8;

   --  Default character. Character clocked out in case of an ignored
   --  transaction.
   type DEF_Register is record
      --  Default character. Character clocked out in case of an ignored
      --  transaction.
      DEF           : DEF_DEF_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEF_Register use record
      DEF           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype ORC_ORC_Field is HAL.UInt8;

   --  Over-read character
   type ORC_Register is record
      --  Over-read character. Character clocked out after an over-read of the
      --  transmit buffer.
      ORC           : ORC_ORC_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ORC_Register use record
      ORC           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------------------------
   -- SPIS_PSEL cluster's Registers --
   -----------------------------------

   ----------------------------------
   -- SPIS_RXD cluster's Registers --
   ----------------------------------

   ----------------------------------
   -- SPIS_TXD cluster's Registers --
   ----------------------------------

   -----------------------------------
   -- SPIS_PSEL cluster's Registers --
   -----------------------------------

   ----------------------------------
   -- SPIS_RXD cluster's Registers --
   ----------------------------------

   ----------------------------------
   -- SPIS_TXD cluster's Registers --
   ----------------------------------

   -----------------
   -- Peripherals --
   -----------------

   --  SPI Slave 0
   type SPIS_Peripheral is record
      --  Acquire SPI semaphore
      TASKS_ACQUIRE   : aliased HAL.UInt32;
      --  Release SPI semaphore, enabling the SPI slave to acquire it
      TASKS_RELEASE   : aliased HAL.UInt32;
      --  Granted transaction completed
      EVENTS_END      : aliased HAL.UInt32;
      --  End of RXD buffer reached
      EVENTS_ENDRX    : aliased HAL.UInt32;
      --  Semaphore acquired
      EVENTS_ACQUIRED : aliased HAL.UInt32;
      --  Shortcut register
      SHORTS          : aliased SHORTS_Register;
      --  Enable interrupt
      INTENSET        : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR        : aliased INTENCLR_Register;
      --  Semaphore status register
      SEMSTAT         : aliased SEMSTAT_Register;
      --  Status from last transaction
      STATUS          : aliased STATUS_Register;
      --  Enable SPI slave
      ENABLE          : aliased ENABLE_Register;
      --  Unspecified
      PSEL            : aliased SPIS_PSEL_Cluster;
      --  Unspecified
      RXD             : aliased SPIS_RXD_Cluster;
      --  Unspecified
      TXD             : aliased SPIS_TXD_Cluster;
      --  Configuration register
      CONFIG          : aliased CONFIG_Register;
      --  Default character. Character clocked out in case of an ignored
      --  transaction.
      DEF             : aliased DEF_Register;
      --  Over-read character
      ORC             : aliased ORC_Register;
   end record
     with Volatile;

   for SPIS_Peripheral use record
      TASKS_ACQUIRE   at 16#24# range 0 .. 31;
      TASKS_RELEASE   at 16#28# range 0 .. 31;
      EVENTS_END      at 16#104# range 0 .. 31;
      EVENTS_ENDRX    at 16#110# range 0 .. 31;
      EVENTS_ACQUIRED at 16#128# range 0 .. 31;
      SHORTS          at 16#200# range 0 .. 31;
      INTENSET        at 16#304# range 0 .. 31;
      INTENCLR        at 16#308# range 0 .. 31;
      SEMSTAT         at 16#400# range 0 .. 31;
      STATUS          at 16#440# range 0 .. 31;
      ENABLE          at 16#500# range 0 .. 31;
      PSEL            at 16#508# range 0 .. 127;
      RXD             at 16#534# range 0 .. 95;
      TXD             at 16#544# range 0 .. 95;
      CONFIG          at 16#554# range 0 .. 31;
      DEF             at 16#55C# range 0 .. 31;
      ORC             at 16#5C0# range 0 .. 31;
   end record;

   --  SPI Slave 0
   SPIS0_Periph : aliased SPIS_Peripheral
     with Import, Address => SPIS0_Base;

   --  SPI Slave 1
   SPIS1_Periph : aliased SPIS_Peripheral
     with Import, Address => SPIS1_Base;

   --  SPI Slave 2
   SPIS2_Periph : aliased SPIS_Peripheral
     with Import, Address => SPIS2_Base;

end NRF_SVD.SPIS;
