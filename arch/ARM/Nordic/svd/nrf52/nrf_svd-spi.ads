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

package NRF_SVD.SPI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Write '1' to Enable interrupt for READY event
   type INTENSET_READY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_READY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for READY event
   type INTENSET_READY_Field_1 is
     (--  Reset value for the field
      Intenset_Ready_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_READY_Field_1 use
     (Intenset_Ready_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Write '1' to Enable interrupt for READY event
      READY         : INTENSET_READY_Field_1 := Intenset_Ready_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      READY         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Write '1' to Disable interrupt for READY event
   type INTENCLR_READY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_READY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for READY event
   type INTENCLR_READY_Field_1 is
     (--  Reset value for the field
      Intenclr_Ready_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_READY_Field_1 use
     (Intenclr_Ready_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Write '1' to Disable interrupt for READY event
      READY         : INTENCLR_READY_Field_1 := Intenclr_Ready_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      READY         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Enable or disable SPI
   type ENABLE_ENABLE_Field is
     (--  Disable SPI
      Disabled,
      --  Enable SPI
      Enabled)
     with Size => 4;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable SPI
   type ENABLE_Register is record
      --  Enable or disable SPI
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.SPI.Disabled;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype RXD_RXD_Field is HAL.UInt8;

   --  RXD register
   type RXD_Register is record
      --  Read-only. *** Reading this field has side effects on other resources
      --  ***. RX data received. Double buffered
      RXD           : RXD_RXD_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXD_Register use record
      RXD           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype TXD_TXD_Field is HAL.UInt8;

   --  TXD register
   type TXD_Register is record
      --  TX data to send. Double buffered
      TXD           : TXD_TXD_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXD_Register use record
      TXD           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
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
      ORDER         : CONFIG_ORDER_Field := NRF_SVD.SPI.Msbfirst;
      --  Serial clock (SCK) phase
      CPHA          : CONFIG_CPHA_Field := NRF_SVD.SPI.Leading;
      --  Serial clock (SCK) polarity
      CPOL          : CONFIG_CPOL_Field := NRF_SVD.SPI.Activehigh;
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

   -----------------
   -- Peripherals --
   -----------------

   --  Serial Peripheral Interface 0
   type SPI_Peripheral is record
      --  TXD byte sent and RXD byte received
      EVENTS_READY : aliased HAL.UInt32;
      --  Enable interrupt
      INTENSET     : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR     : aliased INTENCLR_Register;
      --  Enable SPI
      ENABLE       : aliased ENABLE_Register;
      --  Pin select for SCK.
      PSELSCK      : aliased HAL.UInt32;
      --  Pin select for MISO.
      PSELMISO     : aliased HAL.UInt32;
      --  Pin select for MOSI.
      PSELMOSI     : aliased HAL.UInt32;
      --  RXD register
      RXD          : aliased RXD_Register;
      --  TXD register
      TXD          : aliased TXD_Register;
      --  SPI frequency
      FREQUENCY    : aliased HAL.UInt32;
      --  Configuration register
      CONFIG       : aliased CONFIG_Register;
   end record
     with Volatile;

   for SPI_Peripheral use record
      EVENTS_READY at 16#108# range 0 .. 31;
      INTENSET     at 16#304# range 0 .. 31;
      INTENCLR     at 16#308# range 0 .. 31;
      ENABLE       at 16#500# range 0 .. 31;
      PSELSCK      at 16#508# range 0 .. 31;
      PSELMISO     at 16#50C# range 0 .. 31;
      PSELMOSI     at 16#510# range 0 .. 31;
      RXD          at 16#518# range 0 .. 31;
      TXD          at 16#51C# range 0 .. 31;
      FREQUENCY    at 16#524# range 0 .. 31;
      CONFIG       at 16#554# range 0 .. 31;
   end record;

   --  Serial Peripheral Interface 0
   SPI0_Periph : aliased SPI_Peripheral
     with Import, Address => SPI0_Base;

   --  Serial Peripheral Interface 1
   SPI1_Periph : aliased SPI_Peripheral
     with Import, Address => SPI1_Base;

   --  Serial Peripheral Interface 2
   SPI2_Periph : aliased SPI_Peripheral
     with Import, Address => SPI2_Base;

end NRF_SVD.SPI;
