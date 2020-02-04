--  Copyright (c) 2010 - 2018, Nordic Semiconductor ASA
--
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
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
--  5. Any software provided in binary form under this license must not be
--  reverse engineered, decompiled, modified and/or disassembled.
--
--  THIS SOFTWARE IS PROVIDED BY NORDIC SEMICONDUCTOR ASA "AS IS" AND ANY
--  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL NORDIC SEMICONDUCTOR
--  ASA OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf52.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF_SVD.UICR is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Description collection[0]: Reserved for Nordic firmware design

   --  Description collection[0]: Reserved for Nordic firmware design
   type NRFFW_Registers is array (0 .. 14) of HAL.UInt32;

   --  Description collection[0]: Reserved for Nordic hardware design

   --  Description collection[0]: Reserved for Nordic hardware design
   type NRFHW_Registers is array (0 .. 11) of HAL.UInt32;

   --  Description collection[0]: Reserved for customer

   --  Description collection[0]: Reserved for customer
   type CUSTOMER_Registers is array (0 .. 31) of HAL.UInt32;

   subtype PSELRESET_PIN_Field is HAL.UInt6;

   --  Connection
   type PSELRESET_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for PSELRESET_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Description collection[0]: Mapping of the nRESET function (see POWER
   --  chapter for details)
   type PSELRESET_Register is record
      --  GPIO number P0.n onto which Reset is exposed
      PIN           : PSELRESET_PIN_Field := 16#3F#;
      --  unspecified
      Reserved_6_30 : HAL.UInt25 := 16#1FFFFFF#;
      --  Connection
      CONNECT       : PSELRESET_CONNECT_Field := NRF_SVD.UICR.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSELRESET_Register use record
      PIN           at 0 range 0 .. 5;
      Reserved_6_30 at 0 range 6 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   --  Description collection[0]: Mapping of the nRESET function (see POWER
   --  chapter for details)
   type PSELRESET_Registers is array (0 .. 1) of PSELRESET_Register;

   --  Enable or disable Access Port protection. Any other value than 0xFF
   --  being written to this field will enable protection.
   type APPROTECT_PALL_Field is
     (--  Enable
      Enabled,
      --  Disable
      Disabled)
     with Size => 8;
   for APPROTECT_PALL_Field use
     (Enabled => 0,
      Disabled => 255);

   --  Access Port protection
   type APPROTECT_Register is record
      --  Enable or disable Access Port protection. Any other value than 0xFF
      --  being written to this field will enable protection.
      PALL          : APPROTECT_PALL_Field := NRF_SVD.UICR.Disabled;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#FFFFFF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APPROTECT_Register use record
      PALL          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Setting of pins dedicated to NFC functionality
   type NFCPINS_PROTECT_Field is
     (--  Operation as GPIO pins. Same protection as normal GPIO pins
      Disabled,
      --  Operation as NFC antenna pins. Configures the protection for NFC operation
      Nfc)
     with Size => 1;
   for NFCPINS_PROTECT_Field use
     (Disabled => 0,
      Nfc => 1);

   --  Setting of pins dedicated to NFC functionality: NFC antenna or GPIO
   type NFCPINS_Register is record
      --  Setting of pins dedicated to NFC functionality
      PROTECT       : NFCPINS_PROTECT_Field := NRF_SVD.UICR.Nfc;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#7FFFFFFF#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for NFCPINS_Register use record
      PROTECT       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  User Information Configuration Registers
   type UICR_Peripheral is record
      --  Unspecified
      UNUSED0   : aliased HAL.UInt32;
      --  Unspecified
      UNUSED1   : aliased HAL.UInt32;
      --  Unspecified
      UNUSED2   : aliased HAL.UInt32;
      --  Unspecified
      UNUSED3   : aliased HAL.UInt32;
      --  Description collection[0]: Reserved for Nordic firmware design
      NRFFW     : aliased NRFFW_Registers;
      --  Description collection[0]: Reserved for Nordic hardware design
      NRFHW     : aliased NRFHW_Registers;
      --  Description collection[0]: Reserved for customer
      CUSTOMER  : aliased CUSTOMER_Registers;
      --  Description collection[0]: Mapping of the nRESET function (see POWER
      --  chapter for details)
      PSELRESET : aliased PSELRESET_Registers;
      --  Access Port protection
      APPROTECT : aliased APPROTECT_Register;
      --  Setting of pins dedicated to NFC functionality: NFC antenna or GPIO
      NFCPINS   : aliased NFCPINS_Register;
   end record
     with Volatile;

   for UICR_Peripheral use record
      UNUSED0   at 16#0# range 0 .. 31;
      UNUSED1   at 16#4# range 0 .. 31;
      UNUSED2   at 16#8# range 0 .. 31;
      UNUSED3   at 16#10# range 0 .. 31;
      NRFFW     at 16#14# range 0 .. 479;
      NRFHW     at 16#50# range 0 .. 383;
      CUSTOMER  at 16#80# range 0 .. 1023;
      PSELRESET at 16#200# range 0 .. 63;
      APPROTECT at 16#208# range 0 .. 31;
      NFCPINS   at 16#20C# range 0 .. 31;
   end record;

   --  User Information Configuration Registers
   UICR_Periph : aliased UICR_Peripheral
     with Import, Address => UICR_Base;

end NRF_SVD.UICR;
