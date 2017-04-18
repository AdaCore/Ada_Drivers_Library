--  Copyright (c) 2013, Nordic Semiconductor ASA
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright notice,
--  this list of conditions and the following disclaimer in the documentation
--  and/or other materials provided with the distribution.
--
--  * Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
--  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
--  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf51.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF51_SVD.FICR is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Pre-programmed factory code present.
   type PPFC_PPFC_Field is
     (
      --  Present.
      Present,
      --  Not present.
      Notpresent)
     with Size => 8;
   for PPFC_PPFC_Field use
     (Present => 0,
      Notpresent => 255);

   --  Pre-programmed factory code present.
   type PPFC_Register is record
      --  Read-only. Pre-programmed factory code present.
      PPFC          : PPFC_PPFC_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PPFC_Register use record
      PPFC          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Deprecated array of size of RAM block in bytes. This name is kept for backward compatinility purposes. Use SIZERAMBLOCKS instead.

   --  Deprecated array of size of RAM block in bytes. This name is kept for
   --  backward compatinility purposes. Use SIZERAMBLOCKS instead.
   type SIZERAMBLOCK_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   subtype CONFIGID_HWID_Field is HAL.UInt16;
   subtype CONFIGID_FWID_Field is HAL.UInt16;

   --  Configuration identifier.
   type CONFIGID_Register is record
      --  Read-only. Hardware Identification Number.
      HWID : CONFIGID_HWID_Field;
      --  Read-only. Firmware Identification Number pre-loaded into the flash.
      FWID : CONFIGID_FWID_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIGID_Register use record
      HWID at 0 range 0 .. 15;
      FWID at 0 range 16 .. 31;
   end record;

   --  Device identifier.

   --  Device identifier.
   type DEVICEID_Registers is array (0 .. 1) of HAL.UInt32
     with Volatile;

   --  Encryption root.

   --  Encryption root.
   type ER_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Identity root.

   --  Identity root.
   type IR_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Device address type.
   type DEVICEADDRTYPE_DEVICEADDRTYPE_Field is
     (
      --  Public address.
      Public,
      --  Random address.
      Random)
     with Size => 1;
   for DEVICEADDRTYPE_DEVICEADDRTYPE_Field use
     (Public => 0,
      Random => 1);

   --  Device address type.
   type DEVICEADDRTYPE_Register is record
      --  Read-only. Device address type.
      DEVICEADDRTYPE : DEVICEADDRTYPE_DEVICEADDRTYPE_Field;
      --  unspecified
      Reserved_1_31  : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEVICEADDRTYPE_Register use record
      DEVICEADDRTYPE at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   --  Device address.

   --  Device address.
   type DEVICEADDR_Registers is array (0 .. 1) of HAL.UInt32
     with Volatile;

   --  Override default values for NRF_1Mbit mode.
   type OVERRIDEEN_NRF_1MBIT_Field is
     (
      --  Override the default values for NRF_1Mbit mode.
      Override,
      --  Do not override the default values for NRF_1Mbit mode.
      Notoverride)
     with Size => 1;
   for OVERRIDEEN_NRF_1MBIT_Field use
     (Override => 0,
      Notoverride => 1);

   --  Override default values for BLE_1Mbit mode.
   type OVERRIDEEN_BLE_1MBIT_Field is
     (
      --  Override the default values for BLE_1Mbit mode.
      Override,
      --  Do not override the default values for BLE_1Mbit mode.
      Notoverride)
     with Size => 1;
   for OVERRIDEEN_BLE_1MBIT_Field use
     (Override => 0,
      Notoverride => 1);

   --  Radio calibration override enable.
   type OVERRIDEEN_Register is record
      --  Read-only. Override default values for NRF_1Mbit mode.
      NRF_1MBIT     : OVERRIDEEN_NRF_1MBIT_Field;
      --  unspecified
      Reserved_1_2  : HAL.UInt2;
      --  Read-only. Override default values for BLE_1Mbit mode.
      BLE_1MBIT     : OVERRIDEEN_BLE_1MBIT_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OVERRIDEEN_Register use record
      NRF_1MBIT     at 0 range 0 .. 0;
      Reserved_1_2  at 0 range 1 .. 2;
      BLE_1MBIT     at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Override values for the OVERRIDEn registers in RADIO for NRF_1Mbit mode.

   --  Override values for the OVERRIDEn registers in RADIO for NRF_1Mbit mode.
   type NRF_1MBIT_Registers is array (0 .. 4) of HAL.UInt32
     with Volatile;

   --  Override values for the OVERRIDEn registers in RADIO for BLE_1Mbit mode.

   --  Override values for the OVERRIDEn registers in RADIO for BLE_1Mbit mode.
   type BLE_1MBIT_Registers is array (0 .. 4) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   type FICR_Disc is
     (
      S,
      Default);

   --  Factory Information Configuration.
   type FICR_Peripheral
     (Discriminent : FICR_Disc := S)
   is record
      --  Code memory page size in bytes.
      CODEPAGESIZE   : aliased HAL.UInt32;
      --  Code memory size in pages.
      CODESIZE       : aliased HAL.UInt32;
      --  Length of code region 0 in bytes.
      CLENR0         : aliased HAL.UInt32;
      --  Pre-programmed factory code present.
      PPFC           : aliased PPFC_Register;
      --  Number of individualy controllable RAM blocks.
      NUMRAMBLOCK    : aliased HAL.UInt32;
      --  Configuration identifier.
      CONFIGID       : aliased CONFIGID_Register;
      --  Device identifier.
      DEVICEID       : aliased DEVICEID_Registers;
      --  Encryption root.
      ER             : aliased ER_Registers;
      --  Identity root.
      IR             : aliased IR_Registers;
      --  Device address type.
      DEVICEADDRTYPE : aliased DEVICEADDRTYPE_Register;
      --  Device address.
      DEVICEADDR     : aliased DEVICEADDR_Registers;
      --  Radio calibration override enable.
      OVERRIDEEN     : aliased OVERRIDEEN_Register;
      --  Override values for the OVERRIDEn registers in RADIO for NRF_1Mbit
      --  mode.
      NRF_1MBIT      : aliased NRF_1MBIT_Registers;
      --  Override values for the OVERRIDEn registers in RADIO for BLE_1Mbit
      --  mode.
      BLE_1MBIT      : aliased BLE_1MBIT_Registers;
      case Discriminent is
         when S =>
            --  Size of RAM blocks in bytes.
            SIZERAMBLOCKS : aliased HAL.UInt32;
         when Default =>
            --  Deprecated array of size of RAM block in bytes. This name is
            --  kept for backward compatinility purposes. Use SIZERAMBLOCKS
            --  instead.
            SIZERAMBLOCK : aliased SIZERAMBLOCK_Registers;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for FICR_Peripheral use record
      CODEPAGESIZE   at 16#10# range 0 .. 31;
      CODESIZE       at 16#14# range 0 .. 31;
      CLENR0         at 16#28# range 0 .. 31;
      PPFC           at 16#2C# range 0 .. 31;
      NUMRAMBLOCK    at 16#34# range 0 .. 31;
      CONFIGID       at 16#5C# range 0 .. 31;
      DEVICEID       at 16#60# range 0 .. 63;
      ER             at 16#80# range 0 .. 127;
      IR             at 16#90# range 0 .. 127;
      DEVICEADDRTYPE at 16#A0# range 0 .. 31;
      DEVICEADDR     at 16#A4# range 0 .. 63;
      OVERRIDEEN     at 16#AC# range 0 .. 31;
      NRF_1MBIT      at 16#B0# range 0 .. 159;
      BLE_1MBIT      at 16#EC# range 0 .. 159;
      SIZERAMBLOCKS  at 16#38# range 0 .. 31;
      SIZERAMBLOCK   at 16#38# range 0 .. 127;
   end record;

   --  Factory Information Configuration.
   FICR_Periph : aliased FICR_Peripheral
     with Import, Address => System'To_Address (16#10000000#);

end NRF51_SVD.FICR;
