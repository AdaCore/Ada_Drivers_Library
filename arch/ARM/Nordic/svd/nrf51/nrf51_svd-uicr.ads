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

package NRF51_SVD.UICR is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Readback protect region 0. Will be ignored if pre-programmed factory
   --  code is present on the chip.
   type RBPCONF_PR0_Field is
     (
      --  Enabled.
      Enabled,
      --  Disabled.
      Disabled)
     with Size => 8;
   for RBPCONF_PR0_Field use
     (Enabled => 0,
      Disabled => 255);

   --  Readback protect all code in the device.
   type RBPCONF_PALL_Field is
     (
      --  Enabled.
      Enabled,
      --  Disabled.
      Disabled)
     with Size => 8;
   for RBPCONF_PALL_Field use
     (Enabled => 0,
      Disabled => 255);

   --  Readback protection configuration.
   type RBPCONF_Register is record
      --  Readback protect region 0. Will be ignored if pre-programmed factory
      --  code is present on the chip.
      PR0            : RBPCONF_PR0_Field := NRF51_SVD.UICR.Disabled;
      --  Readback protect all code in the device.
      PALL           : RBPCONF_PALL_Field := NRF51_SVD.UICR.Disabled;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#FFFF#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RBPCONF_Register use record
      PR0            at 0 range 0 .. 7;
      PALL           at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Reset value for CLOCK XTALFREQ register.
   type XTALFREQ_XTALFREQ_Field is
     (
      --  32MHz Xtal is used.
      XTALFREQ_XTALFREQ_Field_32Mhz,
      --  16MHz Xtal is used.
      XTALFREQ_XTALFREQ_Field_16Mhz)
     with Size => 8;
   for XTALFREQ_XTALFREQ_Field use
     (XTALFREQ_XTALFREQ_Field_32Mhz => 0,
      XTALFREQ_XTALFREQ_Field_16Mhz => 255);

   --  Reset value for CLOCK XTALFREQ register.
   type XTALFREQ_Register is record
      --  Reset value for CLOCK XTALFREQ register.
      XTALFREQ      : XTALFREQ_XTALFREQ_Field :=
                       NRF51_SVD.UICR.XTALFREQ_XTALFREQ_Field_16Mhz;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#FFFFFF#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XTALFREQ_Register use record
      XTALFREQ      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype FWID_FWID_Field is HAL.UInt16;

   --  Firmware ID.
   type FWID_Register is record
      --  Read-only. Identification number for the firmware loaded into the
      --  chip.
      FWID           : FWID_FWID_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FWID_Register use record
      FWID           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Reserved for Nordic firmware design.

   --  Reserved for Nordic firmware design.
   type NRFFW_Registers is array (0 .. 14) of HAL.UInt32
     with Volatile;

   --  Reserved for Nordic hardware design.

   --  Reserved for Nordic hardware design.
   type NRFHW_Registers is array (0 .. 11) of HAL.UInt32
     with Volatile;

   --  Reserved for customer.

   --  Reserved for customer.
   type CUSTOMER_Registers is array (0 .. 31) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   type UICR_Disc is
     (
      Mode_1,
      Mode_2);

   --  User Information Configuration.
   type UICR_Peripheral
     (Discriminent : UICR_Disc := Mode_1)
   is record
      --  Length of code region 0.
      CLENR0         : aliased HAL.UInt32;
      --  Readback protection configuration.
      RBPCONF        : aliased RBPCONF_Register;
      --  Reset value for CLOCK XTALFREQ register.
      XTALFREQ       : aliased XTALFREQ_Register;
      --  Firmware ID.
      FWID           : aliased FWID_Register;
      --  Reserved for Nordic hardware design.
      NRFHW          : aliased NRFHW_Registers;
      --  Reserved for customer.
      CUSTOMER       : aliased CUSTOMER_Registers;
      case Discriminent is
         when Mode_1 =>
            --  Bootloader start address.
            BOOTLOADERADDR : aliased HAL.UInt32;
         when Mode_2 =>
            --  Reserved for Nordic firmware design.
            NRFFW : aliased NRFFW_Registers;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for UICR_Peripheral use record
      CLENR0         at 16#0# range 0 .. 31;
      RBPCONF        at 16#4# range 0 .. 31;
      XTALFREQ       at 16#8# range 0 .. 31;
      FWID           at 16#10# range 0 .. 31;
      NRFHW          at 16#50# range 0 .. 383;
      CUSTOMER       at 16#80# range 0 .. 1023;
      BOOTLOADERADDR at 16#14# range 0 .. 31;
      NRFFW          at 16#14# range 0 .. 479;
   end record;

   --  User Information Configuration.
   UICR_Periph : aliased UICR_Peripheral
     with Import, Address => System'To_Address (16#10001000#);

end NRF51_SVD.UICR;
