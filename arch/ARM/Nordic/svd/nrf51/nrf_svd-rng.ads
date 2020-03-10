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

package NRF_SVD.RNG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between VALRDY event and STOP task.
   type SHORTS_VALRDY_STOP_Field is
     (--  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_VALRDY_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcuts for the RNG.
   type SHORTS_Register is record
      --  Shortcut between VALRDY event and STOP task.
      VALRDY_STOP   : SHORTS_VALRDY_STOP_Field := NRF_SVD.RNG.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      VALRDY_STOP   at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Enable interrupt on VALRDY event.
   type INTENSET_VALRDY_Field is
     (--  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_VALRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on VALRDY event.
   type INTENSET_VALRDY_Field_1 is
     (--  Reset value for the field
      Intenset_Valrdy_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_VALRDY_Field_1 use
     (Intenset_Valrdy_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register
   type INTENSET_Register is record
      --  Enable interrupt on VALRDY event.
      VALRDY        : INTENSET_VALRDY_Field_1 := Intenset_Valrdy_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      VALRDY        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Disable interrupt on VALRDY event.
   type INTENCLR_VALRDY_Field is
     (--  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_VALRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on VALRDY event.
   type INTENCLR_VALRDY_Field_1 is
     (--  Reset value for the field
      Intenclr_Valrdy_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_VALRDY_Field_1 use
     (Intenclr_Valrdy_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register
   type INTENCLR_Register is record
      --  Disable interrupt on VALRDY event.
      VALRDY        : INTENCLR_VALRDY_Field_1 := Intenclr_Valrdy_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      VALRDY        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Digital error correction enable.
   type CONFIG_DERCEN_Field is
     (--  Digital error correction disabled.
      Disabled,
      --  Digital error correction enabled.
      Enabled)
     with Size => 1;
   for CONFIG_DERCEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Configuration register.
   type CONFIG_Register is record
      --  Digital error correction enable.
      DERCEN        : CONFIG_DERCEN_Field := NRF_SVD.RNG.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      DERCEN        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype VALUE_VALUE_Field is HAL.UInt8;

   --  RNG random number.
   type VALUE_Register is record
      --  Read-only. Generated random number.
      VALUE         : VALUE_VALUE_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for VALUE_Register use record
      VALUE         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Peripheral power control.
   type POWER_POWER_Field is
     (--  Module power disabled.
      Disabled,
      --  Module power enabled.
      Enabled)
     with Size => 1;
   for POWER_POWER_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Peripheral power control.
   type POWER_Register is record
      --  Peripheral power control.
      POWER         : POWER_POWER_Field := NRF_SVD.RNG.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for POWER_Register use record
      POWER         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Random Number Generator.
   type RNG_Peripheral is record
      --  Start the random number generator.
      TASKS_START   : aliased HAL.UInt32;
      --  Stop the random number generator.
      TASKS_STOP    : aliased HAL.UInt32;
      --  New random number generated and written to VALUE register.
      EVENTS_VALRDY : aliased HAL.UInt32;
      --  Shortcuts for the RNG.
      SHORTS        : aliased SHORTS_Register;
      --  Interrupt enable set register
      INTENSET      : aliased INTENSET_Register;
      --  Interrupt enable clear register
      INTENCLR      : aliased INTENCLR_Register;
      --  Configuration register.
      CONFIG        : aliased CONFIG_Register;
      --  RNG random number.
      VALUE         : aliased VALUE_Register;
      --  Peripheral power control.
      POWER         : aliased POWER_Register;
   end record
     with Volatile;

   for RNG_Peripheral use record
      TASKS_START   at 16#0# range 0 .. 31;
      TASKS_STOP    at 16#4# range 0 .. 31;
      EVENTS_VALRDY at 16#100# range 0 .. 31;
      SHORTS        at 16#200# range 0 .. 31;
      INTENSET      at 16#304# range 0 .. 31;
      INTENCLR      at 16#308# range 0 .. 31;
      CONFIG        at 16#504# range 0 .. 31;
      VALUE         at 16#508# range 0 .. 31;
      POWER         at 16#FFC# range 0 .. 31;
   end record;

   --  Random Number Generator.
   RNG_Periph : aliased RNG_Peripheral
     with Import, Address => RNG_Base;

end NRF_SVD.RNG;
