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

package NRF51_SVD.TEMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Enable interrupt on DATARDY event.
   type INTENSET_DATARDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_DATARDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on DATARDY event.
   type INTENSET_DATARDY_Field_1 is
     (
      --  Reset value for the field
      Intenset_Datardy_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_DATARDY_Field_1 use
     (Intenset_Datardy_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on DATARDY event.
      DATARDY       : INTENSET_DATARDY_Field_1 :=
                       Intenset_Datardy_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      DATARDY       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Disable interrupt on DATARDY event.
   type INTENCLR_DATARDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_DATARDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on DATARDY event.
   type INTENCLR_DATARDY_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Datardy_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_DATARDY_Field_1 use
     (Intenclr_Datardy_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on DATARDY event.
      DATARDY       : INTENCLR_DATARDY_Field_1 :=
                       Intenclr_Datardy_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      DATARDY       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Peripheral power control.
   type POWER_POWER_Field is
     (
      --  Module power disabled.
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
      POWER         : POWER_POWER_Field := NRF51_SVD.TEMP.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for POWER_Register use record
      POWER         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Temperature Sensor.
   type TEMP_Peripheral is record
      --  Start temperature measurement.
      TASKS_START    : aliased HAL.UInt32;
      --  Stop temperature measurement.
      TASKS_STOP     : aliased HAL.UInt32;
      --  Temperature measurement complete, data ready event.
      EVENTS_DATARDY : aliased HAL.UInt32;
      --  Interrupt enable set register.
      INTENSET       : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR       : aliased INTENCLR_Register;
      --  Die temperature in degC, 2's complement format, 0.25 degC pecision.
      TEMP           : aliased HAL.UInt32;
      --  Peripheral power control.
      POWER          : aliased POWER_Register;
   end record
     with Volatile;

   for TEMP_Peripheral use record
      TASKS_START    at 16#0# range 0 .. 31;
      TASKS_STOP     at 16#4# range 0 .. 31;
      EVENTS_DATARDY at 16#100# range 0 .. 31;
      INTENSET       at 16#304# range 0 .. 31;
      INTENCLR       at 16#308# range 0 .. 31;
      TEMP           at 16#508# range 0 .. 31;
      POWER          at 16#FFC# range 0 .. 31;
   end record;

   --  Temperature Sensor.
   TEMP_Periph : aliased TEMP_Peripheral
     with Import, Address => System'To_Address (16#4000C000#);

end NRF51_SVD.TEMP;
