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

package NRF51_SVD.ADC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Enable interrupt on END event.
   type INTENSET_END_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_END_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on END event.
   type INTENSET_END_Field_1 is
     (
      --  Reset value for the field
      Intenset_End_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_END_Field_1 use
     (Intenset_End_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on END event.
      END_k         : INTENSET_END_Field_1 := Intenset_End_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      END_k         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Disable interrupt on END event.
   type INTENCLR_END_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_END_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on END event.
   type INTENCLR_END_Field_1 is
     (
      --  Reset value for the field
      Intenclr_End_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_END_Field_1 use
     (Intenclr_End_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on END event.
      END_k         : INTENCLR_END_Field_1 := Intenclr_End_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      END_k         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  ADC busy register.
   type BUSY_BUSY_Field is
     (
      --  No ongoing ADC conversion is taking place. ADC is ready.
      Ready,
      --  An ADC conversion is taking place. ADC is busy.
      Busy)
     with Size => 1;
   for BUSY_BUSY_Field use
     (Ready => 0,
      Busy => 1);

   --  ADC busy register.
   type BUSY_Register is record
      --  Read-only. ADC busy register.
      BUSY          : BUSY_BUSY_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BUSY_Register use record
      BUSY          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  ADC enable.
   type ENABLE_ENABLE_Field is
     (
      --  ADC is disabled.
      Disabled,
      --  ADC is enabled. If an analog input pin is selected as source of the
      --  conversion, the selected pin is configured as an analog input.
      Enabled)
     with Size => 2;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  ADC enable.
   type ENABLE_Register is record
      --  ADC enable.
      ENABLE        : ENABLE_ENABLE_Field := NRF51_SVD.ADC.Disabled;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  ADC resolution.
   type CONFIG_RES_Field is
     (
      --  8bit ADC resolution.
      CONFIG_RES_Field_8BIT,
      --  9bit ADC resolution.
      CONFIG_RES_Field_9BIT,
      --  10bit ADC resolution.
      CONFIG_RES_Field_10BIT)
     with Size => 2;
   for CONFIG_RES_Field use
     (CONFIG_RES_Field_8BIT => 0,
      CONFIG_RES_Field_9BIT => 1,
      CONFIG_RES_Field_10BIT => 2);

   --  ADC input selection.
   type CONFIG_INPSEL_Field is
     (
      --  Analog input specified by PSEL with no prescaling used as input for
      --  the conversion.
      Analoginputnoprescaling,
      --  Analog input specified by PSEL with 2/3 prescaling used as input for
      --  the conversion.
      Analoginputtwothirdsprescaling,
      --  Analog input specified by PSEL with 1/3 prescaling used as input for
      --  the conversion.
      Analoginputonethirdprescaling,
      --  Supply voltage with 2/3 prescaling used as input for the conversion.
      Supplytwothirdsprescaling,
      --  Supply voltage with 1/3 prescaling used as input for the conversion.
      Supplyonethirdprescaling)
     with Size => 3;
   for CONFIG_INPSEL_Field use
     (Analoginputnoprescaling => 0,
      Analoginputtwothirdsprescaling => 1,
      Analoginputonethirdprescaling => 2,
      Supplytwothirdsprescaling => 5,
      Supplyonethirdprescaling => 6);

   --  ADC reference selection.
   type CONFIG_REFSEL_Field is
     (
      --  Use internal 1.2V bandgap voltage as reference for conversion.
      Vbg,
      --  Use external source configured by EXTREFSEL as reference for
      --  conversion.
      External,
      --  Use supply voltage with 1/2 prescaling as reference for conversion.
      --  Only usable when supply voltage is between 1.7V and 2.6V.
      Supplyonehalfprescaling,
      --  Use supply voltage with 1/3 prescaling as reference for conversion.
      --  Only usable when supply voltage is between 2.5V and 3.6V.
      Supplyonethirdprescaling)
     with Size => 2;
   for CONFIG_REFSEL_Field use
     (Vbg => 0,
      External => 1,
      Supplyonehalfprescaling => 2,
      Supplyonethirdprescaling => 3);

   --  ADC analog pin selection.
   type CONFIG_PSEL_Field is
     (
      --  Analog input pins disabled.
      Disabled,
      --  Use analog input 0 as analog input.
      Analoginput0,
      --  Use analog input 1 as analog input.
      Analoginput1,
      --  Use analog input 2 as analog input.
      Analoginput2,
      --  Use analog input 3 as analog input.
      Analoginput3,
      --  Use analog input 4 as analog input.
      Analoginput4,
      --  Use analog input 5 as analog input.
      Analoginput5,
      --  Use analog input 6 as analog input.
      Analoginput6,
      --  Use analog input 7 as analog input.
      Analoginput7)
     with Size => 8;
   for CONFIG_PSEL_Field use
     (Disabled => 0,
      Analoginput0 => 1,
      Analoginput1 => 2,
      Analoginput2 => 4,
      Analoginput3 => 8,
      Analoginput4 => 16,
      Analoginput5 => 32,
      Analoginput6 => 64,
      Analoginput7 => 128);

   --  ADC external reference pin selection.
   type CONFIG_EXTREFSEL_Field is
     (
      --  Analog external reference inputs disabled.
      None,
      --  Use analog reference 0 as reference.
      Analogreference0,
      --  Use analog reference 1 as reference.
      Analogreference1)
     with Size => 2;
   for CONFIG_EXTREFSEL_Field use
     (None => 0,
      Analogreference0 => 1,
      Analogreference1 => 2);

   --  ADC configuration register.
   type CONFIG_Register is record
      --  ADC resolution.
      RES            : CONFIG_RES_Field :=
                        NRF51_SVD.ADC.CONFIG_RES_Field_8BIT;
      --  ADC input selection.
      INPSEL         : CONFIG_INPSEL_Field :=
                        NRF51_SVD.ADC.Supplyonethirdprescaling;
      --  ADC reference selection.
      REFSEL         : CONFIG_REFSEL_Field := NRF51_SVD.ADC.Vbg;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  ADC analog pin selection.
      PSEL           : CONFIG_PSEL_Field := NRF51_SVD.ADC.Disabled;
      --  ADC external reference pin selection.
      EXTREFSEL      : CONFIG_EXTREFSEL_Field := NRF51_SVD.ADC.None;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      RES            at 0 range 0 .. 1;
      INPSEL         at 0 range 2 .. 4;
      REFSEL         at 0 range 5 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      PSEL           at 0 range 8 .. 15;
      EXTREFSEL      at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype RESULT_RESULT_Field is HAL.UInt10;

   --  Result of ADC conversion.
   type RESULT_Register is record
      --  Read-only. Result of ADC conversion.
      RESULT         : RESULT_RESULT_Field;
      --  unspecified
      Reserved_10_31 : HAL.UInt22;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESULT_Register use record
      RESULT         at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
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
      POWER         : POWER_POWER_Field := NRF51_SVD.ADC.Disabled;
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

   --  Analog to digital converter.
   type ADC_Peripheral is record
      --  Start an ADC conversion.
      TASKS_START : aliased HAL.UInt32;
      --  Stop ADC.
      TASKS_STOP  : aliased HAL.UInt32;
      --  ADC conversion complete.
      EVENTS_END  : aliased HAL.UInt32;
      --  Interrupt enable set register.
      INTENSET    : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR    : aliased INTENCLR_Register;
      --  ADC busy register.
      BUSY        : aliased BUSY_Register;
      --  ADC enable.
      ENABLE      : aliased ENABLE_Register;
      --  ADC configuration register.
      CONFIG      : aliased CONFIG_Register;
      --  Result of ADC conversion.
      RESULT      : aliased RESULT_Register;
      --  Peripheral power control.
      POWER       : aliased POWER_Register;
   end record
     with Volatile;

   for ADC_Peripheral use record
      TASKS_START at 16#0# range 0 .. 31;
      TASKS_STOP  at 16#4# range 0 .. 31;
      EVENTS_END  at 16#100# range 0 .. 31;
      INTENSET    at 16#304# range 0 .. 31;
      INTENCLR    at 16#308# range 0 .. 31;
      BUSY        at 16#400# range 0 .. 31;
      ENABLE      at 16#500# range 0 .. 31;
      CONFIG      at 16#504# range 0 .. 31;
      RESULT      at 16#508# range 0 .. 31;
      POWER       at 16#FFC# range 0 .. 31;
   end record;

   --  Analog to digital converter.
   ADC_Periph : aliased ADC_Peripheral
     with Import, Address => System'To_Address (16#40007000#);

end NRF51_SVD.ADC;
