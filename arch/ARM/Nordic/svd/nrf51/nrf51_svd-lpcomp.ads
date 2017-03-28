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

package NRF51_SVD.LPCOMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between READY event and SAMPLE task.
   type SHORTS_READY_SAMPLE_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_READY_SAMPLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between RADY event and STOP task.
   type SHORTS_READY_STOP_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_READY_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between DOWN event and STOP task.
   type SHORTS_DOWN_STOP_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_DOWN_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between UP event and STOP task.
   type SHORTS_UP_STOP_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_UP_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between CROSS event and STOP task.
   type SHORTS_CROSS_STOP_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_CROSS_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcuts for the LPCOMP.
   type SHORTS_Register is record
      --  Shortcut between READY event and SAMPLE task.
      READY_SAMPLE  : SHORTS_READY_SAMPLE_Field := NRF51_SVD.LPCOMP.Disabled;
      --  Shortcut between RADY event and STOP task.
      READY_STOP    : SHORTS_READY_STOP_Field := NRF51_SVD.LPCOMP.Disabled;
      --  Shortcut between DOWN event and STOP task.
      DOWN_STOP     : SHORTS_DOWN_STOP_Field := NRF51_SVD.LPCOMP.Disabled;
      --  Shortcut between UP event and STOP task.
      UP_STOP       : SHORTS_UP_STOP_Field := NRF51_SVD.LPCOMP.Disabled;
      --  Shortcut between CROSS event and STOP task.
      CROSS_STOP    : SHORTS_CROSS_STOP_Field := NRF51_SVD.LPCOMP.Disabled;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      READY_SAMPLE  at 0 range 0 .. 0;
      READY_STOP    at 0 range 1 .. 1;
      DOWN_STOP     at 0 range 2 .. 2;
      UP_STOP       at 0 range 3 .. 3;
      CROSS_STOP    at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Enable interrupt on READY event.
   type INTENSET_READY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_READY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on READY event.
   type INTENSET_READY_Field_1 is
     (
      --  Reset value for the field
      Intenset_Ready_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_READY_Field_1 use
     (Intenset_Ready_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on DOWN event.
   type INTENSET_DOWN_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_DOWN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on DOWN event.
   type INTENSET_DOWN_Field_1 is
     (
      --  Reset value for the field
      Intenset_Down_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_DOWN_Field_1 use
     (Intenset_Down_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on UP event.
   type INTENSET_UP_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_UP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on UP event.
   type INTENSET_UP_Field_1 is
     (
      --  Reset value for the field
      Intenset_Up_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_UP_Field_1 use
     (Intenset_Up_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on CROSS event.
   type INTENSET_CROSS_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_CROSS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on CROSS event.
   type INTENSET_CROSS_Field_1 is
     (
      --  Reset value for the field
      Intenset_Cross_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_CROSS_Field_1 use
     (Intenset_Cross_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on READY event.
      READY         : INTENSET_READY_Field_1 := Intenset_Ready_Field_Reset;
      --  Enable interrupt on DOWN event.
      DOWN          : INTENSET_DOWN_Field_1 := Intenset_Down_Field_Reset;
      --  Enable interrupt on UP event.
      UP            : INTENSET_UP_Field_1 := Intenset_Up_Field_Reset;
      --  Enable interrupt on CROSS event.
      CROSS         : INTENSET_CROSS_Field_1 := Intenset_Cross_Field_Reset;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      READY         at 0 range 0 .. 0;
      DOWN          at 0 range 1 .. 1;
      UP            at 0 range 2 .. 2;
      CROSS         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Disable interrupt on READY event.
   type INTENCLR_READY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_READY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on READY event.
   type INTENCLR_READY_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Ready_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_READY_Field_1 use
     (Intenclr_Ready_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on DOWN event.
   type INTENCLR_DOWN_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_DOWN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on DOWN event.
   type INTENCLR_DOWN_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Down_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_DOWN_Field_1 use
     (Intenclr_Down_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on UP event.
   type INTENCLR_UP_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_UP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on UP event.
   type INTENCLR_UP_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Up_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_UP_Field_1 use
     (Intenclr_Up_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on CROSS event.
   type INTENCLR_CROSS_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_CROSS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on CROSS event.
   type INTENCLR_CROSS_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Cross_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_CROSS_Field_1 use
     (Intenclr_Cross_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on READY event.
      READY         : INTENCLR_READY_Field_1 := Intenclr_Ready_Field_Reset;
      --  Disable interrupt on DOWN event.
      DOWN          : INTENCLR_DOWN_Field_1 := Intenclr_Down_Field_Reset;
      --  Disable interrupt on UP event.
      UP            : INTENCLR_UP_Field_1 := Intenclr_Up_Field_Reset;
      --  Disable interrupt on CROSS event.
      CROSS         : INTENCLR_CROSS_Field_1 := Intenclr_Cross_Field_Reset;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      READY         at 0 range 0 .. 0;
      DOWN          at 0 range 1 .. 1;
      UP            at 0 range 2 .. 2;
      CROSS         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Result of last compare. Decision point SAMPLE task.
   type RESULT_RESULT_Field is
     (
      --  Input voltage is bellow the reference threshold.
      Bellow,
      --  Input voltage is above the reference threshold.
      Above)
     with Size => 1;
   for RESULT_RESULT_Field use
     (Bellow => 0,
      Above => 1);

   --  Result of last compare.
   type RESULT_Register is record
      --  Read-only. Result of last compare. Decision point SAMPLE task.
      RESULT        : RESULT_RESULT_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESULT_Register use record
      RESULT        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Enable or disable LPCOMP.
   type ENABLE_ENABLE_Field is
     (
      --  Disabled LPCOMP.
      Disabled,
      --  Enable LPCOMP.
      Enabled)
     with Size => 2;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable the LPCOMP.
   type ENABLE_Register is record
      --  Enable or disable LPCOMP.
      ENABLE        : ENABLE_ENABLE_Field := NRF51_SVD.LPCOMP.Disabled;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Analog input pin select.
   type PSEL_PSEL_Field is
     (
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
     with Size => 3;
   for PSEL_PSEL_Field use
     (Analoginput0 => 0,
      Analoginput1 => 1,
      Analoginput2 => 2,
      Analoginput3 => 3,
      Analoginput4 => 4,
      Analoginput5 => 5,
      Analoginput6 => 6,
      Analoginput7 => 7);

   --  Input pin select.
   type PSEL_Register is record
      --  Analog input pin select.
      PSEL          : PSEL_PSEL_Field := NRF51_SVD.LPCOMP.Analoginput0;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSEL_Register use record
      PSEL          at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Reference select.
   type REFSEL_REFSEL_Field is
     (
      --  Use supply with a 1/8 prescaler as reference.
      Supplyoneeighthprescaling,
      --  Use supply with a 2/8 prescaler as reference.
      Supplytwoeighthsprescaling,
      --  Use supply with a 3/8 prescaler as reference.
      Supplythreeeighthsprescaling,
      --  Use supply with a 4/8 prescaler as reference.
      Supplyfoureighthsprescaling,
      --  Use supply with a 5/8 prescaler as reference.
      Supplyfiveeighthsprescaling,
      --  Use supply with a 6/8 prescaler as reference.
      Supplysixeighthsprescaling,
      --  Use supply with a 7/8 prescaler as reference.
      Supplyseveneighthsprescaling,
      --  Use external analog reference as reference.
      Aref)
     with Size => 3;
   for REFSEL_REFSEL_Field use
     (Supplyoneeighthprescaling => 0,
      Supplytwoeighthsprescaling => 1,
      Supplythreeeighthsprescaling => 2,
      Supplyfoureighthsprescaling => 3,
      Supplyfiveeighthsprescaling => 4,
      Supplysixeighthsprescaling => 5,
      Supplyseveneighthsprescaling => 6,
      Aref => 7);

   --  Reference select.
   type REFSEL_Register is record
      --  Reference select.
      REFSEL        : REFSEL_REFSEL_Field :=
                       NRF51_SVD.LPCOMP.Supplyoneeighthprescaling;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for REFSEL_Register use record
      REFSEL        at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  External analog reference pin selection.
   type EXTREFSEL_EXTREFSEL_Field is
     (
      --  Use analog reference 0 as reference.
      Analogreference0,
      --  Use analog reference 1 as reference.
      Analogreference1)
     with Size => 1;
   for EXTREFSEL_EXTREFSEL_Field use
     (Analogreference0 => 0,
      Analogreference1 => 1);

   --  External reference select.
   type EXTREFSEL_Register is record
      --  External analog reference pin selection.
      EXTREFSEL     : EXTREFSEL_EXTREFSEL_Field :=
                       NRF51_SVD.LPCOMP.Analogreference0;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTREFSEL_Register use record
      EXTREFSEL     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Analog detect configuration.
   type ANADETECT_ANADETECT_Field is
     (
      --  Generate ANADETEC on crossing, both upwards and downwards crossing.
      Cross,
      --  Generate ANADETEC on upwards crossing only.
      Up,
      --  Generate ANADETEC on downwards crossing only.
      Down)
     with Size => 2;
   for ANADETECT_ANADETECT_Field use
     (Cross => 0,
      Up => 1,
      Down => 2);

   --  Analog detect configuration.
   type ANADETECT_Register is record
      --  Analog detect configuration.
      ANADETECT     : ANADETECT_ANADETECT_Field := NRF51_SVD.LPCOMP.Cross;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ANADETECT_Register use record
      ANADETECT     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
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
      POWER         : POWER_POWER_Field := NRF51_SVD.LPCOMP.Disabled;
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

   --  Low power comparator.
   type LPCOMP_Peripheral is record
      --  Start the comparator.
      TASKS_START  : aliased HAL.UInt32;
      --  Stop the comparator.
      TASKS_STOP   : aliased HAL.UInt32;
      --  Sample comparator value.
      TASKS_SAMPLE : aliased HAL.UInt32;
      --  LPCOMP is ready and output is valid.
      EVENTS_READY : aliased HAL.UInt32;
      --  Input voltage crossed the threshold going down.
      EVENTS_DOWN  : aliased HAL.UInt32;
      --  Input voltage crossed the threshold going up.
      EVENTS_UP    : aliased HAL.UInt32;
      --  Input voltage crossed the threshold in any direction.
      EVENTS_CROSS : aliased HAL.UInt32;
      --  Shortcuts for the LPCOMP.
      SHORTS       : aliased SHORTS_Register;
      --  Interrupt enable set register.
      INTENSET     : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR     : aliased INTENCLR_Register;
      --  Result of last compare.
      RESULT       : aliased RESULT_Register;
      --  Enable the LPCOMP.
      ENABLE       : aliased ENABLE_Register;
      --  Input pin select.
      PSEL         : aliased PSEL_Register;
      --  Reference select.
      REFSEL       : aliased REFSEL_Register;
      --  External reference select.
      EXTREFSEL    : aliased EXTREFSEL_Register;
      --  Analog detect configuration.
      ANADETECT    : aliased ANADETECT_Register;
      --  Peripheral power control.
      POWER        : aliased POWER_Register;
   end record
     with Volatile;

   for LPCOMP_Peripheral use record
      TASKS_START  at 16#0# range 0 .. 31;
      TASKS_STOP   at 16#4# range 0 .. 31;
      TASKS_SAMPLE at 16#8# range 0 .. 31;
      EVENTS_READY at 16#100# range 0 .. 31;
      EVENTS_DOWN  at 16#104# range 0 .. 31;
      EVENTS_UP    at 16#108# range 0 .. 31;
      EVENTS_CROSS at 16#10C# range 0 .. 31;
      SHORTS       at 16#200# range 0 .. 31;
      INTENSET     at 16#304# range 0 .. 31;
      INTENCLR     at 16#308# range 0 .. 31;
      RESULT       at 16#400# range 0 .. 31;
      ENABLE       at 16#500# range 0 .. 31;
      PSEL         at 16#504# range 0 .. 31;
      REFSEL       at 16#508# range 0 .. 31;
      EXTREFSEL    at 16#50C# range 0 .. 31;
      ANADETECT    at 16#520# range 0 .. 31;
      POWER        at 16#FFC# range 0 .. 31;
   end record;

   --  Low power comparator.
   LPCOMP_Periph : aliased LPCOMP_Peripheral
     with Import, Address => System'To_Address (16#40013000#);

end NRF51_SVD.LPCOMP;
