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

package NRF_SVD.LPCOMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between READY event and SAMPLE task
   type SHORTS_READY_SAMPLE_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_READY_SAMPLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between READY event and STOP task
   type SHORTS_READY_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_READY_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between DOWN event and STOP task
   type SHORTS_DOWN_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_DOWN_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between UP event and STOP task
   type SHORTS_UP_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_UP_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between CROSS event and STOP task
   type SHORTS_CROSS_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_CROSS_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut register
   type SHORTS_Register is record
      --  Shortcut between READY event and SAMPLE task
      READY_SAMPLE  : SHORTS_READY_SAMPLE_Field := NRF_SVD.LPCOMP.Disabled;
      --  Shortcut between READY event and STOP task
      READY_STOP    : SHORTS_READY_STOP_Field := NRF_SVD.LPCOMP.Disabled;
      --  Shortcut between DOWN event and STOP task
      DOWN_STOP     : SHORTS_DOWN_STOP_Field := NRF_SVD.LPCOMP.Disabled;
      --  Shortcut between UP event and STOP task
      UP_STOP       : SHORTS_UP_STOP_Field := NRF_SVD.LPCOMP.Disabled;
      --  Shortcut between CROSS event and STOP task
      CROSS_STOP    : SHORTS_CROSS_STOP_Field := NRF_SVD.LPCOMP.Disabled;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      READY_SAMPLE  at 0 range 0 .. 0;
      READY_STOP    at 0 range 1 .. 1;
      DOWN_STOP     at 0 range 2 .. 2;
      UP_STOP       at 0 range 3 .. 3;
      CROSS_STOP    at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

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

   --  Write '1' to Enable interrupt for DOWN event
   type INTENSET_DOWN_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_DOWN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for DOWN event
   type INTENSET_DOWN_Field_1 is
     (--  Reset value for the field
      Intenset_Down_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_DOWN_Field_1 use
     (Intenset_Down_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for UP event
   type INTENSET_UP_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_UP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for UP event
   type INTENSET_UP_Field_1 is
     (--  Reset value for the field
      Intenset_Up_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_UP_Field_1 use
     (Intenset_Up_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CROSS event
   type INTENSET_CROSS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CROSS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CROSS event
   type INTENSET_CROSS_Field_1 is
     (--  Reset value for the field
      Intenset_Cross_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CROSS_Field_1 use
     (Intenset_Cross_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for READY event
      READY         : INTENSET_READY_Field_1 := Intenset_Ready_Field_Reset;
      --  Write '1' to Enable interrupt for DOWN event
      DOWN          : INTENSET_DOWN_Field_1 := Intenset_Down_Field_Reset;
      --  Write '1' to Enable interrupt for UP event
      UP            : INTENSET_UP_Field_1 := Intenset_Up_Field_Reset;
      --  Write '1' to Enable interrupt for CROSS event
      CROSS         : INTENSET_CROSS_Field_1 := Intenset_Cross_Field_Reset;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      READY         at 0 range 0 .. 0;
      DOWN          at 0 range 1 .. 1;
      UP            at 0 range 2 .. 2;
      CROSS         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
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

   --  Write '1' to Disable interrupt for DOWN event
   type INTENCLR_DOWN_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_DOWN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for DOWN event
   type INTENCLR_DOWN_Field_1 is
     (--  Reset value for the field
      Intenclr_Down_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_DOWN_Field_1 use
     (Intenclr_Down_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for UP event
   type INTENCLR_UP_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_UP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for UP event
   type INTENCLR_UP_Field_1 is
     (--  Reset value for the field
      Intenclr_Up_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_UP_Field_1 use
     (Intenclr_Up_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CROSS event
   type INTENCLR_CROSS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CROSS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CROSS event
   type INTENCLR_CROSS_Field_1 is
     (--  Reset value for the field
      Intenclr_Cross_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CROSS_Field_1 use
     (Intenclr_Cross_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for READY event
      READY         : INTENCLR_READY_Field_1 := Intenclr_Ready_Field_Reset;
      --  Write '1' to Disable interrupt for DOWN event
      DOWN          : INTENCLR_DOWN_Field_1 := Intenclr_Down_Field_Reset;
      --  Write '1' to Disable interrupt for UP event
      UP            : INTENCLR_UP_Field_1 := Intenclr_Up_Field_Reset;
      --  Write '1' to Disable interrupt for CROSS event
      CROSS         : INTENCLR_CROSS_Field_1 := Intenclr_Cross_Field_Reset;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
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
     (--  Input voltage is below the reference threshold (VIN+ &lt; VIN-).
      Below,
      --  Input voltage is above the reference threshold (VIN+ &gt; VIN-).
      Above)
     with Size => 1;
   for RESULT_RESULT_Field use
     (Below => 0,
      Above => 1);

   --  Compare result
   type RESULT_Register is record
      --  Read-only. Result of last compare. Decision point SAMPLE task.
      RESULT        : RESULT_RESULT_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESULT_Register use record
      RESULT        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Enable or disable LPCOMP
   type ENABLE_ENABLE_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 2;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable LPCOMP
   type ENABLE_Register is record
      --  Enable or disable LPCOMP
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.LPCOMP.Disabled;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Analog pin select
   type PSEL_PSEL_Field is
     (--  AIN0 selected as analog input
      Analoginput0,
      --  AIN1 selected as analog input
      Analoginput1,
      --  AIN2 selected as analog input
      Analoginput2,
      --  AIN3 selected as analog input
      Analoginput3,
      --  AIN4 selected as analog input
      Analoginput4,
      --  AIN5 selected as analog input
      Analoginput5,
      --  AIN6 selected as analog input
      Analoginput6,
      --  AIN7 selected as analog input
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

   --  Input pin select
   type PSEL_Register is record
      --  Analog pin select
      PSEL          : PSEL_PSEL_Field := NRF_SVD.LPCOMP.Analoginput0;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSEL_Register use record
      PSEL          at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Reference select
   type REFSEL_REFSEL_Field is
     (--  VDD * 1/8 selected as reference
      Ref1_8Vdd,
      --  VDD * 2/8 selected as reference
      Ref2_8Vdd,
      --  VDD * 3/8 selected as reference
      Ref3_8Vdd,
      --  VDD * 4/8 selected as reference
      Ref4_8Vdd,
      --  VDD * 5/8 selected as reference
      Ref5_8Vdd,
      --  VDD * 6/8 selected as reference
      Ref6_8Vdd,
      --  VDD * 7/8 selected as reference
      Ref7_8Vdd,
      --  External analog reference selected
      Aref,
      --  VDD * 1/16 selected as reference
      Ref1_16Vdd,
      --  VDD * 3/16 selected as reference
      Ref3_16Vdd,
      --  VDD * 5/16 selected as reference
      Ref5_16Vdd,
      --  VDD * 7/16 selected as reference
      Ref7_16Vdd,
      --  VDD * 9/16 selected as reference
      Ref9_16Vdd,
      --  VDD * 11/16 selected as reference
      Ref11_16Vdd,
      --  VDD * 13/16 selected as reference
      Ref13_16Vdd,
      --  VDD * 15/16 selected as reference
      Ref15_16Vdd)
     with Size => 4;
   for REFSEL_REFSEL_Field use
     (Ref1_8Vdd => 0,
      Ref2_8Vdd => 1,
      Ref3_8Vdd => 2,
      Ref4_8Vdd => 3,
      Ref5_8Vdd => 4,
      Ref6_8Vdd => 5,
      Ref7_8Vdd => 6,
      Aref => 7,
      Ref1_16Vdd => 8,
      Ref3_16Vdd => 9,
      Ref5_16Vdd => 10,
      Ref7_16Vdd => 11,
      Ref9_16Vdd => 12,
      Ref11_16Vdd => 13,
      Ref13_16Vdd => 14,
      Ref15_16Vdd => 15);

   --  Reference select
   type REFSEL_Register is record
      --  Reference select
      REFSEL        : REFSEL_REFSEL_Field := NRF_SVD.LPCOMP.Ref5_8Vdd;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REFSEL_Register use record
      REFSEL        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  External analog reference select
   type EXTREFSEL_EXTREFSEL_Field is
     (--  Use AIN0 as external analog reference
      Analogreference0,
      --  Use AIN1 as external analog reference
      Analogreference1)
     with Size => 1;
   for EXTREFSEL_EXTREFSEL_Field use
     (Analogreference0 => 0,
      Analogreference1 => 1);

   --  External reference select
   type EXTREFSEL_Register is record
      --  External analog reference select
      EXTREFSEL     : EXTREFSEL_EXTREFSEL_Field :=
                       NRF_SVD.LPCOMP.Analogreference0;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTREFSEL_Register use record
      EXTREFSEL     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Analog detect configuration
   type ANADETECT_ANADETECT_Field is
     (--  Generate ANADETECT on crossing, both upward crossing and downward crossing
      Cross,
      --  Generate ANADETECT on upward crossing only
      Up,
      --  Generate ANADETECT on downward crossing only
      Down)
     with Size => 2;
   for ANADETECT_ANADETECT_Field use
     (Cross => 0,
      Up => 1,
      Down => 2);

   --  Analog detect configuration
   type ANADETECT_Register is record
      --  Analog detect configuration
      ANADETECT     : ANADETECT_ANADETECT_Field := NRF_SVD.LPCOMP.Cross;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ANADETECT_Register use record
      ANADETECT     at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Comparator hysteresis enable
   type HYST_HYST_Field is
     (--  Comparator hysteresis disabled
      Nohyst,
      --  Comparator hysteresis disabled (typ. 50 mV)
      Hyst50MV)
     with Size => 1;
   for HYST_HYST_Field use
     (Nohyst => 0,
      Hyst50MV => 1);

   --  Comparator hysteresis enable
   type HYST_Register is record
      --  Comparator hysteresis enable
      HYST          : HYST_HYST_Field := NRF_SVD.LPCOMP.Nohyst;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HYST_Register use record
      HYST          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Low Power Comparator
   type LPCOMP_Peripheral is record
      --  Start comparator
      TASKS_START  : aliased HAL.UInt32;
      --  Stop comparator
      TASKS_STOP   : aliased HAL.UInt32;
      --  Sample comparator value
      TASKS_SAMPLE : aliased HAL.UInt32;
      --  LPCOMP is ready and output is valid
      EVENTS_READY : aliased HAL.UInt32;
      --  Downward crossing
      EVENTS_DOWN  : aliased HAL.UInt32;
      --  Upward crossing
      EVENTS_UP    : aliased HAL.UInt32;
      --  Downward or upward crossing
      EVENTS_CROSS : aliased HAL.UInt32;
      --  Shortcut register
      SHORTS       : aliased SHORTS_Register;
      --  Enable interrupt
      INTENSET     : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR     : aliased INTENCLR_Register;
      --  Compare result
      RESULT       : aliased RESULT_Register;
      --  Enable LPCOMP
      ENABLE       : aliased ENABLE_Register;
      --  Input pin select
      PSEL         : aliased PSEL_Register;
      --  Reference select
      REFSEL       : aliased REFSEL_Register;
      --  External reference select
      EXTREFSEL    : aliased EXTREFSEL_Register;
      --  Analog detect configuration
      ANADETECT    : aliased ANADETECT_Register;
      --  Comparator hysteresis enable
      HYST         : aliased HYST_Register;
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
      HYST         at 16#538# range 0 .. 31;
   end record;

   --  Low Power Comparator
   LPCOMP_Periph : aliased LPCOMP_Peripheral
     with Import, Address => LPCOMP_Base;

end NRF_SVD.LPCOMP;
