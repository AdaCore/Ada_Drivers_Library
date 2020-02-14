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

package NRF_SVD.TEMP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Write '1' to Enable interrupt for DATARDY event
   type INTENSET_DATARDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_DATARDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for DATARDY event
   type INTENSET_DATARDY_Field_1 is
     (--  Reset value for the field
      Intenset_Datardy_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_DATARDY_Field_1 use
     (Intenset_Datardy_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for DATARDY event
      DATARDY       : INTENSET_DATARDY_Field_1 :=
                       Intenset_Datardy_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      DATARDY       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Write '1' to Disable interrupt for DATARDY event
   type INTENCLR_DATARDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_DATARDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for DATARDY event
   type INTENCLR_DATARDY_Field_1 is
     (--  Reset value for the field
      Intenclr_Datardy_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_DATARDY_Field_1 use
     (Intenclr_Datardy_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for DATARDY event
      DATARDY       : INTENCLR_DATARDY_Field_1 :=
                       Intenclr_Datardy_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      DATARDY       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype A0_A0_Field is HAL.UInt12;

   --  Slope of 1st piece wise linear function
   type A0_Register is record
      --  Slope of 1st piece wise linear function
      A0             : A0_A0_Field := 16#320#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for A0_Register use record
      A0             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype A1_A1_Field is HAL.UInt12;

   --  Slope of 2nd piece wise linear function
   type A1_Register is record
      --  Slope of 2nd piece wise linear function
      A1             : A1_A1_Field := 16#343#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for A1_Register use record
      A1             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype A2_A2_Field is HAL.UInt12;

   --  Slope of 3rd piece wise linear function
   type A2_Register is record
      --  Slope of 3rd piece wise linear function
      A2             : A2_A2_Field := 16#35D#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for A2_Register use record
      A2             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype A3_A3_Field is HAL.UInt12;

   --  Slope of 4th piece wise linear function
   type A3_Register is record
      --  Slope of 4th piece wise linear function
      A3             : A3_A3_Field := 16#400#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for A3_Register use record
      A3             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype A4_A4_Field is HAL.UInt12;

   --  Slope of 5th piece wise linear function
   type A4_Register is record
      --  Slope of 5th piece wise linear function
      A4             : A4_A4_Field := 16#47F#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for A4_Register use record
      A4             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype A5_A5_Field is HAL.UInt12;

   --  Slope of 6th piece wise linear function
   type A5_Register is record
      --  Slope of 6th piece wise linear function
      A5             : A5_A5_Field := 16#37B#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for A5_Register use record
      A5             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype B0_B0_Field is HAL.UInt14;

   --  y-intercept of 1st piece wise linear function
   type B0_Register is record
      --  y-intercept of 1st piece wise linear function
      B0             : B0_B0_Field := 16#3FCC#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for B0_Register use record
      B0             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype B1_B1_Field is HAL.UInt14;

   --  y-intercept of 2nd piece wise linear function
   type B1_Register is record
      --  y-intercept of 2nd piece wise linear function
      B1             : B1_B1_Field := 16#3F98#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for B1_Register use record
      B1             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype B2_B2_Field is HAL.UInt14;

   --  y-intercept of 3rd piece wise linear function
   type B2_Register is record
      --  y-intercept of 3rd piece wise linear function
      B2             : B2_B2_Field := 16#3F98#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for B2_Register use record
      B2             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype B3_B3_Field is HAL.UInt14;

   --  y-intercept of 4th piece wise linear function
   type B3_Register is record
      --  y-intercept of 4th piece wise linear function
      B3             : B3_B3_Field := 16#12#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for B3_Register use record
      B3             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype B4_B4_Field is HAL.UInt14;

   --  y-intercept of 5th piece wise linear function
   type B4_Register is record
      --  y-intercept of 5th piece wise linear function
      B4             : B4_B4_Field := 16#6A#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for B4_Register use record
      B4             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype B5_B5_Field is HAL.UInt14;

   --  y-intercept of 6th piece wise linear function
   type B5_Register is record
      --  y-intercept of 6th piece wise linear function
      B5             : B5_B5_Field := 16#3DD0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for B5_Register use record
      B5             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype T0_T0_Field is HAL.UInt8;

   --  End point of 1st piece wise linear function
   type T0_Register is record
      --  End point of 1st piece wise linear function
      T0            : T0_T0_Field := 16#E2#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for T0_Register use record
      T0            at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype T1_T1_Field is HAL.UInt8;

   --  End point of 2nd piece wise linear function
   type T1_Register is record
      --  End point of 2nd piece wise linear function
      T1            : T1_T1_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for T1_Register use record
      T1            at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype T2_T2_Field is HAL.UInt8;

   --  End point of 3rd piece wise linear function
   type T2_Register is record
      --  End point of 3rd piece wise linear function
      T2            : T2_T2_Field := 16#14#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for T2_Register use record
      T2            at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype T3_T3_Field is HAL.UInt8;

   --  End point of 4th piece wise linear function
   type T3_Register is record
      --  End point of 4th piece wise linear function
      T3            : T3_T3_Field := 16#19#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for T3_Register use record
      T3            at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype T4_T4_Field is HAL.UInt8;

   --  End point of 5th piece wise linear function
   type T4_Register is record
      --  End point of 5th piece wise linear function
      T4            : T4_T4_Field := 16#50#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for T4_Register use record
      T4            at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Temperature Sensor
   type TEMP_Peripheral is record
      --  Start temperature measurement
      TASKS_START    : aliased HAL.UInt32;
      --  Stop temperature measurement
      TASKS_STOP     : aliased HAL.UInt32;
      --  Temperature measurement complete, data ready
      EVENTS_DATARDY : aliased HAL.UInt32;
      --  Enable interrupt
      INTENSET       : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR       : aliased INTENCLR_Register;
      --  Temperature in degC (0.25deg steps)
      TEMP           : aliased HAL.UInt32;
      --  Slope of 1st piece wise linear function
      A0             : aliased A0_Register;
      --  Slope of 2nd piece wise linear function
      A1             : aliased A1_Register;
      --  Slope of 3rd piece wise linear function
      A2             : aliased A2_Register;
      --  Slope of 4th piece wise linear function
      A3             : aliased A3_Register;
      --  Slope of 5th piece wise linear function
      A4             : aliased A4_Register;
      --  Slope of 6th piece wise linear function
      A5             : aliased A5_Register;
      --  y-intercept of 1st piece wise linear function
      B0             : aliased B0_Register;
      --  y-intercept of 2nd piece wise linear function
      B1             : aliased B1_Register;
      --  y-intercept of 3rd piece wise linear function
      B2             : aliased B2_Register;
      --  y-intercept of 4th piece wise linear function
      B3             : aliased B3_Register;
      --  y-intercept of 5th piece wise linear function
      B4             : aliased B4_Register;
      --  y-intercept of 6th piece wise linear function
      B5             : aliased B5_Register;
      --  End point of 1st piece wise linear function
      T0             : aliased T0_Register;
      --  End point of 2nd piece wise linear function
      T1             : aliased T1_Register;
      --  End point of 3rd piece wise linear function
      T2             : aliased T2_Register;
      --  End point of 4th piece wise linear function
      T3             : aliased T3_Register;
      --  End point of 5th piece wise linear function
      T4             : aliased T4_Register;
   end record
     with Volatile;

   for TEMP_Peripheral use record
      TASKS_START    at 16#0# range 0 .. 31;
      TASKS_STOP     at 16#4# range 0 .. 31;
      EVENTS_DATARDY at 16#100# range 0 .. 31;
      INTENSET       at 16#304# range 0 .. 31;
      INTENCLR       at 16#308# range 0 .. 31;
      TEMP           at 16#508# range 0 .. 31;
      A0             at 16#520# range 0 .. 31;
      A1             at 16#524# range 0 .. 31;
      A2             at 16#528# range 0 .. 31;
      A3             at 16#52C# range 0 .. 31;
      A4             at 16#530# range 0 .. 31;
      A5             at 16#534# range 0 .. 31;
      B0             at 16#540# range 0 .. 31;
      B1             at 16#544# range 0 .. 31;
      B2             at 16#548# range 0 .. 31;
      B3             at 16#54C# range 0 .. 31;
      B4             at 16#550# range 0 .. 31;
      B5             at 16#554# range 0 .. 31;
      T0             at 16#560# range 0 .. 31;
      T1             at 16#564# range 0 .. 31;
      T2             at 16#568# range 0 .. 31;
      T3             at 16#56C# range 0 .. 31;
      T4             at 16#570# range 0 .. 31;
   end record;

   --  Temperature Sensor
   TEMP_Periph : aliased TEMP_Peripheral
     with Import, Address => TEMP_Base;

end NRF_SVD.TEMP;
