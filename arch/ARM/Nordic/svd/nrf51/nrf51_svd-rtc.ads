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

package NRF51_SVD.RTC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Compare event on CC[n] match.

   --  Compare event on CC[n] match.
   type EVENTS_COMPARE_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Enable interrupt on TICK event.
   type INTENSET_TICK_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on TICK event.
   type INTENSET_TICK_Field_1 is
     (
      --  Reset value for the field
      Intenset_Tick_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_TICK_Field_1 use
     (Intenset_Tick_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on OVRFLW event.
   type INTENSET_OVRFLW_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on OVRFLW event.
   type INTENSET_OVRFLW_Field_1 is
     (
      --  Reset value for the field
      Intenset_Ovrflw_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_OVRFLW_Field_1 use
     (Intenset_Ovrflw_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on COMPARE[0] event.
   type INTENSET_COMPARE0_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_COMPARE0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on COMPARE[0] event.
   type INTENSET_COMPARE0_Field_1 is
     (
      --  Reset value for the field
      Intenset_Compare0_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_COMPARE0_Field_1 use
     (Intenset_Compare0_Field_Reset => 0,
      Set => 1);

   --  INTENSET_COMPARE array
   type INTENSET_COMPARE_Field_Array is array (0 .. 3)
     of INTENSET_COMPARE0_Field_1
     with Component_Size => 1, Size => 4;

   --  Type definition for INTENSET_COMPARE
   type INTENSET_COMPARE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COMPARE as a value
            Val : HAL.UInt4;
         when True =>
            --  COMPARE as an array
            Arr : INTENSET_COMPARE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for INTENSET_COMPARE_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on TICK event.
      TICK           : INTENSET_TICK_Field_1 := Intenset_Tick_Field_Reset;
      --  Enable interrupt on OVRFLW event.
      OVRFLW         : INTENSET_OVRFLW_Field_1 := Intenset_Ovrflw_Field_Reset;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Enable interrupt on COMPARE[0] event.
      COMPARE        : INTENSET_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Disable interrupt on TICK event.
   type INTENCLR_TICK_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on TICK event.
   type INTENCLR_TICK_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Tick_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_TICK_Field_1 use
     (Intenclr_Tick_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on OVRFLW event.
   type INTENCLR_OVRFLW_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on OVRFLW event.
   type INTENCLR_OVRFLW_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Ovrflw_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_OVRFLW_Field_1 use
     (Intenclr_Ovrflw_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on COMPARE[0] event.
   type INTENCLR_COMPARE0_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_COMPARE0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on COMPARE[0] event.
   type INTENCLR_COMPARE0_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Compare0_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_COMPARE0_Field_1 use
     (Intenclr_Compare0_Field_Reset => 0,
      Clear => 1);

   --  INTENCLR_COMPARE array
   type INTENCLR_COMPARE_Field_Array is array (0 .. 3)
     of INTENCLR_COMPARE0_Field_1
     with Component_Size => 1, Size => 4;

   --  Type definition for INTENCLR_COMPARE
   type INTENCLR_COMPARE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COMPARE as a value
            Val : HAL.UInt4;
         when True =>
            --  COMPARE as an array
            Arr : INTENCLR_COMPARE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for INTENCLR_COMPARE_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on TICK event.
      TICK           : INTENCLR_TICK_Field_1 := Intenclr_Tick_Field_Reset;
      --  Disable interrupt on OVRFLW event.
      OVRFLW         : INTENCLR_OVRFLW_Field_1 := Intenclr_Ovrflw_Field_Reset;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Disable interrupt on COMPARE[0] event.
      COMPARE        : INTENCLR_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  TICK event enable.
   type EVTEN_TICK_Field is
     (
      --  Event disabled.
      Disabled,
      --  Event enabled.
      Enabled)
     with Size => 1;
   for EVTEN_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  OVRFLW event enable.
   type EVTEN_OVRFLW_Field is
     (
      --  Event disabled.
      Disabled,
      --  Event enabled.
      Enabled)
     with Size => 1;
   for EVTEN_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  COMPARE[0] event enable.
   type EVTEN_COMPARE0_Field is
     (
      --  Event disabled.
      Disabled,
      --  Event enabled.
      Enabled)
     with Size => 1;
   for EVTEN_COMPARE0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  EVTEN_COMPARE array
   type EVTEN_COMPARE_Field_Array is array (0 .. 3) of EVTEN_COMPARE0_Field
     with Component_Size => 1, Size => 4;

   --  Type definition for EVTEN_COMPARE
   type EVTEN_COMPARE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COMPARE as a value
            Val : HAL.UInt4;
         when True =>
            --  COMPARE as an array
            Arr : EVTEN_COMPARE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for EVTEN_COMPARE_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Configures event enable routing to PPI for each RTC event.
   type EVTEN_Register is record
      --  TICK event enable.
      TICK           : EVTEN_TICK_Field := NRF51_SVD.RTC.Disabled;
      --  OVRFLW event enable.
      OVRFLW         : EVTEN_OVRFLW_Field := NRF51_SVD.RTC.Disabled;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  COMPARE[0] event enable.
      COMPARE        : EVTEN_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVTEN_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Enable routing to PPI of TICK event.
   type EVTENSET_TICK_Field is
     (
      --  Event disabled.
      Disabled,
      --  Event enabled.
      Enabled)
     with Size => 1;
   for EVTENSET_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable routing to PPI of TICK event.
   type EVTENSET_TICK_Field_1 is
     (
      --  Reset value for the field
      Evtenset_Tick_Field_Reset,
      --  Enable event on write.
      Set)
     with Size => 1;
   for EVTENSET_TICK_Field_1 use
     (Evtenset_Tick_Field_Reset => 0,
      Set => 1);

   --  Enable routing to PPI of OVRFLW event.
   type EVTENSET_OVRFLW_Field is
     (
      --  Event disabled.
      Disabled,
      --  Event enabled.
      Enabled)
     with Size => 1;
   for EVTENSET_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable routing to PPI of OVRFLW event.
   type EVTENSET_OVRFLW_Field_1 is
     (
      --  Reset value for the field
      Evtenset_Ovrflw_Field_Reset,
      --  Enable event on write.
      Set)
     with Size => 1;
   for EVTENSET_OVRFLW_Field_1 use
     (Evtenset_Ovrflw_Field_Reset => 0,
      Set => 1);

   --  Enable routing to PPI of COMPARE[0] event.
   type EVTENSET_COMPARE0_Field is
     (
      --  Event disabled.
      Disabled,
      --  Event enabled.
      Enabled)
     with Size => 1;
   for EVTENSET_COMPARE0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable routing to PPI of COMPARE[0] event.
   type EVTENSET_COMPARE0_Field_1 is
     (
      --  Reset value for the field
      Evtenset_Compare0_Field_Reset,
      --  Enable event on write.
      Set)
     with Size => 1;
   for EVTENSET_COMPARE0_Field_1 use
     (Evtenset_Compare0_Field_Reset => 0,
      Set => 1);

   --  EVTENSET_COMPARE array
   type EVTENSET_COMPARE_Field_Array is array (0 .. 3)
     of EVTENSET_COMPARE0_Field_1
     with Component_Size => 1, Size => 4;

   --  Type definition for EVTENSET_COMPARE
   type EVTENSET_COMPARE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COMPARE as a value
            Val : HAL.UInt4;
         when True =>
            --  COMPARE as an array
            Arr : EVTENSET_COMPARE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for EVTENSET_COMPARE_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Enable events routing to PPI. The reading of this register gives the
   --  value of EVTEN.
   type EVTENSET_Register is record
      --  Enable routing to PPI of TICK event.
      TICK           : EVTENSET_TICK_Field_1 := Evtenset_Tick_Field_Reset;
      --  Enable routing to PPI of OVRFLW event.
      OVRFLW         : EVTENSET_OVRFLW_Field_1 := Evtenset_Ovrflw_Field_Reset;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Enable routing to PPI of COMPARE[0] event.
      COMPARE        : EVTENSET_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVTENSET_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Disable routing to PPI of TICK event.
   type EVTENCLR_TICK_Field is
     (
      --  Event disabled.
      Disabled,
      --  Event enabled.
      Enabled)
     with Size => 1;
   for EVTENCLR_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable routing to PPI of TICK event.
   type EVTENCLR_TICK_Field_1 is
     (
      --  Reset value for the field
      Evtenclr_Tick_Field_Reset,
      --  Disable event on write.
      Clear)
     with Size => 1;
   for EVTENCLR_TICK_Field_1 use
     (Evtenclr_Tick_Field_Reset => 0,
      Clear => 1);

   --  Disable routing to PPI of OVRFLW event.
   type EVTENCLR_OVRFLW_Field is
     (
      --  Event disabled.
      Disabled,
      --  Event enabled.
      Enabled)
     with Size => 1;
   for EVTENCLR_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable routing to PPI of OVRFLW event.
   type EVTENCLR_OVRFLW_Field_1 is
     (
      --  Reset value for the field
      Evtenclr_Ovrflw_Field_Reset,
      --  Disable event on write.
      Clear)
     with Size => 1;
   for EVTENCLR_OVRFLW_Field_1 use
     (Evtenclr_Ovrflw_Field_Reset => 0,
      Clear => 1);

   --  Disable routing to PPI of COMPARE[0] event.
   type EVTENCLR_COMPARE0_Field is
     (
      --  Event disabled.
      Disabled,
      --  Event enabled.
      Enabled)
     with Size => 1;
   for EVTENCLR_COMPARE0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable routing to PPI of COMPARE[0] event.
   type EVTENCLR_COMPARE0_Field_1 is
     (
      --  Reset value for the field
      Evtenclr_Compare0_Field_Reset,
      --  Disable event on write.
      Clear)
     with Size => 1;
   for EVTENCLR_COMPARE0_Field_1 use
     (Evtenclr_Compare0_Field_Reset => 0,
      Clear => 1);

   --  EVTENCLR_COMPARE array
   type EVTENCLR_COMPARE_Field_Array is array (0 .. 3)
     of EVTENCLR_COMPARE0_Field_1
     with Component_Size => 1, Size => 4;

   --  Type definition for EVTENCLR_COMPARE
   type EVTENCLR_COMPARE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COMPARE as a value
            Val : HAL.UInt4;
         when True =>
            --  COMPARE as an array
            Arr : EVTENCLR_COMPARE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for EVTENCLR_COMPARE_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Disable events routing to PPI. The reading of this register gives the
   --  value of EVTEN.
   type EVTENCLR_Register is record
      --  Disable routing to PPI of TICK event.
      TICK           : EVTENCLR_TICK_Field_1 := Evtenclr_Tick_Field_Reset;
      --  Disable routing to PPI of OVRFLW event.
      OVRFLW         : EVTENCLR_OVRFLW_Field_1 := Evtenclr_Ovrflw_Field_Reset;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Disable routing to PPI of COMPARE[0] event.
      COMPARE        : EVTENCLR_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVTENCLR_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype COUNTER_COUNTER_Field is HAL.UInt24;

   --  Current COUNTER value.
   type COUNTER_Register is record
      --  Read-only. Counter value.
      COUNTER        : COUNTER_COUNTER_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNTER_Register use record
      COUNTER        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PRESCALER_PRESCALER_Field is HAL.UInt12;

   --  12-bit prescaler for COUNTER frequency (32768/(PRESCALER+1)). Must be
   --  written when RTC is STOPed.
   type PRESCALER_Register is record
      --  RTC PRESCALER value.
      PRESCALER      : PRESCALER_PRESCALER_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRESCALER_Register use record
      PRESCALER      at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype CC_COMPARE_Field is HAL.UInt24;

   --  Capture/compare registers.
   type CC_Register is record
      --  Compare value.
      COMPARE        : CC_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_Register use record
      COMPARE        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Capture/compare registers.
   type CC_Registers is array (0 .. 3) of CC_Register
     with Volatile;

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
      POWER         : POWER_POWER_Field := NRF51_SVD.RTC.Disabled;
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

   --  Real time counter 0.
   type RTC_Peripheral is record
      --  Start RTC Counter.
      TASKS_START      : aliased HAL.UInt32;
      --  Stop RTC Counter.
      TASKS_STOP       : aliased HAL.UInt32;
      --  Clear RTC Counter.
      TASKS_CLEAR      : aliased HAL.UInt32;
      --  Set COUNTER to 0xFFFFFFF0.
      TASKS_TRIGOVRFLW : aliased HAL.UInt32;
      --  Event on COUNTER increment.
      EVENTS_TICK      : aliased HAL.UInt32;
      --  Event on COUNTER overflow.
      EVENTS_OVRFLW    : aliased HAL.UInt32;
      --  Compare event on CC[n] match.
      EVENTS_COMPARE   : aliased EVENTS_COMPARE_Registers;
      --  Interrupt enable set register.
      INTENSET         : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR         : aliased INTENCLR_Register;
      --  Configures event enable routing to PPI for each RTC event.
      EVTEN            : aliased EVTEN_Register;
      --  Enable events routing to PPI. The reading of this register gives the
      --  value of EVTEN.
      EVTENSET         : aliased EVTENSET_Register;
      --  Disable events routing to PPI. The reading of this register gives the
      --  value of EVTEN.
      EVTENCLR         : aliased EVTENCLR_Register;
      --  Current COUNTER value.
      COUNTER          : aliased COUNTER_Register;
      --  12-bit prescaler for COUNTER frequency (32768/(PRESCALER+1)). Must be
      --  written when RTC is STOPed.
      PRESCALER        : aliased PRESCALER_Register;
      --  Capture/compare registers.
      CC               : aliased CC_Registers;
      --  Peripheral power control.
      POWER            : aliased POWER_Register;
   end record
     with Volatile;

   for RTC_Peripheral use record
      TASKS_START      at 16#0# range 0 .. 31;
      TASKS_STOP       at 16#4# range 0 .. 31;
      TASKS_CLEAR      at 16#8# range 0 .. 31;
      TASKS_TRIGOVRFLW at 16#C# range 0 .. 31;
      EVENTS_TICK      at 16#100# range 0 .. 31;
      EVENTS_OVRFLW    at 16#104# range 0 .. 31;
      EVENTS_COMPARE   at 16#140# range 0 .. 127;
      INTENSET         at 16#304# range 0 .. 31;
      INTENCLR         at 16#308# range 0 .. 31;
      EVTEN            at 16#340# range 0 .. 31;
      EVTENSET         at 16#344# range 0 .. 31;
      EVTENCLR         at 16#348# range 0 .. 31;
      COUNTER          at 16#504# range 0 .. 31;
      PRESCALER        at 16#508# range 0 .. 31;
      CC               at 16#540# range 0 .. 127;
      POWER            at 16#FFC# range 0 .. 31;
   end record;

   --  Real time counter 0.
   RTC0_Periph : aliased RTC_Peripheral
     with Import, Address => System'To_Address (16#4000B000#);

   --  Real time counter 1.
   RTC1_Periph : aliased RTC_Peripheral
     with Import, Address => System'To_Address (16#40011000#);

end NRF51_SVD.RTC;
