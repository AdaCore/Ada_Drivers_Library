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

package NRF_SVD.RTC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Description collection[0]: Compare event on CC[0] match

   --  Description collection[0]: Compare event on CC[0] match
   type EVENTS_COMPARE_Registers is array (0 .. 3) of HAL.UInt32;

   --  Write '1' to Enable interrupt for TICK event
   type INTENSET_TICK_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for TICK event
   type INTENSET_TICK_Field_1 is
     (--  Reset value for the field
      Intenset_Tick_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_TICK_Field_1 use
     (Intenset_Tick_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for OVRFLW event
   type INTENSET_OVRFLW_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for OVRFLW event
   type INTENSET_OVRFLW_Field_1 is
     (--  Reset value for the field
      Intenset_Ovrflw_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_OVRFLW_Field_1 use
     (Intenset_Ovrflw_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for COMPARE[0] event
   type INTENSET_COMPARE0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_COMPARE0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for COMPARE[0] event
   type INTENSET_COMPARE0_Field_1 is
     (--  Reset value for the field
      Intenset_Compare0_Field_Reset,
      --  Enable
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

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for TICK event
      TICK           : INTENSET_TICK_Field_1 := Intenset_Tick_Field_Reset;
      --  Write '1' to Enable interrupt for OVRFLW event
      OVRFLW         : INTENSET_OVRFLW_Field_1 := Intenset_Ovrflw_Field_Reset;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Write '1' to Enable interrupt for COMPARE[0] event
      COMPARE        : INTENSET_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Write '1' to Disable interrupt for TICK event
   type INTENCLR_TICK_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for TICK event
   type INTENCLR_TICK_Field_1 is
     (--  Reset value for the field
      Intenclr_Tick_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_TICK_Field_1 use
     (Intenclr_Tick_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for OVRFLW event
   type INTENCLR_OVRFLW_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for OVRFLW event
   type INTENCLR_OVRFLW_Field_1 is
     (--  Reset value for the field
      Intenclr_Ovrflw_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_OVRFLW_Field_1 use
     (Intenclr_Ovrflw_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for COMPARE[0] event
   type INTENCLR_COMPARE0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_COMPARE0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for COMPARE[0] event
   type INTENCLR_COMPARE0_Field_1 is
     (--  Reset value for the field
      Intenclr_Compare0_Field_Reset,
      --  Disable
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

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for TICK event
      TICK           : INTENCLR_TICK_Field_1 := Intenclr_Tick_Field_Reset;
      --  Write '1' to Disable interrupt for OVRFLW event
      OVRFLW         : INTENCLR_OVRFLW_Field_1 := Intenclr_Ovrflw_Field_Reset;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Write '1' to Disable interrupt for COMPARE[0] event
      COMPARE        : INTENCLR_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Enable or disable event routing for TICK event
   type EVTEN_TICK_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for EVTEN_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable event routing for OVRFLW event
   type EVTEN_OVRFLW_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for EVTEN_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable event routing for COMPARE[0] event
   type EVTEN_COMPARE0_Field is
     (--  Disable
      Disabled,
      --  Enable
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

   --  Enable or disable event routing
   type EVTEN_Register is record
      --  Enable or disable event routing for TICK event
      TICK           : EVTEN_TICK_Field := NRF_SVD.RTC.Disabled;
      --  Enable or disable event routing for OVRFLW event
      OVRFLW         : EVTEN_OVRFLW_Field := NRF_SVD.RTC.Disabled;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Enable or disable event routing for COMPARE[0] event
      COMPARE        : EVTEN_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVTEN_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Write '1' to Enable event routing for TICK event
   type EVTENSET_TICK_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for EVTENSET_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable event routing for TICK event
   type EVTENSET_TICK_Field_1 is
     (--  Reset value for the field
      Evtenset_Tick_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for EVTENSET_TICK_Field_1 use
     (Evtenset_Tick_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable event routing for OVRFLW event
   type EVTENSET_OVRFLW_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for EVTENSET_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable event routing for OVRFLW event
   type EVTENSET_OVRFLW_Field_1 is
     (--  Reset value for the field
      Evtenset_Ovrflw_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for EVTENSET_OVRFLW_Field_1 use
     (Evtenset_Ovrflw_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable event routing for COMPARE[0] event
   type EVTENSET_COMPARE0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for EVTENSET_COMPARE0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable event routing for COMPARE[0] event
   type EVTENSET_COMPARE0_Field_1 is
     (--  Reset value for the field
      Evtenset_Compare0_Field_Reset,
      --  Enable
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

   --  Enable event routing
   type EVTENSET_Register is record
      --  Write '1' to Enable event routing for TICK event
      TICK           : EVTENSET_TICK_Field_1 := Evtenset_Tick_Field_Reset;
      --  Write '1' to Enable event routing for OVRFLW event
      OVRFLW         : EVTENSET_OVRFLW_Field_1 := Evtenset_Ovrflw_Field_Reset;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Write '1' to Enable event routing for COMPARE[0] event
      COMPARE        : EVTENSET_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVTENSET_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Write '1' to Disable event routing for TICK event
   type EVTENCLR_TICK_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for EVTENCLR_TICK_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable event routing for TICK event
   type EVTENCLR_TICK_Field_1 is
     (--  Reset value for the field
      Evtenclr_Tick_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for EVTENCLR_TICK_Field_1 use
     (Evtenclr_Tick_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable event routing for OVRFLW event
   type EVTENCLR_OVRFLW_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for EVTENCLR_OVRFLW_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable event routing for OVRFLW event
   type EVTENCLR_OVRFLW_Field_1 is
     (--  Reset value for the field
      Evtenclr_Ovrflw_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for EVTENCLR_OVRFLW_Field_1 use
     (Evtenclr_Ovrflw_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable event routing for COMPARE[0] event
   type EVTENCLR_COMPARE0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for EVTENCLR_COMPARE0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable event routing for COMPARE[0] event
   type EVTENCLR_COMPARE0_Field_1 is
     (--  Reset value for the field
      Evtenclr_Compare0_Field_Reset,
      --  Disable
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

   --  Disable event routing
   type EVTENCLR_Register is record
      --  Write '1' to Disable event routing for TICK event
      TICK           : EVTENCLR_TICK_Field_1 := Evtenclr_Tick_Field_Reset;
      --  Write '1' to Disable event routing for OVRFLW event
      OVRFLW         : EVTENCLR_OVRFLW_Field_1 := Evtenclr_Ovrflw_Field_Reset;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Write '1' to Disable event routing for COMPARE[0] event
      COMPARE        : EVTENCLR_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVTENCLR_Register use record
      TICK           at 0 range 0 .. 0;
      OVRFLW         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype COUNTER_COUNTER_Field is HAL.UInt24;

   --  Current COUNTER value
   type COUNTER_Register is record
      --  Read-only. Counter value
      COUNTER        : COUNTER_COUNTER_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNTER_Register use record
      COUNTER        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype PRESCALER_PRESCALER_Field is HAL.UInt12;

   --  12 bit prescaler for COUNTER frequency (32768/(PRESCALER+1)).Must be
   --  written when RTC is stopped
   type PRESCALER_Register is record
      --  Prescaler value
      PRESCALER      : PRESCALER_PRESCALER_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRESCALER_Register use record
      PRESCALER      at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype CC_COMPARE_Field is HAL.UInt24;

   --  Description collection[0]: Compare register 0
   type CC_Register is record
      --  Compare value
      COMPARE        : CC_COMPARE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CC_Register use record
      COMPARE        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Description collection[0]: Compare register 0
   type CC_Registers is array (0 .. 3) of CC_Register;

   -----------------
   -- Peripherals --
   -----------------

   --  Real time counter 0
   type RTC_Peripheral is record
      --  Start RTC COUNTER
      TASKS_START      : aliased HAL.UInt32;
      --  Stop RTC COUNTER
      TASKS_STOP       : aliased HAL.UInt32;
      --  Clear RTC COUNTER
      TASKS_CLEAR      : aliased HAL.UInt32;
      --  Set COUNTER to 0xFFFFF0
      TASKS_TRIGOVRFLW : aliased HAL.UInt32;
      --  Event on COUNTER increment
      EVENTS_TICK      : aliased HAL.UInt32;
      --  Event on COUNTER overflow
      EVENTS_OVRFLW    : aliased HAL.UInt32;
      --  Description collection[0]: Compare event on CC[0] match
      EVENTS_COMPARE   : aliased EVENTS_COMPARE_Registers;
      --  Enable interrupt
      INTENSET         : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR         : aliased INTENCLR_Register;
      --  Enable or disable event routing
      EVTEN            : aliased EVTEN_Register;
      --  Enable event routing
      EVTENSET         : aliased EVTENSET_Register;
      --  Disable event routing
      EVTENCLR         : aliased EVTENCLR_Register;
      --  Current COUNTER value
      COUNTER          : aliased COUNTER_Register;
      --  12 bit prescaler for COUNTER frequency (32768/(PRESCALER+1)).Must be
      --  written when RTC is stopped
      PRESCALER        : aliased PRESCALER_Register;
      --  Description collection[0]: Compare register 0
      CC               : aliased CC_Registers;
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
   end record;

   --  Real time counter 0
   RTC0_Periph : aliased RTC_Peripheral
     with Import, Address => RTC0_Base;

   --  Real time counter 1
   RTC1_Periph : aliased RTC_Peripheral
     with Import, Address => RTC1_Base;

   --  Real time counter 2
   RTC2_Periph : aliased RTC_Peripheral
     with Import, Address => RTC2_Base;

end NRF_SVD.RTC;
