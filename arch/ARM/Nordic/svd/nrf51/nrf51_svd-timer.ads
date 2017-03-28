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

package NRF51_SVD.TIMER is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Capture Timer value to CC[n] registers.

   --  Capture Timer value to CC[n] registers.
   type TASKS_CAPTURE_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Compare event on CC[n] match.

   --  Compare event on CC[n] match.
   type EVENTS_COMPARE_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Shortcut between CC[0] event and the CLEAR task.
   type SHORTS_COMPARE0_CLEAR_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_COMPARE0_CLEAR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between CC[1] event and the CLEAR task.
   type SHORTS_COMPARE1_CLEAR_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_COMPARE1_CLEAR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between CC[2] event and the CLEAR task.
   type SHORTS_COMPARE2_CLEAR_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_COMPARE2_CLEAR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between CC[3] event and the CLEAR task.
   type SHORTS_COMPARE3_CLEAR_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_COMPARE3_CLEAR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between CC[0] event and the STOP task.
   type SHORTS_COMPARE0_STOP_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_COMPARE0_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between CC[1] event and the STOP task.
   type SHORTS_COMPARE1_STOP_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_COMPARE1_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between CC[2] event and the STOP task.
   type SHORTS_COMPARE2_STOP_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_COMPARE2_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between CC[3] event and the STOP task.
   type SHORTS_COMPARE3_STOP_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_COMPARE3_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcuts for Timer.
   type SHORTS_Register is record
      --  Shortcut between CC[0] event and the CLEAR task.
      COMPARE0_CLEAR : SHORTS_COMPARE0_CLEAR_Field :=
                        NRF51_SVD.TIMER.Disabled;
      --  Shortcut between CC[1] event and the CLEAR task.
      COMPARE1_CLEAR : SHORTS_COMPARE1_CLEAR_Field :=
                        NRF51_SVD.TIMER.Disabled;
      --  Shortcut between CC[2] event and the CLEAR task.
      COMPARE2_CLEAR : SHORTS_COMPARE2_CLEAR_Field :=
                        NRF51_SVD.TIMER.Disabled;
      --  Shortcut between CC[3] event and the CLEAR task.
      COMPARE3_CLEAR : SHORTS_COMPARE3_CLEAR_Field :=
                        NRF51_SVD.TIMER.Disabled;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Shortcut between CC[0] event and the STOP task.
      COMPARE0_STOP  : SHORTS_COMPARE0_STOP_Field := NRF51_SVD.TIMER.Disabled;
      --  Shortcut between CC[1] event and the STOP task.
      COMPARE1_STOP  : SHORTS_COMPARE1_STOP_Field := NRF51_SVD.TIMER.Disabled;
      --  Shortcut between CC[2] event and the STOP task.
      COMPARE2_STOP  : SHORTS_COMPARE2_STOP_Field := NRF51_SVD.TIMER.Disabled;
      --  Shortcut between CC[3] event and the STOP task.
      COMPARE3_STOP  : SHORTS_COMPARE3_STOP_Field := NRF51_SVD.TIMER.Disabled;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      COMPARE0_CLEAR at 0 range 0 .. 0;
      COMPARE1_CLEAR at 0 range 1 .. 1;
      COMPARE2_CLEAR at 0 range 2 .. 2;
      COMPARE3_CLEAR at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      COMPARE0_STOP  at 0 range 8 .. 8;
      COMPARE1_STOP  at 0 range 9 .. 9;
      COMPARE2_STOP  at 0 range 10 .. 10;
      COMPARE3_STOP  at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Enable interrupt on COMPARE[0]
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

   --  Enable interrupt on COMPARE[0]
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
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  Enable interrupt on COMPARE[0]
      COMPARE        : INTENSET_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Disable interrupt on COMPARE[0]
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

   --  Disable interrupt on COMPARE[0]
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
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  Disable interrupt on COMPARE[0]
      COMPARE        : INTENCLR_COMPARE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      COMPARE        at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Select Normal or Counter mode.
   type MODE_MODE_Field is
     (
      --  Timer in Normal mode.
      Timer,
      --  Timer in Counter mode.
      Counter)
     with Size => 1;
   for MODE_MODE_Field use
     (Timer => 0,
      Counter => 1);

   --  Timer Mode selection.
   type MODE_Register is record
      --  Select Normal or Counter mode.
      MODE          : MODE_MODE_Field := NRF51_SVD.TIMER.Timer;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODE_Register use record
      MODE          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Sets timer behaviour ro be like the implementation of a timer with width
   --  as indicated.
   type BITMODE_BITMODE_Field is
     (
      --  16-bit timer behaviour.
      BITMODE_BITMODE_Field_16Bit,
      --  8-bit timer behaviour.
      BITMODE_BITMODE_Field_08Bit,
      --  24-bit timer behaviour.
      BITMODE_BITMODE_Field_24Bit,
      --  32-bit timer behaviour.
      BITMODE_BITMODE_Field_32Bit)
     with Size => 2;
   for BITMODE_BITMODE_Field use
     (BITMODE_BITMODE_Field_16Bit => 0,
      BITMODE_BITMODE_Field_08Bit => 1,
      BITMODE_BITMODE_Field_24Bit => 2,
      BITMODE_BITMODE_Field_32Bit => 3);

   --  Sets timer behaviour.
   type BITMODE_Register is record
      --  Sets timer behaviour ro be like the implementation of a timer with
      --  width as indicated.
      BITMODE       : BITMODE_BITMODE_Field :=
                       NRF51_SVD.TIMER.BITMODE_BITMODE_Field_16Bit;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BITMODE_Register use record
      BITMODE       at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype PRESCALER_PRESCALER_Field is HAL.UInt4;

   --  4-bit prescaler to source clock frequency (max value 9). Source clock
   --  frequency is divided by 2^SCALE.
   type PRESCALER_Register is record
      --  Timer PRESCALER value. Max value is 9.
      PRESCALER     : PRESCALER_PRESCALER_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRESCALER_Register use record
      PRESCALER     at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Capture/compare registers.

   --  Capture/compare registers.
   type CC_Registers is array (0 .. 3) of HAL.UInt32
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
      POWER         : POWER_POWER_Field := NRF51_SVD.TIMER.Disabled;
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

   --  Timer 0.
   type TIMER_Peripheral is record
      --  Start Timer.
      TASKS_START    : aliased HAL.UInt32;
      --  Stop Timer.
      TASKS_STOP     : aliased HAL.UInt32;
      --  Increment Timer (In counter mode).
      TASKS_COUNT    : aliased HAL.UInt32;
      --  Clear timer.
      TASKS_CLEAR    : aliased HAL.UInt32;
      --  Shutdown timer.
      TASKS_SHUTDOWN : aliased HAL.UInt32;
      --  Capture Timer value to CC[n] registers.
      TASKS_CAPTURE  : aliased TASKS_CAPTURE_Registers;
      --  Compare event on CC[n] match.
      EVENTS_COMPARE : aliased EVENTS_COMPARE_Registers;
      --  Shortcuts for Timer.
      SHORTS         : aliased SHORTS_Register;
      --  Interrupt enable set register.
      INTENSET       : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR       : aliased INTENCLR_Register;
      --  Timer Mode selection.
      MODE           : aliased MODE_Register;
      --  Sets timer behaviour.
      BITMODE        : aliased BITMODE_Register;
      --  4-bit prescaler to source clock frequency (max value 9). Source clock
      --  frequency is divided by 2^SCALE.
      PRESCALER      : aliased PRESCALER_Register;
      --  Capture/compare registers.
      CC             : aliased CC_Registers;
      --  Peripheral power control.
      POWER          : aliased POWER_Register;
   end record
     with Volatile;

   for TIMER_Peripheral use record
      TASKS_START    at 16#0# range 0 .. 31;
      TASKS_STOP     at 16#4# range 0 .. 31;
      TASKS_COUNT    at 16#8# range 0 .. 31;
      TASKS_CLEAR    at 16#C# range 0 .. 31;
      TASKS_SHUTDOWN at 16#10# range 0 .. 31;
      TASKS_CAPTURE  at 16#40# range 0 .. 127;
      EVENTS_COMPARE at 16#140# range 0 .. 127;
      SHORTS         at 16#200# range 0 .. 31;
      INTENSET       at 16#304# range 0 .. 31;
      INTENCLR       at 16#308# range 0 .. 31;
      MODE           at 16#504# range 0 .. 31;
      BITMODE        at 16#508# range 0 .. 31;
      PRESCALER      at 16#510# range 0 .. 31;
      CC             at 16#540# range 0 .. 127;
      POWER          at 16#FFC# range 0 .. 31;
   end record;

   --  Timer 0.
   TIMER0_Periph : aliased TIMER_Peripheral
     with Import, Address => System'To_Address (16#40008000#);

   --  Timer 1.
   TIMER1_Periph : aliased TIMER_Peripheral
     with Import, Address => System'To_Address (16#40009000#);

   --  Timer 2.
   TIMER2_Periph : aliased TIMER_Peripheral
     with Import, Address => System'To_Address (16#4000A000#);

end NRF51_SVD.TIMER;
