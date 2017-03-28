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

package NRF51_SVD.GPIOTE is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Tasks asssociated with GPIOTE channels.

   --  Tasks asssociated with GPIOTE channels.
   type TASKS_OUT_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Tasks asssociated with GPIOTE channels.

   --  Tasks asssociated with GPIOTE channels.
   type EVENTS_IN_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Enable interrupt on IN[0] event.
   type INTENSET_IN0_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_IN0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on IN[0] event.
   type INTENSET_IN0_Field_1 is
     (
      --  Reset value for the field
      Intenset_In0_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_IN0_Field_1 use
     (Intenset_In0_Field_Reset => 0,
      Set => 1);

   --  INTENSET_IN array
   type INTENSET_IN_Field_Array is array (0 .. 3) of INTENSET_IN0_Field_1
     with Component_Size => 1, Size => 4;

   --  Type definition for INTENSET_IN
   type INTENSET_IN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IN as a value
            Val : HAL.UInt4;
         when True =>
            --  IN as an array
            Arr : INTENSET_IN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for INTENSET_IN_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Enable interrupt on PORT event.
   type INTENSET_PORT_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_PORT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on PORT event.
   type INTENSET_PORT_Field_1 is
     (
      --  Reset value for the field
      Intenset_Port_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_PORT_Field_1 use
     (Intenset_Port_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on IN[0] event.
      IN_k          : INTENSET_IN_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_30 : HAL.UInt27 := 16#0#;
      --  Enable interrupt on PORT event.
      PORT          : INTENSET_PORT_Field_1 := Intenset_Port_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      IN_k          at 0 range 0 .. 3;
      Reserved_4_30 at 0 range 4 .. 30;
      PORT          at 0 range 31 .. 31;
   end record;

   --  Disable interrupt on IN[0] event.
   type INTENCLR_IN0_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_IN0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on IN[0] event.
   type INTENCLR_IN0_Field_1 is
     (
      --  Reset value for the field
      Intenclr_In0_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_IN0_Field_1 use
     (Intenclr_In0_Field_Reset => 0,
      Clear => 1);

   --  INTENCLR_IN array
   type INTENCLR_IN_Field_Array is array (0 .. 3) of INTENCLR_IN0_Field_1
     with Component_Size => 1, Size => 4;

   --  Type definition for INTENCLR_IN
   type INTENCLR_IN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IN as a value
            Val : HAL.UInt4;
         when True =>
            --  IN as an array
            Arr : INTENCLR_IN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for INTENCLR_IN_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Disable interrupt on PORT event.
   type INTENCLR_PORT_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_PORT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on PORT event.
   type INTENCLR_PORT_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Port_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_PORT_Field_1 use
     (Intenclr_Port_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on IN[0] event.
      IN_k          : INTENCLR_IN_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_30 : HAL.UInt27 := 16#0#;
      --  Disable interrupt on PORT event.
      PORT          : INTENCLR_PORT_Field_1 := Intenclr_Port_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      IN_k          at 0 range 0 .. 3;
      Reserved_4_30 at 0 range 4 .. 30;
      PORT          at 0 range 31 .. 31;
   end record;

   --  Mode
   type CONFIG_MODE_Field is
     (
      --  Disabled.
      Disabled,
      --  Channel configure in event mode.
      Event,
      --  Channel configure in task mode.
      Task_k)
     with Size => 2;
   for CONFIG_MODE_Field use
     (Disabled => 0,
      Event => 1,
      Task_k => 3);

   subtype CONFIG_PSEL_Field is HAL.UInt5;

   --  Effects on output when in Task mode, or events on input that generates
   --  an event.
   type CONFIG_POLARITY_Field is
     (
      --  No task or event.
      None,
      --  Low to high.
      Lotohi,
      --  High to low.
      Hitolo,
      --  Toggle.
      Toggle)
     with Size => 2;
   for CONFIG_POLARITY_Field use
     (None => 0,
      Lotohi => 1,
      Hitolo => 2,
      Toggle => 3);

   --  Initial value of the output when the GPIOTE channel is configured as a
   --  Task.
   type CONFIG_OUTINIT_Field is
     (
      --  Initial low output when in task mode.
      Low,
      --  Initial high output when in task mode.
      High)
     with Size => 1;
   for CONFIG_OUTINIT_Field use
     (Low => 0,
      High => 1);

   --  Channel configuration registers.
   type CONFIG_Register is record
      --  Mode
      MODE           : CONFIG_MODE_Field := NRF51_SVD.GPIOTE.Disabled;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Pin select.
      PSEL           : CONFIG_PSEL_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Effects on output when in Task mode, or events on input that
      --  generates an event.
      POLARITY       : CONFIG_POLARITY_Field := NRF51_SVD.GPIOTE.None;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  Initial value of the output when the GPIOTE channel is configured as
      --  a Task.
      OUTINIT        : CONFIG_OUTINIT_Field := NRF51_SVD.GPIOTE.Low;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      MODE           at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      PSEL           at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      POLARITY       at 0 range 16 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      OUTINIT        at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --  Channel configuration registers.
   type CONFIG_Registers is array (0 .. 3) of CONFIG_Register
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
      POWER         : POWER_POWER_Field := NRF51_SVD.GPIOTE.Disabled;
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

   --  GPIO tasks and events.
   type GPIOTE_Peripheral is record
      --  Tasks asssociated with GPIOTE channels.
      TASKS_OUT   : aliased TASKS_OUT_Registers;
      --  Tasks asssociated with GPIOTE channels.
      EVENTS_IN   : aliased EVENTS_IN_Registers;
      --  Event generated from multiple pins.
      EVENTS_PORT : aliased HAL.UInt32;
      --  Interrupt enable set register.
      INTENSET    : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR    : aliased INTENCLR_Register;
      --  Channel configuration registers.
      CONFIG      : aliased CONFIG_Registers;
      --  Peripheral power control.
      POWER       : aliased POWER_Register;
   end record
     with Volatile;

   for GPIOTE_Peripheral use record
      TASKS_OUT   at 16#0# range 0 .. 127;
      EVENTS_IN   at 16#100# range 0 .. 127;
      EVENTS_PORT at 16#17C# range 0 .. 31;
      INTENSET    at 16#304# range 0 .. 31;
      INTENCLR    at 16#308# range 0 .. 31;
      CONFIG      at 16#510# range 0 .. 127;
      POWER       at 16#FFC# range 0 .. 31;
   end record;

   --  GPIO tasks and events.
   GPIOTE_Periph : aliased GPIOTE_Peripheral
     with Import, Address => System'To_Address (16#40006000#);

end NRF51_SVD.GPIOTE;
