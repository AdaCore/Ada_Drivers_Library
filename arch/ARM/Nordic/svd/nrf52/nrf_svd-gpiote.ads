--  Copyright (c) 2010 - 2018, Nordic Semiconductor ASA
--
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
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
--  5. Any software provided in binary form under this license must not be
--  reverse engineered, decompiled, modified and/or disassembled.
--
--  THIS SOFTWARE IS PROVIDED BY NORDIC SEMICONDUCTOR ASA "AS IS" AND ANY
--  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL NORDIC SEMICONDUCTOR
--  ASA OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf52.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF_SVD.GPIOTE is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Description collection[0]: Task for writing to pin specified in
   --  CONFIG[0].PSEL. Action on pin is configured in CONFIG[0].POLARITY.
   type TASKS_OUT_Register is record
      --  Write-only.
      TASKS_OUT     : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_OUT_Register use record
      TASKS_OUT     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Description collection[0]: Task for writing to pin specified in
   --  CONFIG[0].PSEL. Action on pin is configured in CONFIG[0].POLARITY.
   type TASKS_OUT_Registers is array (0 .. 7) of TASKS_OUT_Register;

   --  Description collection[0]: Task for writing to pin specified in
   --  CONFIG[0].PSEL. Action on pin is to set it high.
   type TASKS_SET_Register is record
      --  Write-only.
      TASKS_SET     : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_SET_Register use record
      TASKS_SET     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Description collection[0]: Task for writing to pin specified in
   --  CONFIG[0].PSEL. Action on pin is to set it high.
   type TASKS_SET_Registers is array (0 .. 7) of TASKS_SET_Register;

   --  Description collection[0]: Task for writing to pin specified in
   --  CONFIG[0].PSEL. Action on pin is to set it low.
   type TASKS_CLR_Register is record
      --  Write-only.
      TASKS_CLR     : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_CLR_Register use record
      TASKS_CLR     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Description collection[0]: Task for writing to pin specified in
   --  CONFIG[0].PSEL. Action on pin is to set it low.
   type TASKS_CLR_Registers is array (0 .. 7) of TASKS_CLR_Register;

   --  Description collection[0]: Event generated from pin specified in
   --  CONFIG[0].PSEL
   type EVENTS_IN_Register is record
      EVENTS_IN     : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_IN_Register use record
      EVENTS_IN     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Description collection[0]: Event generated from pin specified in
   --  CONFIG[0].PSEL
   type EVENTS_IN_Registers is array (0 .. 7) of EVENTS_IN_Register;

   --  Event generated from multiple input GPIO pins with SENSE mechanism
   --  enabled
   type EVENTS_PORT_Register is record
      EVENTS_PORT   : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_PORT_Register use record
      EVENTS_PORT   at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Write '1' to Enable interrupt for IN[0] event
   type INTENSET_IN0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_IN0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for IN[0] event
   type INTENSET_IN0_Field_1 is
     (--  Reset value for the field
      Intenset_In0_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_IN0_Field_1 use
     (Intenset_In0_Field_Reset => 0,
      Set => 1);

   --  INTENSET_IN array
   type INTENSET_IN_Field_Array is array (0 .. 7) of INTENSET_IN0_Field_1
     with Component_Size => 1, Size => 8;

   --  Type definition for INTENSET_IN
   type INTENSET_IN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IN as a value
            Val : HAL.UInt8;
         when True =>
            --  IN as an array
            Arr : INTENSET_IN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for INTENSET_IN_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Write '1' to Enable interrupt for PORT event
   type INTENSET_PORT_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_PORT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for PORT event
   type INTENSET_PORT_Field_1 is
     (--  Reset value for the field
      Intenset_Port_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_PORT_Field_1 use
     (Intenset_Port_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for IN[0] event
      IN_k          : INTENSET_IN_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_30 : HAL.UInt23 := 16#0#;
      --  Write '1' to Enable interrupt for PORT event
      PORT          : INTENSET_PORT_Field_1 := Intenset_Port_Field_Reset;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      IN_k          at 0 range 0 .. 7;
      Reserved_8_30 at 0 range 8 .. 30;
      PORT          at 0 range 31 .. 31;
   end record;

   --  Write '1' to Disable interrupt for IN[0] event
   type INTENCLR_IN0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_IN0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for IN[0] event
   type INTENCLR_IN0_Field_1 is
     (--  Reset value for the field
      Intenclr_In0_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_IN0_Field_1 use
     (Intenclr_In0_Field_Reset => 0,
      Clear => 1);

   --  INTENCLR_IN array
   type INTENCLR_IN_Field_Array is array (0 .. 7) of INTENCLR_IN0_Field_1
     with Component_Size => 1, Size => 8;

   --  Type definition for INTENCLR_IN
   type INTENCLR_IN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IN as a value
            Val : HAL.UInt8;
         when True =>
            --  IN as an array
            Arr : INTENCLR_IN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for INTENCLR_IN_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   --  Write '1' to Disable interrupt for PORT event
   type INTENCLR_PORT_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_PORT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for PORT event
   type INTENCLR_PORT_Field_1 is
     (--  Reset value for the field
      Intenclr_Port_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_PORT_Field_1 use
     (Intenclr_Port_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for IN[0] event
      IN_k          : INTENCLR_IN_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_30 : HAL.UInt23 := 16#0#;
      --  Write '1' to Disable interrupt for PORT event
      PORT          : INTENCLR_PORT_Field_1 := Intenclr_Port_Field_Reset;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      IN_k          at 0 range 0 .. 7;
      Reserved_8_30 at 0 range 8 .. 30;
      PORT          at 0 range 31 .. 31;
   end record;

   --  Mode
   type CONFIG_MODE_Field is
     (--  Disabled. Pin specified by PSEL will not be acquired by the GPIOTE module.
      Disabled,
      --  Event mode
      Event,
      --  Task mode
      Task_k)
     with Size => 2;
   for CONFIG_MODE_Field use
     (Disabled => 0,
      Event => 1,
      Task_k => 3);

   subtype CONFIG_PSEL_Field is HAL.UInt5;

   --  When In task mode: Operation to be performed on output when OUT[n] task
   --  is triggered. When In event mode: Operation on input that shall trigger
   --  IN[n] event.
   type CONFIG_POLARITY_Field is
     (--  Task mode: No effect on pin from OUT[n] task. Event mode: no IN[n] event
--  generated on pin activity.
      None,
      --  Task mode: Set pin from OUT[n] task. Event mode: Generate IN[n] event when
--  rising edge on pin.
      Lotohi,
      --  Task mode: Clear pin from OUT[n] task. Event mode: Generate IN[n] event
--  when falling edge on pin.
      Hitolo,
      --  Task mode: Toggle pin from OUT[n]. Event mode: Generate IN[n] when any
--  change on pin.
      Toggle)
     with Size => 2;
   for CONFIG_POLARITY_Field use
     (None => 0,
      Lotohi => 1,
      Hitolo => 2,
      Toggle => 3);

   --  When in task mode: Initial value of the output when the GPIOTE channel
   --  is configured. When in event mode: No effect.
   type CONFIG_OUTINIT_Field is
     (--  Task mode: Initial value of pin before task triggering is low
      Low,
      --  Task mode: Initial value of pin before task triggering is high
      High)
     with Size => 1;
   for CONFIG_OUTINIT_Field use
     (Low => 0,
      High => 1);

   --  Description collection[0]: Configuration for OUT[n], SET[n] and CLR[n]
   --  tasks and IN[n] event
   type CONFIG_Register is record
      --  Mode
      MODE           : CONFIG_MODE_Field := NRF_SVD.GPIOTE.Disabled;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  GPIO number associated with SET[n], CLR[n] and OUT[n] tasks and IN[n]
      --  event
      PSEL           : CONFIG_PSEL_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  When In task mode: Operation to be performed on output when OUT[n]
      --  task is triggered. When In event mode: Operation on input that shall
      --  trigger IN[n] event.
      POLARITY       : CONFIG_POLARITY_Field := NRF_SVD.GPIOTE.None;
      --  unspecified
      Reserved_18_19 : HAL.UInt2 := 16#0#;
      --  When in task mode: Initial value of the output when the GPIOTE
      --  channel is configured. When in event mode: No effect.
      OUTINIT        : CONFIG_OUTINIT_Field := NRF_SVD.GPIOTE.Low;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
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

   --  Description collection[0]: Configuration for OUT[n], SET[n] and CLR[n]
   --  tasks and IN[n] event
   type CONFIG_Registers is array (0 .. 7) of CONFIG_Register;

   -----------------
   -- Peripherals --
   -----------------

   --  GPIO Tasks and Events
   type GPIOTE_Peripheral is record
      --  Description collection[0]: Task for writing to pin specified in
      --  CONFIG[0].PSEL. Action on pin is configured in CONFIG[0].POLARITY.
      TASKS_OUT   : aliased TASKS_OUT_Registers;
      --  Description collection[0]: Task for writing to pin specified in
      --  CONFIG[0].PSEL. Action on pin is to set it high.
      TASKS_SET   : aliased TASKS_SET_Registers;
      --  Description collection[0]: Task for writing to pin specified in
      --  CONFIG[0].PSEL. Action on pin is to set it low.
      TASKS_CLR   : aliased TASKS_CLR_Registers;
      --  Description collection[0]: Event generated from pin specified in
      --  CONFIG[0].PSEL
      EVENTS_IN   : aliased EVENTS_IN_Registers;
      --  Event generated from multiple input GPIO pins with SENSE mechanism
      --  enabled
      EVENTS_PORT : aliased EVENTS_PORT_Register;
      --  Enable interrupt
      INTENSET    : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR    : aliased INTENCLR_Register;
      --  Description collection[0]: Configuration for OUT[n], SET[n] and
      --  CLR[n] tasks and IN[n] event
      CONFIG      : aliased CONFIG_Registers;
   end record
     with Volatile;

   for GPIOTE_Peripheral use record
      TASKS_OUT   at 16#0# range 0 .. 255;
      TASKS_SET   at 16#30# range 0 .. 255;
      TASKS_CLR   at 16#60# range 0 .. 255;
      EVENTS_IN   at 16#100# range 0 .. 255;
      EVENTS_PORT at 16#17C# range 0 .. 31;
      INTENSET    at 16#304# range 0 .. 31;
      INTENCLR    at 16#308# range 0 .. 31;
      CONFIG      at 16#510# range 0 .. 255;
   end record;

   --  GPIO Tasks and Events
   GPIOTE_Periph : aliased GPIOTE_Peripheral
     with Import, Address => GPIOTE_Base;

end NRF_SVD.GPIOTE;
