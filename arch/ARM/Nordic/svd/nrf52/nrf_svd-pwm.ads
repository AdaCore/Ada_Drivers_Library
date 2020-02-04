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

package NRF_SVD.PWM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Stops PWM pulse generation on all channels at the end of current PWM
   --  period, and stops sequence playback
   type TASKS_STOP_Register is record
      --  Write-only.
      TASKS_STOP    : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_STOP_Register use record
      TASKS_STOP    at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Description collection[0]: Loads the first PWM value on all enabled
   --  channels from sequence 0, and starts playing that sequence at the rate
   --  defined in SEQ[0]REFRESH and/or DECODER.MODE. Causes PWM generation to
   --  start it was not running.
   type TASKS_SEQSTART_Register is record
      --  Write-only.
      TASKS_SEQSTART : Boolean := False;
      --  unspecified
      Reserved_1_31  : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_SEQSTART_Register use record
      TASKS_SEQSTART at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   --  Description collection[0]: Loads the first PWM value on all enabled
   --  channels from sequence 0, and starts playing that sequence at the rate
   --  defined in SEQ[0]REFRESH and/or DECODER.MODE. Causes PWM generation to
   --  start it was not running.
   type TASKS_SEQSTART_Registers is array (0 .. 1) of TASKS_SEQSTART_Register;

   --  Steps by one value in the current sequence on all enabled channels if
   --  DECODER.MODE=NextStep. Does not cause PWM generation to start it was not
   --  running.
   type TASKS_NEXTSTEP_Register is record
      --  Write-only.
      TASKS_NEXTSTEP : Boolean := False;
      --  unspecified
      Reserved_1_31  : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TASKS_NEXTSTEP_Register use record
      TASKS_NEXTSTEP at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   --  Response to STOP task, emitted when PWM pulses are no longer generated
   type EVENTS_STOPPED_Register is record
      EVENTS_STOPPED : Boolean := False;
      --  unspecified
      Reserved_1_31  : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_STOPPED_Register use record
      EVENTS_STOPPED at 0 range 0 .. 0;
      Reserved_1_31  at 0 range 1 .. 31;
   end record;

   --  Description collection[0]: First PWM period started on sequence 0
   type EVENTS_SEQSTARTED_Register is record
      EVENTS_SEQSTARTED : Boolean := False;
      --  unspecified
      Reserved_1_31     : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_SEQSTARTED_Register use record
      EVENTS_SEQSTARTED at 0 range 0 .. 0;
      Reserved_1_31     at 0 range 1 .. 31;
   end record;

   --  Description collection[0]: First PWM period started on sequence 0
   type EVENTS_SEQSTARTED_Registers is array (0 .. 1)
     of EVENTS_SEQSTARTED_Register;

   --  Description collection[0]: Emitted at end of every sequence 0, when last
   --  value from RAM has been applied to wave counter
   type EVENTS_SEQEND_Register is record
      EVENTS_SEQEND : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_SEQEND_Register use record
      EVENTS_SEQEND at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Description collection[0]: Emitted at end of every sequence 0, when last
   --  value from RAM has been applied to wave counter
   type EVENTS_SEQEND_Registers is array (0 .. 1) of EVENTS_SEQEND_Register;

   --  Emitted at the end of each PWM period
   type EVENTS_PWMPERIODEND_Register is record
      EVENTS_PWMPERIODEND : Boolean := False;
      --  unspecified
      Reserved_1_31       : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_PWMPERIODEND_Register use record
      EVENTS_PWMPERIODEND at 0 range 0 .. 0;
      Reserved_1_31       at 0 range 1 .. 31;
   end record;

   --  Concatenated sequences have been played the amount of times defined in
   --  LOOP.CNT
   type EVENTS_LOOPSDONE_Register is record
      EVENTS_LOOPSDONE : Boolean := False;
      --  unspecified
      Reserved_1_31    : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EVENTS_LOOPSDONE_Register use record
      EVENTS_LOOPSDONE at 0 range 0 .. 0;
      Reserved_1_31    at 0 range 1 .. 31;
   end record;

   --  Shortcut between SEQEND[0] event and STOP task
   type SHORTS_SEQEND0_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_SEQEND0_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between SEQEND[1] event and STOP task
   type SHORTS_SEQEND1_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_SEQEND1_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between LOOPSDONE event and SEQSTART[0] task
   type SHORTS_LOOPSDONE_SEQSTART0_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_LOOPSDONE_SEQSTART0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  SHORTS_LOOPSDONE_SEQSTART array
   type SHORTS_LOOPSDONE_SEQSTART_Field_Array is array (0 .. 1)
     of SHORTS_LOOPSDONE_SEQSTART0_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for SHORTS_LOOPSDONE_SEQSTART
   type SHORTS_LOOPSDONE_SEQSTART_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LOOPSDONE_SEQSTART as a value
            Val : HAL.UInt2;
         when True =>
            --  LOOPSDONE_SEQSTART as an array
            Arr : SHORTS_LOOPSDONE_SEQSTART_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SHORTS_LOOPSDONE_SEQSTART_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Shortcut between LOOPSDONE event and STOP task
   type SHORTS_LOOPSDONE_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_LOOPSDONE_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut register
   type SHORTS_Register is record
      --  Shortcut between SEQEND[0] event and STOP task
      SEQEND0_STOP       : SHORTS_SEQEND0_STOP_Field := NRF_SVD.PWM.Disabled;
      --  Shortcut between SEQEND[1] event and STOP task
      SEQEND1_STOP       : SHORTS_SEQEND1_STOP_Field := NRF_SVD.PWM.Disabled;
      --  Shortcut between LOOPSDONE event and SEQSTART[0] task
      LOOPSDONE_SEQSTART : SHORTS_LOOPSDONE_SEQSTART_Field :=
                            (As_Array => False, Val => 16#0#);
      --  Shortcut between LOOPSDONE event and STOP task
      LOOPSDONE_STOP     : SHORTS_LOOPSDONE_STOP_Field :=
                            NRF_SVD.PWM.Disabled;
      --  unspecified
      Reserved_5_31      : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      SEQEND0_STOP       at 0 range 0 .. 0;
      SEQEND1_STOP       at 0 range 1 .. 1;
      LOOPSDONE_SEQSTART at 0 range 2 .. 3;
      LOOPSDONE_STOP     at 0 range 4 .. 4;
      Reserved_5_31      at 0 range 5 .. 31;
   end record;

   --  Enable or disable interrupt for STOPPED event
   type INTEN_STOPPED_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for SEQSTARTED[0] event
   type INTEN_SEQSTARTED0_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_SEQSTARTED0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  INTEN_SEQSTARTED array
   type INTEN_SEQSTARTED_Field_Array is array (0 .. 1)
     of INTEN_SEQSTARTED0_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for INTEN_SEQSTARTED
   type INTEN_SEQSTARTED_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEQSTARTED as a value
            Val : HAL.UInt2;
         when True =>
            --  SEQSTARTED as an array
            Arr : INTEN_SEQSTARTED_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for INTEN_SEQSTARTED_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Enable or disable interrupt for SEQEND[0] event
   type INTEN_SEQEND0_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_SEQEND0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  INTEN_SEQEND array
   type INTEN_SEQEND_Field_Array is array (0 .. 1) of INTEN_SEQEND0_Field
     with Component_Size => 1, Size => 2;

   --  Type definition for INTEN_SEQEND
   type INTEN_SEQEND_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEQEND as a value
            Val : HAL.UInt2;
         when True =>
            --  SEQEND as an array
            Arr : INTEN_SEQEND_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for INTEN_SEQEND_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Enable or disable interrupt for PWMPERIODEND event
   type INTEN_PWMPERIODEND_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_PWMPERIODEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt for LOOPSDONE event
   type INTEN_LOOPSDONE_Field is
     (--  Disable
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for INTEN_LOOPSDONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable interrupt
   type INTEN_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Enable or disable interrupt for STOPPED event
      STOPPED       : INTEN_STOPPED_Field := NRF_SVD.PWM.Disabled;
      --  Enable or disable interrupt for SEQSTARTED[0] event
      SEQSTARTED    : INTEN_SEQSTARTED_Field :=
                       (As_Array => False, Val => 16#0#);
      --  Enable or disable interrupt for SEQEND[0] event
      SEQEND        : INTEN_SEQEND_Field := (As_Array => False, Val => 16#0#);
      --  Enable or disable interrupt for PWMPERIODEND event
      PWMPERIODEND  : INTEN_PWMPERIODEND_Field := NRF_SVD.PWM.Disabled;
      --  Enable or disable interrupt for LOOPSDONE event
      LOOPSDONE     : INTEN_LOOPSDONE_Field := NRF_SVD.PWM.Disabled;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTEN_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      STOPPED       at 0 range 1 .. 1;
      SEQSTARTED    at 0 range 2 .. 3;
      SEQEND        at 0 range 4 .. 5;
      PWMPERIODEND  at 0 range 6 .. 6;
      LOOPSDONE     at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Write '1' to Enable interrupt for STOPPED event
   type INTENSET_STOPPED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for STOPPED event
   type INTENSET_STOPPED_Field_1 is
     (--  Reset value for the field
      Intenset_Stopped_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_STOPPED_Field_1 use
     (Intenset_Stopped_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for SEQSTARTED[0] event
   type INTENSET_SEQSTARTED0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_SEQSTARTED0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for SEQSTARTED[0] event
   type INTENSET_SEQSTARTED0_Field_1 is
     (--  Reset value for the field
      Intenset_Seqstarted0_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_SEQSTARTED0_Field_1 use
     (Intenset_Seqstarted0_Field_Reset => 0,
      Set => 1);

   --  INTENSET_SEQSTARTED array
   type INTENSET_SEQSTARTED_Field_Array is array (0 .. 1)
     of INTENSET_SEQSTARTED0_Field_1
     with Component_Size => 1, Size => 2;

   --  Type definition for INTENSET_SEQSTARTED
   type INTENSET_SEQSTARTED_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEQSTARTED as a value
            Val : HAL.UInt2;
         when True =>
            --  SEQSTARTED as an array
            Arr : INTENSET_SEQSTARTED_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for INTENSET_SEQSTARTED_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Write '1' to Enable interrupt for SEQEND[0] event
   type INTENSET_SEQEND0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_SEQEND0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for SEQEND[0] event
   type INTENSET_SEQEND0_Field_1 is
     (--  Reset value for the field
      Intenset_Seqend0_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_SEQEND0_Field_1 use
     (Intenset_Seqend0_Field_Reset => 0,
      Set => 1);

   --  INTENSET_SEQEND array
   type INTENSET_SEQEND_Field_Array is array (0 .. 1)
     of INTENSET_SEQEND0_Field_1
     with Component_Size => 1, Size => 2;

   --  Type definition for INTENSET_SEQEND
   type INTENSET_SEQEND_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEQEND as a value
            Val : HAL.UInt2;
         when True =>
            --  SEQEND as an array
            Arr : INTENSET_SEQEND_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for INTENSET_SEQEND_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Write '1' to Enable interrupt for PWMPERIODEND event
   type INTENSET_PWMPERIODEND_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_PWMPERIODEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for PWMPERIODEND event
   type INTENSET_PWMPERIODEND_Field_1 is
     (--  Reset value for the field
      Intenset_Pwmperiodend_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_PWMPERIODEND_Field_1 use
     (Intenset_Pwmperiodend_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for LOOPSDONE event
   type INTENSET_LOOPSDONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_LOOPSDONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for LOOPSDONE event
   type INTENSET_LOOPSDONE_Field_1 is
     (--  Reset value for the field
      Intenset_Loopsdone_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_LOOPSDONE_Field_1 use
     (Intenset_Loopsdone_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Write '1' to Enable interrupt for STOPPED event
      STOPPED       : INTENSET_STOPPED_Field_1 :=
                       Intenset_Stopped_Field_Reset;
      --  Write '1' to Enable interrupt for SEQSTARTED[0] event
      SEQSTARTED    : INTENSET_SEQSTARTED_Field :=
                       (As_Array => False, Val => 16#0#);
      --  Write '1' to Enable interrupt for SEQEND[0] event
      SEQEND        : INTENSET_SEQEND_Field :=
                       (As_Array => False, Val => 16#0#);
      --  Write '1' to Enable interrupt for PWMPERIODEND event
      PWMPERIODEND  : INTENSET_PWMPERIODEND_Field_1 :=
                       Intenset_Pwmperiodend_Field_Reset;
      --  Write '1' to Enable interrupt for LOOPSDONE event
      LOOPSDONE     : INTENSET_LOOPSDONE_Field_1 :=
                       Intenset_Loopsdone_Field_Reset;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      STOPPED       at 0 range 1 .. 1;
      SEQSTARTED    at 0 range 2 .. 3;
      SEQEND        at 0 range 4 .. 5;
      PWMPERIODEND  at 0 range 6 .. 6;
      LOOPSDONE     at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Write '1' to Disable interrupt for STOPPED event
   type INTENCLR_STOPPED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for STOPPED event
   type INTENCLR_STOPPED_Field_1 is
     (--  Reset value for the field
      Intenclr_Stopped_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_STOPPED_Field_1 use
     (Intenclr_Stopped_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for SEQSTARTED[0] event
   type INTENCLR_SEQSTARTED0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_SEQSTARTED0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for SEQSTARTED[0] event
   type INTENCLR_SEQSTARTED0_Field_1 is
     (--  Reset value for the field
      Intenclr_Seqstarted0_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_SEQSTARTED0_Field_1 use
     (Intenclr_Seqstarted0_Field_Reset => 0,
      Clear => 1);

   --  INTENCLR_SEQSTARTED array
   type INTENCLR_SEQSTARTED_Field_Array is array (0 .. 1)
     of INTENCLR_SEQSTARTED0_Field_1
     with Component_Size => 1, Size => 2;

   --  Type definition for INTENCLR_SEQSTARTED
   type INTENCLR_SEQSTARTED_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEQSTARTED as a value
            Val : HAL.UInt2;
         when True =>
            --  SEQSTARTED as an array
            Arr : INTENCLR_SEQSTARTED_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for INTENCLR_SEQSTARTED_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Write '1' to Disable interrupt for SEQEND[0] event
   type INTENCLR_SEQEND0_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_SEQEND0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for SEQEND[0] event
   type INTENCLR_SEQEND0_Field_1 is
     (--  Reset value for the field
      Intenclr_Seqend0_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_SEQEND0_Field_1 use
     (Intenclr_Seqend0_Field_Reset => 0,
      Clear => 1);

   --  INTENCLR_SEQEND array
   type INTENCLR_SEQEND_Field_Array is array (0 .. 1)
     of INTENCLR_SEQEND0_Field_1
     with Component_Size => 1, Size => 2;

   --  Type definition for INTENCLR_SEQEND
   type INTENCLR_SEQEND_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SEQEND as a value
            Val : HAL.UInt2;
         when True =>
            --  SEQEND as an array
            Arr : INTENCLR_SEQEND_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for INTENCLR_SEQEND_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  Write '1' to Disable interrupt for PWMPERIODEND event
   type INTENCLR_PWMPERIODEND_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_PWMPERIODEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for PWMPERIODEND event
   type INTENCLR_PWMPERIODEND_Field_1 is
     (--  Reset value for the field
      Intenclr_Pwmperiodend_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_PWMPERIODEND_Field_1 use
     (Intenclr_Pwmperiodend_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for LOOPSDONE event
   type INTENCLR_LOOPSDONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_LOOPSDONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for LOOPSDONE event
   type INTENCLR_LOOPSDONE_Field_1 is
     (--  Reset value for the field
      Intenclr_Loopsdone_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_LOOPSDONE_Field_1 use
     (Intenclr_Loopsdone_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Write '1' to Disable interrupt for STOPPED event
      STOPPED       : INTENCLR_STOPPED_Field_1 :=
                       Intenclr_Stopped_Field_Reset;
      --  Write '1' to Disable interrupt for SEQSTARTED[0] event
      SEQSTARTED    : INTENCLR_SEQSTARTED_Field :=
                       (As_Array => False, Val => 16#0#);
      --  Write '1' to Disable interrupt for SEQEND[0] event
      SEQEND        : INTENCLR_SEQEND_Field :=
                       (As_Array => False, Val => 16#0#);
      --  Write '1' to Disable interrupt for PWMPERIODEND event
      PWMPERIODEND  : INTENCLR_PWMPERIODEND_Field_1 :=
                       Intenclr_Pwmperiodend_Field_Reset;
      --  Write '1' to Disable interrupt for LOOPSDONE event
      LOOPSDONE     : INTENCLR_LOOPSDONE_Field_1 :=
                       Intenclr_Loopsdone_Field_Reset;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      STOPPED       at 0 range 1 .. 1;
      SEQSTARTED    at 0 range 2 .. 3;
      SEQEND        at 0 range 4 .. 5;
      PWMPERIODEND  at 0 range 6 .. 6;
      LOOPSDONE     at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Enable or disable PWM module
   type ENABLE_ENABLE_Field is
     (--  Disabled
      Disabled,
      --  Enable
      Enabled)
     with Size => 1;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  PWM module enable register
   type ENABLE_Register is record
      --  Enable or disable PWM module
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.PWM.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Selects up or up and down as wave counter mode
   type MODE_UPDOWN_Field is
     (--  Up counter - edge aligned PWM duty-cycle
      Up,
      --  Up and down counter - center aligned PWM duty cycle
      Upanddown)
     with Size => 1;
   for MODE_UPDOWN_Field use
     (Up => 0,
      Upanddown => 1);

   --  Selects operating mode of the wave counter
   type MODE_Register is record
      --  Selects up or up and down as wave counter mode
      UPDOWN        : MODE_UPDOWN_Field := NRF_SVD.PWM.Up;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODE_Register use record
      UPDOWN        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype COUNTERTOP_COUNTERTOP_Field is HAL.UInt15;

   --  Value up to which the pulse generator counter counts
   type COUNTERTOP_Register is record
      --  Value up to which the pulse generator counter counts. This register
      --  is ignored when DECODER.MODE=WaveForm and only values from RAM will
      --  be used.
      COUNTERTOP     : COUNTERTOP_COUNTERTOP_Field := 16#3FF#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNTERTOP_Register use record
      COUNTERTOP     at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  Pre-scaler of PWM_CLK
   type PRESCALER_PRESCALER_Field is
     (--  Divide by 1 (16MHz)
      Div_1,
      --  Divide by 2 ( 8MHz)
      Div_2,
      --  Divide by 4 ( 4MHz)
      Div_4,
      --  Divide by 8 ( 2MHz)
      Div_8,
      --  Divide by 16 ( 1MHz)
      Div_16,
      --  Divide by 32 ( 500kHz)
      Div_32,
      --  Divide by 64 ( 250kHz)
      Div_64,
      --  Divide by 128 ( 125kHz)
      Div_128)
     with Size => 3;
   for PRESCALER_PRESCALER_Field use
     (Div_1 => 0,
      Div_2 => 1,
      Div_4 => 2,
      Div_8 => 3,
      Div_16 => 4,
      Div_32 => 5,
      Div_64 => 6,
      Div_128 => 7);

   --  Configuration for PWM_CLK
   type PRESCALER_Register is record
      --  Pre-scaler of PWM_CLK
      PRESCALER     : PRESCALER_PRESCALER_Field := NRF_SVD.PWM.Div_1;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRESCALER_Register use record
      PRESCALER     at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  How a sequence is read from RAM and spread to the compare register
   type DECODER_LOAD_Field is
     (--  1st half word (16-bit) used in all PWM channels 0..3
      Common,
      --  1st half word (16-bit) used in channel 0..1; 2nd word in channel 2..3
      Grouped,
      --  1st half word (16-bit) in ch.0; 2nd in ch.1; ...; 4th in ch.3
      Individual,
      --  1st half word (16-bit) in ch.0; 2nd in ch.1; ...; 4th in COUNTERTOP
      Waveform)
     with Size => 2;
   for DECODER_LOAD_Field use
     (Common => 0,
      Grouped => 1,
      Individual => 2,
      Waveform => 3);

   --  Selects source for advancing the active sequence
   type DECODER_MODE_Field is
     (--  SEQ[n].REFRESH is used to determine loading internal compare registers
      Refreshcount,
      --  NEXTSTEP task causes a new value to be loaded to internal compare registers
      Nextstep)
     with Size => 1;
   for DECODER_MODE_Field use
     (Refreshcount => 0,
      Nextstep => 1);

   --  Configuration of the decoder
   type DECODER_Register is record
      --  How a sequence is read from RAM and spread to the compare register
      LOAD          : DECODER_LOAD_Field := NRF_SVD.PWM.Common;
      --  unspecified
      Reserved_2_7  : HAL.UInt6 := 16#0#;
      --  Selects source for advancing the active sequence
      MODE          : DECODER_MODE_Field := NRF_SVD.PWM.Refreshcount;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DECODER_Register use record
      LOAD          at 0 range 0 .. 1;
      Reserved_2_7  at 0 range 2 .. 7;
      MODE          at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  Amount of playback of pattern cycles
   type LOOP_CNT_Field is
     (--  Looping disabled (stop at the end of the sequence)
      Disabled)
     with Size => 16;
   for LOOP_CNT_Field use
     (Disabled => 0);

   --  Amount of playback of a loop
   type LOOP_Register is record
      --  Amount of playback of pattern cycles
      CNT            : LOOP_CNT_Field := NRF_SVD.PWM.Disabled;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LOOP_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ---------------------------------
   -- PWM_SEQ cluster's Registers --
   ---------------------------------

   --  Amount of values (duty cycles) in this sequence
   type CNT_CNT_Field is
     (--  Sequence is disabled, and shall not be started as it is empty
      Disabled)
     with Size => 15;
   for CNT_CNT_Field use
     (Disabled => 0);

   --  Description cluster[0]: Amount of values (duty cycles) in this sequence
   type CNT_SEQ_Register is record
      --  Amount of values (duty cycles) in this sequence
      CNT            : CNT_CNT_Field := NRF_SVD.PWM.Disabled;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNT_SEQ_Register use record
      CNT            at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  Amount of additional PWM periods between samples loaded into compare
   --  register (load every REFRESH.CNT+1 PWM periods)
   type REFRESH_CNT_Field is
     (--  Update every PWM period
      Continuous,
      --  Reset value for the field
      Refresh_Cnt_Field_Reset)
     with Size => 24;
   for REFRESH_CNT_Field use
     (Continuous => 0,
      Refresh_Cnt_Field_Reset => 1);

   --  Description cluster[0]: Amount of additional PWM periods between samples
   --  loaded into compare register
   type REFRESH_SEQ_Register is record
      --  Amount of additional PWM periods between samples loaded into compare
      --  register (load every REFRESH.CNT+1 PWM periods)
      CNT            : REFRESH_CNT_Field := Refresh_Cnt_Field_Reset;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for REFRESH_SEQ_Register use record
      CNT            at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype ENDDELAY_SEQ_CNT_Field is HAL.UInt24;

   --  Description cluster[0]: Time added after the sequence
   type ENDDELAY_SEQ_Register is record
      --  Time added after the sequence in PWM periods
      CNT            : ENDDELAY_SEQ_CNT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENDDELAY_SEQ_Register use record
      CNT            at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Unspecified
   type PWM_SEQ_Cluster is record
      --  Description cluster[0]: Beginning address in Data RAM of this
      --  sequence
      PTR      : aliased HAL.UInt32;
      --  Description cluster[0]: Amount of values (duty cycles) in this
      --  sequence
      CNT      : aliased CNT_SEQ_Register;
      --  Description cluster[0]: Amount of additional PWM periods between
      --  samples loaded into compare register
      REFRESH  : aliased REFRESH_SEQ_Register;
      --  Description cluster[0]: Time added after the sequence
      ENDDELAY : aliased ENDDELAY_SEQ_Register;
   end record
     with Size => 256;

   for PWM_SEQ_Cluster use record
      PTR      at 16#0# range 0 .. 31;
      CNT      at 16#4# range 0 .. 31;
      REFRESH  at 16#8# range 0 .. 31;
      ENDDELAY at 16#C# range 0 .. 31;
   end record;

   --  Unspecified
   type PWM_SEQ_Clusters is array (0 .. 1) of PWM_SEQ_Cluster;

   ----------------------------------
   -- PWM_PSEL cluster's Registers --
   ----------------------------------

   subtype OUT_PSEL_PIN_Field is HAL.UInt5;

   --  Connection
   type OUT_CONNECT_Field is
     (--  Connect
      Connected,
      --  Disconnect
      Disconnected)
     with Size => 1;
   for OUT_CONNECT_Field use
     (Connected => 0,
      Disconnected => 1);

   --  Description collection[0]: Output pin select for PWM channel 0
   type OUT_PSEL_Register is record
      --  Pin number
      PIN           : OUT_PSEL_PIN_Field := 16#1F#;
      --  unspecified
      Reserved_5_30 : HAL.UInt26 := 16#3FFFFFF#;
      --  Connection
      CONNECT       : OUT_CONNECT_Field := NRF_SVD.PWM.Disconnected;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OUT_PSEL_Register use record
      PIN           at 0 range 0 .. 4;
      Reserved_5_30 at 0 range 5 .. 30;
      CONNECT       at 0 range 31 .. 31;
   end record;

   --  Description collection[0]: Output pin select for PWM channel 0
   type OUT_PSEL_Registers is array (0 .. 3) of OUT_PSEL_Register;

   --  Unspecified
   type PWM_PSEL_Cluster is record
      --  Description collection[0]: Output pin select for PWM channel 0
      OUT_k : aliased OUT_PSEL_Registers;
   end record
     with Size => 128;

   for PWM_PSEL_Cluster use record
      OUT_k at 0 range 0 .. 127;
   end record;

   ---------------------------------
   -- PWM_SEQ cluster's Registers --
   ---------------------------------

   ----------------------------------
   -- PWM_PSEL cluster's Registers --
   ----------------------------------

   ---------------------------------
   -- PWM_SEQ cluster's Registers --
   ---------------------------------

   ----------------------------------
   -- PWM_PSEL cluster's Registers --
   ----------------------------------

   -----------------
   -- Peripherals --
   -----------------

   --  Pulse Width Modulation Unit 0
   type PWM_Peripheral is record
      --  Stops PWM pulse generation on all channels at the end of current PWM
      --  period, and stops sequence playback
      TASKS_STOP          : aliased TASKS_STOP_Register;
      --  Description collection[0]: Loads the first PWM value on all enabled
      --  channels from sequence 0, and starts playing that sequence at the
      --  rate defined in SEQ[0]REFRESH and/or DECODER.MODE. Causes PWM
      --  generation to start it was not running.
      TASKS_SEQSTART      : aliased TASKS_SEQSTART_Registers;
      --  Steps by one value in the current sequence on all enabled channels if
      --  DECODER.MODE=NextStep. Does not cause PWM generation to start it was
      --  not running.
      TASKS_NEXTSTEP      : aliased TASKS_NEXTSTEP_Register;
      --  Response to STOP task, emitted when PWM pulses are no longer
      --  generated
      EVENTS_STOPPED      : aliased EVENTS_STOPPED_Register;
      --  Description collection[0]: First PWM period started on sequence 0
      EVENTS_SEQSTARTED   : aliased EVENTS_SEQSTARTED_Registers;
      --  Description collection[0]: Emitted at end of every sequence 0, when
      --  last value from RAM has been applied to wave counter
      EVENTS_SEQEND       : aliased EVENTS_SEQEND_Registers;
      --  Emitted at the end of each PWM period
      EVENTS_PWMPERIODEND : aliased EVENTS_PWMPERIODEND_Register;
      --  Concatenated sequences have been played the amount of times defined
      --  in LOOP.CNT
      EVENTS_LOOPSDONE    : aliased EVENTS_LOOPSDONE_Register;
      --  Shortcut register
      SHORTS              : aliased SHORTS_Register;
      --  Enable or disable interrupt
      INTEN               : aliased INTEN_Register;
      --  Enable interrupt
      INTENSET            : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR            : aliased INTENCLR_Register;
      --  PWM module enable register
      ENABLE              : aliased ENABLE_Register;
      --  Selects operating mode of the wave counter
      MODE                : aliased MODE_Register;
      --  Value up to which the pulse generator counter counts
      COUNTERTOP          : aliased COUNTERTOP_Register;
      --  Configuration for PWM_CLK
      PRESCALER           : aliased PRESCALER_Register;
      --  Configuration of the decoder
      DECODER             : aliased DECODER_Register;
      --  Amount of playback of a loop
      LOOP_k              : aliased LOOP_Register;
      --  Unspecified
      SEQ                 : aliased PWM_SEQ_Clusters;
      --  Unspecified
      PSEL                : aliased PWM_PSEL_Cluster;
   end record
     with Volatile;

   for PWM_Peripheral use record
      TASKS_STOP          at 16#4# range 0 .. 31;
      TASKS_SEQSTART      at 16#8# range 0 .. 63;
      TASKS_NEXTSTEP      at 16#10# range 0 .. 31;
      EVENTS_STOPPED      at 16#104# range 0 .. 31;
      EVENTS_SEQSTARTED   at 16#108# range 0 .. 63;
      EVENTS_SEQEND       at 16#110# range 0 .. 63;
      EVENTS_PWMPERIODEND at 16#118# range 0 .. 31;
      EVENTS_LOOPSDONE    at 16#11C# range 0 .. 31;
      SHORTS              at 16#200# range 0 .. 31;
      INTEN               at 16#300# range 0 .. 31;
      INTENSET            at 16#304# range 0 .. 31;
      INTENCLR            at 16#308# range 0 .. 31;
      ENABLE              at 16#500# range 0 .. 31;
      MODE                at 16#504# range 0 .. 31;
      COUNTERTOP          at 16#508# range 0 .. 31;
      PRESCALER           at 16#50C# range 0 .. 31;
      DECODER             at 16#510# range 0 .. 31;
      LOOP_k              at 16#514# range 0 .. 31;
      SEQ                 at 16#520# range 0 .. 511;
      PSEL                at 16#560# range 0 .. 127;
   end record;

   --  Pulse Width Modulation Unit 0
   PWM0_Periph : aliased PWM_Peripheral
     with Import, Address => PWM0_Base;

   --  Pulse Width Modulation Unit 1
   PWM1_Periph : aliased PWM_Peripheral
     with Import, Address => PWM1_Base;

   --  Pulse Width Modulation Unit 2
   PWM2_Periph : aliased PWM_Peripheral
     with Import, Address => PWM2_Base;

end NRF_SVD.PWM;
