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

package NRF_SVD.CLOCK is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Write '1' to Enable interrupt for HFCLKSTARTED event
   type INTENSET_HFCLKSTARTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_HFCLKSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for HFCLKSTARTED event
   type INTENSET_HFCLKSTARTED_Field_1 is
     (--  Reset value for the field
      Intenset_Hfclkstarted_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_HFCLKSTARTED_Field_1 use
     (Intenset_Hfclkstarted_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for LFCLKSTARTED event
   type INTENSET_LFCLKSTARTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_LFCLKSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for LFCLKSTARTED event
   type INTENSET_LFCLKSTARTED_Field_1 is
     (--  Reset value for the field
      Intenset_Lfclkstarted_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_LFCLKSTARTED_Field_1 use
     (Intenset_Lfclkstarted_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for DONE event
   type INTENSET_DONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_DONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for DONE event
   type INTENSET_DONE_Field_1 is
     (--  Reset value for the field
      Intenset_Done_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_DONE_Field_1 use
     (Intenset_Done_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for CTTO event
   type INTENSET_CTTO_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CTTO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CTTO event
   type INTENSET_CTTO_Field_1 is
     (--  Reset value for the field
      Intenset_Ctto_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CTTO_Field_1 use
     (Intenset_Ctto_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for HFCLKSTARTED event
      HFCLKSTARTED  : INTENSET_HFCLKSTARTED_Field_1 :=
                       Intenset_Hfclkstarted_Field_Reset;
      --  Write '1' to Enable interrupt for LFCLKSTARTED event
      LFCLKSTARTED  : INTENSET_LFCLKSTARTED_Field_1 :=
                       Intenset_Lfclkstarted_Field_Reset;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Write '1' to Enable interrupt for DONE event
      DONE          : INTENSET_DONE_Field_1 := Intenset_Done_Field_Reset;
      --  Write '1' to Enable interrupt for CTTO event
      CTTO          : INTENSET_CTTO_Field_1 := Intenset_Ctto_Field_Reset;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      HFCLKSTARTED  at 0 range 0 .. 0;
      LFCLKSTARTED  at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      DONE          at 0 range 3 .. 3;
      CTTO          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Write '1' to Disable interrupt for HFCLKSTARTED event
   type INTENCLR_HFCLKSTARTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_HFCLKSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for HFCLKSTARTED event
   type INTENCLR_HFCLKSTARTED_Field_1 is
     (--  Reset value for the field
      Intenclr_Hfclkstarted_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_HFCLKSTARTED_Field_1 use
     (Intenclr_Hfclkstarted_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for LFCLKSTARTED event
   type INTENCLR_LFCLKSTARTED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_LFCLKSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for LFCLKSTARTED event
   type INTENCLR_LFCLKSTARTED_Field_1 is
     (--  Reset value for the field
      Intenclr_Lfclkstarted_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_LFCLKSTARTED_Field_1 use
     (Intenclr_Lfclkstarted_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for DONE event
   type INTENCLR_DONE_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_DONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for DONE event
   type INTENCLR_DONE_Field_1 is
     (--  Reset value for the field
      Intenclr_Done_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_DONE_Field_1 use
     (Intenclr_Done_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for CTTO event
   type INTENCLR_CTTO_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CTTO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CTTO event
   type INTENCLR_CTTO_Field_1 is
     (--  Reset value for the field
      Intenclr_Ctto_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CTTO_Field_1 use
     (Intenclr_Ctto_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for HFCLKSTARTED event
      HFCLKSTARTED  : INTENCLR_HFCLKSTARTED_Field_1 :=
                       Intenclr_Hfclkstarted_Field_Reset;
      --  Write '1' to Disable interrupt for LFCLKSTARTED event
      LFCLKSTARTED  : INTENCLR_LFCLKSTARTED_Field_1 :=
                       Intenclr_Lfclkstarted_Field_Reset;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Write '1' to Disable interrupt for DONE event
      DONE          : INTENCLR_DONE_Field_1 := Intenclr_Done_Field_Reset;
      --  Write '1' to Disable interrupt for CTTO event
      CTTO          : INTENCLR_CTTO_Field_1 := Intenclr_Ctto_Field_Reset;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      HFCLKSTARTED  at 0 range 0 .. 0;
      LFCLKSTARTED  at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      DONE          at 0 range 3 .. 3;
      CTTO          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  HFCLKSTART task triggered or not
   type HFCLKRUN_STATUS_Field is
     (--  Task not triggered
      Nottriggered,
      --  Task triggered
      Triggered)
     with Size => 1;
   for HFCLKRUN_STATUS_Field use
     (Nottriggered => 0,
      Triggered => 1);

   --  Status indicating that HFCLKSTART task has been triggered
   type HFCLKRUN_Register is record
      --  Read-only. HFCLKSTART task triggered or not
      STATUS        : HFCLKRUN_STATUS_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFCLKRUN_Register use record
      STATUS        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Source of HFCLK
   type HFCLKSTAT_SRC_Field is
     (--  64 MHz internal oscillator (HFINT)
      Rc,
      --  64 MHz crystal oscillator (HFXO)
      Xtal)
     with Size => 1;
   for HFCLKSTAT_SRC_Field use
     (Rc => 0,
      Xtal => 1);

   --  HFCLK state
   type HFCLKSTAT_STATE_Field is
     (--  HFCLK not running
      Notrunning,
      --  HFCLK running
      Running)
     with Size => 1;
   for HFCLKSTAT_STATE_Field use
     (Notrunning => 0,
      Running => 1);

   --  HFCLK status
   type HFCLKSTAT_Register is record
      --  Read-only. Source of HFCLK
      SRC            : HFCLKSTAT_SRC_Field;
      --  unspecified
      Reserved_1_15  : HAL.UInt15;
      --  Read-only. HFCLK state
      STATE          : HFCLKSTAT_STATE_Field;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFCLKSTAT_Register use record
      SRC            at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      STATE          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  LFCLKSTART task triggered or not
   type LFCLKRUN_STATUS_Field is
     (--  Task not triggered
      Nottriggered,
      --  Task triggered
      Triggered)
     with Size => 1;
   for LFCLKRUN_STATUS_Field use
     (Nottriggered => 0,
      Triggered => 1);

   --  Status indicating that LFCLKSTART task has been triggered
   type LFCLKRUN_Register is record
      --  Read-only. LFCLKSTART task triggered or not
      STATUS        : LFCLKRUN_STATUS_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFCLKRUN_Register use record
      STATUS        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Source of LFCLK
   type LFCLKSTAT_SRC_Field is
     (--  32.768 kHz RC oscillator
      Rc,
      --  32.768 kHz crystal oscillator
      Xtal,
      --  32.768 kHz synthesized from HFCLK
      Synth)
     with Size => 2;
   for LFCLKSTAT_SRC_Field use
     (Rc => 0,
      Xtal => 1,
      Synth => 2);

   --  LFCLK state
   type LFCLKSTAT_STATE_Field is
     (--  LFCLK not running
      Notrunning,
      --  LFCLK running
      Running)
     with Size => 1;
   for LFCLKSTAT_STATE_Field use
     (Notrunning => 0,
      Running => 1);

   --  LFCLK status
   type LFCLKSTAT_Register is record
      --  Read-only. Source of LFCLK
      SRC            : LFCLKSTAT_SRC_Field;
      --  unspecified
      Reserved_2_15  : HAL.UInt14;
      --  Read-only. LFCLK state
      STATE          : LFCLKSTAT_STATE_Field;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFCLKSTAT_Register use record
      SRC            at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      STATE          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Clock source
   type LFCLKSRCCOPY_SRC_Field is
     (--  32.768 kHz RC oscillator
      Rc,
      --  32.768 kHz crystal oscillator
      Xtal,
      --  32.768 kHz synthesized from HFCLK
      Synth)
     with Size => 2;
   for LFCLKSRCCOPY_SRC_Field use
     (Rc => 0,
      Xtal => 1,
      Synth => 2);

   --  Copy of LFCLKSRC register, set when LFCLKSTART task was triggered
   type LFCLKSRCCOPY_Register is record
      --  Read-only. Clock source
      SRC           : LFCLKSRCCOPY_SRC_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFCLKSRCCOPY_Register use record
      SRC           at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Clock source
   type LFCLKSRC_SRC_Field is
     (--  32.768 kHz RC oscillator
      Rc,
      --  32.768 kHz crystal oscillator
      Xtal,
      --  32.768 kHz synthesized from HFCLK
      Synth)
     with Size => 2;
   for LFCLKSRC_SRC_Field use
     (Rc => 0,
      Xtal => 1,
      Synth => 2);

   --  Enable or disable bypass of LFCLK crystal oscillator with external clock
   --  source
   type LFCLKSRC_BYPASS_Field is
     (--  Disable (use with Xtal or low-swing external source)
      Disabled,
      --  Enable (use with rail-to-rail external source)
      Enabled)
     with Size => 1;
   for LFCLKSRC_BYPASS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable external source for LFCLK
   type LFCLKSRC_EXTERNAL_Field is
     (--  Disable external source (use with Xtal)
      Disabled,
      --  Enable use of external source instead of Xtal (SRC needs to be set to Xtal)
      Enabled)
     with Size => 1;
   for LFCLKSRC_EXTERNAL_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Clock source for the LFCLK
   type LFCLKSRC_Register is record
      --  Clock source
      SRC            : LFCLKSRC_SRC_Field := NRF_SVD.CLOCK.Rc;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Enable or disable bypass of LFCLK crystal oscillator with external
      --  clock source
      BYPASS         : LFCLKSRC_BYPASS_Field := NRF_SVD.CLOCK.Disabled;
      --  Enable or disable external source for LFCLK
      EXTERNAL       : LFCLKSRC_EXTERNAL_Field := NRF_SVD.CLOCK.Disabled;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFCLKSRC_Register use record
      SRC            at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      BYPASS         at 0 range 16 .. 16;
      EXTERNAL       at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype CTIV_CTIV_Field is HAL.UInt7;

   --  Calibration timer interval
   type CTIV_Register is record
      --  Calibration timer interval in multiple of 0.25 seconds. Range: 0.25
      --  seconds to 31.75 seconds.
      CTIV          : CTIV_CTIV_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTIV_Register use record
      CTIV          at 0 range 0 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Speed of Trace Port clock. Note that the TRACECLK pin will output this
   --  clock divided by two.
   type TRACECONFIG_TRACEPORTSPEED_Field is
     (--  32 MHz Trace Port clock (TRACECLK = 16 MHz)
      Val_32Mhz,
      --  16 MHz Trace Port clock (TRACECLK = 8 MHz)
      Val_16Mhz,
      --  8 MHz Trace Port clock (TRACECLK = 4 MHz)
      Val_8Mhz,
      --  4 MHz Trace Port clock (TRACECLK = 2 MHz)
      Val_4Mhz)
     with Size => 2;
   for TRACECONFIG_TRACEPORTSPEED_Field use
     (Val_32Mhz => 0,
      Val_16Mhz => 1,
      Val_8Mhz => 2,
      Val_4Mhz => 3);

   --  Pin multiplexing of trace signals.
   type TRACECONFIG_TRACEMUX_Field is
     (--  GPIOs multiplexed onto all trace-pins
      Gpio,
      --  SWO multiplexed onto P0.18, GPIO multiplexed onto other trace pins
      Serial,
      --  TRACECLK and TRACEDATA multiplexed onto P0.20, P0.18, P0.16, P0.15 and
--  P0.14.
      Parallel)
     with Size => 2;
   for TRACECONFIG_TRACEMUX_Field use
     (Gpio => 0,
      Serial => 1,
      Parallel => 2);

   --  Clocking options for the Trace Port debug interface
   type TRACECONFIG_Register is record
      --  Speed of Trace Port clock. Note that the TRACECLK pin will output
      --  this clock divided by two.
      TRACEPORTSPEED : TRACECONFIG_TRACEPORTSPEED_Field :=
                        NRF_SVD.CLOCK.Val_32Mhz;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  Pin multiplexing of trace signals.
      TRACEMUX       : TRACECONFIG_TRACEMUX_Field := NRF_SVD.CLOCK.Gpio;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TRACECONFIG_Register use record
      TRACEPORTSPEED at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      TRACEMUX       at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Clock control
   type CLOCK_Peripheral is record
      --  Start HFCLK crystal oscillator
      TASKS_HFCLKSTART    : aliased HAL.UInt32;
      --  Stop HFCLK crystal oscillator
      TASKS_HFCLKSTOP     : aliased HAL.UInt32;
      --  Start LFCLK source
      TASKS_LFCLKSTART    : aliased HAL.UInt32;
      --  Stop LFCLK source
      TASKS_LFCLKSTOP     : aliased HAL.UInt32;
      --  Start calibration of LFRC oscillator
      TASKS_CAL           : aliased HAL.UInt32;
      --  Start calibration timer
      TASKS_CTSTART       : aliased HAL.UInt32;
      --  Stop calibration timer
      TASKS_CTSTOP        : aliased HAL.UInt32;
      --  HFCLK oscillator started
      EVENTS_HFCLKSTARTED : aliased HAL.UInt32;
      --  LFCLK started
      EVENTS_LFCLKSTARTED : aliased HAL.UInt32;
      --  Calibration of LFCLK RC oscillator complete event
      EVENTS_DONE         : aliased HAL.UInt32;
      --  Calibration timer timeout
      EVENTS_CTTO         : aliased HAL.UInt32;
      --  Enable interrupt
      INTENSET            : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR            : aliased INTENCLR_Register;
      --  Status indicating that HFCLKSTART task has been triggered
      HFCLKRUN            : aliased HFCLKRUN_Register;
      --  HFCLK status
      HFCLKSTAT           : aliased HFCLKSTAT_Register;
      --  Status indicating that LFCLKSTART task has been triggered
      LFCLKRUN            : aliased LFCLKRUN_Register;
      --  LFCLK status
      LFCLKSTAT           : aliased LFCLKSTAT_Register;
      --  Copy of LFCLKSRC register, set when LFCLKSTART task was triggered
      LFCLKSRCCOPY        : aliased LFCLKSRCCOPY_Register;
      --  Clock source for the LFCLK
      LFCLKSRC            : aliased LFCLKSRC_Register;
      --  Calibration timer interval
      CTIV                : aliased CTIV_Register;
      --  Clocking options for the Trace Port debug interface
      TRACECONFIG         : aliased TRACECONFIG_Register;
   end record
     with Volatile;

   for CLOCK_Peripheral use record
      TASKS_HFCLKSTART    at 16#0# range 0 .. 31;
      TASKS_HFCLKSTOP     at 16#4# range 0 .. 31;
      TASKS_LFCLKSTART    at 16#8# range 0 .. 31;
      TASKS_LFCLKSTOP     at 16#C# range 0 .. 31;
      TASKS_CAL           at 16#10# range 0 .. 31;
      TASKS_CTSTART       at 16#14# range 0 .. 31;
      TASKS_CTSTOP        at 16#18# range 0 .. 31;
      EVENTS_HFCLKSTARTED at 16#100# range 0 .. 31;
      EVENTS_LFCLKSTARTED at 16#104# range 0 .. 31;
      EVENTS_DONE         at 16#10C# range 0 .. 31;
      EVENTS_CTTO         at 16#110# range 0 .. 31;
      INTENSET            at 16#304# range 0 .. 31;
      INTENCLR            at 16#308# range 0 .. 31;
      HFCLKRUN            at 16#408# range 0 .. 31;
      HFCLKSTAT           at 16#40C# range 0 .. 31;
      LFCLKRUN            at 16#414# range 0 .. 31;
      LFCLKSTAT           at 16#418# range 0 .. 31;
      LFCLKSRCCOPY        at 16#41C# range 0 .. 31;
      LFCLKSRC            at 16#518# range 0 .. 31;
      CTIV                at 16#538# range 0 .. 31;
      TRACECONFIG         at 16#55C# range 0 .. 31;
   end record;

   --  Clock control
   CLOCK_Periph : aliased CLOCK_Peripheral
     with Import, Address => CLOCK_Base;

end NRF_SVD.CLOCK;
