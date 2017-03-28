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

package NRF51_SVD.CLOCK is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Enable interrupt on HFCLKSTARTED event.
   type INTENSET_HFCLKSTARTED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_HFCLKSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on HFCLKSTARTED event.
   type INTENSET_HFCLKSTARTED_Field_1 is
     (
      --  Reset value for the field
      Intenset_Hfclkstarted_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_HFCLKSTARTED_Field_1 use
     (Intenset_Hfclkstarted_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on LFCLKSTARTED event.
   type INTENSET_LFCLKSTARTED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_LFCLKSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on LFCLKSTARTED event.
   type INTENSET_LFCLKSTARTED_Field_1 is
     (
      --  Reset value for the field
      Intenset_Lfclkstarted_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_LFCLKSTARTED_Field_1 use
     (Intenset_Lfclkstarted_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on DONE event.
   type INTENSET_DONE_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_DONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on DONE event.
   type INTENSET_DONE_Field_1 is
     (
      --  Reset value for the field
      Intenset_Done_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_DONE_Field_1 use
     (Intenset_Done_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on CTTO event.
   type INTENSET_CTTO_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_CTTO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on CTTO event.
   type INTENSET_CTTO_Field_1 is
     (
      --  Reset value for the field
      Intenset_Ctto_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_CTTO_Field_1 use
     (Intenset_Ctto_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on HFCLKSTARTED event.
      HFCLKSTARTED  : INTENSET_HFCLKSTARTED_Field_1 :=
                       Intenset_Hfclkstarted_Field_Reset;
      --  Enable interrupt on LFCLKSTARTED event.
      LFCLKSTARTED  : INTENSET_LFCLKSTARTED_Field_1 :=
                       Intenset_Lfclkstarted_Field_Reset;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Enable interrupt on DONE event.
      DONE          : INTENSET_DONE_Field_1 := Intenset_Done_Field_Reset;
      --  Enable interrupt on CTTO event.
      CTTO          : INTENSET_CTTO_Field_1 := Intenset_Ctto_Field_Reset;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      HFCLKSTARTED  at 0 range 0 .. 0;
      LFCLKSTARTED  at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      DONE          at 0 range 3 .. 3;
      CTTO          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Disable interrupt on HFCLKSTARTED event.
   type INTENCLR_HFCLKSTARTED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_HFCLKSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on HFCLKSTARTED event.
   type INTENCLR_HFCLKSTARTED_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Hfclkstarted_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_HFCLKSTARTED_Field_1 use
     (Intenclr_Hfclkstarted_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on LFCLKSTARTED event.
   type INTENCLR_LFCLKSTARTED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_LFCLKSTARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on LFCLKSTARTED event.
   type INTENCLR_LFCLKSTARTED_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Lfclkstarted_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_LFCLKSTARTED_Field_1 use
     (Intenclr_Lfclkstarted_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on DONE event.
   type INTENCLR_DONE_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_DONE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on DONE event.
   type INTENCLR_DONE_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Done_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_DONE_Field_1 use
     (Intenclr_Done_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on CTTO event.
   type INTENCLR_CTTO_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_CTTO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on CTTO event.
   type INTENCLR_CTTO_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Ctto_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_CTTO_Field_1 use
     (Intenclr_Ctto_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on HFCLKSTARTED event.
      HFCLKSTARTED  : INTENCLR_HFCLKSTARTED_Field_1 :=
                       Intenclr_Hfclkstarted_Field_Reset;
      --  Disable interrupt on LFCLKSTARTED event.
      LFCLKSTARTED  : INTENCLR_LFCLKSTARTED_Field_1 :=
                       Intenclr_Lfclkstarted_Field_Reset;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Disable interrupt on DONE event.
      DONE          : INTENCLR_DONE_Field_1 := Intenclr_Done_Field_Reset;
      --  Disable interrupt on CTTO event.
      CTTO          : INTENCLR_CTTO_Field_1 := Intenclr_Ctto_Field_Reset;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      HFCLKSTARTED  at 0 range 0 .. 0;
      LFCLKSTARTED  at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      DONE          at 0 range 3 .. 3;
      CTTO          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Task HFCLKSTART trigger status.
   type HFCLKRUN_STATUS_Field is
     (
      --  Task HFCLKSTART has not been triggered.
      Nottriggered,
      --  Task HFCLKSTART has been triggered.
      Triggered)
     with Size => 1;
   for HFCLKRUN_STATUS_Field use
     (Nottriggered => 0,
      Triggered => 1);

   --  Task HFCLKSTART trigger status.
   type HFCLKRUN_Register is record
      --  Read-only. Task HFCLKSTART trigger status.
      STATUS        : HFCLKRUN_STATUS_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFCLKRUN_Register use record
      STATUS        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Active clock source for the HF clock.
   type HFCLKSTAT_SRC_Field is
     (
      --  Internal 16MHz RC oscillator running and generating the HFCLK clock.
      Rc,
      --  External 16MHz/32MHz crystal oscillator running and generating the
      --  HFCLK clock.
      Xtal)
     with Size => 1;
   for HFCLKSTAT_SRC_Field use
     (Rc => 0,
      Xtal => 1);

   --  State for the HFCLK.
   type HFCLKSTAT_STATE_Field is
     (
      --  HFCLK clock not running.
      Notrunning,
      --  HFCLK clock running.
      Running)
     with Size => 1;
   for HFCLKSTAT_STATE_Field use
     (Notrunning => 0,
      Running => 1);

   --  High frequency clock status.
   type HFCLKSTAT_Register is record
      --  Read-only. Active clock source for the HF clock.
      SRC            : HFCLKSTAT_SRC_Field;
      --  unspecified
      Reserved_1_15  : HAL.UInt15;
      --  Read-only. State for the HFCLK.
      STATE          : HFCLKSTAT_STATE_Field;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFCLKSTAT_Register use record
      SRC            at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      STATE          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Task LFCLKSTART triggered status.
   type LFCLKRUN_STATUS_Field is
     (
      --  Task LFCLKSTART has not been triggered.
      Nottriggered,
      --  Task LFCLKSTART has been triggered.
      Triggered)
     with Size => 1;
   for LFCLKRUN_STATUS_Field use
     (Nottriggered => 0,
      Triggered => 1);

   --  Task LFCLKSTART triggered status.
   type LFCLKRUN_Register is record
      --  Read-only. Task LFCLKSTART triggered status.
      STATUS        : LFCLKRUN_STATUS_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFCLKRUN_Register use record
      STATUS        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Active clock source for the LF clock.
   type LFCLKSTAT_SRC_Field is
     (
      --  Internal 32KiHz RC oscillator running and generating the LFCLK clock.
      Rc,
      --  External 32KiHz crystal oscillator running and generating the LFCLK
      --  clock.
      Xtal,
      --  Internal 32KiHz synthesizer from the HFCLK running and generating the
      --  LFCLK clock.
      Synth)
     with Size => 2;
   for LFCLKSTAT_SRC_Field use
     (Rc => 0,
      Xtal => 1,
      Synth => 2);

   --  State for the LF clock.
   type LFCLKSTAT_STATE_Field is
     (
      --  LFCLK clock not running.
      Notrunning,
      --  LFCLK clock running.
      Running)
     with Size => 1;
   for LFCLKSTAT_STATE_Field use
     (Notrunning => 0,
      Running => 1);

   --  Low frequency clock status.
   type LFCLKSTAT_Register is record
      --  Read-only. Active clock source for the LF clock.
      SRC            : LFCLKSTAT_SRC_Field;
      --  unspecified
      Reserved_2_15  : HAL.UInt14;
      --  Read-only. State for the LF clock.
      STATE          : LFCLKSTAT_STATE_Field;
      --  unspecified
      Reserved_17_31 : HAL.UInt15;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFCLKSTAT_Register use record
      SRC            at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      STATE          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  Clock source for the LFCLK clock, set when task LKCLKSTART is triggered.
   type LFCLKSRCCOPY_SRC_Field is
     (
      --  Internal 32KiHz RC oscillator.
      Rc,
      --  External 32KiHz crystal.
      Xtal,
      --  Internal 32KiHz synthesizer from HFCLK system clock.
      Synth)
     with Size => 2;
   for LFCLKSRCCOPY_SRC_Field use
     (Rc => 0,
      Xtal => 1,
      Synth => 2);

   --  Clock source for the LFCLK clock, set when task LKCLKSTART is triggered.
   type LFCLKSRCCOPY_Register is record
      --  Read-only. Clock source for the LFCLK clock, set when task LKCLKSTART
      --  is triggered.
      SRC           : LFCLKSRCCOPY_SRC_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFCLKSRCCOPY_Register use record
      SRC           at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Clock source.
   type LFCLKSRC_SRC_Field is
     (
      --  Internal 32KiHz RC oscillator.
      Rc,
      --  External 32KiHz crystal.
      Xtal,
      --  Internal 32KiHz synthesizer from HFCLK system clock.
      Synth)
     with Size => 2;
   for LFCLKSRC_SRC_Field use
     (Rc => 0,
      Xtal => 1,
      Synth => 2);

   --  Clock source for the LFCLK clock.
   type LFCLKSRC_Register is record
      --  Clock source.
      SRC           : LFCLKSRC_SRC_Field := NRF51_SVD.CLOCK.Rc;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFCLKSRC_Register use record
      SRC           at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype CTIV_CTIV_Field is HAL.UInt7;

   --  Calibration timer interval.
   type CTIV_Register is record
      --  Calibration timer interval in 0.25s resolution.
      CTIV          : CTIV_CTIV_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTIV_Register use record
      CTIV          at 0 range 0 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  External Xtal frequency selection.
   type XTALFREQ_XTALFREQ_Field is
     (
      --  32MHz xtal is used as source for the HFCLK oscillator.
      XTALFREQ_XTALFREQ_Field_32Mhz,
      --  16MHz xtal is used as source for the HFCLK oscillator.
      XTALFREQ_XTALFREQ_Field_16Mhz)
     with Size => 8;
   for XTALFREQ_XTALFREQ_Field use
     (XTALFREQ_XTALFREQ_Field_32Mhz => 0,
      XTALFREQ_XTALFREQ_Field_16Mhz => 255);

   --  Crystal frequency.
   type XTALFREQ_Register is record
      --  External Xtal frequency selection.
      XTALFREQ      : XTALFREQ_XTALFREQ_Field :=
                       NRF51_SVD.CLOCK.XTALFREQ_XTALFREQ_Field_16Mhz;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#FFFFFF#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for XTALFREQ_Register use record
      XTALFREQ      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Clock control.
   type CLOCK_Peripheral is record
      --  Start HFCLK clock source.
      TASKS_HFCLKSTART    : aliased HAL.UInt32;
      --  Stop HFCLK clock source.
      TASKS_HFCLKSTOP     : aliased HAL.UInt32;
      --  Start LFCLK clock source.
      TASKS_LFCLKSTART    : aliased HAL.UInt32;
      --  Stop LFCLK clock source.
      TASKS_LFCLKSTOP     : aliased HAL.UInt32;
      --  Start calibration of LFCLK RC oscillator.
      TASKS_CAL           : aliased HAL.UInt32;
      --  Start calibration timer.
      TASKS_CTSTART       : aliased HAL.UInt32;
      --  Stop calibration timer.
      TASKS_CTSTOP        : aliased HAL.UInt32;
      --  HFCLK oscillator started.
      EVENTS_HFCLKSTARTED : aliased HAL.UInt32;
      --  LFCLK oscillator started.
      EVENTS_LFCLKSTARTED : aliased HAL.UInt32;
      --  Calibration of LFCLK RC oscillator completed.
      EVENTS_DONE         : aliased HAL.UInt32;
      --  Calibration timer timeout.
      EVENTS_CTTO         : aliased HAL.UInt32;
      --  Interrupt enable set register.
      INTENSET            : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR            : aliased INTENCLR_Register;
      --  Task HFCLKSTART trigger status.
      HFCLKRUN            : aliased HFCLKRUN_Register;
      --  High frequency clock status.
      HFCLKSTAT           : aliased HFCLKSTAT_Register;
      --  Task LFCLKSTART triggered status.
      LFCLKRUN            : aliased LFCLKRUN_Register;
      --  Low frequency clock status.
      LFCLKSTAT           : aliased LFCLKSTAT_Register;
      --  Clock source for the LFCLK clock, set when task LKCLKSTART is
      --  triggered.
      LFCLKSRCCOPY        : aliased LFCLKSRCCOPY_Register;
      --  Clock source for the LFCLK clock.
      LFCLKSRC            : aliased LFCLKSRC_Register;
      --  Calibration timer interval.
      CTIV                : aliased CTIV_Register;
      --  Crystal frequency.
      XTALFREQ            : aliased XTALFREQ_Register;
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
      XTALFREQ            at 16#550# range 0 .. 31;
   end record;

   --  Clock control.
   CLOCK_Periph : aliased CLOCK_Peripheral
     with Import, Address => System'To_Address (16#40000000#);

end NRF51_SVD.CLOCK;
