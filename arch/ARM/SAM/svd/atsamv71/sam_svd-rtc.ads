--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.RTC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Time Event Selection
   type RTC_CR_TIMEVSEL_Field is
     (
      --  Minute change
      Minute,
      --  Hour change
      Hour,
      --  Every day at midnight
      Midnight,
      --  Every day at noon
      Noon)
     with Size => 2;
   for RTC_CR_TIMEVSEL_Field use
     (Minute => 0,
      Hour => 1,
      Midnight => 2,
      Noon => 3);

   --  Calendar Event Selection
   type RTC_CR_CALEVSEL_Field is
     (
      --  Week change (every Monday at time 00:00:00)
      Week,
      --  Month change (every 01 of each month at time 00:00:00)
      Month,
      --  Year change (every January 1 at time 00:00:00)
      Year)
     with Size => 2;
   for RTC_CR_CALEVSEL_Field use
     (Week => 0,
      Month => 1,
      Year => 2);

   --  Control Register
   type RTC_RTC_CR_Register is record
      --  Update Request Time Register
      UPDTIM         : Boolean := False;
      --  Update Request Calendar Register
      UPDCAL         : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Time Event Selection
      TIMEVSEL       : RTC_CR_TIMEVSEL_Field := SAM_SVD.RTC.Minute;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  Calendar Event Selection
      CALEVSEL       : RTC_CR_CALEVSEL_Field := SAM_SVD.RTC.Week;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_CR_Register use record
      UPDTIM         at 0 range 0 .. 0;
      UPDCAL         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      TIMEVSEL       at 0 range 8 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      CALEVSEL       at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype RTC_RTC_MR_CORRECTION_Field is HAL.UInt7;

   --  RTCOUT0 OutputSource Selection
   type RTC_MR_OUT0_Field is
     (
      --  No waveform, stuck at '0'
      No_Wave,
      --  1 Hz square wave
      Freq1Hz,
      --  32 Hz square wave
      Freq32Hz,
      --  64 Hz square wave
      Freq64Hz,
      --  512 Hz square wave
      Freq512Hz,
      --  Output toggles when alarm flag rises
      Alarm_Toggle,
      --  Output is a copy of the alarm flag
      Alarm_Flag,
      --  Duty cycle programmable pulse
      Prog_Pulse)
     with Size => 3;
   for RTC_MR_OUT0_Field use
     (No_Wave => 0,
      Freq1Hz => 1,
      Freq32Hz => 2,
      Freq64Hz => 3,
      Freq512Hz => 4,
      Alarm_Toggle => 5,
      Alarm_Flag => 6,
      Prog_Pulse => 7);

   --  RTCOUT1 Output Source Selection
   type RTC_MR_OUT1_Field is
     (
      --  No waveform, stuck at '0'
      No_Wave,
      --  1 Hz square wave
      Freq1Hz,
      --  32 Hz square wave
      Freq32Hz,
      --  64 Hz square wave
      Freq64Hz,
      --  512 Hz square wave
      Freq512Hz,
      --  Output toggles when alarm flag rises
      Alarm_Toggle,
      --  Output is a copy of the alarm flag
      Alarm_Flag,
      --  Duty cycle programmable pulse
      Prog_Pulse)
     with Size => 3;
   for RTC_MR_OUT1_Field use
     (No_Wave => 0,
      Freq1Hz => 1,
      Freq32Hz => 2,
      Freq64Hz => 3,
      Freq512Hz => 4,
      Alarm_Toggle => 5,
      Alarm_Flag => 6,
      Prog_Pulse => 7);

   --  High Duration of the Output Pulse
   type RTC_MR_THIGH_Field is
     (
      --  31.2 ms
      H_31Ms,
      --  15.6 ms
      H_16Ms,
      --  3.91 ms
      H_4Ms,
      --  976 us
      H_976Us,
      --  488 us
      H_488Us,
      --  122 us
      H_122Us,
      --  30.5 us
      H_30Us,
      --  15.2 us
      H_15Us)
     with Size => 3;
   for RTC_MR_THIGH_Field use
     (H_31Ms => 0,
      H_16Ms => 1,
      H_4Ms => 2,
      H_976Us => 3,
      H_488Us => 4,
      H_122Us => 5,
      H_30Us => 6,
      H_15Us => 7);

   --  Period of the Output Pulse
   type RTC_MR_TPERIOD_Field is
     (
      --  1 second
      P_1S,
      --  500 ms
      P_500Ms,
      --  250 ms
      P_250Ms,
      --  125 ms
      P_125Ms)
     with Size => 2;
   for RTC_MR_TPERIOD_Field use
     (P_1S => 0,
      P_500Ms => 1,
      P_250Ms => 2,
      P_125Ms => 3);

   --  Mode Register
   type RTC_RTC_MR_Register is record
      --  12-/24-hour Mode
      HRMOD          : Boolean := False;
      --  PERSIAN Calendar
      PERSIAN        : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  NEGative PPM Correction
      NEGPPM         : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Slow Clock Correction
      CORRECTION     : RTC_RTC_MR_CORRECTION_Field := 16#0#;
      --  HIGH PPM Correction
      HIGHPPM        : Boolean := False;
      --  RTCOUT0 OutputSource Selection
      OUT0           : RTC_MR_OUT0_Field := SAM_SVD.RTC.No_Wave;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  RTCOUT1 Output Source Selection
      OUT1           : RTC_MR_OUT1_Field := SAM_SVD.RTC.No_Wave;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  High Duration of the Output Pulse
      THIGH          : RTC_MR_THIGH_Field := SAM_SVD.RTC.H_31Ms;
      --  unspecified
      Reserved_27_27 : HAL.Bit := 16#0#;
      --  Period of the Output Pulse
      TPERIOD        : RTC_MR_TPERIOD_Field := SAM_SVD.RTC.P_1S;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_MR_Register use record
      HRMOD          at 0 range 0 .. 0;
      PERSIAN        at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      NEGPPM         at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      CORRECTION     at 0 range 8 .. 14;
      HIGHPPM        at 0 range 15 .. 15;
      OUT0           at 0 range 16 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      OUT1           at 0 range 20 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      THIGH          at 0 range 24 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      TPERIOD        at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype RTC_RTC_TIMR_SEC_Field is HAL.UInt7;
   subtype RTC_RTC_TIMR_MIN_Field is HAL.UInt7;
   subtype RTC_RTC_TIMR_HOUR_Field is HAL.UInt6;

   --  Time Register
   type RTC_RTC_TIMR_Register is record
      --  Current Second
      SEC            : RTC_RTC_TIMR_SEC_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Current Minute
      MIN            : RTC_RTC_TIMR_MIN_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Current Hour
      HOUR           : RTC_RTC_TIMR_HOUR_Field := 16#0#;
      --  Ante Meridiem Post Meridiem Indicator
      AMPM           : Boolean := False;
      --  unspecified
      Reserved_23_31 : HAL.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_TIMR_Register use record
      SEC            at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MIN            at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HOUR           at 0 range 16 .. 21;
      AMPM           at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype RTC_RTC_CALR_CENT_Field is HAL.UInt7;
   subtype RTC_RTC_CALR_YEAR_Field is HAL.UInt8;
   subtype RTC_RTC_CALR_MONTH_Field is HAL.UInt5;
   subtype RTC_RTC_CALR_DAY_Field is HAL.UInt3;
   subtype RTC_RTC_CALR_DATE_Field is HAL.UInt6;

   --  Calendar Register
   type RTC_RTC_CALR_Register is record
      --  Current Century
      CENT           : RTC_RTC_CALR_CENT_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Current Year
      YEAR           : RTC_RTC_CALR_YEAR_Field := 16#0#;
      --  Current Month
      MONTH          : RTC_RTC_CALR_MONTH_Field := 16#0#;
      --  Current Day in Current Week
      DAY            : RTC_RTC_CALR_DAY_Field := 16#0#;
      --  Current Day in Current Month
      DATE           : RTC_RTC_CALR_DATE_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_CALR_Register use record
      CENT           at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      YEAR           at 0 range 8 .. 15;
      MONTH          at 0 range 16 .. 20;
      DAY            at 0 range 21 .. 23;
      DATE           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype RTC_RTC_TIMALR_SEC_Field is HAL.UInt7;
   subtype RTC_RTC_TIMALR_MIN_Field is HAL.UInt7;
   subtype RTC_RTC_TIMALR_HOUR_Field is HAL.UInt6;

   --  Time Alarm Register
   type RTC_RTC_TIMALR_Register is record
      --  Second Alarm
      SEC            : RTC_RTC_TIMALR_SEC_Field := 16#0#;
      --  Second Alarm Enable
      SECEN          : Boolean := False;
      --  Minute Alarm
      MIN            : RTC_RTC_TIMALR_MIN_Field := 16#0#;
      --  Minute Alarm Enable
      MINEN          : Boolean := False;
      --  Hour Alarm
      HOUR           : RTC_RTC_TIMALR_HOUR_Field := 16#0#;
      --  AM/PM Indicator
      AMPM           : Boolean := False;
      --  Hour Alarm Enable
      HOUREN         : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_TIMALR_Register use record
      SEC            at 0 range 0 .. 6;
      SECEN          at 0 range 7 .. 7;
      MIN            at 0 range 8 .. 14;
      MINEN          at 0 range 15 .. 15;
      HOUR           at 0 range 16 .. 21;
      AMPM           at 0 range 22 .. 22;
      HOUREN         at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RTC_RTC_CALALR_MONTH_Field is HAL.UInt5;
   subtype RTC_RTC_CALALR_DATE_Field is HAL.UInt6;

   --  Calendar Alarm Register
   type RTC_RTC_CALALR_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  Month Alarm
      MONTH          : RTC_RTC_CALALR_MONTH_Field := 16#0#;
      --  unspecified
      Reserved_21_22 : HAL.UInt2 := 16#0#;
      --  Month Alarm Enable
      MTHEN          : Boolean := False;
      --  Date Alarm
      DATE           : RTC_RTC_CALALR_DATE_Field := 16#0#;
      --  unspecified
      Reserved_30_30 : HAL.Bit := 16#0#;
      --  Date Alarm Enable
      DATEEN         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_CALALR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      MONTH          at 0 range 16 .. 20;
      Reserved_21_22 at 0 range 21 .. 22;
      MTHEN          at 0 range 23 .. 23;
      DATE           at 0 range 24 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      DATEEN         at 0 range 31 .. 31;
   end record;

   --  Acknowledge for Update
   type RTC_SR_ACKUPD_Field is
     (
      --  Time and calendar registers cannot be updated.
      Freerun,
      --  Time and calendar registers can be updated.
      Update)
     with Size => 1;
   for RTC_SR_ACKUPD_Field use
     (Freerun => 0,
      Update => 1);

   --  Alarm Flag
   type RTC_SR_ALARM_Field is
     (
      --  No alarm matching condition occurred.
      No_Alarmevent,
      --  An alarm matching condition has occurred.
      Alarmevent)
     with Size => 1;
   for RTC_SR_ALARM_Field use
     (No_Alarmevent => 0,
      Alarmevent => 1);

   --  Second Event
   type RTC_SR_SEC_Field is
     (
      --  No second event has occurred since the last clear.
      No_Secevent,
      --  At least one second event has occurred since the last clear.
      Secevent)
     with Size => 1;
   for RTC_SR_SEC_Field use
     (No_Secevent => 0,
      Secevent => 1);

   --  Time Event
   type RTC_SR_TIMEV_Field is
     (
      --  No time event has occurred since the last clear.
      No_Timevent,
      --  At least one time event has occurred since the last clear.
      Timevent)
     with Size => 1;
   for RTC_SR_TIMEV_Field use
     (No_Timevent => 0,
      Timevent => 1);

   --  Calendar Event
   type RTC_SR_CALEV_Field is
     (
      --  No calendar event has occurred since the last clear.
      No_Calevent,
      --  At least one calendar event has occurred since the last clear.
      Calevent)
     with Size => 1;
   for RTC_SR_CALEV_Field use
     (No_Calevent => 0,
      Calevent => 1);

   --  Time and/or Date Free Running Error
   type RTC_SR_TDERR_Field is
     (
      --  The internal free running counters are carrying valid values since
      --  the last read of the Status Register (RTC_SR).
      Correct,
      --  The internal free running counters have been corrupted (invalid date
      --  or time, non-BCD values) since the last read and/or they are still
      --  invalid.
      Err_Timedate)
     with Size => 1;
   for RTC_SR_TDERR_Field use
     (Correct => 0,
      Err_Timedate => 1);

   --  Status Register
   type RTC_RTC_SR_Register is record
      --  Read-only. Acknowledge for Update
      ACKUPD        : RTC_SR_ACKUPD_Field;
      --  Read-only. Alarm Flag
      ALARM         : RTC_SR_ALARM_Field;
      --  Read-only. Second Event
      SEC           : RTC_SR_SEC_Field;
      --  Read-only. Time Event
      TIMEV         : RTC_SR_TIMEV_Field;
      --  Read-only. Calendar Event
      CALEV         : RTC_SR_CALEV_Field;
      --  Read-only. Time and/or Date Free Running Error
      TDERR         : RTC_SR_TDERR_Field;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_SR_Register use record
      ACKUPD        at 0 range 0 .. 0;
      ALARM         at 0 range 1 .. 1;
      SEC           at 0 range 2 .. 2;
      TIMEV         at 0 range 3 .. 3;
      CALEV         at 0 range 4 .. 4;
      TDERR         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Status Clear Command Register
   type RTC_RTC_SCCR_Register is record
      --  Write-only. Acknowledge Clear
      ACKCLR        : Boolean := False;
      --  Write-only. Alarm Clear
      ALRCLR        : Boolean := False;
      --  Write-only. Second Clear
      SECCLR        : Boolean := False;
      --  Write-only. Time Clear
      TIMCLR        : Boolean := False;
      --  Write-only. Calendar Clear
      CALCLR        : Boolean := False;
      --  Write-only. Time and/or Date Free Running Error Clear
      TDERRCLR      : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_SCCR_Register use record
      ACKCLR        at 0 range 0 .. 0;
      ALRCLR        at 0 range 1 .. 1;
      SECCLR        at 0 range 2 .. 2;
      TIMCLR        at 0 range 3 .. 3;
      CALCLR        at 0 range 4 .. 4;
      TDERRCLR      at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Interrupt Enable Register
   type RTC_RTC_IER_Register is record
      --  Write-only. Acknowledge Update Interrupt Enable
      ACKEN         : Boolean := False;
      --  Write-only. Alarm Interrupt Enable
      ALREN         : Boolean := False;
      --  Write-only. Second Event Interrupt Enable
      SECEN         : Boolean := False;
      --  Write-only. Time Event Interrupt Enable
      TIMEN         : Boolean := False;
      --  Write-only. Calendar Event Interrupt Enable
      CALEN         : Boolean := False;
      --  Write-only. Time and/or Date Error Interrupt Enable
      TDERREN       : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_IER_Register use record
      ACKEN         at 0 range 0 .. 0;
      ALREN         at 0 range 1 .. 1;
      SECEN         at 0 range 2 .. 2;
      TIMEN         at 0 range 3 .. 3;
      CALEN         at 0 range 4 .. 4;
      TDERREN       at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Interrupt Disable Register
   type RTC_RTC_IDR_Register is record
      --  Write-only. Acknowledge Update Interrupt Disable
      ACKDIS        : Boolean := False;
      --  Write-only. Alarm Interrupt Disable
      ALRDIS        : Boolean := False;
      --  Write-only. Second Event Interrupt Disable
      SECDIS        : Boolean := False;
      --  Write-only. Time Event Interrupt Disable
      TIMDIS        : Boolean := False;
      --  Write-only. Calendar Event Interrupt Disable
      CALDIS        : Boolean := False;
      --  Write-only. Time and/or Date Error Interrupt Disable
      TDERRDIS      : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_IDR_Register use record
      ACKDIS        at 0 range 0 .. 0;
      ALRDIS        at 0 range 1 .. 1;
      SECDIS        at 0 range 2 .. 2;
      TIMDIS        at 0 range 3 .. 3;
      CALDIS        at 0 range 4 .. 4;
      TDERRDIS      at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Interrupt Mask Register
   type RTC_RTC_IMR_Register is record
      --  Read-only. Acknowledge Update Interrupt Mask
      ACK           : Boolean;
      --  Read-only. Alarm Interrupt Mask
      ALR           : Boolean;
      --  Read-only. Second Event Interrupt Mask
      SEC           : Boolean;
      --  Read-only. Time Event Interrupt Mask
      TIM           : Boolean;
      --  Read-only. Calendar Event Interrupt Mask
      CAL           : Boolean;
      --  Read-only. Time and/or Date Error Mask
      TDERR         : Boolean;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_IMR_Register use record
      ACK           at 0 range 0 .. 0;
      ALR           at 0 range 1 .. 1;
      SEC           at 0 range 2 .. 2;
      TIM           at 0 range 3 .. 3;
      CAL           at 0 range 4 .. 4;
      TDERR         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  Valid Entry Register
   type RTC_RTC_VER_Register is record
      --  Read-only. Non-valid Time
      NVTIM         : Boolean;
      --  Read-only. Non-valid Calendar
      NVCAL         : Boolean;
      --  Read-only. Non-valid Time Alarm
      NVTIMALR      : Boolean;
      --  Read-only. Non-valid Calendar Alarm
      NVCALALR      : Boolean;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_VER_Register use record
      NVTIM         at 0 range 0 .. 0;
      NVCAL         at 0 range 1 .. 1;
      NVTIMALR      at 0 range 2 .. 2;
      NVCALALR      at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype RTC_RTC_VERSION_VERSION_Field is HAL.UInt12;
   subtype RTC_RTC_VERSION_MFN_Field is HAL.UInt3;

   --  Version Register
   type RTC_RTC_VERSION_Register is record
      --  Read-only. Version of the Hardware Module
      VERSION        : RTC_RTC_VERSION_VERSION_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : RTC_RTC_VERSION_MFN_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_RTC_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Real-time Clock
   type RTC_Peripheral is record
      --  Control Register
      RTC_CR      : aliased RTC_RTC_CR_Register;
      --  Mode Register
      RTC_MR      : aliased RTC_RTC_MR_Register;
      --  Time Register
      RTC_TIMR    : aliased RTC_RTC_TIMR_Register;
      --  Calendar Register
      RTC_CALR    : aliased RTC_RTC_CALR_Register;
      --  Time Alarm Register
      RTC_TIMALR  : aliased RTC_RTC_TIMALR_Register;
      --  Calendar Alarm Register
      RTC_CALALR  : aliased RTC_RTC_CALALR_Register;
      --  Status Register
      RTC_SR      : aliased RTC_RTC_SR_Register;
      --  Status Clear Command Register
      RTC_SCCR    : aliased RTC_RTC_SCCR_Register;
      --  Interrupt Enable Register
      RTC_IER     : aliased RTC_RTC_IER_Register;
      --  Interrupt Disable Register
      RTC_IDR     : aliased RTC_RTC_IDR_Register;
      --  Interrupt Mask Register
      RTC_IMR     : aliased RTC_RTC_IMR_Register;
      --  Valid Entry Register
      RTC_VER     : aliased RTC_RTC_VER_Register;
      --  Version Register
      RTC_VERSION : aliased RTC_RTC_VERSION_Register;
   end record
     with Volatile;

   for RTC_Peripheral use record
      RTC_CR      at 16#0# range 0 .. 31;
      RTC_MR      at 16#4# range 0 .. 31;
      RTC_TIMR    at 16#8# range 0 .. 31;
      RTC_CALR    at 16#C# range 0 .. 31;
      RTC_TIMALR  at 16#10# range 0 .. 31;
      RTC_CALALR  at 16#14# range 0 .. 31;
      RTC_SR      at 16#18# range 0 .. 31;
      RTC_SCCR    at 16#1C# range 0 .. 31;
      RTC_IER     at 16#20# range 0 .. 31;
      RTC_IDR     at 16#24# range 0 .. 31;
      RTC_IMR     at 16#28# range 0 .. 31;
      RTC_VER     at 16#2C# range 0 .. 31;
      RTC_VERSION at 16#FC# range 0 .. 31;
   end record;

   --  Real-time Clock
   RTC_Periph : aliased RTC_Peripheral
     with Import, Address => System'To_Address (16#400E1860#);

end SAM_SVD.RTC;
