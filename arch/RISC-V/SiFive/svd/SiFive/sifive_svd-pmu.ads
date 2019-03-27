--  This spec has been automatically generated from SiFive.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SiFive_SVD.PMU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype WAKEUPI_DELAY_Field is HAL.UInt4;

   --  Wakeup program instructions.
   type WAKEUPI_Register is record
      DELAY_k       : WAKEUPI_DELAY_Field := 16#0#;
      PMU_OUT_0     : Boolean := False;
      PMU_OUT_1     : Boolean := False;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      CORERST       : Boolean := False;
      HFCLKRST      : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WAKEUPI_Register use record
      DELAY_k       at 0 range 0 .. 3;
      PMU_OUT_0     at 0 range 4 .. 4;
      PMU_OUT_1     at 0 range 5 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      CORERST       at 0 range 7 .. 7;
      HFCLKRST      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  Wakeup program instructions.
   type WAKEUPI_Registers is array (0 .. 7) of WAKEUPI_Register;

   subtype SLEEPI_DELAY_Field is HAL.UInt4;

   --  Sleep program instructions.
   type SLEEPI_Register is record
      DELAY_k       : SLEEPI_DELAY_Field := 16#0#;
      PMU_OUT_0     : Boolean := False;
      PMU_OUT_1     : Boolean := False;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      CORERST       : Boolean := False;
      HFCLKRST      : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SLEEPI_Register use record
      DELAY_k       at 0 range 0 .. 3;
      PMU_OUT_0     at 0 range 4 .. 4;
      PMU_OUT_1     at 0 range 5 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      CORERST       at 0 range 7 .. 7;
      HFCLKRST      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  Sleep program instructions.
   type SLEEPI_Registers is array (0 .. 7) of SLEEPI_Register;

   --  PMU interrupt enables.
   type IE_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      RTC           : Boolean := False;
      DWAKEUP       : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IE_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      RTC           at 0 range 1 .. 1;
      DWAKEUP       at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   type CAUSE_Wakeup_Cause_Values is
     (--  Reset.
      Reset,
      --  RTC Wakeup.
      Rtc,
      --  Digital input wakeup.
      Dwakeup)
     with Size => 2;
   for CAUSE_Wakeup_Cause_Values use
     (Reset => 0,
      Rtc => 1,
      Dwakeup => 2);

   type CAUSE_Reset_Cause_Values is
     (--  Reset value for the field
      Cause_Reset_Cause_Values_Reset,
      --  External reset.
      External,
      --  Watchdog timer reset.
      Watchdog)
     with Size => 2;
   for CAUSE_Reset_Cause_Values use
     (Cause_Reset_Cause_Values_Reset => 0,
      External => 1,
      Watchdog => 2);

   --  PMU wakeup cause.
   type CAUSE_Register is record
      WAKEUP_CAUSE   : CAUSE_Wakeup_Cause_Values := SiFive_SVD.PMU.Reset;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      RESET_CAUSE    : CAUSE_Reset_Cause_Values :=
                        Cause_Reset_Cause_Values_Reset;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAUSE_Register use record
      WAKEUP_CAUSE   at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      RESET_CAUSE    at 0 range 8 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Power Management Unit.
   type PMU_Peripheral is record
      --  Wakeup program instructions.
      WAKEUPI : aliased WAKEUPI_Registers;
      --  Sleep program instructions.
      SLEEPI  : aliased SLEEPI_Registers;
      --  PMU interrupt enables.
      IE      : aliased IE_Register;
      --  PMU wakeup cause.
      CAUSE   : aliased CAUSE_Register;
      --  Initiate sleep sequence.
      SLEEP   : aliased HAL.UInt32;
      --  PMU key register.
      KEY     : aliased HAL.UInt32;
   end record
     with Volatile;

   for PMU_Peripheral use record
      WAKEUPI at 16#0# range 0 .. 255;
      SLEEPI  at 16#20# range 0 .. 255;
      IE      at 16#40# range 0 .. 31;
      CAUSE   at 16#44# range 0 .. 31;
      SLEEP   at 16#48# range 0 .. 31;
      KEY     at 16#4C# range 0 .. 31;
   end record;

   --  Power Management Unit.
   PMU_Periph : aliased PMU_Peripheral
     with Import, Address => PMU_Base;

end SiFive_SVD.PMU;
