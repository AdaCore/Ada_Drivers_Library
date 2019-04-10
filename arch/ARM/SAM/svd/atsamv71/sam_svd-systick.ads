--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.SysTick is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  no description available
   type CSR_ENABLE_Field is
     (
      --  counter disabled
      Value_0,
      --  counter enabled
      Value_1)
     with Size => 1;
   for CSR_ENABLE_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CSR_TICKINT_Field is
     (
      --  counting down to 0 does not assert the SysTick exception request
      Value_0,
      --  counting down to 0 asserts the SysTick exception request
      Value_1)
     with Size => 1;
   for CSR_TICKINT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CSR_CLKSOURCE_Field is
     (
      --  external clock
      Value_0,
      --  processor clock
      Value_1)
     with Size => 1;
   for CSR_CLKSOURCE_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  SysTick Control and Status Register
   type SysTick_CSR_Register is record
      --  no description available
      ENABLE         : CSR_ENABLE_Field := SAM_SVD.SysTick.Value_0;
      --  no description available
      TICKINT        : CSR_TICKINT_Field := SAM_SVD.SysTick.Value_0;
      --  no description available
      CLKSOURCE      : CSR_CLKSOURCE_Field := SAM_SVD.SysTick.Value_0;
      --  unspecified
      Reserved_3_15  : HAL.UInt13 := 16#0#;
      --  no description available
      COUNTFLAG      : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SysTick_CSR_Register use record
      ENABLE         at 0 range 0 .. 0;
      TICKINT        at 0 range 1 .. 1;
      CLKSOURCE      at 0 range 2 .. 2;
      Reserved_3_15  at 0 range 3 .. 15;
      COUNTFLAG      at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype SysTick_RVR_RELOAD_Field is HAL.UInt24;

   --  SysTick Reload Value Register
   type SysTick_RVR_Register is record
      --  Value to load into the SysTick Current Value Register when the
      --  counter reaches 0
      RELOAD         : SysTick_RVR_RELOAD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SysTick_RVR_Register use record
      RELOAD         at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype SysTick_CVR_CURRENT_Field is HAL.UInt24;

   --  SysTick Current Value Register
   type SysTick_CVR_Register is record
      --  Current value at the time the register is accessed
      CURRENT        : SysTick_CVR_CURRENT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SysTick_CVR_Register use record
      CURRENT        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype SysTick_CALIB_TENMS_Field is HAL.UInt24;

   --  no description available
   type CALIB_SKEW_Field is
     (
      --  10ms calibration value is exact
      Value_0,
      --  10ms calibration value is inexact, because of the clock frequency
      Value_1)
     with Size => 1;
   for CALIB_SKEW_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CALIB_NOREF_Field is
     (
      --  The reference clock is provided
      Value_0,
      --  The reference clock is not provided
      Value_1)
     with Size => 1;
   for CALIB_NOREF_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  SysTick Calibration Value Register
   type SysTick_CALIB_Register is record
      --  Reload value to use for 10ms timing
      TENMS          : SysTick_CALIB_TENMS_Field := 16#0#;
      --  unspecified
      Reserved_24_29 : HAL.UInt6 := 16#0#;
      --  no description available
      SKEW           : CALIB_SKEW_Field := SAM_SVD.SysTick.Value_0;
      --  no description available
      NOREF          : CALIB_NOREF_Field := SAM_SVD.SysTick.Value_0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SysTick_CALIB_Register use record
      TENMS          at 0 range 0 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      SKEW           at 0 range 30 .. 30;
      NOREF          at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  System timer
   type SysTick_Peripheral is record
      --  SysTick Control and Status Register
      CSR   : aliased SysTick_CSR_Register;
      --  SysTick Reload Value Register
      RVR   : aliased SysTick_RVR_Register;
      --  SysTick Current Value Register
      CVR   : aliased SysTick_CVR_Register;
      --  SysTick Calibration Value Register
      CALIB : aliased SysTick_CALIB_Register;
   end record
     with Volatile;

   for SysTick_Peripheral use record
      CSR   at 16#0# range 0 .. 31;
      RVR   at 16#4# range 0 .. 31;
      CVR   at 16#8# range 0 .. 31;
      CALIB at 16#C# range 0 .. 31;
   end record;

   --  System timer
   SysTick_Periph : aliased SysTick_Peripheral
     with Import, Address => System'To_Address (16#E000E010#);

end SAM_SVD.SysTick;
