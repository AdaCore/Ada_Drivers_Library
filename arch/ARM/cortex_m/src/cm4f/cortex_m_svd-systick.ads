--  This spec has been automatically generated from cm4f.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

--  24Bit System Tick Timer for use in RTOS
package Cortex_M_SVD.SysTick is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Enable SysTick Timer
   type CSR_ENABLE_Field is
     (
      --  counter disabled
      Disable,
      --  counter enabled
      Enable)
     with Size => 1;
   for CSR_ENABLE_Field use
     (Disable => 0,
      Enable => 1);

   --  Generate Tick Interrupt
   type CSR_TICKINT_Field is
     (--  Counting down to zero asserts the SysTick exception request
      Disable,
      --  Counting down to zero does not assert the SysTick exception request
      Enable)
     with Size => 1;
   for CSR_TICKINT_Field use
     (Disable => 0,
      Enable => 1);

   --  Source to count from
   type CSR_CLKSOURCE_Field is
     (
      --  External Clock
      External_Clk,
      --  CPU Clock
      Cpu_Clk)
     with Size => 1;
   for CSR_CLKSOURCE_Field use
     (External_Clk => 0,
      Cpu_Clk => 1);

   --  SysTick Control and Status Register
   type SYST_CSR_Register is record
      --  Enable SysTick Timer
      ENABLE         : CSR_ENABLE_Field := Cortex_M_SVD.SysTick.Disable;
      --  Generate Tick Interrupt
      TICKINT        : CSR_TICKINT_Field := Cortex_M_SVD.SysTick.Disable;
      --  Source to count from
      CLKSOURCE      : CSR_CLKSOURCE_Field := Cortex_M_SVD.SysTick.Cpu_Clk;
      --  unspecified
      Reserved_3_15  : HAL.UInt13 := 16#0#;
      --  SysTick counted to zero
      COUNTFLAG      : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYST_CSR_Register use record
      ENABLE         at 0 range 0 .. 0;
      TICKINT        at 0 range 1 .. 1;
      CLKSOURCE      at 0 range 2 .. 2;
      Reserved_3_15  at 0 range 3 .. 15;
      COUNTFLAG      at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype SYST_RVR_RELOAD_Field is HAL.UInt24;

   --  SysTick Reload Value Register
   type SYST_RVR_Register is record
      --  Value to auto reload SysTick after reaching zero
      RELOAD         : SYST_RVR_RELOAD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYST_RVR_Register use record
      RELOAD         at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype SYST_CVR_CURRENT_Field is HAL.UInt24;

   --  SysTick Current Value Register
   type SYST_CVR_Register is record
      --  Current value
      CURRENT        : SYST_CVR_CURRENT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYST_CVR_Register use record
      CURRENT        at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype SYST_CALIB_TENMS_Field is HAL.UInt24;

   --  Clock Skew
   type CALIB_SKEW_Field is
     (
      --  10ms calibration value is exact
      Exact,
      --  10ms calibration value is inexact, because of the clock frequency
      Inexact)
     with Size => 1;
   for CALIB_SKEW_Field use
     (Exact => 0,
      Inexact => 1);

   --  No Ref
   type CALIB_NOREF_Field is
     (
      --  Ref Clk available
      Ref_Clk_Available,
      --  Ref Clk not available
      Ref_Clk_Unavailable)
     with Size => 1;
   for CALIB_NOREF_Field use
     (Ref_Clk_Available => 0,
      Ref_Clk_Unavailable => 1);

   --  SysTick Calibration Value Register
   type SYST_CALIB_Register is record
      --  Read-only. Reload value to use for 10ms timing
      TENMS          : SYST_CALIB_TENMS_Field;
      --  unspecified
      Reserved_24_29 : HAL.UInt6;
      --  Read-only. Clock Skew
      SKEW           : CALIB_SKEW_Field;
      --  Read-only. No Ref
      NOREF          : CALIB_NOREF_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYST_CALIB_Register use record
      TENMS          at 0 range 0 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      SKEW           at 0 range 30 .. 30;
      NOREF          at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  24Bit System Tick Timer for use in RTOS
   type SysTick_Peripheral is record
      --  SysTick Control and Status Register
      CSR   : aliased SYST_CSR_Register;
      --  SysTick Reload Value Register
      RVR   : aliased SYST_RVR_Register;
      --  SysTick Current Value Register
      CVR   : aliased SYST_CVR_Register;
      --  SysTick Calibration Value Register
      CALIB : aliased SYST_CALIB_Register;
   end record
     with Volatile;

   for SysTick_Peripheral use record
      CSR   at 16#0# range 0 .. 31;
      RVR   at 16#4# range 0 .. 31;
      CVR   at 16#8# range 0 .. 31;
      CALIB at 16#C# range 0 .. 31;
   end record;

   --  24Bit System Tick Timer for use in RTOS
   SysTick_Periph : aliased SysTick_Peripheral
     with Import, Address => SysTick_Base;

end Cortex_M_SVD.SysTick;
