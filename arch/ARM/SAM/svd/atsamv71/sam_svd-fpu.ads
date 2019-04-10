--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.FPU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Floating-point Context Control Register
   type FPU_FPCCR_Register is record
      --  Lazy state preservation is active. Floating-point stack frame has
      --  been allocated but saving state to it has been deferred.
      LSPACT        : Boolean := False;
      --  Privilege level was user when the floating-point stack frame was
      --  allocated.
      USER          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Mode was Thread Mode when the floating-point stack frame was
      --  allocated.
      THREAD        : Boolean := False;
      --  Priority permitted setting the HardFault handler to the pending state
      --  when the floating-point stack frame was allocated.
      HFRDY         : Boolean := False;
      --  MemManage is enabled and priority permitted setting the MemManage
      --  handler to the pending state when the floating-point stack frame was
      --  allocated.
      MMRDY         : Boolean := False;
      --  BusFault is enabled and priority permitted setting the BusFault
      --  handler to the pending state when the floating-point stack frame was
      --  allocated.
      BFRDY         : Boolean := False;
      --  unspecified
      Reserved_7_7  : HAL.Bit := 16#0#;
      --  DebugMonitor is enabled and priority permits setting MON_PEND when
      --  the floating-point stack frame was allocated.
      MONRDY        : Boolean := False;
      --  unspecified
      Reserved_9_29 : HAL.UInt21 := 16#0#;
      --  Enable automatic lazy state preservation for floating-point context.
      LSPEN         : Boolean := False;
      --  Enables CONTROL.FPCA setting on execution of a floating-point
      --  instruction. This results in automatic hardware state preservation
      --  and restoration, for floating-point context, on exception entry and
      --  exit.
      ASPEN         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FPU_FPCCR_Register use record
      LSPACT        at 0 range 0 .. 0;
      USER          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      THREAD        at 0 range 3 .. 3;
      HFRDY         at 0 range 4 .. 4;
      MMRDY         at 0 range 5 .. 5;
      BFRDY         at 0 range 6 .. 6;
      Reserved_7_7  at 0 range 7 .. 7;
      MONRDY        at 0 range 8 .. 8;
      Reserved_9_29 at 0 range 9 .. 29;
      LSPEN         at 0 range 30 .. 30;
      ASPEN         at 0 range 31 .. 31;
   end record;

   subtype FPU_FPCAR_ADDRESS_Field is HAL.UInt29;

   --  Floating-point Context Address Register
   type FPU_FPCAR_Register is record
      --  unspecified
      Reserved_0_2 : HAL.UInt3 := 16#0#;
      --  The location of the unpopulated floating-point register space
      --  allocated on an exception stack frame.
      ADDRESS      : FPU_FPCAR_ADDRESS_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FPU_FPCAR_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      ADDRESS      at 0 range 3 .. 31;
   end record;

   subtype FPU_FPDSCR_RMode_Field is HAL.UInt2;

   --  Floating-point Default Status Control Register
   type FPU_FPDSCR_Register is record
      --  unspecified
      Reserved_0_21  : HAL.UInt22 := 16#0#;
      --  Default value for FPSCR.RMode.
      RMode          : FPU_FPDSCR_RMode_Field := 16#0#;
      --  Default value for FPSCR.FZ.
      FZ             : Boolean := False;
      --  Default value for FPSCR.DN.
      DN             : Boolean := False;
      --  Default value for FPSCR.AHP.
      AHP            : Boolean := False;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FPU_FPDSCR_Register use record
      Reserved_0_21  at 0 range 0 .. 21;
      RMode          at 0 range 22 .. 23;
      FZ             at 0 range 24 .. 24;
      DN             at 0 range 25 .. 25;
      AHP            at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Floating Point Unit Registers
   type FPU_Peripheral is record
      --  Floating-point Context Control Register
      FPCCR  : aliased FPU_FPCCR_Register;
      --  Floating-point Context Address Register
      FPCAR  : aliased FPU_FPCAR_Register;
      --  Floating-point Default Status Control Register
      FPDSCR : aliased FPU_FPDSCR_Register;
   end record
     with Volatile;

   for FPU_Peripheral use record
      FPCCR  at 16#0# range 0 .. 31;
      FPCAR  at 16#4# range 0 .. 31;
      FPDSCR at 16#8# range 0 .. 31;
   end record;

   --  Floating Point Unit Registers
   FPU_Periph : aliased FPU_Peripheral
     with Import, Address => System'To_Address (16#E000EF34#);

end SAM_SVD.FPU;
