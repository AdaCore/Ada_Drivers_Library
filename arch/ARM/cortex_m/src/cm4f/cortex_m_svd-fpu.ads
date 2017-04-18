--  This spec has been automatically generated from cm4f.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

--  Floating Point Unit
package Cortex_M_SVD.FPU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Access privileges for coprocessor 10.
   type CPACR_CP10_Field is
     (
      --  Any attempted access generates a NOCP UsageFault.
      Access_Denied,
      --  Privileged access only. An unprivileged addess generates a NOCP
      --  UsageFault.
      Privileged,
      --  Full access.
      Full_Access)
     with Size => 2;
   for CPACR_CP10_Field use
     (Access_Denied => 0,
      Privileged => 1,
      Full_Access => 3);

   --  CPACR_CP array
   type CPACR_CP_Field_Array is array (10 .. 11) of CPACR_CP10_Field
     with Component_Size => 2, Size => 4;

   --  Type definition for CPACR_CP
   type CPACR_CP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CP as a value
            Val : HAL.UInt4;
         when True =>
            --  CP as an array
            Arr : CPACR_CP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CPACR_CP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Coprocessor Access Control Register
   type CPACR_Register is record
      --  unspecified
      Reserved_0_19  : HAL.UInt20 := 16#0#;
      --  Access privileges for coprocessor 10.
      CP             : CPACR_CP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPACR_Register use record
      Reserved_0_19  at 0 range 0 .. 19;
      CP             at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Floating-Point Context Control Register
   type FPCCR_Register is record
      --  Lazy state preservation activation.
      LSPACT        : Boolean := False;
      --  Read-only. If set, privilege level was user when the floating-point
      --  stack frame was allocated.
      USER          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Read-only. If set, the mode was Thread Mode when the floating-point
      --  stack frame was allocated.
      THREAD        : Boolean := False;
      --  Read-only. If set, priority permitted setting the HardFault handler
      --  to the pending state when the floating-point stack frame was
      --  allocated.
      HFRDY         : Boolean := False;
      --  Read-only. If set, MemManage is enabled and priority permitted
      --  setting the MemManage handler to the pending state when the
      --  floating-point stack frame was allocated.
      MMRDY         : Boolean := False;
      --  Read-only. If set, BusFault is enabled and priority permitted setting
      --  the BusFault handler to the pending state when the floating-point
      --  stack frame was allocated.
      BFRDY         : Boolean := False;
      --  unspecified
      Reserved_7_7  : HAL.Bit := 16#0#;
      --  Read-only. If set, DebugMonitor is enabled and priority permitted
      --  setting the DebugMonitor handler to the pending state when the
      --  floating-point stack frame was allocated.
      MONRDY        : Boolean := False;
      --  unspecified
      Reserved_9_29 : HAL.UInt21 := 16#0#;
      --  Enables automatic lazy state preservation for floating-point context.
      LSPEN         : Boolean := True;
      --  Enables CONTROL.FPCA setting on execution of a floating point
      --  instruction. This results in automatic hardware state preservation
      --  and restoration, for floating-point context, on exception entry and
      --  exit.
      ASPEN         : Boolean := True;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FPCCR_Register use record
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

   subtype FPCAR_ADDRESS_Field is HAL.UInt29;

   --  Floating-Point Context Address Register
   type FPCAR_Register is record
      --  unspecified
      Reserved_0_2 : HAL.UInt3 := 16#0#;
      --  The FPCAR register holds the location of the unpopulated
      --  floating-point register space allocated on an exception stack frame.
      ADDRESS      : FPCAR_ADDRESS_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FPCAR_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      ADDRESS      at 0 range 3 .. 31;
   end record;

   subtype FPDSCR_RMode_Field is HAL.UInt2;

   --  Floating-Point Default Status Control Register
   type FPDSCR_Register is record
      --  unspecified
      Reserved_0_21  : HAL.UInt22 := 16#0#;
      --  Default value for FPSCR.RMode.
      RMode          : FPDSCR_RMode_Field := 16#0#;
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

   for FPDSCR_Register use record
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

   --  Floating Point Unit
   type FPU_Peripheral is record
      --  Coprocessor Access Control Register
      CPACR  : aliased CPACR_Register;
      --  Floating-Point Context Control Register
      FPCCR  : aliased FPCCR_Register;
      --  Floating-Point Context Address Register
      FPCAR  : aliased FPCAR_Register;
      --  Floating-Point Default Status Control Register
      FPDSCR : aliased FPDSCR_Register;
   end record
     with Volatile;

   for FPU_Peripheral use record
      CPACR  at 16#0# range 0 .. 31;
      FPCCR  at 16#1AC# range 0 .. 31;
      FPCAR  at 16#1B0# range 0 .. 31;
      FPDSCR at 16#1B4# range 0 .. 31;
   end record;

   --  Floating Point Unit
   FPU_Periph : aliased FPU_Peripheral
     with Import, Address => FPU_Base;

end Cortex_M_SVD.FPU;
