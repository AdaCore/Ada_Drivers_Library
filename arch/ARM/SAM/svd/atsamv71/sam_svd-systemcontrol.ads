--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.SystemControl is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Auxiliary Control Register,
   type SystemControl_ACTLR_Register is record
      --  Disables interruption of multi-cycle instructions.
      DISMCYCINT    : Boolean := False;
      --  Disables write buffer use during default memory map accesses.
      DISDEFWBUF    : Boolean := False;
      --  Disables folding of IT instructions.
      DISFOLD       : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_ACTLR_Register use record
      DISMCYCINT    at 0 range 0 .. 0;
      DISDEFWBUF    at 0 range 1 .. 1;
      DISFOLD       at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype SystemControl_CPUID_REVISION_Field is HAL.UInt4;
   subtype SystemControl_CPUID_PARTNO_Field is HAL.UInt12;
   subtype SystemControl_CPUID_VARIANT_Field is HAL.UInt4;
   subtype SystemControl_CPUID_IMPLEMENTER_Field is HAL.UInt8;

   --  CPUID Base Register
   type SystemControl_CPUID_Register is record
      --  Indicates patch release: 0x0 = Patch 0
      REVISION       : SystemControl_CPUID_REVISION_Field := 16#0#;
      --  Indicates part number
      PARTNO         : SystemControl_CPUID_PARTNO_Field := 16#0#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#0#;
      --  Indicates processor revision: 0x2 = Revision 2
      VARIANT        : SystemControl_CPUID_VARIANT_Field := 16#0#;
      --  Implementer code
      IMPLEMENTER    : SystemControl_CPUID_IMPLEMENTER_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_CPUID_Register use record
      REVISION       at 0 range 0 .. 3;
      PARTNO         at 0 range 4 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      VARIANT        at 0 range 20 .. 23;
      IMPLEMENTER    at 0 range 24 .. 31;
   end record;

   subtype SystemControl_ICSR_VECTACTIVE_Field is HAL.UInt9;

   --  no description available
   type ICSR_RETTOBASE_Field is
     (
      --  there are preempted active exceptions to execute
      Value_0,
      --  there are no active exceptions, or the currently-executing exception
      --  is the only active exception
      Value_1)
     with Size => 1;
   for ICSR_RETTOBASE_Field use
     (Value_0 => 0,
      Value_1 => 1);

   subtype SystemControl_ICSR_VECTPENDING_Field is HAL.UInt6;

   --  no description available
   type ICSR_ISRPREEMPT_Field is
     (
      --  Will not service
      Value_0,
      --  Will service a pending exception
      Value_1)
     with Size => 1;
   for ICSR_ISRPREEMPT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type ICSR_PENDSTCLR_Field is
     (
      --  no effect
      Value_0,
      --  removes the pending state from the SysTick exception
      Value_1)
     with Size => 1;
   for ICSR_PENDSTCLR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type ICSR_PENDSTSET_Field is
     (
      --  write: no effect; read: SysTick exception is not pending
      Value_0,
      --  write: changes SysTick exception state to pending; read: SysTick
      --  exception is pending
      Value_1)
     with Size => 1;
   for ICSR_PENDSTSET_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type ICSR_PENDSVCLR_Field is
     (
      --  no effect
      Value_0,
      --  removes the pending state from the PendSV exception
      Value_1)
     with Size => 1;
   for ICSR_PENDSVCLR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type ICSR_PENDSVSET_Field is
     (
      --  write: no effect; read: PendSV exception is not pending
      Value_0,
      --  write: changes PendSV exception state to pending; read: PendSV
      --  exception is pending
      Value_1)
     with Size => 1;
   for ICSR_PENDSVSET_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type ICSR_NMIPENDSET_Field is
     (
      --  write: no effect; read: NMI exception is not pending
      Value_0,
      --  write: changes NMI exception state to pending; read: NMI exception is
      --  pending
      Value_1)
     with Size => 1;
   for ICSR_NMIPENDSET_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Interrupt Control and State Register
   type SystemControl_ICSR_Register is record
      --  Active exception number
      VECTACTIVE     : SystemControl_ICSR_VECTACTIVE_Field := 16#0#;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  no description available
      RETTOBASE      : ICSR_RETTOBASE_Field := SAM_SVD.SystemControl.Value_0;
      --  Exception number of the highest priority pending enabled exception
      VECTPENDING    : SystemControl_ICSR_VECTPENDING_Field := 16#0#;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
      --  no description available
      ISRPENDING     : Boolean := False;
      --  no description available
      ISRPREEMPT     : ICSR_ISRPREEMPT_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_24_24 : HAL.Bit := 16#0#;
      --  no description available
      PENDSTCLR      : ICSR_PENDSTCLR_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      PENDSTSET      : ICSR_PENDSTSET_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      PENDSVCLR      : ICSR_PENDSVCLR_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      PENDSVSET      : ICSR_PENDSVSET_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_29_30 : HAL.UInt2 := 16#0#;
      --  no description available
      NMIPENDSET     : ICSR_NMIPENDSET_Field := SAM_SVD.SystemControl.Value_0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_ICSR_Register use record
      VECTACTIVE     at 0 range 0 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      RETTOBASE      at 0 range 11 .. 11;
      VECTPENDING    at 0 range 12 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      ISRPENDING     at 0 range 22 .. 22;
      ISRPREEMPT     at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      PENDSTCLR      at 0 range 25 .. 25;
      PENDSTSET      at 0 range 26 .. 26;
      PENDSVCLR      at 0 range 27 .. 27;
      PENDSVSET      at 0 range 28 .. 28;
      Reserved_29_30 at 0 range 29 .. 30;
      NMIPENDSET     at 0 range 31 .. 31;
   end record;

   subtype SystemControl_VTOR_TBLOFF_Field is HAL.UInt25;

   --  Vector Table Offset Register
   type SystemControl_VTOR_Register is record
      --  unspecified
      Reserved_0_6 : HAL.UInt7 := 16#0#;
      --  Vector table base offset
      TBLOFF       : SystemControl_VTOR_TBLOFF_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_VTOR_Register use record
      Reserved_0_6 at 0 range 0 .. 6;
      TBLOFF       at 0 range 7 .. 31;
   end record;

   --  no description available
   type AIRCR_SYSRESETREQ_Field is
     (
      --  no system reset request
      Value_0,
      --  asserts a signal to the outer system that requests a reset
      Value_1)
     with Size => 1;
   for AIRCR_SYSRESETREQ_Field use
     (Value_0 => 0,
      Value_1 => 1);

   subtype SystemControl_AIRCR_PRIGROUP_Field is HAL.UInt3;

   --  no description available
   type AIRCR_ENDIANNESS_Field is
     (
      --  Little-endian
      Value_0,
      --  Big-endian
      Value_1)
     with Size => 1;
   for AIRCR_ENDIANNESS_Field use
     (Value_0 => 0,
      Value_1 => 1);

   subtype SystemControl_AIRCR_VECTKEY_Field is HAL.UInt16;

   --  Application Interrupt and Reset Control Register
   type SystemControl_AIRCR_Register is record
      --  no description available
      VECTRESET      : Boolean := False;
      --  no description available
      VECTCLRACTIVE  : Boolean := False;
      --  no description available
      SYSRESETREQ    : AIRCR_SYSRESETREQ_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Interrupt priority grouping field. This field determines the split of
      --  group priority from subpriority.
      PRIGROUP       : SystemControl_AIRCR_PRIGROUP_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  no description available
      ENDIANNESS     : AIRCR_ENDIANNESS_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  Register key
      VECTKEY        : SystemControl_AIRCR_VECTKEY_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_AIRCR_Register use record
      VECTRESET      at 0 range 0 .. 0;
      VECTCLRACTIVE  at 0 range 1 .. 1;
      SYSRESETREQ    at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      PRIGROUP       at 0 range 8 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      ENDIANNESS     at 0 range 15 .. 15;
      VECTKEY        at 0 range 16 .. 31;
   end record;

   --  no description available
   type SCR_SLEEPONEXIT_Field is
     (
      --  o not sleep when returning to Thread mode
      Value_0,
      --  enter sleep, or deep sleep, on return from an ISR
      Value_1)
     with Size => 1;
   for SCR_SLEEPONEXIT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SCR_SLEEPDEEP_Field is
     (
      --  sleep
      Value_0,
      --  deep sleep
      Value_1)
     with Size => 1;
   for SCR_SLEEPDEEP_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SCR_SEVONPEND_Field is
     (
      --  only enabled interrupts or events can wakeup the processor, disabled
      --  interrupts are excluded
      Value_0,
      --  enabled events and all interrupts, including disabled interrupts, can
      --  wakeup the processor
      Value_1)
     with Size => 1;
   for SCR_SEVONPEND_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  System Control Register
   type SystemControl_SCR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  no description available
      SLEEPONEXIT   : SCR_SLEEPONEXIT_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      SLEEPDEEP     : SCR_SLEEPDEEP_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  no description available
      SEVONPEND     : SCR_SEVONPEND_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_SCR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      SLEEPONEXIT   at 0 range 1 .. 1;
      SLEEPDEEP     at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      SEVONPEND     at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  no description available
   type CCR_NONBASETHRDENA_Field is
     (
      --  processor can enter Thread mode only when no exception is active
      Value_0,
      --  processor can enter Thread mode from any level under the control of
      --  an EXC_RETURN value
      Value_1)
     with Size => 1;
   for CCR_NONBASETHRDENA_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Enables unprivileged software access to the STIR
   type CCR_USERSETMPEND_Field is
     (
      --  disable
      Value_0,
      --  enable
      Value_1)
     with Size => 1;
   for CCR_USERSETMPEND_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Enables unaligned access traps
   type CCR_UNALIGN_TRP_Field is
     (
      --  do not trap unaligned halfword and word accesses
      Value_0,
      --  trap unaligned halfword and word accesses
      Value_1)
     with Size => 1;
   for CCR_UNALIGN_TRP_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Enables faulting or halting when the processor executes an SDIV or UDIV
   --  instruction with a divisor of 0
   type CCR_DIV_0_TRP_Field is
     (
      --  do not trap divide by 0
      Value_0,
      --  trap divide by 0
      Value_1)
     with Size => 1;
   for CCR_DIV_0_TRP_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Enables handlers with priority -1 or -2 to ignore data BusFaults caused
   --  by load and store instructions.
   type CCR_BFHFNMIGN_Field is
     (
      --  data bus faults caused by load and store instructions cause a lock-up
      Value_0,
      --  handlers running at priority -1 and -2 ignore data bus faults caused
      --  by load and store instructions
      Value_1)
     with Size => 1;
   for CCR_BFHFNMIGN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Indicates stack alignment on exception entry
   type CCR_STKALIGN_Field is
     (
      --  4-byte aligned
      Value_0,
      --  8-byte aligned
      Value_1)
     with Size => 1;
   for CCR_STKALIGN_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Configuration and Control Register
   type SystemControl_CCR_Register is record
      --  no description available
      NONBASETHRDENA : CCR_NONBASETHRDENA_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  Enables unprivileged software access to the STIR
      USERSETMPEND   : CCR_USERSETMPEND_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Enables unaligned access traps
      UNALIGN_TRP    : CCR_UNALIGN_TRP_Field := SAM_SVD.SystemControl.Value_0;
      --  Enables faulting or halting when the processor executes an SDIV or
      --  UDIV instruction with a divisor of 0
      DIV_0_TRP      : CCR_DIV_0_TRP_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Enables handlers with priority -1 or -2 to ignore data BusFaults
      --  caused by load and store instructions.
      BFHFNMIGN      : CCR_BFHFNMIGN_Field := SAM_SVD.SystemControl.Value_0;
      --  Indicates stack alignment on exception entry
      STKALIGN       : CCR_STKALIGN_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_CCR_Register use record
      NONBASETHRDENA at 0 range 0 .. 0;
      USERSETMPEND   at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      UNALIGN_TRP    at 0 range 3 .. 3;
      DIV_0_TRP      at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      BFHFNMIGN      at 0 range 8 .. 8;
      STKALIGN       at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype SystemControl_SHPR1_PRI_4_Field is HAL.UInt8;
   subtype SystemControl_SHPR1_PRI_5_Field is HAL.UInt8;
   subtype SystemControl_SHPR1_PRI_6_Field is HAL.UInt8;

   --  System Handler Priority Register 1
   type SystemControl_SHPR1_Register is record
      --  Priority of system handler 4, MemManage
      PRI_4          : SystemControl_SHPR1_PRI_4_Field := 16#0#;
      --  Priority of system handler 5, BusFault
      PRI_5          : SystemControl_SHPR1_PRI_5_Field := 16#0#;
      --  Priority of system handler 6, UsageFault
      PRI_6          : SystemControl_SHPR1_PRI_6_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_SHPR1_Register use record
      PRI_4          at 0 range 0 .. 7;
      PRI_5          at 0 range 8 .. 15;
      PRI_6          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype SystemControl_SHPR2_PRI_11_Field is HAL.UInt8;

   --  System Handler Priority Register 2
   type SystemControl_SHPR2_Register is record
      --  unspecified
      Reserved_0_23 : HAL.UInt24 := 16#0#;
      --  Priority of system handler 11, SVCall
      PRI_11        : SystemControl_SHPR2_PRI_11_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_SHPR2_Register use record
      Reserved_0_23 at 0 range 0 .. 23;
      PRI_11        at 0 range 24 .. 31;
   end record;

   subtype SystemControl_SHPR3_PRI_14_Field is HAL.UInt8;
   subtype SystemControl_SHPR3_PRI_15_Field is HAL.UInt8;

   --  System Handler Priority Register 3
   type SystemControl_SHPR3_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  Priority of system handler 14, PendSV
      PRI_14        : SystemControl_SHPR3_PRI_14_Field := 16#0#;
      --  Priority of system handler 15, SysTick exception
      PRI_15        : SystemControl_SHPR3_PRI_15_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_SHPR3_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      PRI_14        at 0 range 16 .. 23;
      PRI_15        at 0 range 24 .. 31;
   end record;

   --  no description available
   type SHCSR_MEMFAULTACT_Field is
     (
      --  exception is not active
      Value_0,
      --  exception is active
      Value_1)
     with Size => 1;
   for SHCSR_MEMFAULTACT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_BUSFAULTACT_Field is
     (
      --  exception is not active
      Value_0,
      --  exception is active
      Value_1)
     with Size => 1;
   for SHCSR_BUSFAULTACT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_USGFAULTACT_Field is
     (
      --  exception is not active
      Value_0,
      --  exception is active
      Value_1)
     with Size => 1;
   for SHCSR_USGFAULTACT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_SVCALLACT_Field is
     (
      --  exception is not active
      Value_0,
      --  exception is active
      Value_1)
     with Size => 1;
   for SHCSR_SVCALLACT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_MONITORACT_Field is
     (
      --  exception is not active
      Value_0,
      --  exception is active
      Value_1)
     with Size => 1;
   for SHCSR_MONITORACT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_PENDSVACT_Field is
     (
      --  exception is not active
      Value_0,
      --  exception is active
      Value_1)
     with Size => 1;
   for SHCSR_PENDSVACT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_SYSTICKACT_Field is
     (
      --  exception is not active
      Value_0,
      --  exception is active
      Value_1)
     with Size => 1;
   for SHCSR_SYSTICKACT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_USGFAULTPENDED_Field is
     (
      --  exception is not pending
      Value_0,
      --  exception is pending
      Value_1)
     with Size => 1;
   for SHCSR_USGFAULTPENDED_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_MEMFAULTPENDED_Field is
     (
      --  exception is not pending
      Value_0,
      --  exception is pending
      Value_1)
     with Size => 1;
   for SHCSR_MEMFAULTPENDED_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_BUSFAULTPENDED_Field is
     (
      --  exception is not pending
      Value_0,
      --  exception is pending
      Value_1)
     with Size => 1;
   for SHCSR_BUSFAULTPENDED_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_SVCALLPENDED_Field is
     (
      --  exception is not pending
      Value_0,
      --  exception is pending
      Value_1)
     with Size => 1;
   for SHCSR_SVCALLPENDED_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_MEMFAULTENA_Field is
     (
      --  disable the exception
      Value_0,
      --  enable the exception
      Value_1)
     with Size => 1;
   for SHCSR_MEMFAULTENA_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_BUSFAULTENA_Field is
     (
      --  disable the exception
      Value_0,
      --  enable the exception
      Value_1)
     with Size => 1;
   for SHCSR_BUSFAULTENA_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type SHCSR_USGFAULTENA_Field is
     (
      --  disable the exception
      Value_0,
      --  enable the exception
      Value_1)
     with Size => 1;
   for SHCSR_USGFAULTENA_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  System Handler Control and State Register
   type SystemControl_SHCSR_Register is record
      --  no description available
      MEMFAULTACT    : SHCSR_MEMFAULTACT_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  no description available
      BUSFAULTACT    : SHCSR_BUSFAULTACT_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  no description available
      USGFAULTACT    : SHCSR_USGFAULTACT_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_4_6   : HAL.UInt3 := 16#0#;
      --  no description available
      SVCALLACT      : SHCSR_SVCALLACT_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      MONITORACT     : SHCSR_MONITORACT_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  no description available
      PENDSVACT      : SHCSR_PENDSVACT_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      SYSTICKACT     : SHCSR_SYSTICKACT_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  no description available
      USGFAULTPENDED : SHCSR_USGFAULTPENDED_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  no description available
      MEMFAULTPENDED : SHCSR_MEMFAULTPENDED_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  no description available
      BUSFAULTPENDED : SHCSR_BUSFAULTPENDED_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  no description available
      SVCALLPENDED   : SHCSR_SVCALLPENDED_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  no description available
      MEMFAULTENA    : SHCSR_MEMFAULTENA_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  no description available
      BUSFAULTENA    : SHCSR_BUSFAULTENA_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  no description available
      USGFAULTENA    : SHCSR_USGFAULTENA_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_SHCSR_Register use record
      MEMFAULTACT    at 0 range 0 .. 0;
      BUSFAULTACT    at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      USGFAULTACT    at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      SVCALLACT      at 0 range 7 .. 7;
      MONITORACT     at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      PENDSVACT      at 0 range 10 .. 10;
      SYSTICKACT     at 0 range 11 .. 11;
      USGFAULTPENDED at 0 range 12 .. 12;
      MEMFAULTPENDED at 0 range 13 .. 13;
      BUSFAULTPENDED at 0 range 14 .. 14;
      SVCALLPENDED   at 0 range 15 .. 15;
      MEMFAULTENA    at 0 range 16 .. 16;
      BUSFAULTENA    at 0 range 17 .. 17;
      USGFAULTENA    at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  no description available
   type CFSR_IACCVIOL_Field is
     (
      --  no instruction access violation fault
      Value_0,
      --  the processor attempted an instruction fetch from a location that
      --  does not permit execution
      Value_1)
     with Size => 1;
   for CFSR_IACCVIOL_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_DACCVIOL_Field is
     (
      --  no data access violation fault
      Value_0,
      --  the processor attempted a load or store at a location that does not
      --  permit the operation
      Value_1)
     with Size => 1;
   for CFSR_DACCVIOL_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_MUNSTKERR_Field is
     (
      --  no unstacking fault
      Value_0,
      --  unstack for an exception return has caused one or more access
      --  violations
      Value_1)
     with Size => 1;
   for CFSR_MUNSTKERR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_MSTKERR_Field is
     (
      --  no stacking fault
      Value_0,
      --  stacking for an exception entry has caused one or more access
      --  violations
      Value_1)
     with Size => 1;
   for CFSR_MSTKERR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_MLSPERR_Field is
     (
      --  No MemManage fault occurred during floating-point lazy state
      --  preservation
      Value_0,
      --  A MemManage fault occurred during floating-point lazy state
      --  preservation
      Value_1)
     with Size => 1;
   for CFSR_MLSPERR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_MMARVALID_Field is
     (
      --  value in MMAR is not a valid fault address
      Value_0,
      --  MMAR holds a valid fault address
      Value_1)
     with Size => 1;
   for CFSR_MMARVALID_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_IBUSERR_Field is
     (
      --  no instruction bus error
      Value_0,
      --  instruction bus error
      Value_1)
     with Size => 1;
   for CFSR_IBUSERR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_PRECISERR_Field is
     (
      --  no precise data bus error
      Value_0,
      --  a data bus error has occurred, and the PC value stacked for the
      --  exception return points to the instruction that caused the fault
      Value_1)
     with Size => 1;
   for CFSR_PRECISERR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_IMPRECISERR_Field is
     (
      --  no imprecise data bus error
      Value_0,
      --  a data bus error has occurred, but the return address in the stack
      --  frame is not related to the instruction that caused the error
      Value_1)
     with Size => 1;
   for CFSR_IMPRECISERR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_UNSTKERR_Field is
     (
      --  no unstacking fault
      Value_0,
      --  unstack for an exception return has caused one or more BusFaults
      Value_1)
     with Size => 1;
   for CFSR_UNSTKERR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_STKERR_Field is
     (
      --  no stacking fault
      Value_0,
      --  stacking for an exception entry has caused one or more BusFaults
      Value_1)
     with Size => 1;
   for CFSR_STKERR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_LSPERR_Field is
     (
      --  No bus fault occurred during floating-point lazy state preservation
      Value_0,
      --  A bus fault occurred during floating-point lazy state preservation
      Value_1)
     with Size => 1;
   for CFSR_LSPERR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_BFARVALID_Field is
     (
      --  value in BFAR is not a valid fault address
      Value_0,
      --  BFAR holds a valid fault address
      Value_1)
     with Size => 1;
   for CFSR_BFARVALID_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_UNDEFINSTR_Field is
     (
      --  no undefined instruction UsageFault
      Value_0,
      --  the processor has attempted to execute an undefined instruction
      Value_1)
     with Size => 1;
   for CFSR_UNDEFINSTR_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_INVSTATE_Field is
     (
      --  no invalid state UsageFault
      Value_0,
      --  the processor has attempted to execute an instruction that makes
      --  illegal use of the EPSR
      Value_1)
     with Size => 1;
   for CFSR_INVSTATE_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_INVPC_Field is
     (
      --  no invalid PC load UsageFault
      Value_0,
      --  the processor has attempted an illegal load of EXC_RETURN to the PC
      Value_1)
     with Size => 1;
   for CFSR_INVPC_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_NOCP_Field is
     (
      --  no UsageFault caused by attempting to access a coprocessor
      Value_0,
      --  the processor has attempted to access a coprocessor
      Value_1)
     with Size => 1;
   for CFSR_NOCP_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_UNALIGNED_Field is
     (
      --  no unaligned access fault, or unaligned access trapping not enabled
      Value_0,
      --  the processor has made an unaligned memory access
      Value_1)
     with Size => 1;
   for CFSR_UNALIGNED_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type CFSR_DIVBYZERO_Field is
     (
      --  no divide by zero fault, or divide by zero trapping not enabled
      Value_0,
      --  the processor has executed an SDIV or UDIV instruction with a divisor
      --  of 0
      Value_1)
     with Size => 1;
   for CFSR_DIVBYZERO_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Configurable Fault Status Registers
   type SystemControl_CFSR_Register is record
      --  no description available
      IACCVIOL       : CFSR_IACCVIOL_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      DACCVIOL       : CFSR_DACCVIOL_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  no description available
      MUNSTKERR      : CFSR_MUNSTKERR_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      MSTKERR        : CFSR_MSTKERR_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      MLSPERR        : CFSR_MLSPERR_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  no description available
      MMARVALID      : CFSR_MMARVALID_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      IBUSERR        : CFSR_IBUSERR_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      PRECISERR      : CFSR_PRECISERR_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      IMPRECISERR    : CFSR_IMPRECISERR_Field :=
                        SAM_SVD.SystemControl.Value_0;
      --  no description available
      UNSTKERR       : CFSR_UNSTKERR_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      STKERR         : CFSR_STKERR_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      LSPERR         : CFSR_LSPERR_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  no description available
      BFARVALID      : CFSR_BFARVALID_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      UNDEFINSTR     : CFSR_UNDEFINSTR_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      INVSTATE       : CFSR_INVSTATE_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      INVPC          : CFSR_INVPC_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      NOCP           : CFSR_NOCP_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_20_23 : HAL.UInt4 := 16#0#;
      --  no description available
      UNALIGNED      : CFSR_UNALIGNED_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      DIVBYZERO      : CFSR_DIVBYZERO_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_CFSR_Register use record
      IACCVIOL       at 0 range 0 .. 0;
      DACCVIOL       at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      MUNSTKERR      at 0 range 3 .. 3;
      MSTKERR        at 0 range 4 .. 4;
      MLSPERR        at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      MMARVALID      at 0 range 7 .. 7;
      IBUSERR        at 0 range 8 .. 8;
      PRECISERR      at 0 range 9 .. 9;
      IMPRECISERR    at 0 range 10 .. 10;
      UNSTKERR       at 0 range 11 .. 11;
      STKERR         at 0 range 12 .. 12;
      LSPERR         at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      BFARVALID      at 0 range 15 .. 15;
      UNDEFINSTR     at 0 range 16 .. 16;
      INVSTATE       at 0 range 17 .. 17;
      INVPC          at 0 range 18 .. 18;
      NOCP           at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      UNALIGNED      at 0 range 24 .. 24;
      DIVBYZERO      at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  no description available
   type HFSR_VECTTBL_Field is
     (
      --  no BusFault on vector table read
      Value_0,
      --  BusFault on vector table read
      Value_1)
     with Size => 1;
   for HFSR_VECTTBL_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type HFSR_FORCED_Field is
     (
      --  no forced HardFault
      Value_0,
      --  forced HardFault
      Value_1)
     with Size => 1;
   for HFSR_FORCED_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  HardFault Status register
   type SystemControl_HFSR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  no description available
      VECTTBL       : HFSR_VECTTBL_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_2_29 : HAL.UInt28 := 16#0#;
      --  no description available
      FORCED        : HFSR_FORCED_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      DEBUGEVT      : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_HFSR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      VECTTBL       at 0 range 1 .. 1;
      Reserved_2_29 at 0 range 2 .. 29;
      FORCED        at 0 range 30 .. 30;
      DEBUGEVT      at 0 range 31 .. 31;
   end record;

   --  no description available
   type DFSR_HALTED_Field is
     (
      --  No active halt request debug event
      Value_0,
      --  Halt request debug event active
      Value_1)
     with Size => 1;
   for DFSR_HALTED_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type DFSR_BKPT_Field is
     (
      --  No current breakpoint debug event
      Value_0,
      --  At least one current breakpoint debug event
      Value_1)
     with Size => 1;
   for DFSR_BKPT_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type DFSR_DWTTRAP_Field is
     (
      --  No current debug events generated by the DWT
      Value_0,
      --  At least one current debug event generated by the DWT
      Value_1)
     with Size => 1;
   for DFSR_DWTTRAP_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type DFSR_VCATCH_Field is
     (
      --  No Vector catch triggered
      Value_0,
      --  Vector catch triggered
      Value_1)
     with Size => 1;
   for DFSR_VCATCH_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  no description available
   type DFSR_EXTERNAL_Field is
     (
      --  No EDBGRQ debug event
      Value_0,
      --  EDBGRQ debug event
      Value_1)
     with Size => 1;
   for DFSR_EXTERNAL_Field use
     (Value_0 => 0,
      Value_1 => 1);

   --  Debug Fault Status Register
   type SystemControl_DFSR_Register is record
      --  no description available
      HALTED        : DFSR_HALTED_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      BKPT          : DFSR_BKPT_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      DWTTRAP       : DFSR_DWTTRAP_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      VCATCH        : DFSR_VCATCH_Field := SAM_SVD.SystemControl.Value_0;
      --  no description available
      EXTERNAL      : DFSR_EXTERNAL_Field := SAM_SVD.SystemControl.Value_0;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_DFSR_Register use record
      HALTED        at 0 range 0 .. 0;
      BKPT          at 0 range 1 .. 1;
      DWTTRAP       at 0 range 2 .. 2;
      VCATCH        at 0 range 3 .. 3;
      EXTERNAL      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  SystemControl_CPACR_CP array element
   subtype SystemControl_CPACR_CP_Element is HAL.UInt2;

   --  SystemControl_CPACR_CP array
   type SystemControl_CPACR_CP_Field_Array is array (10 .. 11)
     of SystemControl_CPACR_CP_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for SystemControl_CPACR_CP
   type SystemControl_CPACR_CP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CP as a value
            Val : HAL.UInt4;
         when True =>
            --  CP as an array
            Arr : SystemControl_CPACR_CP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SystemControl_CPACR_CP_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Coprocessor Access Control Register
   type SystemControl_CPACR_Register is record
      --  unspecified
      Reserved_0_19  : HAL.UInt20 := 16#0#;
      --  Access privileges for coprocessor 10.
      CP             : SystemControl_CPACR_CP_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SystemControl_CPACR_Register use record
      Reserved_0_19  at 0 range 0 .. 19;
      CP             at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  System Control Registers
   type SystemControl_Peripheral is record
      --  Auxiliary Control Register,
      ACTLR : aliased SystemControl_ACTLR_Register;
      --  CPUID Base Register
      CPUID : aliased SystemControl_CPUID_Register;
      --  Interrupt Control and State Register
      ICSR  : aliased SystemControl_ICSR_Register;
      --  Vector Table Offset Register
      VTOR  : aliased SystemControl_VTOR_Register;
      --  Application Interrupt and Reset Control Register
      AIRCR : aliased SystemControl_AIRCR_Register;
      --  System Control Register
      SCR   : aliased SystemControl_SCR_Register;
      --  Configuration and Control Register
      CCR   : aliased SystemControl_CCR_Register;
      --  System Handler Priority Register 1
      SHPR1 : aliased SystemControl_SHPR1_Register;
      --  System Handler Priority Register 2
      SHPR2 : aliased SystemControl_SHPR2_Register;
      --  System Handler Priority Register 3
      SHPR3 : aliased SystemControl_SHPR3_Register;
      --  System Handler Control and State Register
      SHCSR : aliased SystemControl_SHCSR_Register;
      --  Configurable Fault Status Registers
      CFSR  : aliased SystemControl_CFSR_Register;
      --  HardFault Status register
      HFSR  : aliased SystemControl_HFSR_Register;
      --  Debug Fault Status Register
      DFSR  : aliased SystemControl_DFSR_Register;
      --  MemManage Address Register
      MMFAR : aliased HAL.UInt32;
      --  BusFault Address Register
      BFAR  : aliased HAL.UInt32;
      --  Auxiliary Fault Status Register
      AFSR  : aliased HAL.UInt32;
      --  Coprocessor Access Control Register
      CPACR : aliased SystemControl_CPACR_Register;
   end record
     with Volatile;

   for SystemControl_Peripheral use record
      ACTLR at 16#8# range 0 .. 31;
      CPUID at 16#D00# range 0 .. 31;
      ICSR  at 16#D04# range 0 .. 31;
      VTOR  at 16#D08# range 0 .. 31;
      AIRCR at 16#D0C# range 0 .. 31;
      SCR   at 16#D10# range 0 .. 31;
      CCR   at 16#D14# range 0 .. 31;
      SHPR1 at 16#D18# range 0 .. 31;
      SHPR2 at 16#D1C# range 0 .. 31;
      SHPR3 at 16#D20# range 0 .. 31;
      SHCSR at 16#D24# range 0 .. 31;
      CFSR  at 16#D28# range 0 .. 31;
      HFSR  at 16#D2C# range 0 .. 31;
      DFSR  at 16#D30# range 0 .. 31;
      MMFAR at 16#D34# range 0 .. 31;
      BFAR  at 16#D38# range 0 .. 31;
      AFSR  at 16#D3C# range 0 .. 31;
      CPACR at 16#D88# range 0 .. 31;
   end record;

   --  System Control Registers
   SystemControl_Periph : aliased SystemControl_Peripheral
     with Import, Address => System'To_Address (16#E000E000#);

end SAM_SVD.SystemControl;
