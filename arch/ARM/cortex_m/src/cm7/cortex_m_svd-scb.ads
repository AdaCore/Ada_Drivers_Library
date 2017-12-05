--  This spec has been automatically generated from cm7.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package Cortex_M_SVD.SCB is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Disables Interruption Folding
   type ACTLR_DISFOLD_Field is
     (
      --  Interruption folding enabled
      Normal,
      --  Interruption folding disabled
      Disable)
     with Size => 1;
   for ACTLR_DISFOLD_Field use
     (Normal => 0,
      Disable => 1);

   --  Disabled FPU exception outputs
   type ACTLR_PFEXCODIS_Field is
     (
      --  Normal operation.
      Normal,
      --  FPU exception outputs are disabled.
      Disable)
     with Size => 1;
   for ACTLR_PFEXCODIS_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables dynamic read allocate mode for Write-Back Write-Allocate memory
   --  regions:
   type ACTLR_DISRAMODE_Field is
     (
      --  Normal operation.
      Normal,
      --  Dynamic disabled.
      Disable)
     with Size => 1;
   for ACTLR_DISRAMODE_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables ITM and DWT ATB flush:
   type ACTLR_DISITMATBFLUSH_Field is
     (
      --  Reset value for the field
      Actlr_Disitmatbflush_Field_Reset,
      --  ITM and DWT ATB flush disabled. AFVALID is ignored and AFREADY is
      --  held HIGH.
      Disable)
     with Size => 1;
   for ACTLR_DISITMATBFLUSH_Field use
     (Actlr_Disitmatbflush_Field_Reset => 0,
      Disable => 1);

   --  Disables the Branch Target Address Cache (BTAC):
   type ACTLR_DISBTACREAD_Field is
     (
      --  Normal operation.
      Normal,
      --  BTAC is not used and only static branch prediction can occur.
      Disable)
     with Size => 1;
   for ACTLR_DISBTACREAD_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables the Branch Target Address Cache allocation:
   type ACTLR_DISBTACALLOC_Field is
     (
      --  Normal operation.
      Normal,
      --  No new entries are allocated in BTAC, but existing entries can be
      --  updated.
      Disable)
     with Size => 1;
   for ACTLR_DISBTACALLOC_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables critical AXI Read-Under-Read:
   type ACTLR_DISCRITAXIRUR_Field is
     (
      --  Normal operation.
      Normal,
      --  An AXI read to Strongly-ordered or device memory, or an LDREX to
      --  shared memory, is not put on AXI if there are any outstanding reads
      --  on AXI. Transactions on AXI cannot be interrupted. This bit might
      --  reduce the time that these transactions are in progress and might
      --  improve worst case interrupt latency. Performance is decreased when
      --  this bit is set.
      Disable)
     with Size => 1;
   for ACTLR_DISCRITAXIRUR_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables dual-issued direct branches:
   type ACTLR_DISDI_DB_Field is
     (
      --  Normal operation.
      Normal,
      --  Direct branches instruction type cannot be dual-issued in channel 0
      Disable)
     with Size => 1;
   for ACTLR_DISDI_DB_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables dual-issued indirect branches:
   type ACTLR_DISDI_IB_Field is
     (
      --  Normal operation.
      Normal,
      --  Indirect branches, but not loads to PC instruction type cannot be
      --  dual-issued in channel 0
      Disable)
     with Size => 1;
   for ACTLR_DISDI_IB_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables dual-issued loads to PC:
   type ACTLR_DISDI_LPC_Field is
     (
      --  Normal operation.
      Normal,
      --  Loads to PC instruction type cannot be dual-issued in channel 0
      Disable)
     with Size => 1;
   for ACTLR_DISDI_LPC_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables integer MAC and MUL dual-issued instructions:
   type ACTLR_DISDI_MAC_MUL_Field is
     (
      --  Normal operation.
      Normal,
      --  Integer MAC and MUL instruction type cannot be dual-issued in channel
      --  0
      Disable)
     with Size => 1;
   for ACTLR_DISDI_MAC_MUL_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables VFP dual-issued instruction:
   type ACTLR_DISDI_VFP_Field is
     (
      --  Normal operation.
      Normal,
      --  VFP instruction type cannot be dual-issued in channel 0
      Disable)
     with Size => 1;
   for ACTLR_DISDI_VFP_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables direct branches instructions in channel 1:
   type ACTLR_DISISSCH1_DB_Field is
     (
      --  Normal operation.
      Normal,
      --  Direct branches instruction type cannot be issued in channel 1
      Disable)
     with Size => 1;
   for ACTLR_DISISSCH1_DB_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables indirect branches instructions in channel 1:
   type ACTLR_DISISSCH1_IB_Field is
     (
      --  Normal operation.
      Normal,
      --  Indirect branches, but not loads to PC instruction type cannot be
      --  issued in channel 1
      Disable)
     with Size => 1;
   for ACTLR_DISISSCH1_IB_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables loads to PC instructions in channel 1:
   type ACTLR_DISISSCH1_LPC_Field is
     (
      --  Normal operation.
      Normal,
      --  Loads to PC instruction type cannot be issued in channel 1
      Disable)
     with Size => 1;
   for ACTLR_DISISSCH1_LPC_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables integer MAC and MUL instructions in channel 1:
   type ACTLR_DISISSCH1_MAC_MUL_Field is
     (
      --  Normal operation.
      Normal,
      --  Integer MAC and MUL instruction type cannot be issued in channel 1
      Disable)
     with Size => 1;
   for ACTLR_DISISSCH1_MAC_MUL_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables VFP instructions in channel 1:
   type ACTLR_DISISSCH1_VFP_Field is
     (
      --  Normal operation.
      Normal,
      --  VFP instruction type cannot be issued in channel 1
      Disable)
     with Size => 1;
   for ACTLR_DISISSCH1_VFP_Field use
     (Normal => 0,
      Disable => 1);

   --  Disables dybnamic allocation of ADD ans SUB instructions:
   type ACTLR_DISDYNADD_Field is
     (
      --  Normal operation. Some ADD and SUB instructions are resolved in EX1.
      Normal,
      --  All ADD and SUB instructions are resolved in EX2.
      Disable)
     with Size => 1;
   for ACTLR_DISDYNADD_Field use
     (Normal => 0,
      Disable => 1);

   --  Auxiliary Control Register. The ACTLR provides disable bits for the
   --  following processor functions: FPU exception outputs, Dual-issue
   --  functionality, Flushing of the trace output from the ITM and DWT,
   --  Dynamic read allocate mode. By default this register is set to provide
   --  optimum performance from the Cortex-M7 processor, and does not normally
   --  require modification.
   type ACTLR_Register is record
      --  unspecified
      Reserved_0_1      : HAL.UInt2 := 16#0#;
      --  Disables Interruption Folding
      DISFOLD           : ACTLR_DISFOLD_Field := Cortex_M_SVD.SCB.Normal;
      --  unspecified
      Reserved_3_9      : HAL.UInt7 := 16#0#;
      --  Disabled FPU exception outputs
      PFEXCODIS         : ACTLR_PFEXCODIS_Field := Cortex_M_SVD.SCB.Normal;
      --  Disables dynamic read allocate mode for Write-Back Write-Allocate
      --  memory regions:
      DISRAMODE         : ACTLR_DISRAMODE_Field := Cortex_M_SVD.SCB.Normal;
      --  Read-only. Disables ITM and DWT ATB flush:
      DISITMATBFLUSH    : ACTLR_DISITMATBFLUSH_Field :=
                           Actlr_Disitmatbflush_Field_Reset;
      --  Disables the Branch Target Address Cache (BTAC):
      DISBTACREAD       : ACTLR_DISBTACREAD_Field := Cortex_M_SVD.SCB.Normal;
      --  Disables the Branch Target Address Cache allocation:
      DISBTACALLOC      : ACTLR_DISBTACALLOC_Field := Cortex_M_SVD.SCB.Normal;
      --  Disables critical AXI Read-Under-Read:
      DISCRITAXIRUR     : ACTLR_DISCRITAXIRUR_Field :=
                           Cortex_M_SVD.SCB.Normal;
      --  Disables dual-issued direct branches:
      DISDI_DB          : ACTLR_DISDI_DB_Field := Cortex_M_SVD.SCB.Normal;
      --  Disables dual-issued indirect branches:
      DISDI_IB          : ACTLR_DISDI_IB_Field := Cortex_M_SVD.SCB.Normal;
      --  Disables dual-issued loads to PC:
      DISDI_LPC         : ACTLR_DISDI_LPC_Field := Cortex_M_SVD.SCB.Normal;
      --  Disables integer MAC and MUL dual-issued instructions:
      DISDI_MAC_MUL     : ACTLR_DISDI_MAC_MUL_Field :=
                           Cortex_M_SVD.SCB.Normal;
      --  Disables VFP dual-issued instruction:
      DISDI_VFP         : ACTLR_DISDI_VFP_Field := Cortex_M_SVD.SCB.Normal;
      --  Disables direct branches instructions in channel 1:
      DISISSCH1_DB      : ACTLR_DISISSCH1_DB_Field := Cortex_M_SVD.SCB.Normal;
      --  Disables indirect branches instructions in channel 1:
      DISISSCH1_IB      : ACTLR_DISISSCH1_IB_Field := Cortex_M_SVD.SCB.Normal;
      --  Disables loads to PC instructions in channel 1:
      DISISSCH1_LPC     : ACTLR_DISISSCH1_LPC_Field :=
                           Cortex_M_SVD.SCB.Normal;
      --  Disables integer MAC and MUL instructions in channel 1:
      DISISSCH1_MAC_MUL : ACTLR_DISISSCH1_MAC_MUL_Field :=
                           Cortex_M_SVD.SCB.Normal;
      --  Disables VFP instructions in channel 1:
      DISISSCH1_VFP     : ACTLR_DISISSCH1_VFP_Field :=
                           Cortex_M_SVD.SCB.Normal;
      --  Disables dybnamic allocation of ADD ans SUB instructions:
      DISDYNADD         : ACTLR_DISDYNADD_Field := Cortex_M_SVD.SCB.Normal;
      --  unspecified
      Reserved_27_31    : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACTLR_Register use record
      Reserved_0_1      at 0 range 0 .. 1;
      DISFOLD           at 0 range 2 .. 2;
      Reserved_3_9      at 0 range 3 .. 9;
      PFEXCODIS         at 0 range 10 .. 10;
      DISRAMODE         at 0 range 11 .. 11;
      DISITMATBFLUSH    at 0 range 12 .. 12;
      DISBTACREAD       at 0 range 13 .. 13;
      DISBTACALLOC      at 0 range 14 .. 14;
      DISCRITAXIRUR     at 0 range 15 .. 15;
      DISDI_DB          at 0 range 16 .. 16;
      DISDI_IB          at 0 range 17 .. 17;
      DISDI_LPC         at 0 range 18 .. 18;
      DISDI_MAC_MUL     at 0 range 19 .. 19;
      DISDI_VFP         at 0 range 20 .. 20;
      DISISSCH1_DB      at 0 range 21 .. 21;
      DISISSCH1_IB      at 0 range 22 .. 22;
      DISISSCH1_LPC     at 0 range 23 .. 23;
      DISISSCH1_MAC_MUL at 0 range 24 .. 24;
      DISISSCH1_VFP     at 0 range 25 .. 25;
      DISDYNADD         at 0 range 26 .. 26;
      Reserved_27_31    at 0 range 27 .. 31;
   end record;

   --  Revision number, the p value in the rnpn product revision identifier.
   type CPUID_Revision_Field is
     (
      --  Patch 0
      P0,
      --  Patch 1
      P1,
      --  Patch 2
      P2)
     with Size => 4;
   for CPUID_Revision_Field use
     (P0 => 0,
      P1 => 1,
      P2 => 2);

   --  Part number of the processor.
   type CPUID_PartNo_Field is
     (
      --  Cortes-M7
      Cortex_M7)
     with Size => 12;
   for CPUID_PartNo_Field use
     (Cortex_M7 => 3111);

   subtype CPUID_Constant_Field is HAL.UInt4;

   --  Variant number, the r value in the rnpn product revision identifier.
   type CPUID_Variant_Field is
     (
      --  Revision 0
      R0,
      --  Revision 1
      R1)
     with Size => 4;
   for CPUID_Variant_Field use
     (R0 => 0,
      R1 => 1);

   --  Implementer code.
   type CPUID_Implementer_Field is
     (
      --  ARM
      Arm)
     with Size => 8;
   for CPUID_Implementer_Field use
     (Arm => 65);

   --  CPUID Base Register
   type CPUID_Register is record
      --  Read-only. Revision number, the p value in the rnpn product revision
      --  identifier.
      Revision    : CPUID_Revision_Field;
      --  Read-only. Part number of the processor.
      PartNo      : CPUID_PartNo_Field;
      --  Read-only. Reads as 0xF.
      Constant_k  : CPUID_Constant_Field;
      --  Read-only. Variant number, the r value in the rnpn product revision
      --  identifier.
      Variant     : CPUID_Variant_Field;
      --  Read-only. Implementer code.
      Implementer : CPUID_Implementer_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUID_Register use record
      Revision    at 0 range 0 .. 3;
      PartNo      at 0 range 4 .. 15;
      Constant_k  at 0 range 16 .. 19;
      Variant     at 0 range 20 .. 23;
      Implementer at 0 range 24 .. 31;
   end record;

   subtype ICSR_VECTACTIVE_Field is HAL.UInt9;

   --  Indicates whether there are preempted active exceptions.
   type ICSR_RETTOBASE_Field is
     (
      --  There are preempted active exceptions to execute.
      Preempted,
      --  There are no active exceptions, or the currently-executing exception
      --  is the only active exception.
      No_Preempted)
     with Size => 1;
   for ICSR_RETTOBASE_Field use
     (Preempted => 0,
      No_Preempted => 1);

   subtype ICSR_VECTPENDING_Field is HAL.UInt9;

   --  Interrupt Control and State Register
   type ICSR_Register is record
      --  Read-only. Contains the active exception number. Subtract 16 from
      --  this value to obtain the CMSIS IRQ number required to index into the
      --  Interrupt Clear-Enable, Set-Enable, Clear-Pending, Set-Pending or
      --  Priority Registers.
      VECTACTIVE     : ICSR_VECTACTIVE_Field := 16#0#;
      --  unspecified
      Reserved_9_10  : HAL.UInt2 := 16#0#;
      --  Read-only. Indicates whether there are preempted active exceptions.
      RETTOBASE      : ICSR_RETTOBASE_Field := Cortex_M_SVD.SCB.Preempted;
      --  Read-only. Indicates the exception number of the highest priority
      --  pending enabled exception.
      VECTPENDING    : ICSR_VECTPENDING_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Interrupt pending flag, excluding NMI and Faults
      ISRPENDING     : Boolean := False;
      --  unspecified
      Reserved_23_24 : HAL.UInt2 := 16#0#;
      --  Write-only. SysTick exception clear-pending bit.
      PENDSTCLR      : Boolean := False;
      --  SysTick exception set-pending bit.
      PENDSTSET      : Boolean := False;
      --  Write-only. PendSV clear-pending bit.
      PENDSVCLR      : Boolean := False;
      --  PendSV set-pending bit.
      PENDSVSET      : Boolean := False;
      --  unspecified
      Reserved_29_30 : HAL.UInt2 := 16#0#;
      --  NMI set-pending bit.
      NMIPENDSET     : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICSR_Register use record
      VECTACTIVE     at 0 range 0 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      RETTOBASE      at 0 range 11 .. 11;
      VECTPENDING    at 0 range 12 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      ISRPENDING     at 0 range 22 .. 22;
      Reserved_23_24 at 0 range 23 .. 24;
      PENDSTCLR      at 0 range 25 .. 25;
      PENDSTSET      at 0 range 26 .. 26;
      PENDSVCLR      at 0 range 27 .. 27;
      PENDSVSET      at 0 range 28 .. 28;
      Reserved_29_30 at 0 range 29 .. 30;
      NMIPENDSET     at 0 range 31 .. 31;
   end record;

   --  Interrupt priority grouping field. This field determines the split of
   --  group priority from subpriority.
   type AIRCR_PRIGROUP_Field is
     (
      --  Group priority bits: [7:1], subpriority bits [0]
      Bxxxxxxx_Y,
      --  Group priority bits: [7:2], subpriority bits [1:0]
      Bxxxxxx_YY,
      --  Group priority bits: [7:3], subpriority bits [2:0]
      Bxxxxx_YYY,
      --  Group priority bits: [7:4], subpriority bits [3:0]
      Bxxxx_YYYY,
      --  Group priority bits: [7:5], subpriority bits [4:0]
      Bxxx_YYYYY,
      --  Group priority bits: [7:6], subpriority bits [5:0]
      Bxx_YYYYYY,
      --  Group priority bits: [7], subpriority bits [6:0]
      Bx_YYYYYYY,
      --  Group priority bits: None, subpriority bits [7:0]
      B_YYYYYYYY)
     with Size => 3;
   for AIRCR_PRIGROUP_Field use
     (Bxxxxxxx_Y => 0,
      Bxxxxxx_YY => 1,
      Bxxxxx_YYY => 2,
      Bxxxx_YYYY => 3,
      Bxxx_YYYYY => 4,
      Bxx_YYYYYY => 5,
      Bx_YYYYYYY => 6,
      B_YYYYYYYY => 7);

   --  Data endianness bit setting is implementation defined.
   type AIRCR_ENDIANNESS_Field is
     (
      --  Data is little endian
      Little_Endian,
      --  Data is big endian
      Big_Endian)
     with Size => 1;
   for AIRCR_ENDIANNESS_Field use
     (Little_Endian => 0,
      Big_Endian => 1);

   --  Register key. On write, write 0x5FA to VECTKEY, otherwise the write is
   --  ignored. Reads as 0xFA05
   type AIRCR_VECTKEY_Field is
     (
      --  The write key
      Key,
      --  The read key
      Key_Read)
     with Size => 16;
   for AIRCR_VECTKEY_Field use
     (Key => 1530,
      Key_Read => 64005);

   --  Application Interrupt and Reset Control Register
   type AIRCR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Write-only. System reset request bit setting is implementation
      --  defined.
      SYSRESETREQ    : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Interrupt priority grouping field. This field determines the split of
      --  group priority from subpriority.
      PRIGROUP       : AIRCR_PRIGROUP_Field := Cortex_M_SVD.SCB.Bxxxxxxx_Y;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  Read-only. Data endianness bit setting is implementation defined.
      ENDIANNESS     : AIRCR_ENDIANNESS_Field :=
                        Cortex_M_SVD.SCB.Little_Endian;
      --  Register key. On write, write 0x5FA to VECTKEY, otherwise the write
      --  is ignored. Reads as 0xFA05
      VECTKEY        : AIRCR_VECTKEY_Field := Cortex_M_SVD.SCB.Key_Read;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AIRCR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      SYSRESETREQ    at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      PRIGROUP       at 0 range 8 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      ENDIANNESS     at 0 range 15 .. 15;
      VECTKEY        at 0 range 16 .. 31;
   end record;

   --  System Control Register
   type SCR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Indicates sleep-on-exit when returning from Handler mode to Thread
      --  mode
      SLEEPONEXIT   : Boolean := False;
      --  Controls whether the processor uses sleep or deep sleep as its
      --  low-power mode
      SLEEPDEEP     : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  Send event on pending bit
      SEVONPEND     : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      SLEEPONEXIT   at 0 range 1 .. 1;
      SLEEPDEEP     at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      SEVONPEND     at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Indicates how the processor enters Thread mode
   type CCR_NONBASETHREADENA_Field is
     (
      --  Processor can enter Thread mode only when no exception is active
      No_Active_Exception,
      --  Processor can enter Thread mode from any level under the control of
      --  an EXC_RETURN value
      On_Exc_Return)
     with Size => 1;
   for CCR_NONBASETHREADENA_Field use
     (No_Active_Exception => 0,
      On_Exc_Return => 1);

   --  Configuration and Control Register
   type CCR_Register is record
      --  Indicates how the processor enters Thread mode
      NONBASETHREADENA : CCR_NONBASETHREADENA_Field :=
                          Cortex_M_SVD.SCB.No_Active_Exception;
      --  Enables unprivileged software access to the STIR
      USERSETMPEND     : Boolean := False;
      --  unspecified
      Reserved_2_2     : HAL.Bit := 16#0#;
      --  Enables unalign access traps.
      UNALIGNED_TRP    : Boolean := False;
      --  Enables faulting or halting when the processor executes an SDIF or
      --  UDIV instruction with a divisor of 0.
      DIV0_TRP         : Boolean := False;
      --  unspecified
      Reserved_5_7     : HAL.UInt3 := 16#0#;
      --  Enables handlers with priority -1 or -2 to ignore data BusFaults
      --  caused by load and store instructions. This applies to the hard
      --  fault, NMI, and FAULTMASK escalated handlers.
      BFHFNMIGN        : Boolean := False;
      --  Read-only. Always reads-as-one. It indicates stack alignment on
      --  exception entry is 8-byte aligned.
      STKALIGN         : Boolean := True;
      --  unspecified
      Reserved_10_15   : HAL.UInt6 := 16#0#;
      --  Enables L1 data cache.
      DC               : Boolean := False;
      --  Enables L1 instruction cache.
      IC               : Boolean := False;
      --  unspecified
      Reserved_18_31   : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      NONBASETHREADENA at 0 range 0 .. 0;
      USERSETMPEND     at 0 range 1 .. 1;
      Reserved_2_2     at 0 range 2 .. 2;
      UNALIGNED_TRP    at 0 range 3 .. 3;
      DIV0_TRP         at 0 range 4 .. 4;
      Reserved_5_7     at 0 range 5 .. 7;
      BFHFNMIGN        at 0 range 8 .. 8;
      STKALIGN         at 0 range 9 .. 9;
      Reserved_10_15   at 0 range 10 .. 15;
      DC               at 0 range 16 .. 16;
      IC               at 0 range 17 .. 17;
      Reserved_18_31   at 0 range 18 .. 31;
   end record;

   subtype SHPR1_PRI_4_Field is HAL.UInt8;
   subtype SHPR1_PRI_5_Field is HAL.UInt8;
   subtype SHPR1_PRI_6_Field is HAL.UInt8;

   --  System Handler Priority Register 1
   type SHPR1_Register is record
      --  Priority of the system handler, MemManage
      PRI_4          : SHPR1_PRI_4_Field := 16#0#;
      --  Priority of the system handler, BusFault
      PRI_5          : SHPR1_PRI_5_Field := 16#0#;
      --  Priority of the system handler, UsageFault
      PRI_6          : SHPR1_PRI_6_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHPR1_Register use record
      PRI_4          at 0 range 0 .. 7;
      PRI_5          at 0 range 8 .. 15;
      PRI_6          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype SHPR2_PRI_11_Field is HAL.UInt8;

   --  System Handler Priority Register 2
   type SHPR2_Register is record
      --  unspecified
      Reserved_0_23 : HAL.UInt24 := 16#0#;
      --  Priority of the system handler, SVCall
      PRI_11        : SHPR2_PRI_11_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHPR2_Register use record
      Reserved_0_23 at 0 range 0 .. 23;
      PRI_11        at 0 range 24 .. 31;
   end record;

   subtype SHPR3_PRI_14_Field is HAL.UInt8;
   subtype SHPR3_PRI_15_Field is HAL.UInt8;

   --  System Handler Priority Register 3
   type SHPR3_Register is record
      --  unspecified
      Reserved_0_15 : HAL.UInt16 := 16#0#;
      --  Priority of the system handler, PendSV
      PRI_14        : SHPR3_PRI_14_Field := 16#0#;
      --  Priority of the system handler, SysTick
      PRI_15        : SHPR3_PRI_15_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHPR3_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      PRI_14        at 0 range 16 .. 23;
      PRI_15        at 0 range 24 .. 31;
   end record;

   --  System Handler Control and State Register
   type SHPRS_Register is record
      --  Read-only. MemManage exception active bit, reads as 1 if exception is
      --  active.
      MEMFAULTACT    : Boolean := False;
      --  Read-only. BusFault exception active bit, reads as 1 if exception is
      --  active.
      BUSFAULTACT    : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Read-only. UsageFault exception active bit, reads as 1 if exception
      --  is active.
      USGFAULTACT    : Boolean := False;
      --  unspecified
      Reserved_4_6   : HAL.UInt3 := 16#0#;
      --  Read-only. SVCall active bit, reads as 1 if exception is active.
      SVCALLACT      : Boolean := False;
      --  Read-only. Debug Monitor active bit, reads as 1 if exception is
      --  active.
      MONITORACT     : Boolean := False;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  Read-only. PendSV exception active bit, reads as 1 if exception is
      --  active.
      PENDSVACT      : Boolean := False;
      --  Read-only. Systick exception active bit, reads as 1 if exception is
      --  active.
      SYSTICKACT     : Boolean := False;
      --  Read-only. UsageFault exception pending bit, reads as 1 if exception
      --  is pending
      USGFAULTPENDED : Boolean := False;
      --  Read-only. MemManage exception pending bit, reads as 1 if exception
      --  is pending
      MEMFAULTPENDED : Boolean := False;
      --  Read-only. BusFault exception pending bit, reads as 1 if exception is
      --  pending
      BUSFAULTPENDED : Boolean := False;
      --  Read-only. SVCall pending bit, reads as 1 if exception is pending
      SVCALLPENDED   : Boolean := False;
      --  MemManage enable bit, set to 1 to enable
      MEMFAULTENA    : Boolean := False;
      --  BusFault enable bit, set to 1 to enable
      BUSFAULTENA    : Boolean := False;
      --  UsageFault enable bit, set to 1 to enable
      USGFAULTENA    : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHPRS_Register use record
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

   --  MemManage Fault Status Register
   type MMSR_Register is record
      --  Instruction access violation flag
      IACCVIOL     : Boolean := False;
      --  Data access violation flag
      DACCVIOL     : Boolean := False;
      --  unspecified
      Reserved_2_2 : HAL.Bit := 16#0#;
      --  MemManage fault on unstacking for a return from exception
      MUNSTKERR    : Boolean := False;
      --  MemManage fault on stacking for exception entry
      MSTKERR      : Boolean := False;
      --  MemManage fault during floating-point lazy state preservation.
      MLSPERR      : Boolean := False;
      --  unspecified
      Reserved_6_6 : HAL.Bit := 16#0#;
      --  MemManage fault address register valid flag.
      MMARVALID    : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for MMSR_Register use record
      IACCVIOL     at 0 range 0 .. 0;
      DACCVIOL     at 0 range 1 .. 1;
      Reserved_2_2 at 0 range 2 .. 2;
      MUNSTKERR    at 0 range 3 .. 3;
      MSTKERR      at 0 range 4 .. 4;
      MLSPERR      at 0 range 5 .. 5;
      Reserved_6_6 at 0 range 6 .. 6;
      MMARVALID    at 0 range 7 .. 7;
   end record;

   --  BusFault Status Register
   type BFSR_Register is record
      --  Instruction bus error
      IBUSERR      : Boolean := False;
      --  Precise data bus error
      PRECISERR    : Boolean := False;
      --  Precise data bus error
      IMPRECISERR  : Boolean := False;
      --  BusFault on unstacking for a return from exception.
      UNSTKERR     : Boolean := False;
      --  BusFault on stacking for exception entry.
      STKERR       : Boolean := False;
      --  BusFault on floating-point lazy state preservation.
      LSPERR       : Boolean := False;
      --  unspecified
      Reserved_6_6 : HAL.Bit := 16#0#;
      --  BusFault Address Register valid flag.
      BFARVALID    : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 8, Bit_Order => System.Low_Order_First;

   for BFSR_Register use record
      IBUSERR      at 0 range 0 .. 0;
      PRECISERR    at 0 range 1 .. 1;
      IMPRECISERR  at 0 range 2 .. 2;
      UNSTKERR     at 0 range 3 .. 3;
      STKERR       at 0 range 4 .. 4;
      LSPERR       at 0 range 5 .. 5;
      Reserved_6_6 at 0 range 6 .. 6;
      BFARVALID    at 0 range 7 .. 7;
   end record;

   --  UsageFault Status Register
   type UFSR_Register is record
      --  Undefined instruction UsageFault
      UNDEFINSTR     : Boolean := False;
      --  Invalid State UsageFault
      INVSTATE       : Boolean := False;
      --  Invalid PC load UsageFault, caused by an invalid PC load by
      --  EXC_RETURN
      INVPC          : Boolean := False;
      --  No coprocessor UsageFault
      NOCP           : Boolean := False;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Unaligned access UsageFault
      UNALIGNED      : Boolean := False;
      --  Divide by zero UsageFault.
      DIVBYZERO      : Boolean := False;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 16,
          Bit_Order => System.Low_Order_First;

   for UFSR_Register use record
      UNDEFINSTR     at 0 range 0 .. 0;
      INVSTATE       at 0 range 1 .. 1;
      INVPC          at 0 range 2 .. 2;
      NOCP           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      UNALIGNED      at 0 range 8 .. 8;
      DIVBYZERO      at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
   end record;

   --  HardFault Status Register
   type HFSR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Indicates a BusFault on a vector table read during exception
      --  processing.
      VECTTBL       : Boolean := False;
      --  unspecified
      Reserved_2_29 : HAL.UInt28 := 16#0#;
      --  Indicates a forced hard fault, generated by escalation of a fault
      --  with configurable priority that cannot be handled, either because of
      --  priority or because it is disabled.
      FORCED        : Boolean := False;
      --  Reserved for Debug use. When writing to the register, you must write
      --  1 to this bit, otherwise behavior is UNPREDICTABLE.
      DEBUGEVT      : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFSR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      VECTTBL       at 0 range 1 .. 1;
      Reserved_2_29 at 0 range 2 .. 29;
      FORCED        at 0 range 30 .. 30;
      DEBUGEVT      at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  System control block
   type SCB_Peripheral is record
      --  Auxiliary Control Register. The ACTLR provides disable bits for the
      --  following processor functions: FPU exception outputs, Dual-issue
      --  functionality, Flushing of the trace output from the ITM and DWT,
      --  Dynamic read allocate mode. By default this register is set to
      --  provide optimum performance from the Cortex-M7 processor, and does
      --  not normally require modification.
      ACTLR : aliased ACTLR_Register;
      --  CPUID Base Register
      CPUID : aliased CPUID_Register;
      --  Interrupt Control and State Register
      ICSR  : aliased ICSR_Register;
      --  Vector Table Offset Register
      VTOR  : aliased HAL.UInt32;
      --  Application Interrupt and Reset Control Register
      AIRCR : aliased AIRCR_Register;
      --  System Control Register
      SCR   : aliased SCR_Register;
      --  Configuration and Control Register
      CCR   : aliased CCR_Register;
      --  System Handler Priority Register 1
      SHPR1 : aliased SHPR1_Register;
      --  System Handler Priority Register 2
      SHPR2 : aliased SHPR2_Register;
      --  System Handler Priority Register 3
      SHPR3 : aliased SHPR3_Register;
      --  System Handler Control and State Register
      SHPRS : aliased SHPRS_Register;
      --  MemManage Fault Status Register
      MMSR  : aliased MMSR_Register;
      --  BusFault Status Register
      BFSR  : aliased BFSR_Register;
      --  UsageFault Status Register
      UFSR  : aliased UFSR_Register;
      --  HardFault Status Register
      HFSR  : aliased HFSR_Register;
      --  MemManage Fault Address Register
      MMAR  : aliased HAL.UInt32;
      --  BusFault Address Register
      BFAR  : aliased HAL.UInt32;
   end record
     with Volatile;

   for SCB_Peripheral use record
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
      SHPRS at 16#D24# range 0 .. 31;
      MMSR  at 16#D28# range 0 .. 7;
      BFSR  at 16#D29# range 0 .. 7;
      UFSR  at 16#D2A# range 0 .. 15;
      HFSR  at 16#D2C# range 0 .. 31;
      MMAR  at 16#D34# range 0 .. 31;
      BFAR  at 16#D38# range 0 .. 31;
   end record;

   --  System control block
   SCB_Periph : aliased SCB_Peripheral
     with Import, Address => System'To_Address (16#E000E000#);

end Cortex_M_SVD.SCB;
