--  This spec has been automatically generated from cm0.svd

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

   subtype ICSR_VECTACTIVE_Field is HAL.UInt6;
   subtype ICSR_VECTPENDING_Field is HAL.UInt6;

   --  Interrupt Control and State Register
   type ICSR_Register is record
      --  Read-only. Contains the active exception number. Subtract 16 from
      --  this value to obtain the CMSIS IRQ number required to index into the
      --  Interrupt Clear-Enable, Set-Enable, Clear-Pending, Set-Pending or
      --  Priority Registers.
      VECTACTIVE     : ICSR_VECTACTIVE_Field := 16#0#;
      --  unspecified
      Reserved_6_11  : HAL.UInt6 := 16#0#;
      --  Read-only. Indicates the exception number of the highest priority
      --  pending enabled exception.
      VECTPENDING    : ICSR_VECTPENDING_Field := 16#0#;
      --  unspecified
      Reserved_18_21 : HAL.UInt4 := 16#0#;
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
      VECTACTIVE     at 0 range 0 .. 5;
      Reserved_6_11  at 0 range 6 .. 11;
      VECTPENDING    at 0 range 12 .. 17;
      Reserved_18_21 at 0 range 18 .. 21;
      ISRPENDING     at 0 range 22 .. 22;
      Reserved_23_24 at 0 range 23 .. 24;
      PENDSTCLR      at 0 range 25 .. 25;
      PENDSTSET      at 0 range 26 .. 26;
      PENDSVCLR      at 0 range 27 .. 27;
      PENDSVSET      at 0 range 28 .. 28;
      Reserved_29_30 at 0 range 29 .. 30;
      NMIPENDSET     at 0 range 31 .. 31;
   end record;

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
      Reserved_0_0  : HAL.Bit := 16#0#;
      --  Write-only. Reserved for debug use. This bit reads as 0. When writing
      --  to the register youmust write 0 to this bit, otherwise behavior is
      --  Unpredictable.
      VECTCLRACTIVE : Boolean := False;
      --  Write-only. System reset request bit setting is implementation
      --  defined.
      SYSRESETREQ   : Boolean := False;
      --  unspecified
      Reserved_3_14 : HAL.UInt12 := 16#0#;
      --  Read-only. Data endianness bit setting is implementation defined.
      ENDIANNESS    : AIRCR_ENDIANNESS_Field :=
                       Cortex_M_SVD.SCB.Little_Endian;
      --  Register key. On write, write 0x5FA to VECTKEY, otherwise the write
      --  is ignored. Reads as 0xFA05
      VECTKEY       : AIRCR_VECTKEY_Field := Cortex_M_SVD.SCB.Key_Read;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AIRCR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      VECTCLRACTIVE at 0 range 1 .. 1;
      SYSRESETREQ   at 0 range 2 .. 2;
      Reserved_3_14 at 0 range 3 .. 14;
      ENDIANNESS    at 0 range 15 .. 15;
      VECTKEY       at 0 range 16 .. 31;
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

   --  Configuration and Control Register
   type CCR_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3 := 16#0#;
      --  Enables unalign access traps.
      UNALIGNED_TRP  : Boolean := False;
      --  unspecified
      Reserved_4_8   : HAL.UInt5 := 16#0#;
      --  Read-only. Always reads-as-one. It indicates stack alignment on
      --  exception entry is 8-byte aligned.
      STKALIGN       : Boolean := True;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      UNALIGNED_TRP  at 0 range 3 .. 3;
      Reserved_4_8   at 0 range 4 .. 8;
      STKALIGN       at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
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

   -----------------
   -- Peripherals --
   -----------------

   --  System control block
   type SCB_Peripheral is record
      --  CPUID Base Register
      CPUID : aliased CPUID_Register;
      --  Interrupt Control and State Register
      ICSR  : aliased ICSR_Register;
      --  Application Interrupt and Reset Control Register
      AIRCR : aliased AIRCR_Register;
      --  System Control Register
      SCR   : aliased SCR_Register;
      --  Configuration and Control Register
      CCR   : aliased CCR_Register;
      --  System Handler Priority Register 2
      SHPR2 : aliased SHPR2_Register;
      --  System Handler Priority Register 3
      SHPR3 : aliased SHPR3_Register;
   end record
     with Volatile;

   for SCB_Peripheral use record
      CPUID at 16#0# range 0 .. 31;
      ICSR  at 16#4# range 0 .. 31;
      AIRCR at 16#C# range 0 .. 31;
      SCR   at 16#10# range 0 .. 31;
      CCR   at 16#14# range 0 .. 31;
      SHPR2 at 16#1C# range 0 .. 31;
      SHPR3 at 16#20# range 0 .. 31;
   end record;

   --  System control block
   SCB_Periph : aliased SCB_Peripheral
     with Import, Address => System'To_Address (16#E000ED00#);

end Cortex_M_SVD.SCB;
