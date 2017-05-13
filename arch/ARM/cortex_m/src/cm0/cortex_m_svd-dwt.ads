--  This spec has been automatically generated from cm0.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

--  Data Watchpoint Trace
package Cortex_M_SVD.DWT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CTRL_Reserved_0_27_Field is HAL.UInt28;
   subtype CTRL_NUMCOMP_Field is HAL.UInt4;

   --  Control Register
   type CTRL_Register is record
      --  Read-only. Reserved bits 0..27
      Reserved_0_27 : CTRL_Reserved_0_27_Field := 16#0#;
      --  Number of comparators available
      NUMCOMP       : CTRL_NUMCOMP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      Reserved_0_27 at 0 range 0 .. 27;
      NUMCOMP       at 0 range 28 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Data Watchpoint Trace
   type DWT_Peripheral is record
      --  Control Register
      CTRL      : aliased CTRL_Register;
      --  Program Counter Sample Register
      PCSR      : aliased HAL.UInt32;
      --  Comparator Register 0
      COMP0     : aliased HAL.UInt32;
      --  Mask Register 0
      MASK0     : aliased HAL.UInt32;
      --  Function Register 0
      FUNCTION0 : aliased HAL.UInt32;
   end record
     with Volatile;

   for DWT_Peripheral use record
      CTRL      at 16#0# range 0 .. 31;
      PCSR      at 16#1C# range 0 .. 31;
      COMP0     at 16#20# range 0 .. 31;
      MASK0     at 16#24# range 0 .. 31;
      FUNCTION0 at 16#28# range 0 .. 31;
   end record;

   --  Data Watchpoint Trace
   DWT_Periph : aliased DWT_Peripheral
     with Import, Address => DWT_Base;

end Cortex_M_SVD.DWT;
