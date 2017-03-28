--  This spec has been automatically generated from cm4.svd

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

   subtype CTRL_POSTPRESET_Field is HAL.UInt4;
   subtype CTRL_POSTINIT_Field is HAL.UInt4;
   subtype CTRL_SYNCTAP_Field is HAL.UInt2;
   subtype CTRL_Reserved_13_15_Field is HAL.UInt3;
   subtype CTRL_NUMCOMP_Field is HAL.UInt4;

   --  Control Register
   type CTRL_Register is record
      --  enable cycle counter
      CYCCNTENA      : Boolean := False;
      --  ???
      POSTPRESET     : CTRL_POSTPRESET_Field := 16#0#;
      --  ???
      POSTINIT       : CTRL_POSTINIT_Field := 16#0#;
      --  ???
      CYCTAP         : Boolean := False;
      --  ???
      SYNCTAP        : CTRL_SYNCTAP_Field := 16#0#;
      --  enable POSTCNT as timer for PC sample packets
      PCSAMPLENA     : Boolean := False;
      --  Reserved bits 13..15
      Reserved_13_15 : CTRL_Reserved_13_15_Field := 16#0#;
      --  enable interrupt event tracing
      EXCTRCENA      : Boolean := False;
      --  enable CPI count event
      CPIEVTENA      : Boolean := False;
      --  enable interrupt overhead event
      EXCEVTENA      : Boolean := False;
      --  enable Sleep count event
      SLEEPEVTENA    : Boolean := False;
      --  enable Load Store Unit (LSU) count event
      LSUEVTENA      : Boolean := False;
      --  enable Folded instruction count event
      FOLDEVTENA     : Boolean := False;
      --  enable Cycle count event
      CYCEVTENA      : Boolean := False;
      --  Read-only. Reserved bit 23
      Reserved_23    : Boolean := False;
      --  No profiling counters
      NOPRFCNT       : Boolean := False;
      --  No cycle counter
      NOCYCCNT       : Boolean := False;
      --  No external match signals
      NOEXTTRIG      : Boolean := False;
      --  No trace sampling and exception tracing
      NOTRCPKT       : Boolean := False;
      --  Number of comparators
      NUMCOMP        : CTRL_NUMCOMP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTRL_Register use record
      CYCCNTENA      at 0 range 0 .. 0;
      POSTPRESET     at 0 range 1 .. 4;
      POSTINIT       at 0 range 5 .. 8;
      CYCTAP         at 0 range 9 .. 9;
      SYNCTAP        at 0 range 10 .. 11;
      PCSAMPLENA     at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      EXCTRCENA      at 0 range 16 .. 16;
      CPIEVTENA      at 0 range 17 .. 17;
      EXCEVTENA      at 0 range 18 .. 18;
      SLEEPEVTENA    at 0 range 19 .. 19;
      LSUEVTENA      at 0 range 20 .. 20;
      FOLDEVTENA     at 0 range 21 .. 21;
      CYCEVTENA      at 0 range 22 .. 22;
      Reserved_23    at 0 range 23 .. 23;
      NOPRFCNT       at 0 range 24 .. 24;
      NOCYCCNT       at 0 range 25 .. 25;
      NOEXTTRIG      at 0 range 26 .. 26;
      NOTRCPKT       at 0 range 27 .. 27;
      NUMCOMP        at 0 range 28 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Data Watchpoint Trace
   type DWT_Peripheral is record
      --  Control Register
      CTRL      : aliased CTRL_Register;
      --  Cycle Count Register
      CYCCNT    : aliased HAL.UInt32;
      --  CPI Count Register
      CPICNT    : aliased HAL.UInt32;
      --  Exception Overhead Count Register
      EXCCNT    : aliased HAL.UInt32;
      --  Sleep Count Register
      SLEEPCNT  : aliased HAL.UInt32;
      --  LSU Count Register
      LSUCNT    : aliased HAL.UInt32;
      --  Folded-instruction Count Register
      FOLDCNT   : aliased HAL.UInt32;
      --  Program Counter Sample Register
      PCSR      : aliased HAL.UInt32;
      --  Comparator Register 0
      COMP0     : aliased HAL.UInt32;
      --  Mask Register 0
      MASK0     : aliased HAL.UInt32;
      --  Function Register 0
      FUNCTION0 : aliased HAL.UInt32;
      --  Reserved 0
      RESERVED0 : aliased HAL.UInt32;
      --  Comparator Register 1
      COMP1     : aliased HAL.UInt32;
      --  Mask Register 1
      MASK1     : aliased HAL.UInt32;
      --  Function Register 1
      FUNCTION1 : aliased HAL.UInt32;
      --  Reserved 1
      RESERVED1 : aliased HAL.UInt32;
      --  Comparator Register 2
      COMP2     : aliased HAL.UInt32;
      --  Mask Register 2
      MASK2     : aliased HAL.UInt32;
      --  Function Register 2
      FUNCTION2 : aliased HAL.UInt32;
      --  Reserved 2
      RESERVED2 : aliased HAL.UInt32;
      --  Comparator Register 3
      COMP3     : aliased HAL.UInt32;
      --  Mask Register 3
      MASK3     : aliased HAL.UInt32;
      --  Function Register 3
      FUNCTION3 : aliased HAL.UInt32;
   end record
     with Volatile;

   for DWT_Peripheral use record
      CTRL      at 16#0# range 0 .. 31;
      CYCCNT    at 16#4# range 0 .. 31;
      CPICNT    at 16#8# range 0 .. 31;
      EXCCNT    at 16#C# range 0 .. 31;
      SLEEPCNT  at 16#10# range 0 .. 31;
      LSUCNT    at 16#14# range 0 .. 31;
      FOLDCNT   at 16#18# range 0 .. 31;
      PCSR      at 16#1C# range 0 .. 31;
      COMP0     at 16#20# range 0 .. 31;
      MASK0     at 16#24# range 0 .. 31;
      FUNCTION0 at 16#28# range 0 .. 31;
      RESERVED0 at 16#2C# range 0 .. 31;
      COMP1     at 16#30# range 0 .. 31;
      MASK1     at 16#34# range 0 .. 31;
      FUNCTION1 at 16#38# range 0 .. 31;
      RESERVED1 at 16#3C# range 0 .. 31;
      COMP2     at 16#40# range 0 .. 31;
      MASK2     at 16#44# range 0 .. 31;
      FUNCTION2 at 16#48# range 0 .. 31;
      RESERVED2 at 16#4C# range 0 .. 31;
      COMP3     at 16#50# range 0 .. 31;
      MASK3     at 16#54# range 0 .. 31;
      FUNCTION3 at 16#58# range 0 .. 31;
   end record;

   --  Data Watchpoint Trace
   DWT_Periph : aliased DWT_Peripheral
     with Import, Address => DWT_Base;

end Cortex_M_SVD.DWT;
