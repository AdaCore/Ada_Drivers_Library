--  This spec has been automatically generated from cm7.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

--  Processor Features
package Cortex_M_SVD.PF is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Level of Coherency
   type CLIDR_LoC_Field is
     (
      --  Level 1, if neither instruction nor data cache is implemented
      Level_1,
      --  Level 2, if either cache is implemented
      Level_2)
     with Size => 3;
   for CLIDR_LoC_Field use
     (Level_1 => 0,
      Level_2 => 1);

   --  Level of Unification
   type CLIDR_LoU_Field is
     (
      --  Level 1, if neither instruction nor data cache is implemented
      Level_1,
      --  Level 2, if either cache is implemented
      Level_2)
     with Size => 3;
   for CLIDR_LoU_Field use
     (Level_1 => 0,
      Level_2 => 1);

   --  Cache Level ID Register
   type CLIDR_Register is record
      --  Read-only. An instrumentation Cache is implemented at L1
      ICL1           : Boolean;
      --  Read-only. Data Cache is implemented at L1
      DCL1           : Boolean;
      --  Read-only. Unified Cache is implemented at L1
      UCL1           : Boolean;
      --  unspecified
      Reserved_3_23  : HAL.UInt21;
      --  Read-only. Level of Coherency
      LoC            : CLIDR_LoC_Field;
      --  Read-only. Level of Unification
      LoU            : CLIDR_LoU_Field;
      --  unspecified
      Reserved_30_31 : HAL.UInt2;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLIDR_Register use record
      ICL1           at 0 range 0 .. 0;
      DCL1           at 0 range 1 .. 1;
      UCL1           at 0 range 2 .. 2;
      Reserved_3_23  at 0 range 3 .. 23;
      LoC            at 0 range 24 .. 26;
      LoU            at 0 range 27 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Smallest cache line of all the instruction caches.
   type CTR_IMinLine_Field is
     (
      --  8 words for the Cortex-M7 processor.
      Line_8_Words)
     with Size => 4;
   for CTR_IMinLine_Field use
     (Line_8_Words => 3);

   --  Smallest cache line of all the data caches.
   type CTR_DMinLine_Field is
     (
      --  8 words for the Cortex-M7 processor.
      Line_8_Words)
     with Size => 4;
   for CTR_DMinLine_Field use
     (Line_8_Words => 3);

   subtype CTR_ERG_Field is HAL.UInt4;

   --  Cache Writable Granule.
   type CTR_CWG_Field is
     (
      --  8 words granularity for the Cortex-M7 processor.
      Granularity_8_Words)
     with Size => 4;
   for CTR_CWG_Field use
     (Granularity_8_Words => 3);

   --  Cache Writable Granule.
   type CTR_Format_Field is
     (
      --  ARMv7 register forat.
      Armv7)
     with Size => 3;
   for CTR_Format_Field use
     (Armv7 => 4);

   --  Cache Type Register
   type CTR_Register is record
      --  Read-only. Smallest cache line of all the instruction caches.
      IMinLine       : CTR_IMinLine_Field;
      --  unspecified
      Reserved_4_15  : HAL.UInt12;
      --  Read-only. Smallest cache line of all the data caches.
      DMinLine       : CTR_DMinLine_Field;
      --  Read-only. Exclusive Reservation Granule. The local monitor within
      --  the processor does not hold any physical address. It treats any STREX
      --  instruction access as matching the address of the previous LDREX
      --  instruction. This means that the implemented exclusive reservation
      --  granule is the entire memory address range.
      ERG            : CTR_ERG_Field;
      --  Read-only. Cache Writable Granule.
      CWG            : CTR_CWG_Field;
      --  unspecified
      Reserved_28_28 : HAL.Bit;
      --  Read-only. Cache Writable Granule.
      Format         : CTR_Format_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CTR_Register use record
      IMinLine       at 0 range 0 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      DMinLine       at 0 range 16 .. 19;
      ERG            at 0 range 20 .. 23;
      CWG            at 0 range 24 .. 27;
      Reserved_28_28 at 0 range 28 .. 28;
      Format         at 0 range 29 .. 31;
   end record;

   subtype CCSIDR_LineSize_Field is HAL.UInt3;
   subtype CCSIDR_Associativity_Field is HAL.UInt10;
   subtype CCSIDR_NumSets_Field is HAL.UInt15;

   --  Cache Size ID Register
   type CCSIDR_Register is record
      --  Read-only. Indicates the number of words in each cache line. Encoded
      --  as 2 - log(2) number of words. e.g. 0 means 4 words, 1 means 8 words.
      LineSize      : CCSIDR_LineSize_Field;
      --  Read-only. Indicates the number of ways as (ways - 1)
      Associativity : CCSIDR_Associativity_Field;
      --  Read-only. Indicates the number of sets as (sets - 1)
      NumSets       : CCSIDR_NumSets_Field;
      --  Read-only. Indicates support available for write allocation.
      WA            : Boolean;
      --  Read-only. Indicates support available for read allocation.
      RA            : Boolean;
      --  Read-only. Indicates support available for write back.
      WB            : Boolean;
      --  Read-only. Indicates support available for write through.
      WT            : Boolean;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCSIDR_Register use record
      LineSize      at 0 range 0 .. 2;
      Associativity at 0 range 3 .. 12;
      NumSets       at 0 range 13 .. 27;
      WA            at 0 range 28 .. 28;
      RA            at 0 range 29 .. 29;
      WB            at 0 range 30 .. 30;
      WT            at 0 range 31 .. 31;
   end record;

   --  Selects the cache currently visible in the CCSIDR.
   type CSSELR_InD_Field is
     (
      --  Data cache is selected.
      Data_Cache,
      --  Instruction cache is selected.
      Instruction_Cache)
     with Size => 1;
   for CSSELR_InD_Field use
     (Data_Cache => 0,
      Instruction_Cache => 1);

   --  Identifies the cache level selected.
   type CSSELR_Level_Field is
     (
      --  Level 1 cache is selected.
      Level_1)
     with Size => 3;
   for CSSELR_Level_Field use
     (Level_1 => 0);

   --  Cache Size Selection Register
   type CSSELR_Register is record
      --  Selects the cache currently visible in the CCSIDR.
      InD           : CSSELR_InD_Field := Cortex_M_SVD.PF.Data_Cache;
      --  Identifies the cache level selected.
      Level         : CSSELR_Level_Field := Cortex_M_SVD.PF.Level_1;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSSELR_Register use record
      InD           at 0 range 0 .. 0;
      Level         at 0 range 1 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Processor Features
   type PF_Peripheral is record
      --  Cache Level ID Register
      CLIDR  : aliased CLIDR_Register;
      --  Cache Type Register
      CTR    : aliased CTR_Register;
      --  Cache Size ID Register
      CCSIDR : aliased CCSIDR_Register;
      --  Cache Size Selection Register
      CSSELR : aliased CSSELR_Register;
   end record
     with Volatile;

   for PF_Peripheral use record
      CLIDR  at 16#0# range 0 .. 31;
      CTR    at 16#4# range 0 .. 31;
      CCSIDR at 16#8# range 0 .. 31;
      CSSELR at 16#C# range 0 .. 31;
   end record;

   --  Processor Features
   PF_Periph : aliased PF_Peripheral
     with Import, Address => PF_Base;

end Cortex_M_SVD.PF;
