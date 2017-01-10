--  This spec has been automatically generated from cm7.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

--  Cache maintenance operations
package Cortex_M_SVD.Cache is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype DCISW_Set_Field is HAL.UInt9;
   subtype DCISW_Way_Field is HAL.UInt2;

   --  Data cache invalidate by set/way
   type DCISW_Register is record
      --  unspecified
      Reserved_0_4   : HAL.UInt5 := 16#0#;
      --  Write-only. Set/index that operation applies to. The number of
      --  indices in a cache depends on the configured cache size. When this is
      --  less than the maximum, use the LSB of this field.
      Set            : DCISW_Set_Field := 16#0#;
      --  unspecified
      Reserved_14_29 : HAL.UInt16 := 16#0#;
      --  Write-only. Way that operation applies to. For the data cache, values
      --  0, 1, 2 and 3 are supported..
      Way            : DCISW_Way_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCISW_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      Set            at 0 range 5 .. 13;
      Reserved_14_29 at 0 range 14 .. 29;
      Way            at 0 range 30 .. 31;
   end record;

   subtype DCCSW_Set_Field is HAL.UInt9;
   subtype DCCSW_Way_Field is HAL.UInt2;

   --  Data cache clean by set/way
   type DCCSW_Register is record
      --  unspecified
      Reserved_0_4   : HAL.UInt5 := 16#0#;
      --  Write-only. Set/index that operation applies to. The number of
      --  indices in a cache depends on the configured cache size. When this is
      --  less than the maximum, use the LSB of this field.
      Set            : DCCSW_Set_Field := 16#0#;
      --  unspecified
      Reserved_14_29 : HAL.UInt16 := 16#0#;
      --  Write-only. Way that operation applies to. For the data cache, values
      --  0, 1, 2 and 3 are supported..
      Way            : DCCSW_Way_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCCSW_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      Set            at 0 range 5 .. 13;
      Reserved_14_29 at 0 range 14 .. 29;
      Way            at 0 range 30 .. 31;
   end record;

   subtype DCCISW_Set_Field is HAL.UInt9;
   subtype DCCISW_Way_Field is HAL.UInt2;

   --  Data cache clean and invalidate by set/way
   type DCCISW_Register is record
      --  unspecified
      Reserved_0_4   : HAL.UInt5 := 16#0#;
      --  Write-only. Set/index that operation applies to. The number of
      --  indices in a cache depends on the configured cache size. When this is
      --  less than the maximum, use the LSB of this field.
      Set            : DCCISW_Set_Field := 16#0#;
      --  unspecified
      Reserved_14_29 : HAL.UInt16 := 16#0#;
      --  Write-only. Way that operation applies to. For the data cache, values
      --  0, 1, 2 and 3 are supported..
      Way            : DCCISW_Way_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCCISW_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      Set            at 0 range 5 .. 13;
      Reserved_14_29 at 0 range 14 .. 29;
      Way            at 0 range 30 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Cache maintenance operations
   type Cache_Peripheral is record
      --  Instruction cache invalidate all to the PoU
      ICIALLU  : aliased HAL.UInt32;
      --  Instruction cache invalidate by address to the PoU
      ICIMVAU  : aliased HAL.UInt32;
      --  Data cache invalidate by address to the PoC
      DCIMVAC  : aliased HAL.UInt32;
      --  Data cache invalidate by set/way
      DCISW    : aliased DCISW_Register;
      --  Data cache clean by address to the PoU
      DCCMVAU  : aliased HAL.UInt32;
      --  Data cache clean by address to the PoC
      DCCMVAC  : aliased HAL.UInt32;
      --  Data cache clean by set/way
      DCCSW    : aliased DCCSW_Register;
      --  Data cache clean and invalidate by address to the PoC
      DCCIMVAC : aliased HAL.UInt32;
      --  Data cache clean and invalidate by set/way
      DCCISW   : aliased DCCISW_Register;
   end record
     with Volatile;

   for Cache_Peripheral use record
      ICIALLU  at 16#0# range 0 .. 31;
      ICIMVAU  at 16#8# range 0 .. 31;
      DCIMVAC  at 16#C# range 0 .. 31;
      DCISW    at 16#10# range 0 .. 31;
      DCCMVAU  at 16#14# range 0 .. 31;
      DCCMVAC  at 16#18# range 0 .. 31;
      DCCSW    at 16#1C# range 0 .. 31;
      DCCIMVAC at 16#20# range 0 .. 31;
      DCCISW   at 16#24# range 0 .. 31;
   end record;

   --  Cache maintenance operations
   Cache_Periph : aliased Cache_Peripheral
     with Import, Address => Cache_Base;

end Cortex_M_SVD.Cache;
