--  This spec has been automatically generated from SiFive.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SiFive_SVD.WDT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CONFIG_SCALE_Field is HAL.UInt4;

   --  Watchdog Configuration Register.
   type CONFIG_Register is record
      SCALE          : CONFIG_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      RSTEN          : Boolean := False;
      ZEROCMP        : Boolean := False;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      ENALWAYS       : Boolean := False;
      ENCOREAWAKE    : Boolean := False;
      --  unspecified
      Reserved_14_27 : HAL.UInt14 := 16#0#;
      CMP_IP         : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      SCALE          at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      RSTEN          at 0 range 8 .. 8;
      ZEROCMP        at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      ENALWAYS       at 0 range 12 .. 12;
      ENCOREAWAKE    at 0 range 13 .. 13;
      Reserved_14_27 at 0 range 14 .. 27;
      CMP_IP         at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype COUNT_CNT_Field is HAL.UInt31;

   --  Watchdog Count Register.
   type COUNT_Register is record
      CNT            : COUNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for COUNT_Register use record
      CNT            at 0 range 0 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype SCALE_COUNT_CNT_Field is HAL.UInt16;

   --  Watchdog Scaled Counter Register.
   type SCALE_COUNT_Register is record
      CNT            : SCALE_COUNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCALE_COUNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype COMPARE_CMP_Field is HAL.UInt16;

   --  Watchdog Compare Register.
   type COMPARE_Register is record
      CMP            : COMPARE_CMP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for COMPARE_Register use record
      CMP            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Watchdog Timer.
   type WDT_Peripheral is record
      --  Watchdog Configuration Register.
      CONFIG      : aliased CONFIG_Register;
      --  Watchdog Count Register.
      COUNT       : aliased COUNT_Register;
      --  Watchdog Scaled Counter Register.
      SCALE_COUNT : aliased SCALE_COUNT_Register;
      --  Watchdog Feed Address.
      FEED        : aliased HAL.UInt32;
      --  Watchdog Key Register.
      KEY         : aliased HAL.UInt32;
      --  Watchdog Compare Register.
      COMPARE     : aliased COMPARE_Register;
   end record
     with Volatile;

   for WDT_Peripheral use record
      CONFIG      at 16#0# range 0 .. 31;
      COUNT       at 16#8# range 0 .. 31;
      SCALE_COUNT at 16#10# range 0 .. 31;
      FEED        at 16#18# range 0 .. 31;
      KEY         at 16#1C# range 0 .. 31;
      COMPARE     at 16#20# range 0 .. 31;
   end record;

   --  Watchdog Timer.
   WDT_Periph : aliased WDT_Peripheral
     with Import, Address => WDT_Base;

end SiFive_SVD.WDT;
