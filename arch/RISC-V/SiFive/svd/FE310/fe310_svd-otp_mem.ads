--  This spec has been automatically generated from FE310.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package FE310_SVD.OTP_Mem is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype LFROSC_TRIM_VALUE_Field is HAL.UInt5;

   --  LFROSC Trim Setting, left unburned on HiFive1.
   type LFROSC_TRIM_Register is record
      VALUE         : LFROSC_TRIM_VALUE_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFROSC_TRIM_Register use record
      VALUE         at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype HFROSC_TRIM_VALUE_Field is HAL.UInt5;

   --  HFROSC Trim Setting for 72MHz.
   type HFROSC_TRIM_Register is record
      VALUE         : HFROSC_TRIM_VALUE_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFROSC_TRIM_Register use record
      VALUE         at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Code to jump to SPI-FLASH.

   --  Code to jump to SPI-FLASH.
   type LAST_FENCE_Registers is array (0 .. 2) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  One-Time Programmable Memory.
   type OTP_Mem_Peripheral is record
      --  Code to jump to LAST FENCE.
      BOOT        : aliased HAL.UInt32;
      --  Board Identifier.
      STAMP       : aliased HAL.UInt32;
      --  LFROSC Trim Setting, left unburned on HiFive1.
      LFROSC_TRIM : aliased LFROSC_TRIM_Register;
      --  HFROSC Trim Setting for 72MHz.
      HFROSC_TRIM : aliased HFROSC_TRIM_Register;
      --  OTP Lifecycle Counter.
      LIFECYCLE   : aliased HAL.UInt32;
      --  Code to jump to SPI-FLASH.
      LAST_FENCE  : aliased LAST_FENCE_Registers;
   end record
     with Volatile;

   for OTP_Mem_Peripheral use record
      BOOT        at 16#0# range 0 .. 31;
      STAMP       at 16#1FE4# range 0 .. 31;
      LFROSC_TRIM at 16#1FE8# range 0 .. 31;
      HFROSC_TRIM at 16#1FEC# range 0 .. 31;
      LIFECYCLE   at 16#1FF0# range 0 .. 31;
      LAST_FENCE  at 16#1FF4# range 0 .. 95;
   end record;

   --  One-Time Programmable Memory.
   OTP_Mem_Periph : aliased OTP_Mem_Peripheral
     with Import, Address => System'To_Address (16#20000#);

end FE310_SVD.OTP_Mem;
