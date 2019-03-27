--  This spec has been automatically generated from SiFive.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SiFive_SVD.AON is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype LFROSCCFG_DIV_Field is HAL.UInt6;
   subtype LFROSCCFG_TRIM_Field is HAL.UInt5;

   --  LF Ring Oscillator Configuration Register.
   type LFROSCCFG_Register is record
      DIV            : LFROSCCFG_DIV_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : HAL.UInt10 := 16#0#;
      TRIM           : LFROSCCFG_TRIM_Field := 16#0#;
      --  unspecified
      Reserved_21_29 : HAL.UInt9 := 16#0#;
      ENABLE         : Boolean := False;
      READY          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LFROSCCFG_Register use record
      DIV            at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      TRIM           at 0 range 16 .. 20;
      Reserved_21_29 at 0 range 21 .. 29;
      ENABLE         at 0 range 30 .. 30;
      READY          at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  AON Clock Configuration.
   type AON_Peripheral is record
      --  LF Ring Oscillator Configuration Register.
      LFROSCCFG : aliased LFROSCCFG_Register;
   end record
     with Volatile;

   for AON_Peripheral use record
      LFROSCCFG at 0 range 0 .. 31;
   end record;

   --  AON Clock Configuration.
   AON_Periph : aliased AON_Peripheral
     with Import, Address => AON_Base;

end SiFive_SVD.AON;
