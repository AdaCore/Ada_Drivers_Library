--  This spec has been automatically generated from FE310.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package FE310_SVD.RTC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CONFIG_SCALE_Field is HAL.UInt4;

   --  RTC Configuration Register.
   type CONFIG_Register is record
      SCALE          : CONFIG_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_4_11  : HAL.UInt8 := 16#0#;
      ENALWAYS       : Boolean := False;
      --  unspecified
      Reserved_13_27 : HAL.UInt15 := 16#0#;
      CMP_IP         : Boolean := False;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      SCALE          at 0 range 0 .. 3;
      Reserved_4_11  at 0 range 4 .. 11;
      ENALWAYS       at 0 range 12 .. 12;
      Reserved_13_27 at 0 range 13 .. 27;
      CMP_IP         at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype HI_CNT_Field is HAL.UInt16;

   --  RTC Count Register High.
   type HI_Register is record
      CNT            : HI_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HI_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Real-Time Clock.
   type RTC_Peripheral is record
      --  RTC Configuration Register.
      CONFIG      : aliased CONFIG_Register;
      --  RTC Count Register Low.
      LO          : aliased HAL.UInt32;
      --  RTC Count Register High.
      HI          : aliased HI_Register;
      --  RTC Scaled Counter Register.
      SCALE_COUNT : aliased HAL.UInt32;
      --  RTC Compare Register.
      COMPARE     : aliased HAL.UInt32;
   end record
     with Volatile;

   for RTC_Peripheral use record
      CONFIG      at 16#0# range 0 .. 31;
      LO          at 16#8# range 0 .. 31;
      HI          at 16#C# range 0 .. 31;
      SCALE_COUNT at 16#10# range 0 .. 31;
      COMPARE     at 16#20# range 0 .. 31;
   end record;

   --  Real-Time Clock.
   RTC_Periph : aliased RTC_Peripheral
     with Import, Address => System'To_Address (16#10000040#);

end FE310_SVD.RTC;
