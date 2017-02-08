--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.PWR is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_PLS_Field is HAL.UInt3;
   subtype CR_VOS_Field is HAL.UInt2;
   subtype CR_UDEN_Field is HAL.UInt2;

   --  power control register
   type CR_Register is record
      --  Low-power deep sleep
      LPDS           : Boolean := False;
      --  Power down deepsleep
      PDDS           : Boolean := False;
      --  Clear wakeup flag
      CWUF           : Boolean := False;
      --  Clear standby flag
      CSBF           : Boolean := False;
      --  Power voltage detector enable
      PVDE           : Boolean := False;
      --  PVD level selection
      PLS            : CR_PLS_Field := 16#0#;
      --  Disable backup domain write protection
      DBP            : Boolean := False;
      --  Flash power down in Stop mode
      FPDS           : Boolean := False;
      --  Low-Power Regulator Low Voltage in deepsleep
      LPLVDS         : Boolean := False;
      --  Main regulator low voltage in deepsleep mode
      MRLVDS         : Boolean := False;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  Regulator voltage scaling output selection
      VOS            : CR_VOS_Field := 16#3#;
      --  Over-drive enable
      ODEN           : Boolean := False;
      --  Over-drive switching enabled
      ODSWEN         : Boolean := False;
      --  Under-drive enable in stop mode
      UDEN           : CR_UDEN_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      LPDS           at 0 range 0 .. 0;
      PDDS           at 0 range 1 .. 1;
      CWUF           at 0 range 2 .. 2;
      CSBF           at 0 range 3 .. 3;
      PVDE           at 0 range 4 .. 4;
      PLS            at 0 range 5 .. 7;
      DBP            at 0 range 8 .. 8;
      FPDS           at 0 range 9 .. 9;
      LPLVDS         at 0 range 10 .. 10;
      MRLVDS         at 0 range 11 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      VOS            at 0 range 14 .. 15;
      ODEN           at 0 range 16 .. 16;
      ODSWEN         at 0 range 17 .. 17;
      UDEN           at 0 range 18 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype CSR_UDRDY_Field is HAL.UInt2;

   --  power control/status register
   type CSR_Register is record
      --  Read-only. Wakeup flag
      WUF            : Boolean := False;
      --  Read-only. Standby flag
      SBF            : Boolean := False;
      --  Read-only. PVD output
      PVDO           : Boolean := False;
      --  Read-only. Backup regulator ready
      BRR            : Boolean := False;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Enable WKUP pin
      EWUP           : Boolean := False;
      --  Backup regulator enable
      BRE            : Boolean := False;
      --  unspecified
      Reserved_10_13 : HAL.UInt4 := 16#0#;
      --  Regulator voltage scaling output selection ready bit
      VOSRDY         : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Read-only. Over-drive mode ready
      ODRDY          : Boolean := False;
      --  Read-only. Over-drive mode switching ready
      ODSWRDY        : Boolean := False;
      --  Under-drive ready flag
      UDRDY          : CSR_UDRDY_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      WUF            at 0 range 0 .. 0;
      SBF            at 0 range 1 .. 1;
      PVDO           at 0 range 2 .. 2;
      BRR            at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      EWUP           at 0 range 8 .. 8;
      BRE            at 0 range 9 .. 9;
      Reserved_10_13 at 0 range 10 .. 13;
      VOSRDY         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      ODRDY          at 0 range 16 .. 16;
      ODSWRDY        at 0 range 17 .. 17;
      UDRDY          at 0 range 18 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Power control
   type PWR_Peripheral is record
      --  power control register
      CR  : aliased CR_Register;
      --  power control/status register
      CSR : aliased CSR_Register;
   end record
     with Volatile;

   for PWR_Peripheral use record
      CR  at 16#0# range 0 .. 31;
      CSR at 16#4# range 0 .. 31;
   end record;

   --  Power control
   PWR_Periph : aliased PWR_Peripheral
     with Import, Address => System'To_Address (16#40007000#);

end STM32_SVD.PWR;
