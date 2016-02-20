--  Automatically generated from STM32F46_79x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.PWR is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- CR_Register --
   -----------------

   subtype CR_LPDS_Field is STM32_SVD.Bit;
   subtype CR_PDDS_Field is STM32_SVD.Bit;
   subtype CR_CWUF_Field is STM32_SVD.Bit;
   subtype CR_CSBF_Field is STM32_SVD.Bit;
   subtype CR_PVDE_Field is STM32_SVD.Bit;
   subtype CR_PLS_Field is STM32_SVD.UInt3;
   subtype CR_DBP_Field is STM32_SVD.Bit;
   subtype CR_FPDS_Field is STM32_SVD.Bit;
   subtype CR_LPLVDS_Field is STM32_SVD.Bit;
   subtype CR_MRLVDS_Field is STM32_SVD.Bit;
   subtype CR_VOS_Field is STM32_SVD.UInt2;
   subtype CR_ODEN_Field is STM32_SVD.Bit;
   subtype CR_ODSWEN_Field is STM32_SVD.Bit;
   subtype CR_UDEN_Field is STM32_SVD.UInt2;

   --  power control register
   type CR_Register is record
      --  Low-power deep sleep
      LPDS           : CR_LPDS_Field := 16#0#;
      --  Power down deepsleep
      PDDS           : CR_PDDS_Field := 16#0#;
      --  Clear wakeup flag
      CWUF           : CR_CWUF_Field := 16#0#;
      --  Clear standby flag
      CSBF           : CR_CSBF_Field := 16#0#;
      --  Power voltage detector enable
      PVDE           : CR_PVDE_Field := 16#0#;
      --  PVD level selection
      PLS            : CR_PLS_Field := 16#0#;
      --  Disable backup domain write protection
      DBP            : CR_DBP_Field := 16#0#;
      --  Flash power down in Stop mode
      FPDS           : CR_FPDS_Field := 16#0#;
      --  Low-Power Regulator Low Voltage in deepsleep
      LPLVDS         : CR_LPLVDS_Field := 16#0#;
      --  Main regulator low voltage in deepsleep mode
      MRLVDS         : CR_MRLVDS_Field := 16#0#;
      --  unspecified
      Reserved_12_13 : STM32_SVD.UInt2 := 16#0#;
      --  Regulator voltage scaling output selection
      VOS            : CR_VOS_Field := 16#3#;
      --  Over-drive enable
      ODEN           : CR_ODEN_Field := 16#0#;
      --  Over-drive switching enabled
      ODSWEN         : CR_ODSWEN_Field := 16#0#;
      --  Under-drive enable in stop mode
      UDEN           : CR_UDEN_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32_SVD.UInt12 := 16#0#;
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

   ------------------
   -- CSR_Register --
   ------------------

   subtype CSR_WUF_Field is STM32_SVD.Bit;
   subtype CSR_SBF_Field is STM32_SVD.Bit;
   subtype CSR_PVDO_Field is STM32_SVD.Bit;
   subtype CSR_BRR_Field is STM32_SVD.Bit;
   subtype CSR_EWUP_Field is STM32_SVD.Bit;
   subtype CSR_BRE_Field is STM32_SVD.Bit;
   subtype CSR_VOSRDY_Field is STM32_SVD.Bit;
   subtype CSR_ODRDY_Field is STM32_SVD.Bit;
   subtype CSR_ODSWRDY_Field is STM32_SVD.Bit;
   subtype CSR_UDRDY_Field is STM32_SVD.UInt2;

   --  power control/status register
   type CSR_Register is record
      --  Wakeup flag
      WUF            : CSR_WUF_Field := 16#0#;
      --  Standby flag
      SBF            : CSR_SBF_Field := 16#0#;
      --  PVD output
      PVDO           : CSR_PVDO_Field := 16#0#;
      --  Backup regulator ready
      BRR            : CSR_BRR_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : STM32_SVD.UInt4 := 16#0#;
      --  Enable WKUP pin
      EWUP           : CSR_EWUP_Field := 16#0#;
      --  Backup regulator enable
      BRE            : CSR_BRE_Field := 16#0#;
      --  unspecified
      Reserved_10_13 : STM32_SVD.UInt4 := 16#0#;
      --  Regulator voltage scaling output selection ready bit
      VOSRDY         : CSR_VOSRDY_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#0#;
      --  Over-drive mode ready
      ODRDY          : CSR_ODRDY_Field := 16#0#;
      --  Over-drive mode switching ready
      ODSWRDY        : CSR_ODSWRDY_Field := 16#0#;
      --  Under-drive ready flag
      UDRDY          : CSR_UDRDY_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32_SVD.UInt12 := 16#0#;
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
      CR  : CR_Register;
      --  power control/status register
      CSR : CSR_Register;
   end record
     with Volatile;

   for PWR_Peripheral use record
      CR  at 0 range 0 .. 31;
      CSR at 4 range 0 .. 31;
   end record;

   --  Power control
   PWR_Periph : aliased PWR_Peripheral
     with Import, Address => System'To_Address (16#40007000#);

end STM32_SVD.PWR;
