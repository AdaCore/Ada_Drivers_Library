--  Automatically generated from STM32F7x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.PWR is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ------------------
   -- CR1_Register --
   ------------------

   subtype CR1_LPDS_Field is STM32_SVD.Bit;
   subtype CR1_PDDS_Field is STM32_SVD.Bit;
   subtype CR1_CSBF_Field is STM32_SVD.Bit;
   subtype CR1_PVDE_Field is STM32_SVD.Bit;
   subtype CR1_PLS_Field is STM32_SVD.UInt3;
   subtype CR1_DBP_Field is STM32_SVD.Bit;
   subtype CR1_FPDS_Field is STM32_SVD.Bit;
   subtype CR1_LPUDS_Field is STM32_SVD.Bit;
   subtype CR1_MRUDS_Field is STM32_SVD.Bit;
   subtype CR1_ADCDC1_Field is STM32_SVD.Bit;
   subtype CR1_VOS_Field is STM32_SVD.UInt2;
   subtype CR1_ODEN_Field is STM32_SVD.Bit;
   subtype CR1_ODSWEN_Field is STM32_SVD.Bit;
   subtype CR1_UDEN_Field is STM32_SVD.UInt2;

   --  power control register
   type CR1_Register is record
      --  Low-power deep sleep
      LPDS           : CR1_LPDS_Field := 16#0#;
      --  Power down deepsleep
      PDDS           : CR1_PDDS_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : STM32_SVD.Bit := 16#0#;
      --  Clear standby flag
      CSBF           : CR1_CSBF_Field := 16#0#;
      --  Power voltage detector enable
      PVDE           : CR1_PVDE_Field := 16#0#;
      --  PVD level selection
      PLS            : CR1_PLS_Field := 16#0#;
      --  Disable backup domain write protection
      DBP            : CR1_DBP_Field := 16#0#;
      --  Flash power down in Stop mode
      FPDS           : CR1_FPDS_Field := 16#0#;
      --  Low-power regulator in deepsleep under-drive mode
      LPUDS          : CR1_LPUDS_Field := 16#0#;
      --  Main regulator in deepsleep under-drive mode
      MRUDS          : CR1_MRUDS_Field := 16#0#;
      --  unspecified
      Reserved_12_12 : STM32_SVD.Bit := 16#0#;
      --  ADCDC1
      ADCDC1         : CR1_ADCDC1_Field := 16#0#;
      --  Regulator voltage scaling output selection
      VOS            : CR1_VOS_Field := 16#3#;
      --  Over-drive enable
      ODEN           : CR1_ODEN_Field := 16#0#;
      --  Over-drive switching enabled
      ODSWEN         : CR1_ODSWEN_Field := 16#0#;
      --  Under-drive enable in stop mode
      UDEN           : CR1_UDEN_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32_SVD.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      LPDS           at 0 range 0 .. 0;
      PDDS           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      CSBF           at 0 range 3 .. 3;
      PVDE           at 0 range 4 .. 4;
      PLS            at 0 range 5 .. 7;
      DBP            at 0 range 8 .. 8;
      FPDS           at 0 range 9 .. 9;
      LPUDS          at 0 range 10 .. 10;
      MRUDS          at 0 range 11 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      ADCDC1         at 0 range 13 .. 13;
      VOS            at 0 range 14 .. 15;
      ODEN           at 0 range 16 .. 16;
      ODSWEN         at 0 range 17 .. 17;
      UDEN           at 0 range 18 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   -------------------
   -- CSR1_Register --
   -------------------

   subtype CSR1_WUIF_Field is STM32_SVD.Bit;
   subtype CSR1_SBF_Field is STM32_SVD.Bit;
   subtype CSR1_PVDO_Field is STM32_SVD.Bit;
   subtype CSR1_BRR_Field is STM32_SVD.Bit;
   subtype CSR1_BRE_Field is STM32_SVD.Bit;
   subtype CSR1_VOSRDY_Field is STM32_SVD.Bit;
   subtype CSR1_ODRDY_Field is STM32_SVD.Bit;
   subtype CSR1_ODSWRDY_Field is STM32_SVD.Bit;
   subtype CSR1_UDRDY_Field is STM32_SVD.UInt2;

   --  power control/status register
   type CSR1_Register is record
      --  Wakeup internal flag
      WUIF           : CSR1_WUIF_Field := 16#0#;
      --  Standby flag
      SBF            : CSR1_SBF_Field := 16#0#;
      --  PVD output
      PVDO           : CSR1_PVDO_Field := 16#0#;
      --  Backup regulator ready
      BRR            : CSR1_BRR_Field := 16#0#;
      --  unspecified
      Reserved_4_8   : STM32_SVD.UInt5 := 16#0#;
      --  Backup regulator enable
      BRE            : CSR1_BRE_Field := 16#0#;
      --  unspecified
      Reserved_10_13 : STM32_SVD.UInt4 := 16#0#;
      --  Regulator voltage scaling output selection ready bit
      VOSRDY         : CSR1_VOSRDY_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#0#;
      --  Over-drive mode ready
      ODRDY          : CSR1_ODRDY_Field := 16#0#;
      --  Over-drive mode switching ready
      ODSWRDY        : CSR1_ODSWRDY_Field := 16#0#;
      --  Under-drive ready flag
      UDRDY          : CSR1_UDRDY_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32_SVD.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR1_Register use record
      WUIF           at 0 range 0 .. 0;
      SBF            at 0 range 1 .. 1;
      PVDO           at 0 range 2 .. 2;
      BRR            at 0 range 3 .. 3;
      Reserved_4_8   at 0 range 4 .. 8;
      BRE            at 0 range 9 .. 9;
      Reserved_10_13 at 0 range 10 .. 13;
      VOSRDY         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      ODRDY          at 0 range 16 .. 16;
      ODSWRDY        at 0 range 17 .. 17;
      UDRDY          at 0 range 18 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   ------------------
   -- CR2_Register --
   ------------------

   ---------------
   -- CR2.CWUPF --
   ---------------

   --  CR2_CWUPF array element
   subtype CR2_CWUPF_Element is STM32_SVD.Bit;

   --  CR2_CWUPF array
   type CR2_CWUPF_Field_Array is array (0 .. 5) of CR2_CWUPF_Element
     with Component_Size => 1, Size => 6;

   --  Type definition for CR2_CWUPF
   type CR2_CWUPF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CWUPF as a value
            Val : STM32_SVD.UInt6;
         when True =>
            --  CWUPF as an array
            Arr : CR2_CWUPF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for CR2_CWUPF_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --------------
   -- CR2.WUPP --
   --------------

   --  CR2_WUPP array element
   subtype CR2_WUPP_Element is STM32_SVD.Bit;

   --  CR2_WUPP array
   type CR2_WUPP_Field_Array is array (0 .. 5) of CR2_WUPP_Element
     with Component_Size => 1, Size => 6;

   --  Type definition for CR2_WUPP
   type CR2_WUPP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WUPP as a value
            Val : STM32_SVD.UInt6;
         when True =>
            --  WUPP as an array
            Arr : CR2_WUPP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for CR2_WUPP_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  power control register
   type CR2_Register is record
      --  Clear Wakeup Pin flag for PA0
      CWUPF          : CR2_CWUPF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_6_7   : STM32_SVD.UInt2 := 16#0#;
      --  Wakeup pin polarity bit for PA0
      WUPP           : CR2_WUPP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      CWUPF          at 0 range 0 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      WUPP           at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -------------------
   -- CSR2_Register --
   -------------------

   ---------------
   -- CSR2.WUPF --
   ---------------

   --  CSR2_WUPF array element
   subtype CSR2_WUPF_Element is STM32_SVD.Bit;

   --  CSR2_WUPF array
   type CSR2_WUPF_Field_Array is array (0 .. 5) of CSR2_WUPF_Element
     with Component_Size => 1, Size => 6;

   --  Type definition for CSR2_WUPF
   type CSR2_WUPF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WUPF as a value
            Val : STM32_SVD.UInt6;
         when True =>
            --  WUPF as an array
            Arr : CSR2_WUPF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for CSR2_WUPF_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   ---------------
   -- CSR2.EWUP --
   ---------------

   --  CSR2_EWUP array element
   subtype CSR2_EWUP_Element is STM32_SVD.Bit;

   --  CSR2_EWUP array
   type CSR2_EWUP_Field_Array is array (0 .. 5) of CSR2_EWUP_Element
     with Component_Size => 1, Size => 6;

   --  Type definition for CSR2_EWUP
   type CSR2_EWUP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EWUP as a value
            Val : STM32_SVD.UInt6;
         when True =>
            --  EWUP as an array
            Arr : CSR2_EWUP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for CSR2_EWUP_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  power control/status register
   type CSR2_Register is record
      --  Wakeup Pin flag for PA0
      WUPF           : CSR2_WUPF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_6_7   : STM32_SVD.UInt2 := 16#0#;
      --  Enable Wakeup pin for PA0
      EWUP           : CSR2_EWUP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR2_Register use record
      WUPF           at 0 range 0 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      EWUP           at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Power control
   type PWR_Peripheral is record
      --  power control register
      CR1  : CR1_Register;
      --  power control/status register
      CSR1 : CSR1_Register;
      --  power control register
      CR2  : CR2_Register;
      --  power control/status register
      CSR2 : CSR2_Register;
   end record
     with Volatile;

   for PWR_Peripheral use record
      CR1  at 0 range 0 .. 31;
      CSR1 at 4 range 0 .. 31;
      CR2  at 8 range 0 .. 31;
      CSR2 at 12 range 0 .. 31;
   end record;

   --  Power control
   PWR_Periph : aliased PWR_Peripheral
     with Import, Address => System'To_Address (16#40007000#);

end STM32_SVD.PWR;
