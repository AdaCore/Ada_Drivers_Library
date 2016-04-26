--  This spec has been automatically generated from STM32F7x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.PWR is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ------------------
   -- CR1_Register --
   ------------------

   subtype CR1_PLS_Field is HAL.UInt3;
   subtype CR1_VOS_Field is HAL.UInt2;
   subtype CR1_UDEN_Field is HAL.UInt2;

   --  power control register
   type CR1_Register is record
      --  Low-power deep sleep
      LPDS           : Boolean := False;
      --  Power down deepsleep
      PDDS           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Clear standby flag
      CSBF           : Boolean := False;
      --  Power voltage detector enable
      PVDE           : Boolean := False;
      --  PVD level selection
      PLS            : CR1_PLS_Field := 16#0#;
      --  Disable backup domain write protection
      DBP            : Boolean := False;
      --  Flash power down in Stop mode
      FPDS           : Boolean := False;
      --  Low-power regulator in deepsleep under-drive mode
      LPUDS          : Boolean := False;
      --  Main regulator in deepsleep under-drive mode
      MRUDS          : Boolean := False;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  ADCDC1
      ADCDC1         : Boolean := False;
      --  Regulator voltage scaling output selection
      VOS            : CR1_VOS_Field := 16#3#;
      --  Over-drive enable
      ODEN           : Boolean := False;
      --  Over-drive switching enabled
      ODSWEN         : Boolean := False;
      --  Under-drive enable in stop mode
      UDEN           : CR1_UDEN_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
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

   subtype CSR1_UDRDY_Field is HAL.UInt2;

   --  power control/status register
   type CSR1_Register is record
      --  Read-only. Wakeup internal flag
      WUIF           : Boolean := False;
      --  Read-only. Standby flag
      SBF            : Boolean := False;
      --  Read-only. PVD output
      PVDO           : Boolean := False;
      --  Read-only. Backup regulator ready
      BRR            : Boolean := False;
      --  unspecified
      Reserved_4_8   : HAL.UInt5 := 16#0#;
      --  Backup regulator enable
      BRE            : Boolean := False;
      --  unspecified
      Reserved_10_13 : HAL.UInt4 := 16#0#;
      --  Regulator voltage scaling output selection ready bit
      VOSRDY         : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Over-drive mode ready
      ODRDY          : Boolean := False;
      --  Over-drive mode switching ready
      ODSWRDY        : Boolean := False;
      --  Under-drive ready flag
      UDRDY          : CSR1_UDRDY_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
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

   --  CR2_CWUPF array
   type CR2_CWUPF_Field_Array is array (1 .. 6) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for CR2_CWUPF
   type CR2_CWUPF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CWUPF as a value
            Val : HAL.UInt6;
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

   --  CR2_WUPP array
   type CR2_WUPP_Field_Array is array (1 .. 6) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for CR2_WUPP
   type CR2_WUPP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WUPP as a value
            Val : HAL.UInt6;
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
      --  Read-only. Clear Wakeup Pin flag for PA0
      CWUPF          : CR2_CWUPF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Wakeup pin polarity bit for PA0
      WUPP           : CR2_WUPP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
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

   --  CSR2_WUPF array
   type CSR2_WUPF_Field_Array is array (1 .. 6) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for CSR2_WUPF
   type CSR2_WUPF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  WUPF as a value
            Val : HAL.UInt6;
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

   --  CSR2_EWUP array
   type CSR2_EWUP_Field_Array is array (1 .. 6) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for CSR2_EWUP
   type CSR2_EWUP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EWUP as a value
            Val : HAL.UInt6;
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
      --  Read-only. Wakeup Pin flag for PA0
      WUPF           : CSR2_WUPF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Enable Wakeup pin for PA0
      EWUP           : CSR2_EWUP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
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
     with Import, Address => PWR_Base;

end STM32_SVD.PWR;
