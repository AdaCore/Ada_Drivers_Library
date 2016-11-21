--  This spec has been automatically generated from STM32F7x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.CRYP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_ALGOMODE0_Field is HAL.UInt3;
   subtype CR_DATATYPE_Field is HAL.UInt2;
   subtype CR_KEYSIZE_Field is HAL.UInt2;
   subtype CR_GCM_CCMPH_Field is HAL.UInt2;

   --  control register
   type CR_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Algorithm direction
      ALGODIR        : Boolean := False;
      --  Algorithm mode
      ALGOMODE0      : CR_ALGOMODE0_Field := 16#0#;
      --  Data type selection
      DATATYPE       : CR_DATATYPE_Field := 16#0#;
      --  Key size selection (AES mode only)
      KEYSIZE        : CR_KEYSIZE_Field := 16#0#;
      --  unspecified
      Reserved_10_13 : HAL.UInt4 := 16#0#;
      --  Write-only. FIFO flush
      FFLUSH         : Boolean := False;
      --  Cryptographic processor enable
      CRYPEN         : Boolean := False;
      --  GCM_CCMPH
      GCM_CCMPH      : CR_GCM_CCMPH_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : HAL.Bit := 16#0#;
      --  ALGOMODE
      ALGOMODE3      : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      ALGODIR        at 0 range 2 .. 2;
      ALGOMODE0      at 0 range 3 .. 5;
      DATATYPE       at 0 range 6 .. 7;
      KEYSIZE        at 0 range 8 .. 9;
      Reserved_10_13 at 0 range 10 .. 13;
      FFLUSH         at 0 range 14 .. 14;
      CRYPEN         at 0 range 15 .. 15;
      GCM_CCMPH      at 0 range 16 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      ALGOMODE3      at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  status register
   type SR_Register is record
      --  Read-only. Input FIFO empty
      IFEM          : Boolean;
      --  Read-only. Input FIFO not full
      IFNF          : Boolean;
      --  Read-only. Output FIFO not empty
      OFNE          : Boolean;
      --  Read-only. Output FIFO full
      OFFU          : Boolean;
      --  Read-only. Busy bit
      BUSY          : Boolean;
      --  unspecified
      Reserved_5_31 : HAL.UInt27;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      IFEM          at 0 range 0 .. 0;
      IFNF          at 0 range 1 .. 1;
      OFNE          at 0 range 2 .. 2;
      OFFU          at 0 range 3 .. 3;
      BUSY          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  DMA control register
   type DMACR_Register is record
      --  DMA input enable
      DIEN          : Boolean := False;
      --  DMA output enable
      DOEN          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMACR_Register use record
      DIEN          at 0 range 0 .. 0;
      DOEN          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  interrupt mask set/clear register
   type IMSCR_Register is record
      --  Input FIFO service interrupt mask
      INIM          : Boolean := False;
      --  Output FIFO service interrupt mask
      OUTIM         : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMSCR_Register use record
      INIM          at 0 range 0 .. 0;
      OUTIM         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  raw interrupt status register
   type RISR_Register is record
      --  Read-only. Input FIFO service raw interrupt status
      INRIS         : Boolean;
      --  Read-only. Output FIFO service raw interrupt status
      OUTRIS        : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RISR_Register use record
      INRIS         at 0 range 0 .. 0;
      OUTRIS        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  masked interrupt status register
   type MISR_Register is record
      --  Read-only. Input FIFO service masked interrupt status
      INMIS         : Boolean;
      --  Read-only. Output FIFO service masked interrupt status
      OUTMIS        : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR_Register use record
      INMIS         at 0 range 0 .. 0;
      OUTMIS        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  K0LR_b array
   type K0LR_b_Field_Array is array (224 .. 255) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K0LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K0LR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K0LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K0RR_b array
   type K0RR_b_Field_Array is array (192 .. 223) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K0RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K0RR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K0RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K1LR_b array
   type K1LR_b_Field_Array is array (160 .. 191) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K1LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K1LR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K1LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K1RR_b array
   type K1RR_b_Field_Array is array (128 .. 159) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K1RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K1RR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K1RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K2LR_b array
   type K2LR_b_Field_Array is array (96 .. 127) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K2LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K2LR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K2LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K2RR_b array
   type K2RR_b_Field_Array is array (64 .. 95) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K2RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K2RR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K2RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K3LR_b array
   type K3LR_b_Field_Array is array (32 .. 63) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K3LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K3LR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K3LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  K3RR_b array
   type K3RR_b_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  key registers
   type K3RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : HAL.UInt32;
         when True =>
            --  b as an array
            Arr : K3RR_b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for K3RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  IV0LR_IV array
   type IV0LR_IV_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  initialization vector registers
   type IV0LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IV as a value
            Val : HAL.UInt32;
         when True =>
            --  IV as an array
            Arr : IV0LR_IV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for IV0LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  IV0RR_IV array
   type IV0RR_IV_Field_Array is array (32 .. 63) of Boolean
     with Component_Size => 1, Size => 32;

   --  initialization vector registers
   type IV0RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IV as a value
            Val : HAL.UInt32;
         when True =>
            --  IV as an array
            Arr : IV0RR_IV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for IV0RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  IV1LR_IV array
   type IV1LR_IV_Field_Array is array (64 .. 95) of Boolean
     with Component_Size => 1, Size => 32;

   --  initialization vector registers
   type IV1LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IV as a value
            Val : HAL.UInt32;
         when True =>
            --  IV as an array
            Arr : IV1LR_IV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for IV1LR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  IV1RR_IV array
   type IV1RR_IV_Field_Array is array (96 .. 127) of Boolean
     with Component_Size => 1, Size => 32;

   --  initialization vector registers
   type IV1RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IV as a value
            Val : HAL.UInt32;
         when True =>
            --  IV as an array
            Arr : IV1RR_IV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for IV1RR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Cryptographic processor
   type CRYP_Peripheral is record
      --  control register
      CR         : aliased CR_Register;
      --  status register
      SR         : aliased SR_Register;
      --  data input register
      DIN        : aliased HAL.UInt32;
      --  data output register
      DOUT       : aliased HAL.UInt32;
      --  DMA control register
      DMACR      : aliased DMACR_Register;
      --  interrupt mask set/clear register
      IMSCR      : aliased IMSCR_Register;
      --  raw interrupt status register
      RISR       : aliased RISR_Register;
      --  masked interrupt status register
      MISR       : aliased MISR_Register;
      --  key registers
      K0LR       : aliased K0LR_Register;
      --  key registers
      K0RR       : aliased K0RR_Register;
      --  key registers
      K1LR       : aliased K1LR_Register;
      --  key registers
      K1RR       : aliased K1RR_Register;
      --  key registers
      K2LR       : aliased K2LR_Register;
      --  key registers
      K2RR       : aliased K2RR_Register;
      --  key registers
      K3LR       : aliased K3LR_Register;
      --  key registers
      K3RR       : aliased K3RR_Register;
      --  initialization vector registers
      IV0LR      : aliased IV0LR_Register;
      --  initialization vector registers
      IV0RR      : aliased IV0RR_Register;
      --  initialization vector registers
      IV1LR      : aliased IV1LR_Register;
      --  initialization vector registers
      IV1RR      : aliased IV1RR_Register;
      --  context swap register
      CSGCMCCM0R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM1R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM2R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM3R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM4R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM5R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM6R : aliased HAL.UInt32;
      --  context swap register
      CSGCMCCM7R : aliased HAL.UInt32;
      --  context swap register
      CSGCM0R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM1R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM2R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM3R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM4R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM5R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM6R    : aliased HAL.UInt32;
      --  context swap register
      CSGCM7R    : aliased HAL.UInt32;
   end record
     with Volatile;

   for CRYP_Peripheral use record
      CR         at 16#0# range 0 .. 31;
      SR         at 16#4# range 0 .. 31;
      DIN        at 16#8# range 0 .. 31;
      DOUT       at 16#C# range 0 .. 31;
      DMACR      at 16#10# range 0 .. 31;
      IMSCR      at 16#14# range 0 .. 31;
      RISR       at 16#18# range 0 .. 31;
      MISR       at 16#1C# range 0 .. 31;
      K0LR       at 16#20# range 0 .. 31;
      K0RR       at 16#24# range 0 .. 31;
      K1LR       at 16#28# range 0 .. 31;
      K1RR       at 16#2C# range 0 .. 31;
      K2LR       at 16#30# range 0 .. 31;
      K2RR       at 16#34# range 0 .. 31;
      K3LR       at 16#38# range 0 .. 31;
      K3RR       at 16#3C# range 0 .. 31;
      IV0LR      at 16#40# range 0 .. 31;
      IV0RR      at 16#44# range 0 .. 31;
      IV1LR      at 16#48# range 0 .. 31;
      IV1RR      at 16#4C# range 0 .. 31;
      CSGCMCCM0R at 16#50# range 0 .. 31;
      CSGCMCCM1R at 16#54# range 0 .. 31;
      CSGCMCCM2R at 16#58# range 0 .. 31;
      CSGCMCCM3R at 16#5C# range 0 .. 31;
      CSGCMCCM4R at 16#60# range 0 .. 31;
      CSGCMCCM5R at 16#64# range 0 .. 31;
      CSGCMCCM6R at 16#68# range 0 .. 31;
      CSGCMCCM7R at 16#6C# range 0 .. 31;
      CSGCM0R    at 16#70# range 0 .. 31;
      CSGCM1R    at 16#74# range 0 .. 31;
      CSGCM2R    at 16#78# range 0 .. 31;
      CSGCM3R    at 16#7C# range 0 .. 31;
      CSGCM4R    at 16#80# range 0 .. 31;
      CSGCM5R    at 16#84# range 0 .. 31;
      CSGCM6R    at 16#88# range 0 .. 31;
      CSGCM7R    at 16#8C# range 0 .. 31;
   end record;

   --  Cryptographic processor
   CRYP_Periph : aliased CRYP_Peripheral
     with Import, Address => System'To_Address (16#50060000#);

end STM32_SVD.CRYP;
