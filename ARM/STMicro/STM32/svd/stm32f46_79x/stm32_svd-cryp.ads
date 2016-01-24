--  Automatically generated from STM32F46_79x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.CRYP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- CR_Register --
   -----------------

   subtype CR_ALGODIR_Field is STM32_SVD.Bit;
   subtype CR_ALGOMODE0_Field is STM32_SVD.UInt3;
   subtype CR_DATATYPE_Field is STM32_SVD.UInt2;
   subtype CR_KEYSIZE_Field is STM32_SVD.UInt2;
   subtype CR_FFLUSH_Field is STM32_SVD.Bit;
   subtype CR_CRYPEN_Field is STM32_SVD.Bit;
   subtype CR_GCM_CCMPH_Field is STM32_SVD.UInt2;
   subtype CR_ALGOMODE3_Field is STM32_SVD.Bit;

   --  control register
   type CR_Register is record
      --  unspecified
      Reserved_0_1   : STM32_SVD.UInt2 := 16#0#;
      --  Algorithm direction
      ALGODIR        : CR_ALGODIR_Field := 16#0#;
      --  Algorithm mode
      ALGOMODE0      : CR_ALGOMODE0_Field := 16#0#;
      --  Data type selection
      DATATYPE       : CR_DATATYPE_Field := 16#0#;
      --  Key size selection (AES mode only)
      KEYSIZE        : CR_KEYSIZE_Field := 16#0#;
      --  unspecified
      Reserved_10_13 : STM32_SVD.UInt4 := 16#0#;
      --  FIFO flush
      FFLUSH         : CR_FFLUSH_Field := 16#0#;
      --  Cryptographic processor enable
      CRYPEN         : CR_CRYPEN_Field := 16#0#;
      --  GCM_CCMPH
      GCM_CCMPH      : CR_GCM_CCMPH_Field := 16#0#;
      --  unspecified
      Reserved_18_18 : STM32_SVD.Bit := 16#0#;
      --  ALGOMODE
      ALGOMODE3      : CR_ALGOMODE3_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32_SVD.UInt12 := 16#0#;
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

   -----------------
   -- SR_Register --
   -----------------

   subtype SR_IFEM_Field is STM32_SVD.Bit;
   subtype SR_IFNF_Field is STM32_SVD.Bit;
   subtype SR_OFNE_Field is STM32_SVD.Bit;
   subtype SR_OFFU_Field is STM32_SVD.Bit;
   subtype SR_BUSY_Field is STM32_SVD.Bit;

   --  status register
   type SR_Register is record
      --  Input FIFO empty
      IFEM          : SR_IFEM_Field := 16#1#;
      --  Input FIFO not full
      IFNF          : SR_IFNF_Field := 16#1#;
      --  Output FIFO not empty
      OFNE          : SR_OFNE_Field := 16#0#;
      --  Output FIFO full
      OFFU          : SR_OFFU_Field := 16#0#;
      --  Busy bit
      BUSY          : SR_BUSY_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : STM32_SVD.UInt27 := 16#0#;
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

   --------------------
   -- DMACR_Register --
   --------------------

   subtype DMACR_DIEN_Field is STM32_SVD.Bit;
   subtype DMACR_DOEN_Field is STM32_SVD.Bit;

   --  DMA control register
   type DMACR_Register is record
      --  DMA input enable
      DIEN          : DMACR_DIEN_Field := 16#0#;
      --  DMA output enable
      DOEN          : DMACR_DOEN_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMACR_Register use record
      DIEN          at 0 range 0 .. 0;
      DOEN          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --------------------
   -- IMSCR_Register --
   --------------------

   subtype IMSCR_INIM_Field is STM32_SVD.Bit;
   subtype IMSCR_OUTIM_Field is STM32_SVD.Bit;

   --  interrupt mask set/clear register
   type IMSCR_Register is record
      --  Input FIFO service interrupt mask
      INIM          : IMSCR_INIM_Field := 16#0#;
      --  Output FIFO service interrupt mask
      OUTIM         : IMSCR_OUTIM_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMSCR_Register use record
      INIM          at 0 range 0 .. 0;
      OUTIM         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -------------------
   -- RISR_Register --
   -------------------

   subtype RISR_INRIS_Field is STM32_SVD.Bit;
   subtype RISR_OUTRIS_Field is STM32_SVD.Bit;

   --  raw interrupt status register
   type RISR_Register is record
      --  Input FIFO service raw interrupt status
      INRIS         : RISR_INRIS_Field := 16#1#;
      --  Output FIFO service raw interrupt status
      OUTRIS        : RISR_OUTRIS_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RISR_Register use record
      INRIS         at 0 range 0 .. 0;
      OUTRIS        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -------------------
   -- MISR_Register --
   -------------------

   subtype MISR_INMIS_Field is STM32_SVD.Bit;
   subtype MISR_OUTMIS_Field is STM32_SVD.Bit;

   --  masked interrupt status register
   type MISR_Register is record
      --  Input FIFO service masked interrupt status
      INMIS         : MISR_INMIS_Field := 16#0#;
      --  Output FIFO service masked interrupt status
      OUTMIS        : MISR_OUTMIS_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MISR_Register use record
      INMIS         at 0 range 0 .. 0;
      OUTMIS        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -------------------
   -- K0LR_Register --
   -------------------

   --  K0LR_b array element
   subtype K0LR_b_Element is STM32_SVD.Bit;

   --  K0LR_b array
   type K0LR_b_Field_Array is array (0 .. 31) of K0LR_b_Element
     with Component_Size => 1, Size => 32;

   --  key registers
   type K0LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : STM32_SVD.Word;
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

   -------------------
   -- K0RR_Register --
   -------------------

   --  K0RR_b array element
   subtype K0RR_b_Element is STM32_SVD.Bit;

   --  K0RR_b array
   type K0RR_b_Field_Array is array (0 .. 31) of K0RR_b_Element
     with Component_Size => 1, Size => 32;

   --  key registers
   type K0RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : STM32_SVD.Word;
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

   -------------------
   -- K1LR_Register --
   -------------------

   --  K1LR_b array element
   subtype K1LR_b_Element is STM32_SVD.Bit;

   --  K1LR_b array
   type K1LR_b_Field_Array is array (0 .. 31) of K1LR_b_Element
     with Component_Size => 1, Size => 32;

   --  key registers
   type K1LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : STM32_SVD.Word;
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

   -------------------
   -- K1RR_Register --
   -------------------

   --  K1RR_b array element
   subtype K1RR_b_Element is STM32_SVD.Bit;

   --  K1RR_b array
   type K1RR_b_Field_Array is array (0 .. 31) of K1RR_b_Element
     with Component_Size => 1, Size => 32;

   --  key registers
   type K1RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : STM32_SVD.Word;
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

   -------------------
   -- K2LR_Register --
   -------------------

   --  K2LR_b array element
   subtype K2LR_b_Element is STM32_SVD.Bit;

   --  K2LR_b array
   type K2LR_b_Field_Array is array (0 .. 31) of K2LR_b_Element
     with Component_Size => 1, Size => 32;

   --  key registers
   type K2LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : STM32_SVD.Word;
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

   -------------------
   -- K2RR_Register --
   -------------------

   --  K2RR_b array element
   subtype K2RR_b_Element is STM32_SVD.Bit;

   --  K2RR_b array
   type K2RR_b_Field_Array is array (0 .. 31) of K2RR_b_Element
     with Component_Size => 1, Size => 32;

   --  key registers
   type K2RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : STM32_SVD.Word;
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

   -------------------
   -- K3LR_Register --
   -------------------

   --  K3LR_b array element
   subtype K3LR_b_Element is STM32_SVD.Bit;

   --  K3LR_b array
   type K3LR_b_Field_Array is array (0 .. 31) of K3LR_b_Element
     with Component_Size => 1, Size => 32;

   --  key registers
   type K3LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : STM32_SVD.Word;
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

   -------------------
   -- K3RR_Register --
   -------------------

   --  K3RR_b array element
   subtype K3RR_b_Element is STM32_SVD.Bit;

   --  K3RR_b array
   type K3RR_b_Field_Array is array (0 .. 31) of K3RR_b_Element
     with Component_Size => 1, Size => 32;

   --  key registers
   type K3RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  b as a value
            Val : STM32_SVD.Word;
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

   --------------------
   -- IV0LR_Register --
   --------------------

   --  IV0LR_IV array element
   subtype IV0LR_IV_Element is STM32_SVD.Bit;

   --  IV0LR_IV array
   type IV0LR_IV_Field_Array is array (0 .. 31) of IV0LR_IV_Element
     with Component_Size => 1, Size => 32;

   --  initialization vector registers
   type IV0LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IV as a value
            Val : STM32_SVD.Word;
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

   --------------------
   -- IV0RR_Register --
   --------------------

   --  IV0RR_IV array element
   subtype IV0RR_IV_Element is STM32_SVD.Bit;

   --  IV0RR_IV array
   type IV0RR_IV_Field_Array is array (0 .. 31) of IV0RR_IV_Element
     with Component_Size => 1, Size => 32;

   --  initialization vector registers
   type IV0RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IV as a value
            Val : STM32_SVD.Word;
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

   --------------------
   -- IV1LR_Register --
   --------------------

   --  IV1LR_IV array element
   subtype IV1LR_IV_Element is STM32_SVD.Bit;

   --  IV1LR_IV array
   type IV1LR_IV_Field_Array is array (0 .. 31) of IV1LR_IV_Element
     with Component_Size => 1, Size => 32;

   --  initialization vector registers
   type IV1LR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IV as a value
            Val : STM32_SVD.Word;
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

   --------------------
   -- IV1RR_Register --
   --------------------

   --  IV1RR_IV array element
   subtype IV1RR_IV_Element is STM32_SVD.Bit;

   --  IV1RR_IV array
   type IV1RR_IV_Field_Array is array (0 .. 31) of IV1RR_IV_Element
     with Component_Size => 1, Size => 32;

   --  initialization vector registers
   type IV1RR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  IV as a value
            Val : STM32_SVD.Word;
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
      CR         : CR_Register;
      --  status register
      SR         : SR_Register;
      --  data input register
      DIN        : STM32_SVD.Word;
      --  data output register
      DOUT       : STM32_SVD.Word;
      --  DMA control register
      DMACR      : DMACR_Register;
      --  interrupt mask set/clear register
      IMSCR      : IMSCR_Register;
      --  raw interrupt status register
      RISR       : RISR_Register;
      --  masked interrupt status register
      MISR       : MISR_Register;
      --  key registers
      K0LR       : K0LR_Register;
      --  key registers
      K0RR       : K0RR_Register;
      --  key registers
      K1LR       : K1LR_Register;
      --  key registers
      K1RR       : K1RR_Register;
      --  key registers
      K2LR       : K2LR_Register;
      --  key registers
      K2RR       : K2RR_Register;
      --  key registers
      K3LR       : K3LR_Register;
      --  key registers
      K3RR       : K3RR_Register;
      --  initialization vector registers
      IV0LR      : IV0LR_Register;
      --  initialization vector registers
      IV0RR      : IV0RR_Register;
      --  initialization vector registers
      IV1LR      : IV1LR_Register;
      --  initialization vector registers
      IV1RR      : IV1RR_Register;
      --  context swap register
      CSGCMCCM0R : STM32_SVD.Word;
      --  context swap register
      CSGCMCCM1R : STM32_SVD.Word;
      --  context swap register
      CSGCMCCM2R : STM32_SVD.Word;
      --  context swap register
      CSGCMCCM3R : STM32_SVD.Word;
      --  context swap register
      CSGCMCCM4R : STM32_SVD.Word;
      --  context swap register
      CSGCMCCM5R : STM32_SVD.Word;
      --  context swap register
      CSGCMCCM6R : STM32_SVD.Word;
      --  context swap register
      CSGCMCCM7R : STM32_SVD.Word;
      --  context swap register
      CSGCM0R    : STM32_SVD.Word;
      --  context swap register
      CSGCM1R    : STM32_SVD.Word;
      --  context swap register
      CSGCM2R    : STM32_SVD.Word;
      --  context swap register
      CSGCM3R    : STM32_SVD.Word;
      --  context swap register
      CSGCM4R    : STM32_SVD.Word;
      --  context swap register
      CSGCM5R    : STM32_SVD.Word;
      --  context swap register
      CSGCM6R    : STM32_SVD.Word;
      --  context swap register
      CSGCM7R    : STM32_SVD.Word;
   end record
     with Volatile;

   for CRYP_Peripheral use record
      CR         at 0 range 0 .. 31;
      SR         at 4 range 0 .. 31;
      DIN        at 8 range 0 .. 31;
      DOUT       at 12 range 0 .. 31;
      DMACR      at 16 range 0 .. 31;
      IMSCR      at 20 range 0 .. 31;
      RISR       at 24 range 0 .. 31;
      MISR       at 28 range 0 .. 31;
      K0LR       at 32 range 0 .. 31;
      K0RR       at 36 range 0 .. 31;
      K1LR       at 40 range 0 .. 31;
      K1RR       at 44 range 0 .. 31;
      K2LR       at 48 range 0 .. 31;
      K2RR       at 52 range 0 .. 31;
      K3LR       at 56 range 0 .. 31;
      K3RR       at 60 range 0 .. 31;
      IV0LR      at 64 range 0 .. 31;
      IV0RR      at 68 range 0 .. 31;
      IV1LR      at 72 range 0 .. 31;
      IV1RR      at 76 range 0 .. 31;
      CSGCMCCM0R at 80 range 0 .. 31;
      CSGCMCCM1R at 84 range 0 .. 31;
      CSGCMCCM2R at 88 range 0 .. 31;
      CSGCMCCM3R at 92 range 0 .. 31;
      CSGCMCCM4R at 96 range 0 .. 31;
      CSGCMCCM5R at 100 range 0 .. 31;
      CSGCMCCM6R at 104 range 0 .. 31;
      CSGCMCCM7R at 108 range 0 .. 31;
      CSGCM0R    at 112 range 0 .. 31;
      CSGCM1R    at 116 range 0 .. 31;
      CSGCM2R    at 120 range 0 .. 31;
      CSGCM3R    at 124 range 0 .. 31;
      CSGCM4R    at 128 range 0 .. 31;
      CSGCM5R    at 132 range 0 .. 31;
      CSGCM6R    at 136 range 0 .. 31;
      CSGCM7R    at 140 range 0 .. 31;
   end record;

   --  Cryptographic processor
   CRYP_Periph : aliased CRYP_Peripheral
     with Import, Address => System'To_Address (16#50060000#);

end STM32_SVD.CRYP;
