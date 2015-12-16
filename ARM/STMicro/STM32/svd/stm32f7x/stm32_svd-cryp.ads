--  Automatically generated from CMSIS-SVD description file
pragma Restrictions (No_Elaboration_Code);

with System;

package STM32_SVD.CRYP is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

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

   subtype K0LR_b224_Field is STM32_SVD.Bit;

   type b_Field_Array is array (0 .. 31) of K0LR_b224_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for b
   type b_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of b224
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of b224
            Arr : b_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for b_Union use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  key registers
   type K0LR_Register is record
      --  b224
      b : b_Union := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for K0LR_Register use record
      b at 0 range 0 .. 31;
   end record;

   subtype K0RR_b192_Field is STM32_SVD.Bit;

   type b_Field_Array_1 is array (0 .. 31) of K0RR_b192_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for b
   type b_Union_1 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of b192
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of b192
            Arr : b_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for b_Union_1 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  key registers
   type K0RR_Register is record
      --  b192
      b : b_Union_1 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for K0RR_Register use record
      b at 0 range 0 .. 31;
   end record;

   subtype K1LR_b160_Field is STM32_SVD.Bit;

   type b_Field_Array_2 is array (0 .. 31) of K1LR_b160_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for b
   type b_Union_2 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of b160
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of b160
            Arr : b_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for b_Union_2 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  key registers
   type K1LR_Register is record
      --  b160
      b : b_Union_2 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for K1LR_Register use record
      b at 0 range 0 .. 31;
   end record;

   subtype K1RR_b128_Field is STM32_SVD.Bit;

   type b_Field_Array_3 is array (0 .. 31) of K1RR_b128_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for b
   type b_Union_3 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of b128
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of b128
            Arr : b_Field_Array_3;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for b_Union_3 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  key registers
   type K1RR_Register is record
      --  b128
      b : b_Union_3 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for K1RR_Register use record
      b at 0 range 0 .. 31;
   end record;

   subtype K2LR_b96_Field is STM32_SVD.Bit;

   type b_Field_Array_4 is array (0 .. 31) of K2LR_b96_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for b
   type b_Union_4 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of b96
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of b96
            Arr : b_Field_Array_4;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for b_Union_4 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  key registers
   type K2LR_Register is record
      --  b96
      b : b_Union_4 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for K2LR_Register use record
      b at 0 range 0 .. 31;
   end record;

   subtype K2RR_b64_Field is STM32_SVD.Bit;

   type b_Field_Array_5 is array (0 .. 31) of K2RR_b64_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for b
   type b_Union_5 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of b64
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of b64
            Arr : b_Field_Array_5;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for b_Union_5 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  key registers
   type K2RR_Register is record
      --  b64
      b : b_Union_5 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for K2RR_Register use record
      b at 0 range 0 .. 31;
   end record;

   subtype K3LR_b32_Field is STM32_SVD.Bit;

   type b_Field_Array_6 is array (0 .. 31) of K3LR_b32_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for b
   type b_Union_6 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of b32
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of b32
            Arr : b_Field_Array_6;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for b_Union_6 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  key registers
   type K3LR_Register is record
      --  b32
      b : b_Union_6 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for K3LR_Register use record
      b at 0 range 0 .. 31;
   end record;

   subtype K3RR_b0_Field is STM32_SVD.Bit;

   type b_Field_Array_7 is array (0 .. 31) of K3RR_b0_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for b
   type b_Union_7 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of b0
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of b0
            Arr : b_Field_Array_7;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for b_Union_7 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  key registers
   type K3RR_Register is record
      --  b0
      b : b_Union_7 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for K3RR_Register use record
      b at 0 range 0 .. 31;
   end record;

   subtype IV0LR_IV31_Field is STM32_SVD.Bit;

   type IV_Field_Array is array (0 .. 31) of IV0LR_IV31_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for IV
   type IV_Union (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of IV31
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of IV31
            Arr : IV_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for IV_Union use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  initialization vector registers
   type IV0LR_Register is record
      --  IV31
      IV : IV_Union := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IV0LR_Register use record
      IV at 0 range 0 .. 31;
   end record;

   subtype IV0RR_IV63_Field is STM32_SVD.Bit;

   type IV_Field_Array_1 is array (0 .. 31) of IV0RR_IV63_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for IV
   type IV_Union_1 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of IV63
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of IV63
            Arr : IV_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for IV_Union_1 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  initialization vector registers
   type IV0RR_Register is record
      --  IV63
      IV : IV_Union_1 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IV0RR_Register use record
      IV at 0 range 0 .. 31;
   end record;

   subtype IV1LR_IV95_Field is STM32_SVD.Bit;

   type IV_Field_Array_2 is array (0 .. 31) of IV1LR_IV95_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for IV
   type IV_Union_2 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of IV95
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of IV95
            Arr : IV_Field_Array_2;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for IV_Union_2 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  initialization vector registers
   type IV1LR_Register is record
      --  IV95
      IV : IV_Union_2 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IV1LR_Register use record
      IV at 0 range 0 .. 31;
   end record;

   subtype IV1RR_IV127_Field is STM32_SVD.Bit;

   type IV_Field_Array_3 is array (0 .. 31) of IV1RR_IV127_Field
     with Component_Size => 1, Size => 32;

   --  Type definition for IV
   type IV_Union_3 (As_Array : Boolean := False) is record
      case As_Array is
         when False =>
            --  Value vision of IV127
            Val : STM32_SVD.Word;
         when True =>
            --  Array vision of IV127
            Arr : IV_Field_Array_3;
      end case;
   end record
     with Unchecked_Union, Size => 32;

   for IV_Union_3 use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  initialization vector registers
   type IV1RR_Register is record
      --  IV127
      IV : IV_Union_3 := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IV1RR_Register use record
      IV at 0 range 0 .. 31;
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
     with Import, Address => System'To_Address(16#50060000#);

end STM32_SVD.CRYP;
