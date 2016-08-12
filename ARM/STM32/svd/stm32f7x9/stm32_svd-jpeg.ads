--  This spec has been automatically generated from STM32F7x9.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.JPEG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  JPEG codec configuration register 0
   type JPEG_CONFR0_Register is record
      --  Write-only. Start
      START         : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for JPEG_CONFR0_Register use record
      START         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype JPEG_CONFR1_NF_Field is HAL.UInt2;
   subtype JPEG_CONFR1_COLORSPACE_Field is HAL.UInt2;
   subtype JPEG_CONFR1_NS_Field is HAL.UInt2;
   subtype JPEG_CONFR1_YSIZE_Field is HAL.Short;

   --  JPEG codec configuration register 1
   type JPEG_CONFR1_Register is record
      --  Number of color components
      NF            : JPEG_CONFR1_NF_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Decoding Enable
      DE            : Boolean := False;
      --  Color Space
      COLORSPACE    : JPEG_CONFR1_COLORSPACE_Field := 16#0#;
      --  Number of components for Scan
      NS            : JPEG_CONFR1_NS_Field := 16#0#;
      --  Header Processing
      HDR           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
      --  Y Size
      YSIZE         : JPEG_CONFR1_YSIZE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for JPEG_CONFR1_Register use record
      NF            at 0 range 0 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      DE            at 0 range 3 .. 3;
      COLORSPACE    at 0 range 4 .. 5;
      NS            at 0 range 6 .. 7;
      HDR           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      YSIZE         at 0 range 16 .. 31;
   end record;

   subtype JPEG_CONFR2_NMCU_Field is HAL.UInt26;

   --  JPEG codec configuration register 2
   type JPEG_CONFR2_Register is record
      --  Number of MCU
      NMCU           : JPEG_CONFR2_NMCU_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for JPEG_CONFR2_Register use record
      NMCU           at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype JPEG_CONFR3_XSIZE_Field is HAL.Short;

   --  JPEG codec configuration register 3
   type JPEG_CONFR3_Register is record
      --  unspecified
      Reserved_0_15 : HAL.Short := 16#0#;
      --  X size
      XSIZE         : JPEG_CONFR3_XSIZE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for JPEG_CONFR3_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      XSIZE         at 0 range 16 .. 31;
   end record;

   subtype JPEG_CONFR_QT_Field is HAL.UInt2;
   subtype JPEG_CONFR_NB_Field is HAL.UInt4;
   subtype JPEG_CONFR_VSF_Field is HAL.UInt4;
   subtype JPEG_CONFR_HSF_Field is HAL.UInt4;

   --  JPEG codec configuration register 4
   type JPEG_CONFR_Register is record
      --  Huffman DC
      HD             : Boolean := False;
      --  Huffman AC
      HA             : Boolean := False;
      --  Quantization Table
      QT             : JPEG_CONFR_QT_Field := 16#0#;
      --  Number of Block
      NB             : JPEG_CONFR_NB_Field := 16#0#;
      --  Vertical Sampling Factor
      VSF            : JPEG_CONFR_VSF_Field := 16#0#;
      --  Horizontal Sampling Factor
      HSF            : JPEG_CONFR_HSF_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for JPEG_CONFR_Register use record
      HD             at 0 range 0 .. 0;
      HA             at 0 range 1 .. 1;
      QT             at 0 range 2 .. 3;
      NB             at 0 range 4 .. 7;
      VSF            at 0 range 8 .. 11;
      HSF            at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  JPEG control register
   type JPEG_CR_Register is record
      --  JPEG Core Enable
      JCEN           : Boolean := False;
      --  Input FIFO Threshold Interrupt Enable
      IFTIE          : Boolean := False;
      --  Input FIFO Not Full Interrupt Enable
      IFNFIE         : Boolean := False;
      --  Output FIFO Threshold Interrupt Enable
      OFTIE          : Boolean := False;
      --  Output FIFO Not Empty Interrupt Enable
      OFNEIE         : Boolean := False;
      --  End of Conversion Interrupt Enable
      EOCIE          : Boolean := False;
      --  Header Parsing Done Interrupt Enable
      HPDIE          : Boolean := False;
      --  unspecified
      Reserved_7_10  : HAL.UInt4 := 16#0#;
      --  Input DMA Enable
      IDMAEN         : Boolean := False;
      --  Output DMA Enable
      ODMAEN         : Boolean := False;
      --  Read-only. Input FIFO Flush
      IFF            : Boolean := False;
      --  Read-only. Output FIFO Flush
      OFF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for JPEG_CR_Register use record
      JCEN           at 0 range 0 .. 0;
      IFTIE          at 0 range 1 .. 1;
      IFNFIE         at 0 range 2 .. 2;
      OFTIE          at 0 range 3 .. 3;
      OFNEIE         at 0 range 4 .. 4;
      EOCIE          at 0 range 5 .. 5;
      HPDIE          at 0 range 6 .. 6;
      Reserved_7_10  at 0 range 7 .. 10;
      IDMAEN         at 0 range 11 .. 11;
      ODMAEN         at 0 range 12 .. 12;
      IFF            at 0 range 13 .. 13;
      OFF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  JPEG status register
   type JPEG_SR_Register is record
      --  unspecified
      Reserved_0_0  : HAL.Bit;
      --  Read-only. Input FIFO Threshold Flag
      IFTF          : Boolean;
      --  Read-only. Input FIFO Not Full Flag
      IFNFF         : Boolean;
      --  Read-only. Output FIFO Threshold Flag
      OFTF          : Boolean;
      --  Read-only. Output FIFO Not Empty Flag
      OFNEF         : Boolean;
      --  Read-only. End of Conversion Flag
      EOCF          : Boolean;
      --  Read-only. Header Parsing Done Flag
      HPDF          : Boolean;
      --  Read-only. Codec Operation Flag
      COF           : Boolean;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for JPEG_SR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      IFTF          at 0 range 1 .. 1;
      IFNFF         at 0 range 2 .. 2;
      OFTF          at 0 range 3 .. 3;
      OFNEF         at 0 range 4 .. 4;
      EOCF          at 0 range 5 .. 5;
      HPDF          at 0 range 6 .. 6;
      COF           at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  JPEG clear flag register
   type JPEG_CFR_Register is record
      --  unspecified
      Reserved_0_4  : HAL.UInt5 := 16#0#;
      --  Write-only. Clear End of Conversion Flag
      CEOCF         : Boolean := False;
      --  Write-only. Clear Header Parsing Done Flag
      CHPDF         : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for JPEG_CFR_Register use record
      Reserved_0_4  at 0 range 0 .. 4;
      CEOCF         at 0 range 5 .. 5;
      CHPDF         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  JPEG codec
   type JPEG_Peripheral is record
      --  JPEG codec configuration register 0
      JPEG_CONFR0 : JPEG_CONFR0_Register;
      --  JPEG codec configuration register 1
      JPEG_CONFR1 : JPEG_CONFR1_Register;
      --  JPEG codec configuration register 2
      JPEG_CONFR2 : JPEG_CONFR2_Register;
      --  JPEG codec configuration register 3
      JPEG_CONFR3 : JPEG_CONFR3_Register;
      --  JPEG codec configuration register 4
      JPEG_CONFR4 : JPEG_CONFR_Register;
      --  JPEG codec configuration register 5
      JPEG_CONFR5 : JPEG_CONFR_Register;
      --  JPEG codec configuration register 6
      JPEG_CONFR6 : JPEG_CONFR_Register;
      --  JPEG codec configuration register 7
      JPEG_CONFR7 : JPEG_CONFR_Register;
      --  JPEG control register
      JPEG_CR     : JPEG_CR_Register;
      --  JPEG status register
      JPEG_SR     : JPEG_SR_Register;
      --  JPEG clear flag register
      JPEG_CFR    : JPEG_CFR_Register;
      --  JPEG data input register
      JPEG_DIR    : HAL.Word;
      --  JPEG data output register
      JPEG_DOR    : HAL.Word;
   end record
     with Volatile;

   for JPEG_Peripheral use record
      JPEG_CONFR0 at 0 range 0 .. 31;
      JPEG_CONFR1 at 4 range 0 .. 31;
      JPEG_CONFR2 at 8 range 0 .. 31;
      JPEG_CONFR3 at 12 range 0 .. 31;
      JPEG_CONFR4 at 16 range 0 .. 31;
      JPEG_CONFR5 at 20 range 0 .. 31;
      JPEG_CONFR6 at 24 range 0 .. 31;
      JPEG_CONFR7 at 28 range 0 .. 31;
      JPEG_CR     at 32 range 0 .. 31;
      JPEG_SR     at 36 range 0 .. 31;
      JPEG_CFR    at 40 range 0 .. 31;
      JPEG_DIR    at 44 range 0 .. 31;
      JPEG_DOR    at 48 range 0 .. 31;
   end record;

   --  JPEG codec
   JPEG_Periph : aliased JPEG_Peripheral
     with Import, Address => JPEG_Base;

end STM32_SVD.JPEG;
