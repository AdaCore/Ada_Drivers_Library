--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DMA2D is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_MODE_Field is HAL.UInt2;

   --  control register
   type CR_Register is record
      --  Start
      START          : Boolean := False;
      --  Suspend
      SUSP           : Boolean := False;
      --  Abort
      ABORT_k        : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : Boolean := False;
      --  Transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  Transfer watermark interrupt enable
      TWIE           : Boolean := False;
      --  CLUT access error interrupt enable
      CAEIE          : Boolean := False;
      --  CLUT transfer complete interrupt enable
      CTCIE          : Boolean := False;
      --  Configuration Error Interrupt Enable
      CEIE           : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  DMA2D mode
      MODE           : CR_MODE_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      START          at 0 range 0 .. 0;
      SUSP           at 0 range 1 .. 1;
      ABORT_k        at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      TEIE           at 0 range 8 .. 8;
      TCIE           at 0 range 9 .. 9;
      TWIE           at 0 range 10 .. 10;
      CAEIE          at 0 range 11 .. 11;
      CTCIE          at 0 range 12 .. 12;
      CEIE           at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      MODE           at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Interrupt Status Register
   type ISR_Register is record
      --  Read-only. Transfer error interrupt flag
      TEIF          : Boolean;
      --  Read-only. Transfer complete interrupt flag
      TCIF          : Boolean;
      --  Read-only. Transfer watermark interrupt flag
      TWIF          : Boolean;
      --  Read-only. CLUT access error interrupt flag
      CAEIF         : Boolean;
      --  Read-only. CLUT transfer complete interrupt flag
      CTCIF         : Boolean;
      --  Read-only. Configuration error interrupt flag
      CEIF          : Boolean;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      TEIF          at 0 range 0 .. 0;
      TCIF          at 0 range 1 .. 1;
      TWIF          at 0 range 2 .. 2;
      CAEIF         at 0 range 3 .. 3;
      CTCIF         at 0 range 4 .. 4;
      CEIF          at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   --  interrupt flag clear register
   type IFCR_Register is record
      --  Clear Transfer error interrupt flag
      CTEIF         : Boolean := False;
      --  Clear transfer complete interrupt flag
      CTCIF         : Boolean := False;
      --  Clear transfer watermark interrupt flag
      CTWIF         : Boolean := False;
      --  Clear CLUT access error interrupt flag
      CAECIF        : Boolean := False;
      --  Clear CLUT transfer complete interrupt flag
      CCTCIF        : Boolean := False;
      --  Clear configuration error interrupt flag
      CCEIF         : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IFCR_Register use record
      CTEIF         at 0 range 0 .. 0;
      CTCIF         at 0 range 1 .. 1;
      CTWIF         at 0 range 2 .. 2;
      CAECIF        at 0 range 3 .. 3;
      CCTCIF        at 0 range 4 .. 4;
      CCEIF         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype FGOR_LO_Field is HAL.UInt14;

   --  foreground offset register
   type FGOR_Register is record
      --  Line offset
      LO             : FGOR_LO_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FGOR_Register use record
      LO             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype BGOR_LO_Field is HAL.UInt14;

   --  background offset register
   type BGOR_Register is record
      --  Line offset
      LO             : BGOR_LO_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BGOR_Register use record
      LO             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype FGPFCCR_CM_Field is HAL.UInt4;
   subtype FGPFCCR_CS_Field is HAL.UInt8;
   subtype FGPFCCR_AM_Field is HAL.UInt2;
   subtype FGPFCCR_ALPHA_Field is HAL.UInt8;

   --  foreground PFC control register
   type FGPFCCR_Register is record
      --  Color mode
      CM             : FGPFCCR_CM_Field := 16#0#;
      --  CLUT color mode
      CCM            : Boolean := False;
      --  Start
      START          : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  CLUT size
      CS             : FGPFCCR_CS_Field := 16#0#;
      --  Alpha mode
      AM             : FGPFCCR_AM_Field := 16#0#;
      --  unspecified
      Reserved_18_23 : HAL.UInt6 := 16#0#;
      --  Alpha value
      ALPHA          : FGPFCCR_ALPHA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FGPFCCR_Register use record
      CM             at 0 range 0 .. 3;
      CCM            at 0 range 4 .. 4;
      START          at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      CS             at 0 range 8 .. 15;
      AM             at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      ALPHA          at 0 range 24 .. 31;
   end record;

   subtype FGCOLR_BLUE_Field is HAL.UInt8;
   subtype FGCOLR_GREEN_Field is HAL.UInt8;
   subtype FGCOLR_RED_Field is HAL.UInt8;

   --  foreground color register
   type FGCOLR_Register is record
      --  Blue Value
      BLUE           : FGCOLR_BLUE_Field := 16#0#;
      --  Green Value
      GREEN          : FGCOLR_GREEN_Field := 16#0#;
      --  Red Value
      RED            : FGCOLR_RED_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FGCOLR_Register use record
      BLUE           at 0 range 0 .. 7;
      GREEN          at 0 range 8 .. 15;
      RED            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype BGPFCCR_CM_Field is HAL.UInt4;
   subtype BGPFCCR_CS_Field is HAL.UInt8;
   subtype BGPFCCR_AM_Field is HAL.UInt2;
   subtype BGPFCCR_ALPHA_Field is HAL.UInt8;

   --  background PFC control register
   type BGPFCCR_Register is record
      --  Color mode
      CM             : BGPFCCR_CM_Field := 16#0#;
      --  CLUT Color mode
      CCM            : Boolean := False;
      --  Start
      START          : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  CLUT size
      CS             : BGPFCCR_CS_Field := 16#0#;
      --  Alpha mode
      AM             : BGPFCCR_AM_Field := 16#0#;
      --  unspecified
      Reserved_18_23 : HAL.UInt6 := 16#0#;
      --  Alpha value
      ALPHA          : BGPFCCR_ALPHA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BGPFCCR_Register use record
      CM             at 0 range 0 .. 3;
      CCM            at 0 range 4 .. 4;
      START          at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      CS             at 0 range 8 .. 15;
      AM             at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      ALPHA          at 0 range 24 .. 31;
   end record;

   subtype BGCOLR_BLUE_Field is HAL.UInt8;
   subtype BGCOLR_GREEN_Field is HAL.UInt8;
   subtype BGCOLR_RED_Field is HAL.UInt8;

   --  background color register
   type BGCOLR_Register is record
      --  Blue Value
      BLUE           : BGCOLR_BLUE_Field := 16#0#;
      --  Green Value
      GREEN          : BGCOLR_GREEN_Field := 16#0#;
      --  Red Value
      RED            : BGCOLR_RED_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BGCOLR_Register use record
      BLUE           at 0 range 0 .. 7;
      GREEN          at 0 range 8 .. 15;
      RED            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype OPFCCR_CM_Field is HAL.UInt3;

   --  output PFC control register
   type OPFCCR_Register is record
      --  Color mode
      CM            : OPFCCR_CM_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPFCCR_Register use record
      CM            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype OCOLR_BLUE_Field is HAL.UInt8;
   subtype OCOLR_GREEN_Field is HAL.UInt8;
   subtype OCOLR_RED_Field is HAL.UInt8;
   subtype OCOLR_APLHA_Field is HAL.UInt8;

   --  output color register
   type OCOLR_Register is record
      --  Blue Value
      BLUE  : OCOLR_BLUE_Field := 16#0#;
      --  Green Value
      GREEN : OCOLR_GREEN_Field := 16#0#;
      --  Red Value
      RED   : OCOLR_RED_Field := 16#0#;
      --  Alpha Channel Value
      APLHA : OCOLR_APLHA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OCOLR_Register use record
      BLUE  at 0 range 0 .. 7;
      GREEN at 0 range 8 .. 15;
      RED   at 0 range 16 .. 23;
      APLHA at 0 range 24 .. 31;
   end record;

   subtype OOR_LO_Field is HAL.UInt14;

   --  output offset register
   type OOR_Register is record
      --  Line Offset
      LO             : OOR_LO_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OOR_Register use record
      LO             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype NLR_NL_Field is HAL.UInt16;
   subtype NLR_PL_Field is HAL.UInt14;

   --  number of line register
   type NLR_Register is record
      --  Number of lines
      NL             : NLR_NL_Field := 16#0#;
      --  Pixel per lines
      PL             : NLR_PL_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for NLR_Register use record
      NL             at 0 range 0 .. 15;
      PL             at 0 range 16 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype LWR_LW_Field is HAL.UInt16;

   --  line watermark register
   type LWR_Register is record
      --  Line watermark
      LW             : LWR_LW_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LWR_Register use record
      LW             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype AMTCR_DT_Field is HAL.UInt8;

   --  AHB master timer configuration register
   type AMTCR_Register is record
      --  Enable
      EN             : Boolean := False;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  Dead Time
      DT             : AMTCR_DT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMTCR_Register use record
      EN             at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      DT             at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FGCLUT_BLUE_Field is HAL.UInt8;
   subtype FGCLUT_GREEN_Field is HAL.UInt8;
   subtype FGCLUT_RED_Field is HAL.UInt8;
   subtype FGCLUT_APLHA_Field is HAL.UInt8;

   --  FGCLUT
   type FGCLUT_Register is record
      --  BLUE
      BLUE  : FGCLUT_BLUE_Field := 16#0#;
      --  GREEN
      GREEN : FGCLUT_GREEN_Field := 16#0#;
      --  RED
      RED   : FGCLUT_RED_Field := 16#0#;
      --  APLHA
      APLHA : FGCLUT_APLHA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FGCLUT_Register use record
      BLUE  at 0 range 0 .. 7;
      GREEN at 0 range 8 .. 15;
      RED   at 0 range 16 .. 23;
      APLHA at 0 range 24 .. 31;
   end record;

   subtype BGCLUT_BLUE_Field is HAL.UInt8;
   subtype BGCLUT_GREEN_Field is HAL.UInt8;
   subtype BGCLUT_RED_Field is HAL.UInt8;
   subtype BGCLUT_APLHA_Field is HAL.UInt8;

   --  BGCLUT
   type BGCLUT_Register is record
      --  BLUE
      BLUE  : BGCLUT_BLUE_Field := 16#0#;
      --  GREEN
      GREEN : BGCLUT_GREEN_Field := 16#0#;
      --  RED
      RED   : BGCLUT_RED_Field := 16#0#;
      --  APLHA
      APLHA : BGCLUT_APLHA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BGCLUT_Register use record
      BLUE  at 0 range 0 .. 7;
      GREEN at 0 range 8 .. 15;
      RED   at 0 range 16 .. 23;
      APLHA at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DMA2D controller
   type DMA2D_Peripheral is record
      --  control register
      CR      : aliased CR_Register;
      --  Interrupt Status Register
      ISR     : aliased ISR_Register;
      --  interrupt flag clear register
      IFCR    : aliased IFCR_Register;
      --  foreground memory address register
      FGMAR   : aliased HAL.UInt32;
      --  foreground offset register
      FGOR    : aliased FGOR_Register;
      --  background memory address register
      BGMAR   : aliased HAL.UInt32;
      --  background offset register
      BGOR    : aliased BGOR_Register;
      --  foreground PFC control register
      FGPFCCR : aliased FGPFCCR_Register;
      --  foreground color register
      FGCOLR  : aliased FGCOLR_Register;
      --  background PFC control register
      BGPFCCR : aliased BGPFCCR_Register;
      --  background color register
      BGCOLR  : aliased BGCOLR_Register;
      --  foreground CLUT memory address register
      FGCMAR  : aliased HAL.UInt32;
      --  background CLUT memory address register
      BGCMAR  : aliased HAL.UInt32;
      --  output PFC control register
      OPFCCR  : aliased OPFCCR_Register;
      --  output color register
      OCOLR   : aliased OCOLR_Register;
      --  output memory address register
      OMAR    : aliased HAL.UInt32;
      --  output offset register
      OOR     : aliased OOR_Register;
      --  number of line register
      NLR     : aliased NLR_Register;
      --  line watermark register
      LWR     : aliased LWR_Register;
      --  AHB master timer configuration register
      AMTCR   : aliased AMTCR_Register;
      --  FGCLUT
      FGCLUT  : aliased FGCLUT_Register;
      --  BGCLUT
      BGCLUT  : aliased BGCLUT_Register;
   end record
     with Volatile;

   for DMA2D_Peripheral use record
      CR      at 16#0# range 0 .. 31;
      ISR     at 16#4# range 0 .. 31;
      IFCR    at 16#8# range 0 .. 31;
      FGMAR   at 16#C# range 0 .. 31;
      FGOR    at 16#10# range 0 .. 31;
      BGMAR   at 16#14# range 0 .. 31;
      BGOR    at 16#18# range 0 .. 31;
      FGPFCCR at 16#1C# range 0 .. 31;
      FGCOLR  at 16#20# range 0 .. 31;
      BGPFCCR at 16#24# range 0 .. 31;
      BGCOLR  at 16#28# range 0 .. 31;
      FGCMAR  at 16#2C# range 0 .. 31;
      BGCMAR  at 16#30# range 0 .. 31;
      OPFCCR  at 16#34# range 0 .. 31;
      OCOLR   at 16#38# range 0 .. 31;
      OMAR    at 16#3C# range 0 .. 31;
      OOR     at 16#40# range 0 .. 31;
      NLR     at 16#44# range 0 .. 31;
      LWR     at 16#48# range 0 .. 31;
      AMTCR   at 16#4C# range 0 .. 31;
      FGCLUT  at 16#400# range 0 .. 31;
      BGCLUT  at 16#800# range 0 .. 31;
   end record;

   --  DMA2D controller
   DMA2D_Periph : aliased DMA2D_Peripheral
     with Import, Address => System'To_Address (16#4002B000#);

end STM32_SVD.DMA2D;
