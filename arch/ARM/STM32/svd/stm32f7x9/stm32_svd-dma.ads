--  This spec has been automatically generated from STM32F7x9.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DMA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  low interrupt status register
   type LISR_Register is record
      --  Read-only. Stream x FIFO error interrupt flag (x=3..0)
      FEIF0          : Boolean;
      --  unspecified
      Reserved_1_1   : HAL.Bit;
      --  Read-only. Stream x direct mode error interrupt flag (x=3..0)
      DMEIF0         : Boolean;
      --  Read-only. Stream x transfer error interrupt flag (x=3..0)
      TEIF0          : Boolean;
      --  Read-only. Stream x half transfer interrupt flag (x=3..0)
      HTIF0          : Boolean;
      --  Read-only. Stream x transfer complete interrupt flag (x = 3..0)
      TCIF0          : Boolean;
      --  Read-only. Stream x FIFO error interrupt flag (x=3..0)
      FEIF1          : Boolean;
      --  unspecified
      Reserved_7_7   : HAL.Bit;
      --  Read-only. Stream x direct mode error interrupt flag (x=3..0)
      DMEIF1         : Boolean;
      --  Read-only. Stream x transfer error interrupt flag (x=3..0)
      TEIF1          : Boolean;
      --  Read-only. Stream x half transfer interrupt flag (x=3..0)
      HTIF1          : Boolean;
      --  Read-only. Stream x transfer complete interrupt flag (x = 3..0)
      TCIF1          : Boolean;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Stream x FIFO error interrupt flag (x=3..0)
      FEIF2          : Boolean;
      --  unspecified
      Reserved_17_17 : HAL.Bit;
      --  Read-only. Stream x direct mode error interrupt flag (x=3..0)
      DMEIF2         : Boolean;
      --  Read-only. Stream x transfer error interrupt flag (x=3..0)
      TEIF2          : Boolean;
      --  Read-only. Stream x half transfer interrupt flag (x=3..0)
      HTIF2          : Boolean;
      --  Read-only. Stream x transfer complete interrupt flag (x = 3..0)
      TCIF2          : Boolean;
      --  Read-only. Stream x FIFO error interrupt flag (x=3..0)
      FEIF3          : Boolean;
      --  unspecified
      Reserved_23_23 : HAL.Bit;
      --  Read-only. Stream x direct mode error interrupt flag (x=3..0)
      DMEIF3         : Boolean;
      --  Read-only. Stream x transfer error interrupt flag (x=3..0)
      TEIF3          : Boolean;
      --  Read-only. Stream x half transfer interrupt flag (x=3..0)
      HTIF3          : Boolean;
      --  Read-only. Stream x transfer complete interrupt flag (x = 3..0)
      TCIF3          : Boolean;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LISR_Register use record
      FEIF0          at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      DMEIF0         at 0 range 2 .. 2;
      TEIF0          at 0 range 3 .. 3;
      HTIF0          at 0 range 4 .. 4;
      TCIF0          at 0 range 5 .. 5;
      FEIF1          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      DMEIF1         at 0 range 8 .. 8;
      TEIF1          at 0 range 9 .. 9;
      HTIF1          at 0 range 10 .. 10;
      TCIF1          at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      FEIF2          at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      DMEIF2         at 0 range 18 .. 18;
      TEIF2          at 0 range 19 .. 19;
      HTIF2          at 0 range 20 .. 20;
      TCIF2          at 0 range 21 .. 21;
      FEIF3          at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      DMEIF3         at 0 range 24 .. 24;
      TEIF3          at 0 range 25 .. 25;
      HTIF3          at 0 range 26 .. 26;
      TCIF3          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  high interrupt status register
   type HISR_Register is record
      --  Read-only. Stream x FIFO error interrupt flag (x=7..4)
      FEIF4          : Boolean;
      --  unspecified
      Reserved_1_1   : HAL.Bit;
      --  Read-only. Stream x direct mode error interrupt flag (x=7..4)
      DMEIF4         : Boolean;
      --  Read-only. Stream x transfer error interrupt flag (x=7..4)
      TEIF4          : Boolean;
      --  Read-only. Stream x half transfer interrupt flag (x=7..4)
      HTIF4          : Boolean;
      --  Read-only. Stream x transfer complete interrupt flag (x=7..4)
      TCIF4          : Boolean;
      --  Read-only. Stream x FIFO error interrupt flag (x=7..4)
      FEIF5          : Boolean;
      --  unspecified
      Reserved_7_7   : HAL.Bit;
      --  Read-only. Stream x direct mode error interrupt flag (x=7..4)
      DMEIF5         : Boolean;
      --  Read-only. Stream x transfer error interrupt flag (x=7..4)
      TEIF5          : Boolean;
      --  Read-only. Stream x half transfer interrupt flag (x=7..4)
      HTIF5          : Boolean;
      --  Read-only. Stream x transfer complete interrupt flag (x=7..4)
      TCIF5          : Boolean;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Stream x FIFO error interrupt flag (x=7..4)
      FEIF6          : Boolean;
      --  unspecified
      Reserved_17_17 : HAL.Bit;
      --  Read-only. Stream x direct mode error interrupt flag (x=7..4)
      DMEIF6         : Boolean;
      --  Read-only. Stream x transfer error interrupt flag (x=7..4)
      TEIF6          : Boolean;
      --  Read-only. Stream x half transfer interrupt flag (x=7..4)
      HTIF6          : Boolean;
      --  Read-only. Stream x transfer complete interrupt flag (x=7..4)
      TCIF6          : Boolean;
      --  Read-only. Stream x FIFO error interrupt flag (x=7..4)
      FEIF7          : Boolean;
      --  unspecified
      Reserved_23_23 : HAL.Bit;
      --  Read-only. Stream x direct mode error interrupt flag (x=7..4)
      DMEIF7         : Boolean;
      --  Read-only. Stream x transfer error interrupt flag (x=7..4)
      TEIF7          : Boolean;
      --  Read-only. Stream x half transfer interrupt flag (x=7..4)
      HTIF7          : Boolean;
      --  Read-only. Stream x transfer complete interrupt flag (x=7..4)
      TCIF7          : Boolean;
      --  unspecified
      Reserved_28_31 : HAL.UInt4;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HISR_Register use record
      FEIF4          at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      DMEIF4         at 0 range 2 .. 2;
      TEIF4          at 0 range 3 .. 3;
      HTIF4          at 0 range 4 .. 4;
      TCIF4          at 0 range 5 .. 5;
      FEIF5          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      DMEIF5         at 0 range 8 .. 8;
      TEIF5          at 0 range 9 .. 9;
      HTIF5          at 0 range 10 .. 10;
      TCIF5          at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      FEIF6          at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      DMEIF6         at 0 range 18 .. 18;
      TEIF6          at 0 range 19 .. 19;
      HTIF6          at 0 range 20 .. 20;
      TCIF6          at 0 range 21 .. 21;
      FEIF7          at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      DMEIF7         at 0 range 24 .. 24;
      TEIF7          at 0 range 25 .. 25;
      HTIF7          at 0 range 26 .. 26;
      TCIF7          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  low interrupt flag clear register
   type LIFCR_Register is record
      --  Stream x clear FIFO error interrupt flag (x = 3..0)
      CFEIF0         : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 3..0)
      CDMEIF0        : Boolean := False;
      --  Stream x clear transfer error interrupt flag (x = 3..0)
      CTEIF0         : Boolean := False;
      --  Stream x clear half transfer interrupt flag (x = 3..0)
      CHTIF0         : Boolean := False;
      --  Stream x clear transfer complete interrupt flag (x = 3..0)
      CTCIF0         : Boolean := False;
      --  Stream x clear FIFO error interrupt flag (x = 3..0)
      CFEIF1         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 3..0)
      CDMEIF1        : Boolean := False;
      --  Stream x clear transfer error interrupt flag (x = 3..0)
      CTEIF1         : Boolean := False;
      --  Stream x clear half transfer interrupt flag (x = 3..0)
      CHTIF1         : Boolean := False;
      --  Stream x clear transfer complete interrupt flag (x = 3..0)
      CTCIF1         : Boolean := False;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Stream x clear FIFO error interrupt flag (x = 3..0)
      CFEIF2         : Boolean := False;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 3..0)
      CDMEIF2        : Boolean := False;
      --  Stream x clear transfer error interrupt flag (x = 3..0)
      CTEIF2         : Boolean := False;
      --  Stream x clear half transfer interrupt flag (x = 3..0)
      CHTIF2         : Boolean := False;
      --  Stream x clear transfer complete interrupt flag (x = 3..0)
      CTCIF2         : Boolean := False;
      --  Stream x clear FIFO error interrupt flag (x = 3..0)
      CFEIF3         : Boolean := False;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 3..0)
      CDMEIF3        : Boolean := False;
      --  Stream x clear transfer error interrupt flag (x = 3..0)
      CTEIF3         : Boolean := False;
      --  Stream x clear half transfer interrupt flag (x = 3..0)
      CHTIF3         : Boolean := False;
      --  Stream x clear transfer complete interrupt flag (x = 3..0)
      CTCIF3         : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LIFCR_Register use record
      CFEIF0         at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      CDMEIF0        at 0 range 2 .. 2;
      CTEIF0         at 0 range 3 .. 3;
      CHTIF0         at 0 range 4 .. 4;
      CTCIF0         at 0 range 5 .. 5;
      CFEIF1         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      CDMEIF1        at 0 range 8 .. 8;
      CTEIF1         at 0 range 9 .. 9;
      CHTIF1         at 0 range 10 .. 10;
      CTCIF1         at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      CFEIF2         at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      CDMEIF2        at 0 range 18 .. 18;
      CTEIF2         at 0 range 19 .. 19;
      CHTIF2         at 0 range 20 .. 20;
      CTCIF2         at 0 range 21 .. 21;
      CFEIF3         at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      CDMEIF3        at 0 range 24 .. 24;
      CTEIF3         at 0 range 25 .. 25;
      CHTIF3         at 0 range 26 .. 26;
      CTCIF3         at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  high interrupt flag clear register
   type HIFCR_Register is record
      --  Stream x clear FIFO error interrupt flag (x = 7..4)
      CFEIF4         : Boolean := False;
      --  unspecified
      Reserved_1_1   : HAL.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 7..4)
      CDMEIF4        : Boolean := False;
      --  Stream x clear transfer error interrupt flag (x = 7..4)
      CTEIF4         : Boolean := False;
      --  Stream x clear half transfer interrupt flag (x = 7..4)
      CHTIF4         : Boolean := False;
      --  Stream x clear transfer complete interrupt flag (x = 7..4)
      CTCIF4         : Boolean := False;
      --  Stream x clear FIFO error interrupt flag (x = 7..4)
      CFEIF5         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 7..4)
      CDMEIF5        : Boolean := False;
      --  Stream x clear transfer error interrupt flag (x = 7..4)
      CTEIF5         : Boolean := False;
      --  Stream x clear half transfer interrupt flag (x = 7..4)
      CHTIF5         : Boolean := False;
      --  Stream x clear transfer complete interrupt flag (x = 7..4)
      CTCIF5         : Boolean := False;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Stream x clear FIFO error interrupt flag (x = 7..4)
      CFEIF6         : Boolean := False;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 7..4)
      CDMEIF6        : Boolean := False;
      --  Stream x clear transfer error interrupt flag (x = 7..4)
      CTEIF6         : Boolean := False;
      --  Stream x clear half transfer interrupt flag (x = 7..4)
      CHTIF6         : Boolean := False;
      --  Stream x clear transfer complete interrupt flag (x = 7..4)
      CTCIF6         : Boolean := False;
      --  Stream x clear FIFO error interrupt flag (x = 7..4)
      CFEIF7         : Boolean := False;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 7..4)
      CDMEIF7        : Boolean := False;
      --  Stream x clear transfer error interrupt flag (x = 7..4)
      CTEIF7         : Boolean := False;
      --  Stream x clear half transfer interrupt flag (x = 7..4)
      CHTIF7         : Boolean := False;
      --  Stream x clear transfer complete interrupt flag (x = 7..4)
      CTCIF7         : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HIFCR_Register use record
      CFEIF4         at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      CDMEIF4        at 0 range 2 .. 2;
      CTEIF4         at 0 range 3 .. 3;
      CHTIF4         at 0 range 4 .. 4;
      CTCIF4         at 0 range 5 .. 5;
      CFEIF5         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      CDMEIF5        at 0 range 8 .. 8;
      CTEIF5         at 0 range 9 .. 9;
      CHTIF5         at 0 range 10 .. 10;
      CTCIF5         at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      CFEIF6         at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      CDMEIF6        at 0 range 18 .. 18;
      CTEIF6         at 0 range 19 .. 19;
      CHTIF6         at 0 range 20 .. 20;
      CTCIF6         at 0 range 21 .. 21;
      CFEIF7         at 0 range 22 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      CDMEIF7        at 0 range 24 .. 24;
      CTEIF7         at 0 range 25 .. 25;
      CHTIF7         at 0 range 26 .. 26;
      CTCIF7         at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --------------------------------
   -- Stream cluster's Registers --
   --------------------------------

   subtype SxCR_Stream_DIR_Field is HAL.UInt2;
   subtype SxCR_Stream_PSIZE_Field is HAL.UInt2;
   subtype SxCR_Stream_MSIZE_Field is HAL.UInt2;
   subtype SxCR_Stream_PL_Field is HAL.UInt2;
   subtype SxCR_Stream_PBURST_Field is HAL.UInt2;
   subtype SxCR_Stream_MBURST_Field is HAL.UInt2;
   subtype SxCR_Stream_CHSEL_Field is HAL.UInt4;

   --  stream x configuration register
   type SxCR_Stream_Register is record
      --  Stream enable / flag stream ready when read low
      EN             : Boolean := False;
      --  Direct mode error interrupt enable
      DMEIE          : Boolean := False;
      --  Transfer error interrupt enable
      TEIE           : Boolean := False;
      --  Half transfer interrupt enable
      HTIE           : Boolean := False;
      --  Transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  Peripheral flow controller
      PFCTRL         : Boolean := False;
      --  Data transfer direction
      DIR            : SxCR_Stream_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral data size
      PSIZE          : SxCR_Stream_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : SxCR_Stream_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : Boolean := False;
      --  Priority level
      PL             : SxCR_Stream_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : Boolean := False;
      --  Current target (only in double buffer mode)
      CT             : Boolean := False;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : SxCR_Stream_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : SxCR_Stream_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : SxCR_Stream_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : HAL.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SxCR_Stream_Register use record
      EN             at 0 range 0 .. 0;
      DMEIE          at 0 range 1 .. 1;
      TEIE           at 0 range 2 .. 2;
      HTIE           at 0 range 3 .. 3;
      TCIE           at 0 range 4 .. 4;
      PFCTRL         at 0 range 5 .. 5;
      DIR            at 0 range 6 .. 7;
      CIRC           at 0 range 8 .. 8;
      PINC           at 0 range 9 .. 9;
      MINC           at 0 range 10 .. 10;
      PSIZE          at 0 range 11 .. 12;
      MSIZE          at 0 range 13 .. 14;
      PINCOS         at 0 range 15 .. 15;
      PL             at 0 range 16 .. 17;
      DBM            at 0 range 18 .. 18;
      CT             at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      PBURST         at 0 range 21 .. 22;
      MBURST         at 0 range 23 .. 24;
      CHSEL          at 0 range 25 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype SxNDTR_Stream_NDT_Field is HAL.UInt16;

   --  stream x number of data register
   type SxNDTR_Stream_Register is record
      --  Number of data items to transfer
      NDT            : SxNDTR_Stream_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SxNDTR_Stream_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype SxFCR_Stream_FTH_Field is HAL.UInt2;
   subtype SxFCR_Stream_FS_Field is HAL.UInt3;

   --  stream x FIFO control register
   type SxFCR_Stream_Register is record
      --  FIFO threshold selection
      FTH           : SxFCR_Stream_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : Boolean := False;
      --  Read-only. FIFO status
      FS            : SxFCR_Stream_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SxFCR_Stream_Register use record
      FTH           at 0 range 0 .. 1;
      DMDIS         at 0 range 2 .. 2;
      FS            at 0 range 3 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      FEIE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Stream registers
   type Stream_Cluster is record
      --  stream x configuration register
      SxCR   : aliased SxCR_Stream_Register;
      --  stream x number of data register
      SxNDTR : aliased SxNDTR_Stream_Register;
      --  stream x peripheral address register
      SxPAR  : aliased HAL.UInt32;
      --  stream x memory 0 address register
      SxM0AR : aliased HAL.UInt32;
      --  stream x memory 1 address register
      SxM1AR : aliased HAL.UInt32;
      --  stream x FIFO control register
      SxFCR  : aliased SxFCR_Stream_Register;
   end record
     with Volatile, Size => 192;

   for Stream_Cluster use record
      SxCR   at 16#0# range 0 .. 31;
      SxNDTR at 16#4# range 0 .. 31;
      SxPAR  at 16#8# range 0 .. 31;
      SxM0AR at 16#C# range 0 .. 31;
      SxM1AR at 16#10# range 0 .. 31;
      SxFCR  at 16#14# range 0 .. 31;
   end record;

   --  Stream registers
   type Stream_Clusters is array (0 .. 7) of Stream_Cluster;

   --------------------------------
   -- Stream cluster's Registers --
   --------------------------------

   -----------------
   -- Peripherals --
   -----------------

   --  DMA controller
   type DMA_Peripheral is record
      --  low interrupt status register
      LISR   : aliased LISR_Register;
      --  high interrupt status register
      HISR   : aliased HISR_Register;
      --  low interrupt flag clear register
      LIFCR  : aliased LIFCR_Register;
      --  high interrupt flag clear register
      HIFCR  : aliased HIFCR_Register;
      --  Stream registers
      Stream : aliased Stream_Clusters;
   end record
     with Volatile;

   for DMA_Peripheral use record
      LISR   at 16#0# range 0 .. 31;
      HISR   at 16#4# range 0 .. 31;
      LIFCR  at 16#8# range 0 .. 31;
      HIFCR  at 16#C# range 0 .. 31;
      Stream at 16#10# range 0 .. 1535;
   end record;

   --  DMA controller
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#40026000#);

   --  DMA controller
   DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#40026400#);

end STM32_SVD.DMA;
