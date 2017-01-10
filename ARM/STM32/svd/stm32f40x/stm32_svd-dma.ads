--  This spec has been automatically generated from STM32F40x.svd

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

   subtype S0CR_DIR_Field is HAL.UInt2;
   subtype S0CR_PSIZE_Field is HAL.UInt2;
   subtype S0CR_MSIZE_Field is HAL.UInt2;
   subtype S0CR_PL_Field is HAL.UInt2;
   subtype S0CR_PBURST_Field is HAL.UInt2;
   subtype S0CR_MBURST_Field is HAL.UInt2;
   subtype S0CR_CHSEL_Field is HAL.UInt3;

   --  stream x configuration register
   type S0CR_Register is record
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
      DIR            : S0CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral data size
      PSIZE          : S0CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S0CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : Boolean := False;
      --  Priority level
      PL             : S0CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : Boolean := False;
      --  Current target (only in double buffer mode)
      CT             : Boolean := False;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : S0CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S0CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S0CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S0CR_Register use record
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
      CHSEL          at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype S0NDTR_NDT_Field is HAL.UInt16;

   --  stream x number of data register
   type S0NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S0NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S0NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype S0FCR_FTH_Field is HAL.UInt2;
   subtype S0FCR_FS_Field is HAL.UInt3;

   --  stream x FIFO control register
   type S0FCR_Register is record
      --  FIFO threshold selection
      FTH           : S0FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : Boolean := False;
      --  Read-only. FIFO status
      FS            : S0FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S0FCR_Register use record
      FTH           at 0 range 0 .. 1;
      DMDIS         at 0 range 2 .. 2;
      FS            at 0 range 3 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      FEIE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype S1CR_DIR_Field is HAL.UInt2;
   subtype S1CR_PSIZE_Field is HAL.UInt2;
   subtype S1CR_MSIZE_Field is HAL.UInt2;
   subtype S1CR_PL_Field is HAL.UInt2;
   subtype S1CR_PBURST_Field is HAL.UInt2;
   subtype S1CR_MBURST_Field is HAL.UInt2;
   subtype S1CR_CHSEL_Field is HAL.UInt3;

   --  stream x configuration register
   type S1CR_Register is record
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
      DIR            : S1CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral data size
      PSIZE          : S1CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S1CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : Boolean := False;
      --  Priority level
      PL             : S1CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : Boolean := False;
      --  Current target (only in double buffer mode)
      CT             : Boolean := False;
      --  ACK
      ACK            : Boolean := False;
      --  Peripheral burst transfer configuration
      PBURST         : S1CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S1CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S1CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S1CR_Register use record
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
      ACK            at 0 range 20 .. 20;
      PBURST         at 0 range 21 .. 22;
      MBURST         at 0 range 23 .. 24;
      CHSEL          at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype S1NDTR_NDT_Field is HAL.UInt16;

   --  stream x number of data register
   type S1NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S1NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S1NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype S1FCR_FTH_Field is HAL.UInt2;
   subtype S1FCR_FS_Field is HAL.UInt3;

   --  stream x FIFO control register
   type S1FCR_Register is record
      --  FIFO threshold selection
      FTH           : S1FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : Boolean := False;
      --  Read-only. FIFO status
      FS            : S1FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S1FCR_Register use record
      FTH           at 0 range 0 .. 1;
      DMDIS         at 0 range 2 .. 2;
      FS            at 0 range 3 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      FEIE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype S2CR_DIR_Field is HAL.UInt2;
   subtype S2CR_PSIZE_Field is HAL.UInt2;
   subtype S2CR_MSIZE_Field is HAL.UInt2;
   subtype S2CR_PL_Field is HAL.UInt2;
   subtype S2CR_PBURST_Field is HAL.UInt2;
   subtype S2CR_MBURST_Field is HAL.UInt2;
   subtype S2CR_CHSEL_Field is HAL.UInt3;

   --  stream x configuration register
   type S2CR_Register is record
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
      DIR            : S2CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral data size
      PSIZE          : S2CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S2CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : Boolean := False;
      --  Priority level
      PL             : S2CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : Boolean := False;
      --  Current target (only in double buffer mode)
      CT             : Boolean := False;
      --  ACK
      ACK            : Boolean := False;
      --  Peripheral burst transfer configuration
      PBURST         : S2CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S2CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S2CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S2CR_Register use record
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
      ACK            at 0 range 20 .. 20;
      PBURST         at 0 range 21 .. 22;
      MBURST         at 0 range 23 .. 24;
      CHSEL          at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype S2NDTR_NDT_Field is HAL.UInt16;

   --  stream x number of data register
   type S2NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S2NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S2NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype S2FCR_FTH_Field is HAL.UInt2;
   subtype S2FCR_FS_Field is HAL.UInt3;

   --  stream x FIFO control register
   type S2FCR_Register is record
      --  FIFO threshold selection
      FTH           : S2FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : Boolean := False;
      --  Read-only. FIFO status
      FS            : S2FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S2FCR_Register use record
      FTH           at 0 range 0 .. 1;
      DMDIS         at 0 range 2 .. 2;
      FS            at 0 range 3 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      FEIE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype S3CR_DIR_Field is HAL.UInt2;
   subtype S3CR_PSIZE_Field is HAL.UInt2;
   subtype S3CR_MSIZE_Field is HAL.UInt2;
   subtype S3CR_PL_Field is HAL.UInt2;
   subtype S3CR_PBURST_Field is HAL.UInt2;
   subtype S3CR_MBURST_Field is HAL.UInt2;
   subtype S3CR_CHSEL_Field is HAL.UInt3;

   --  stream x configuration register
   type S3CR_Register is record
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
      DIR            : S3CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral data size
      PSIZE          : S3CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S3CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : Boolean := False;
      --  Priority level
      PL             : S3CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : Boolean := False;
      --  Current target (only in double buffer mode)
      CT             : Boolean := False;
      --  ACK
      ACK            : Boolean := False;
      --  Peripheral burst transfer configuration
      PBURST         : S3CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S3CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S3CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S3CR_Register use record
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
      ACK            at 0 range 20 .. 20;
      PBURST         at 0 range 21 .. 22;
      MBURST         at 0 range 23 .. 24;
      CHSEL          at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype S3NDTR_NDT_Field is HAL.UInt16;

   --  stream x number of data register
   type S3NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S3NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S3NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype S3FCR_FTH_Field is HAL.UInt2;
   subtype S3FCR_FS_Field is HAL.UInt3;

   --  stream x FIFO control register
   type S3FCR_Register is record
      --  FIFO threshold selection
      FTH           : S3FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : Boolean := False;
      --  Read-only. FIFO status
      FS            : S3FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S3FCR_Register use record
      FTH           at 0 range 0 .. 1;
      DMDIS         at 0 range 2 .. 2;
      FS            at 0 range 3 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      FEIE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype S4CR_DIR_Field is HAL.UInt2;
   subtype S4CR_PSIZE_Field is HAL.UInt2;
   subtype S4CR_MSIZE_Field is HAL.UInt2;
   subtype S4CR_PL_Field is HAL.UInt2;
   subtype S4CR_PBURST_Field is HAL.UInt2;
   subtype S4CR_MBURST_Field is HAL.UInt2;
   subtype S4CR_CHSEL_Field is HAL.UInt3;

   --  stream x configuration register
   type S4CR_Register is record
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
      DIR            : S4CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral data size
      PSIZE          : S4CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S4CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : Boolean := False;
      --  Priority level
      PL             : S4CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : Boolean := False;
      --  Current target (only in double buffer mode)
      CT             : Boolean := False;
      --  ACK
      ACK            : Boolean := False;
      --  Peripheral burst transfer configuration
      PBURST         : S4CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S4CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S4CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S4CR_Register use record
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
      ACK            at 0 range 20 .. 20;
      PBURST         at 0 range 21 .. 22;
      MBURST         at 0 range 23 .. 24;
      CHSEL          at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype S4NDTR_NDT_Field is HAL.UInt16;

   --  stream x number of data register
   type S4NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S4NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S4NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype S4FCR_FTH_Field is HAL.UInt2;
   subtype S4FCR_FS_Field is HAL.UInt3;

   --  stream x FIFO control register
   type S4FCR_Register is record
      --  FIFO threshold selection
      FTH           : S4FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : Boolean := False;
      --  Read-only. FIFO status
      FS            : S4FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S4FCR_Register use record
      FTH           at 0 range 0 .. 1;
      DMDIS         at 0 range 2 .. 2;
      FS            at 0 range 3 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      FEIE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype S5CR_DIR_Field is HAL.UInt2;
   subtype S5CR_PSIZE_Field is HAL.UInt2;
   subtype S5CR_MSIZE_Field is HAL.UInt2;
   subtype S5CR_PL_Field is HAL.UInt2;
   subtype S5CR_PBURST_Field is HAL.UInt2;
   subtype S5CR_MBURST_Field is HAL.UInt2;
   subtype S5CR_CHSEL_Field is HAL.UInt3;

   --  stream x configuration register
   type S5CR_Register is record
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
      DIR            : S5CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral data size
      PSIZE          : S5CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S5CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : Boolean := False;
      --  Priority level
      PL             : S5CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : Boolean := False;
      --  Current target (only in double buffer mode)
      CT             : Boolean := False;
      --  ACK
      ACK            : Boolean := False;
      --  Peripheral burst transfer configuration
      PBURST         : S5CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S5CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S5CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S5CR_Register use record
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
      ACK            at 0 range 20 .. 20;
      PBURST         at 0 range 21 .. 22;
      MBURST         at 0 range 23 .. 24;
      CHSEL          at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype S5NDTR_NDT_Field is HAL.UInt16;

   --  stream x number of data register
   type S5NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S5NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S5NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype S5FCR_FTH_Field is HAL.UInt2;
   subtype S5FCR_FS_Field is HAL.UInt3;

   --  stream x FIFO control register
   type S5FCR_Register is record
      --  FIFO threshold selection
      FTH           : S5FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : Boolean := False;
      --  Read-only. FIFO status
      FS            : S5FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S5FCR_Register use record
      FTH           at 0 range 0 .. 1;
      DMDIS         at 0 range 2 .. 2;
      FS            at 0 range 3 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      FEIE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype S6CR_DIR_Field is HAL.UInt2;
   subtype S6CR_PSIZE_Field is HAL.UInt2;
   subtype S6CR_MSIZE_Field is HAL.UInt2;
   subtype S6CR_PL_Field is HAL.UInt2;
   subtype S6CR_PBURST_Field is HAL.UInt2;
   subtype S6CR_MBURST_Field is HAL.UInt2;
   subtype S6CR_CHSEL_Field is HAL.UInt3;

   --  stream x configuration register
   type S6CR_Register is record
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
      DIR            : S6CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral data size
      PSIZE          : S6CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S6CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : Boolean := False;
      --  Priority level
      PL             : S6CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : Boolean := False;
      --  Current target (only in double buffer mode)
      CT             : Boolean := False;
      --  ACK
      ACK            : Boolean := False;
      --  Peripheral burst transfer configuration
      PBURST         : S6CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S6CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S6CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S6CR_Register use record
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
      ACK            at 0 range 20 .. 20;
      PBURST         at 0 range 21 .. 22;
      MBURST         at 0 range 23 .. 24;
      CHSEL          at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype S6NDTR_NDT_Field is HAL.UInt16;

   --  stream x number of data register
   type S6NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S6NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S6NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype S6FCR_FTH_Field is HAL.UInt2;
   subtype S6FCR_FS_Field is HAL.UInt3;

   --  stream x FIFO control register
   type S6FCR_Register is record
      --  FIFO threshold selection
      FTH           : S6FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : Boolean := False;
      --  Read-only. FIFO status
      FS            : S6FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S6FCR_Register use record
      FTH           at 0 range 0 .. 1;
      DMDIS         at 0 range 2 .. 2;
      FS            at 0 range 3 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      FEIE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype S7CR_DIR_Field is HAL.UInt2;
   subtype S7CR_PSIZE_Field is HAL.UInt2;
   subtype S7CR_MSIZE_Field is HAL.UInt2;
   subtype S7CR_PL_Field is HAL.UInt2;
   subtype S7CR_PBURST_Field is HAL.UInt2;
   subtype S7CR_MBURST_Field is HAL.UInt2;
   subtype S7CR_CHSEL_Field is HAL.UInt3;

   --  stream x configuration register
   type S7CR_Register is record
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
      DIR            : S7CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : Boolean := False;
      --  Peripheral increment mode
      PINC           : Boolean := False;
      --  Memory increment mode
      MINC           : Boolean := False;
      --  Peripheral data size
      PSIZE          : S7CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S7CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : Boolean := False;
      --  Priority level
      PL             : S7CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : Boolean := False;
      --  Current target (only in double buffer mode)
      CT             : Boolean := False;
      --  ACK
      ACK            : Boolean := False;
      --  Peripheral burst transfer configuration
      PBURST         : S7CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S7CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S7CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S7CR_Register use record
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
      ACK            at 0 range 20 .. 20;
      PBURST         at 0 range 21 .. 22;
      MBURST         at 0 range 23 .. 24;
      CHSEL          at 0 range 25 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype S7NDTR_NDT_Field is HAL.UInt16;

   --  stream x number of data register
   type S7NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S7NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S7NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype S7FCR_FTH_Field is HAL.UInt2;
   subtype S7FCR_FS_Field is HAL.UInt3;

   --  stream x FIFO control register
   type S7FCR_Register is record
      --  FIFO threshold selection
      FTH           : S7FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : Boolean := False;
      --  Read-only. FIFO status
      FS            : S7FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S7FCR_Register use record
      FTH           at 0 range 0 .. 1;
      DMDIS         at 0 range 2 .. 2;
      FS            at 0 range 3 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      FEIE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

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
      --  stream x configuration register
      S0CR   : aliased S0CR_Register;
      --  stream x number of data register
      S0NDTR : aliased S0NDTR_Register;
      --  stream x peripheral address register
      S0PAR  : aliased HAL.UInt32;
      --  stream x memory 0 address register
      S0M0AR : aliased HAL.UInt32;
      --  stream x memory 1 address register
      S0M1AR : aliased HAL.UInt32;
      --  stream x FIFO control register
      S0FCR  : aliased S0FCR_Register;
      --  stream x configuration register
      S1CR   : aliased S1CR_Register;
      --  stream x number of data register
      S1NDTR : aliased S1NDTR_Register;
      --  stream x peripheral address register
      S1PAR  : aliased HAL.UInt32;
      --  stream x memory 0 address register
      S1M0AR : aliased HAL.UInt32;
      --  stream x memory 1 address register
      S1M1AR : aliased HAL.UInt32;
      --  stream x FIFO control register
      S1FCR  : aliased S1FCR_Register;
      --  stream x configuration register
      S2CR   : aliased S2CR_Register;
      --  stream x number of data register
      S2NDTR : aliased S2NDTR_Register;
      --  stream x peripheral address register
      S2PAR  : aliased HAL.UInt32;
      --  stream x memory 0 address register
      S2M0AR : aliased HAL.UInt32;
      --  stream x memory 1 address register
      S2M1AR : aliased HAL.UInt32;
      --  stream x FIFO control register
      S2FCR  : aliased S2FCR_Register;
      --  stream x configuration register
      S3CR   : aliased S3CR_Register;
      --  stream x number of data register
      S3NDTR : aliased S3NDTR_Register;
      --  stream x peripheral address register
      S3PAR  : aliased HAL.UInt32;
      --  stream x memory 0 address register
      S3M0AR : aliased HAL.UInt32;
      --  stream x memory 1 address register
      S3M1AR : aliased HAL.UInt32;
      --  stream x FIFO control register
      S3FCR  : aliased S3FCR_Register;
      --  stream x configuration register
      S4CR   : aliased S4CR_Register;
      --  stream x number of data register
      S4NDTR : aliased S4NDTR_Register;
      --  stream x peripheral address register
      S4PAR  : aliased HAL.UInt32;
      --  stream x memory 0 address register
      S4M0AR : aliased HAL.UInt32;
      --  stream x memory 1 address register
      S4M1AR : aliased HAL.UInt32;
      --  stream x FIFO control register
      S4FCR  : aliased S4FCR_Register;
      --  stream x configuration register
      S5CR   : aliased S5CR_Register;
      --  stream x number of data register
      S5NDTR : aliased S5NDTR_Register;
      --  stream x peripheral address register
      S5PAR  : aliased HAL.UInt32;
      --  stream x memory 0 address register
      S5M0AR : aliased HAL.UInt32;
      --  stream x memory 1 address register
      S5M1AR : aliased HAL.UInt32;
      --  stream x FIFO control register
      S5FCR  : aliased S5FCR_Register;
      --  stream x configuration register
      S6CR   : aliased S6CR_Register;
      --  stream x number of data register
      S6NDTR : aliased S6NDTR_Register;
      --  stream x peripheral address register
      S6PAR  : aliased HAL.UInt32;
      --  stream x memory 0 address register
      S6M0AR : aliased HAL.UInt32;
      --  stream x memory 1 address register
      S6M1AR : aliased HAL.UInt32;
      --  stream x FIFO control register
      S6FCR  : aliased S6FCR_Register;
      --  stream x configuration register
      S7CR   : aliased S7CR_Register;
      --  stream x number of data register
      S7NDTR : aliased S7NDTR_Register;
      --  stream x peripheral address register
      S7PAR  : aliased HAL.UInt32;
      --  stream x memory 0 address register
      S7M0AR : aliased HAL.UInt32;
      --  stream x memory 1 address register
      S7M1AR : aliased HAL.UInt32;
      --  stream x FIFO control register
      S7FCR  : aliased S7FCR_Register;
   end record
     with Volatile;

   for DMA_Peripheral use record
      LISR   at 16#0# range 0 .. 31;
      HISR   at 16#4# range 0 .. 31;
      LIFCR  at 16#8# range 0 .. 31;
      HIFCR  at 16#C# range 0 .. 31;
      S0CR   at 16#10# range 0 .. 31;
      S0NDTR at 16#14# range 0 .. 31;
      S0PAR  at 16#18# range 0 .. 31;
      S0M0AR at 16#1C# range 0 .. 31;
      S0M1AR at 16#20# range 0 .. 31;
      S0FCR  at 16#24# range 0 .. 31;
      S1CR   at 16#28# range 0 .. 31;
      S1NDTR at 16#2C# range 0 .. 31;
      S1PAR  at 16#30# range 0 .. 31;
      S1M0AR at 16#34# range 0 .. 31;
      S1M1AR at 16#38# range 0 .. 31;
      S1FCR  at 16#3C# range 0 .. 31;
      S2CR   at 16#40# range 0 .. 31;
      S2NDTR at 16#44# range 0 .. 31;
      S2PAR  at 16#48# range 0 .. 31;
      S2M0AR at 16#4C# range 0 .. 31;
      S2M1AR at 16#50# range 0 .. 31;
      S2FCR  at 16#54# range 0 .. 31;
      S3CR   at 16#58# range 0 .. 31;
      S3NDTR at 16#5C# range 0 .. 31;
      S3PAR  at 16#60# range 0 .. 31;
      S3M0AR at 16#64# range 0 .. 31;
      S3M1AR at 16#68# range 0 .. 31;
      S3FCR  at 16#6C# range 0 .. 31;
      S4CR   at 16#70# range 0 .. 31;
      S4NDTR at 16#74# range 0 .. 31;
      S4PAR  at 16#78# range 0 .. 31;
      S4M0AR at 16#7C# range 0 .. 31;
      S4M1AR at 16#80# range 0 .. 31;
      S4FCR  at 16#84# range 0 .. 31;
      S5CR   at 16#88# range 0 .. 31;
      S5NDTR at 16#8C# range 0 .. 31;
      S5PAR  at 16#90# range 0 .. 31;
      S5M0AR at 16#94# range 0 .. 31;
      S5M1AR at 16#98# range 0 .. 31;
      S5FCR  at 16#9C# range 0 .. 31;
      S6CR   at 16#A0# range 0 .. 31;
      S6NDTR at 16#A4# range 0 .. 31;
      S6PAR  at 16#A8# range 0 .. 31;
      S6M0AR at 16#AC# range 0 .. 31;
      S6M1AR at 16#B0# range 0 .. 31;
      S6FCR  at 16#B4# range 0 .. 31;
      S7CR   at 16#B8# range 0 .. 31;
      S7NDTR at 16#BC# range 0 .. 31;
      S7PAR  at 16#C0# range 0 .. 31;
      S7M0AR at 16#C4# range 0 .. 31;
      S7M1AR at 16#C8# range 0 .. 31;
      S7FCR  at 16#CC# range 0 .. 31;
   end record;

   --  DMA controller
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#40026000#);

   --  DMA controller
   DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#40026400#);

end STM32_SVD.DMA;
