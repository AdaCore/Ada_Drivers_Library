--  Automatically generated from STM32F46_79x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.DMA is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -------------------
   -- LISR_Register --
   -------------------

   subtype LISR_FEIF0_Field is STM32_SVD.Bit;
   subtype LISR_DMEIF0_Field is STM32_SVD.Bit;
   subtype LISR_TEIF0_Field is STM32_SVD.Bit;
   subtype LISR_HTIF0_Field is STM32_SVD.Bit;
   subtype LISR_TCIF0_Field is STM32_SVD.Bit;
   subtype LISR_FEIF1_Field is STM32_SVD.Bit;
   subtype LISR_DMEIF1_Field is STM32_SVD.Bit;
   subtype LISR_TEIF1_Field is STM32_SVD.Bit;
   subtype LISR_HTIF1_Field is STM32_SVD.Bit;
   subtype LISR_TCIF1_Field is STM32_SVD.Bit;
   subtype LISR_FEIF2_Field is STM32_SVD.Bit;
   subtype LISR_DMEIF2_Field is STM32_SVD.Bit;
   subtype LISR_TEIF2_Field is STM32_SVD.Bit;
   subtype LISR_HTIF2_Field is STM32_SVD.Bit;
   subtype LISR_TCIF2_Field is STM32_SVD.Bit;
   subtype LISR_FEIF3_Field is STM32_SVD.Bit;
   subtype LISR_DMEIF3_Field is STM32_SVD.Bit;
   subtype LISR_TEIF3_Field is STM32_SVD.Bit;
   subtype LISR_HTIF3_Field is STM32_SVD.Bit;
   subtype LISR_TCIF3_Field is STM32_SVD.Bit;

   --  low interrupt status register
   type LISR_Register is record
      --  Stream x FIFO error interrupt flag (x=3..0)
      FEIF0          : LISR_FEIF0_Field := 16#0#;
      --  unspecified
      Reserved_1_1   : STM32_SVD.Bit := 16#0#;
      --  Stream x direct mode error interrupt flag (x=3..0)
      DMEIF0         : LISR_DMEIF0_Field := 16#0#;
      --  Stream x transfer error interrupt flag (x=3..0)
      TEIF0          : LISR_TEIF0_Field := 16#0#;
      --  Stream x half transfer interrupt flag (x=3..0)
      HTIF0          : LISR_HTIF0_Field := 16#0#;
      --  Stream x transfer complete interrupt flag (x = 3..0)
      TCIF0          : LISR_TCIF0_Field := 16#0#;
      --  Stream x FIFO error interrupt flag (x=3..0)
      FEIF1          : LISR_FEIF1_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32_SVD.Bit := 16#0#;
      --  Stream x direct mode error interrupt flag (x=3..0)
      DMEIF1         : LISR_DMEIF1_Field := 16#0#;
      --  Stream x transfer error interrupt flag (x=3..0)
      TEIF1          : LISR_TEIF1_Field := 16#0#;
      --  Stream x half transfer interrupt flag (x=3..0)
      HTIF1          : LISR_HTIF1_Field := 16#0#;
      --  Stream x transfer complete interrupt flag (x = 3..0)
      TCIF1          : LISR_TCIF1_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : STM32_SVD.UInt4 := 16#0#;
      --  Stream x FIFO error interrupt flag (x=3..0)
      FEIF2          : LISR_FEIF2_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32_SVD.Bit := 16#0#;
      --  Stream x direct mode error interrupt flag (x=3..0)
      DMEIF2         : LISR_DMEIF2_Field := 16#0#;
      --  Stream x transfer error interrupt flag (x=3..0)
      TEIF2          : LISR_TEIF2_Field := 16#0#;
      --  Stream x half transfer interrupt flag (x=3..0)
      HTIF2          : LISR_HTIF2_Field := 16#0#;
      --  Stream x transfer complete interrupt flag (x = 3..0)
      TCIF2          : LISR_TCIF2_Field := 16#0#;
      --  Stream x FIFO error interrupt flag (x=3..0)
      FEIF3          : LISR_FEIF3_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32_SVD.Bit := 16#0#;
      --  Stream x direct mode error interrupt flag (x=3..0)
      DMEIF3         : LISR_DMEIF3_Field := 16#0#;
      --  Stream x transfer error interrupt flag (x=3..0)
      TEIF3          : LISR_TEIF3_Field := 16#0#;
      --  Stream x half transfer interrupt flag (x=3..0)
      HTIF3          : LISR_HTIF3_Field := 16#0#;
      --  Stream x transfer complete interrupt flag (x = 3..0)
      TCIF3          : LISR_TCIF3_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   -------------------
   -- HISR_Register --
   -------------------

   subtype HISR_FEIF4_Field is STM32_SVD.Bit;
   subtype HISR_DMEIF4_Field is STM32_SVD.Bit;
   subtype HISR_TEIF4_Field is STM32_SVD.Bit;
   subtype HISR_HTIF4_Field is STM32_SVD.Bit;
   subtype HISR_TCIF4_Field is STM32_SVD.Bit;
   subtype HISR_FEIF5_Field is STM32_SVD.Bit;
   subtype HISR_DMEIF5_Field is STM32_SVD.Bit;
   subtype HISR_TEIF5_Field is STM32_SVD.Bit;
   subtype HISR_HTIF5_Field is STM32_SVD.Bit;
   subtype HISR_TCIF5_Field is STM32_SVD.Bit;
   subtype HISR_FEIF6_Field is STM32_SVD.Bit;
   subtype HISR_DMEIF6_Field is STM32_SVD.Bit;
   subtype HISR_TEIF6_Field is STM32_SVD.Bit;
   subtype HISR_HTIF6_Field is STM32_SVD.Bit;
   subtype HISR_TCIF6_Field is STM32_SVD.Bit;
   subtype HISR_FEIF7_Field is STM32_SVD.Bit;
   subtype HISR_DMEIF7_Field is STM32_SVD.Bit;
   subtype HISR_TEIF7_Field is STM32_SVD.Bit;
   subtype HISR_HTIF7_Field is STM32_SVD.Bit;
   subtype HISR_TCIF7_Field is STM32_SVD.Bit;

   --  high interrupt status register
   type HISR_Register is record
      --  Stream x FIFO error interrupt flag (x=7..4)
      FEIF4          : HISR_FEIF4_Field := 16#0#;
      --  unspecified
      Reserved_1_1   : STM32_SVD.Bit := 16#0#;
      --  Stream x direct mode error interrupt flag (x=7..4)
      DMEIF4         : HISR_DMEIF4_Field := 16#0#;
      --  Stream x transfer error interrupt flag (x=7..4)
      TEIF4          : HISR_TEIF4_Field := 16#0#;
      --  Stream x half transfer interrupt flag (x=7..4)
      HTIF4          : HISR_HTIF4_Field := 16#0#;
      --  Stream x transfer complete interrupt flag (x=7..4)
      TCIF4          : HISR_TCIF4_Field := 16#0#;
      --  Stream x FIFO error interrupt flag (x=7..4)
      FEIF5          : HISR_FEIF5_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32_SVD.Bit := 16#0#;
      --  Stream x direct mode error interrupt flag (x=7..4)
      DMEIF5         : HISR_DMEIF5_Field := 16#0#;
      --  Stream x transfer error interrupt flag (x=7..4)
      TEIF5          : HISR_TEIF5_Field := 16#0#;
      --  Stream x half transfer interrupt flag (x=7..4)
      HTIF5          : HISR_HTIF5_Field := 16#0#;
      --  Stream x transfer complete interrupt flag (x=7..4)
      TCIF5          : HISR_TCIF5_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : STM32_SVD.UInt4 := 16#0#;
      --  Stream x FIFO error interrupt flag (x=7..4)
      FEIF6          : HISR_FEIF6_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32_SVD.Bit := 16#0#;
      --  Stream x direct mode error interrupt flag (x=7..4)
      DMEIF6         : HISR_DMEIF6_Field := 16#0#;
      --  Stream x transfer error interrupt flag (x=7..4)
      TEIF6          : HISR_TEIF6_Field := 16#0#;
      --  Stream x half transfer interrupt flag (x=7..4)
      HTIF6          : HISR_HTIF6_Field := 16#0#;
      --  Stream x transfer complete interrupt flag (x=7..4)
      TCIF6          : HISR_TCIF6_Field := 16#0#;
      --  Stream x FIFO error interrupt flag (x=7..4)
      FEIF7          : HISR_FEIF7_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32_SVD.Bit := 16#0#;
      --  Stream x direct mode error interrupt flag (x=7..4)
      DMEIF7         : HISR_DMEIF7_Field := 16#0#;
      --  Stream x transfer error interrupt flag (x=7..4)
      TEIF7          : HISR_TEIF7_Field := 16#0#;
      --  Stream x half transfer interrupt flag (x=7..4)
      HTIF7          : HISR_HTIF7_Field := 16#0#;
      --  Stream x transfer complete interrupt flag (x=7..4)
      TCIF7          : HISR_TCIF7_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   --------------------
   -- LIFCR_Register --
   --------------------

   subtype LIFCR_CFEIF0_Field is STM32_SVD.Bit;
   subtype LIFCR_CDMEIF0_Field is STM32_SVD.Bit;
   subtype LIFCR_CTEIF0_Field is STM32_SVD.Bit;
   subtype LIFCR_CHTIF0_Field is STM32_SVD.Bit;
   subtype LIFCR_CTCIF0_Field is STM32_SVD.Bit;
   subtype LIFCR_CFEIF1_Field is STM32_SVD.Bit;
   subtype LIFCR_CDMEIF1_Field is STM32_SVD.Bit;
   subtype LIFCR_CTEIF1_Field is STM32_SVD.Bit;
   subtype LIFCR_CHTIF1_Field is STM32_SVD.Bit;
   subtype LIFCR_CTCIF1_Field is STM32_SVD.Bit;
   subtype LIFCR_CFEIF2_Field is STM32_SVD.Bit;
   subtype LIFCR_CDMEIF2_Field is STM32_SVD.Bit;
   subtype LIFCR_CTEIF2_Field is STM32_SVD.Bit;
   subtype LIFCR_CHTIF2_Field is STM32_SVD.Bit;
   subtype LIFCR_CTCIF2_Field is STM32_SVD.Bit;
   subtype LIFCR_CFEIF3_Field is STM32_SVD.Bit;
   subtype LIFCR_CDMEIF3_Field is STM32_SVD.Bit;
   subtype LIFCR_CTEIF3_Field is STM32_SVD.Bit;
   subtype LIFCR_CHTIF3_Field is STM32_SVD.Bit;
   subtype LIFCR_CTCIF3_Field is STM32_SVD.Bit;

   --  low interrupt flag clear register
   type LIFCR_Register is record
      --  Stream x clear FIFO error interrupt flag (x = 3..0)
      CFEIF0         : LIFCR_CFEIF0_Field := 16#0#;
      --  unspecified
      Reserved_1_1   : STM32_SVD.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 3..0)
      CDMEIF0        : LIFCR_CDMEIF0_Field := 16#0#;
      --  Stream x clear transfer error interrupt flag (x = 3..0)
      CTEIF0         : LIFCR_CTEIF0_Field := 16#0#;
      --  Stream x clear half transfer interrupt flag (x = 3..0)
      CHTIF0         : LIFCR_CHTIF0_Field := 16#0#;
      --  Stream x clear transfer complete interrupt flag (x = 3..0)
      CTCIF0         : LIFCR_CTCIF0_Field := 16#0#;
      --  Stream x clear FIFO error interrupt flag (x = 3..0)
      CFEIF1         : LIFCR_CFEIF1_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32_SVD.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 3..0)
      CDMEIF1        : LIFCR_CDMEIF1_Field := 16#0#;
      --  Stream x clear transfer error interrupt flag (x = 3..0)
      CTEIF1         : LIFCR_CTEIF1_Field := 16#0#;
      --  Stream x clear half transfer interrupt flag (x = 3..0)
      CHTIF1         : LIFCR_CHTIF1_Field := 16#0#;
      --  Stream x clear transfer complete interrupt flag (x = 3..0)
      CTCIF1         : LIFCR_CTCIF1_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : STM32_SVD.UInt4 := 16#0#;
      --  Stream x clear FIFO error interrupt flag (x = 3..0)
      CFEIF2         : LIFCR_CFEIF2_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32_SVD.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 3..0)
      CDMEIF2        : LIFCR_CDMEIF2_Field := 16#0#;
      --  Stream x clear transfer error interrupt flag (x = 3..0)
      CTEIF2         : LIFCR_CTEIF2_Field := 16#0#;
      --  Stream x clear half transfer interrupt flag (x = 3..0)
      CHTIF2         : LIFCR_CHTIF2_Field := 16#0#;
      --  Stream x clear transfer complete interrupt flag (x = 3..0)
      CTCIF2         : LIFCR_CTCIF2_Field := 16#0#;
      --  Stream x clear FIFO error interrupt flag (x = 3..0)
      CFEIF3         : LIFCR_CFEIF3_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32_SVD.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 3..0)
      CDMEIF3        : LIFCR_CDMEIF3_Field := 16#0#;
      --  Stream x clear transfer error interrupt flag (x = 3..0)
      CTEIF3         : LIFCR_CTEIF3_Field := 16#0#;
      --  Stream x clear half transfer interrupt flag (x = 3..0)
      CHTIF3         : LIFCR_CHTIF3_Field := 16#0#;
      --  Stream x clear transfer complete interrupt flag (x = 3..0)
      CTCIF3         : LIFCR_CTCIF3_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   --------------------
   -- HIFCR_Register --
   --------------------

   subtype HIFCR_CFEIF4_Field is STM32_SVD.Bit;
   subtype HIFCR_CDMEIF4_Field is STM32_SVD.Bit;
   subtype HIFCR_CTEIF4_Field is STM32_SVD.Bit;
   subtype HIFCR_CHTIF4_Field is STM32_SVD.Bit;
   subtype HIFCR_CTCIF4_Field is STM32_SVD.Bit;
   subtype HIFCR_CFEIF5_Field is STM32_SVD.Bit;
   subtype HIFCR_CDMEIF5_Field is STM32_SVD.Bit;
   subtype HIFCR_CTEIF5_Field is STM32_SVD.Bit;
   subtype HIFCR_CHTIF5_Field is STM32_SVD.Bit;
   subtype HIFCR_CTCIF5_Field is STM32_SVD.Bit;
   subtype HIFCR_CFEIF6_Field is STM32_SVD.Bit;
   subtype HIFCR_CDMEIF6_Field is STM32_SVD.Bit;
   subtype HIFCR_CTEIF6_Field is STM32_SVD.Bit;
   subtype HIFCR_CHTIF6_Field is STM32_SVD.Bit;
   subtype HIFCR_CTCIF6_Field is STM32_SVD.Bit;
   subtype HIFCR_CFEIF7_Field is STM32_SVD.Bit;
   subtype HIFCR_CDMEIF7_Field is STM32_SVD.Bit;
   subtype HIFCR_CTEIF7_Field is STM32_SVD.Bit;
   subtype HIFCR_CHTIF7_Field is STM32_SVD.Bit;
   subtype HIFCR_CTCIF7_Field is STM32_SVD.Bit;

   --  high interrupt flag clear register
   type HIFCR_Register is record
      --  Stream x clear FIFO error interrupt flag (x = 7..4)
      CFEIF4         : HIFCR_CFEIF4_Field := 16#0#;
      --  unspecified
      Reserved_1_1   : STM32_SVD.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 7..4)
      CDMEIF4        : HIFCR_CDMEIF4_Field := 16#0#;
      --  Stream x clear transfer error interrupt flag (x = 7..4)
      CTEIF4         : HIFCR_CTEIF4_Field := 16#0#;
      --  Stream x clear half transfer interrupt flag (x = 7..4)
      CHTIF4         : HIFCR_CHTIF4_Field := 16#0#;
      --  Stream x clear transfer complete interrupt flag (x = 7..4)
      CTCIF4         : HIFCR_CTCIF4_Field := 16#0#;
      --  Stream x clear FIFO error interrupt flag (x = 7..4)
      CFEIF5         : HIFCR_CFEIF5_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32_SVD.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 7..4)
      CDMEIF5        : HIFCR_CDMEIF5_Field := 16#0#;
      --  Stream x clear transfer error interrupt flag (x = 7..4)
      CTEIF5         : HIFCR_CTEIF5_Field := 16#0#;
      --  Stream x clear half transfer interrupt flag (x = 7..4)
      CHTIF5         : HIFCR_CHTIF5_Field := 16#0#;
      --  Stream x clear transfer complete interrupt flag (x = 7..4)
      CTCIF5         : HIFCR_CTCIF5_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : STM32_SVD.UInt4 := 16#0#;
      --  Stream x clear FIFO error interrupt flag (x = 7..4)
      CFEIF6         : HIFCR_CFEIF6_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32_SVD.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 7..4)
      CDMEIF6        : HIFCR_CDMEIF6_Field := 16#0#;
      --  Stream x clear transfer error interrupt flag (x = 7..4)
      CTEIF6         : HIFCR_CTEIF6_Field := 16#0#;
      --  Stream x clear half transfer interrupt flag (x = 7..4)
      CHTIF6         : HIFCR_CHTIF6_Field := 16#0#;
      --  Stream x clear transfer complete interrupt flag (x = 7..4)
      CTCIF6         : HIFCR_CTCIF6_Field := 16#0#;
      --  Stream x clear FIFO error interrupt flag (x = 7..4)
      CFEIF7         : HIFCR_CFEIF7_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32_SVD.Bit := 16#0#;
      --  Stream x clear direct mode error interrupt flag (x = 7..4)
      CDMEIF7        : HIFCR_CDMEIF7_Field := 16#0#;
      --  Stream x clear transfer error interrupt flag (x = 7..4)
      CTEIF7         : HIFCR_CTEIF7_Field := 16#0#;
      --  Stream x clear half transfer interrupt flag (x = 7..4)
      CHTIF7         : HIFCR_CHTIF7_Field := 16#0#;
      --  Stream x clear transfer complete interrupt flag (x = 7..4)
      CTCIF7         : HIFCR_CTCIF7_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   -------------------
   -- S0CR_Register --
   -------------------

   subtype S0CR_EN_Field is STM32_SVD.Bit;
   subtype S0CR_DMEIE_Field is STM32_SVD.Bit;
   subtype S0CR_TEIE_Field is STM32_SVD.Bit;
   subtype S0CR_HTIE_Field is STM32_SVD.Bit;
   subtype S0CR_TCIE_Field is STM32_SVD.Bit;
   subtype S0CR_PFCTRL_Field is STM32_SVD.Bit;
   subtype S0CR_DIR_Field is STM32_SVD.UInt2;
   subtype S0CR_CIRC_Field is STM32_SVD.Bit;
   subtype S0CR_PINC_Field is STM32_SVD.Bit;
   subtype S0CR_MINC_Field is STM32_SVD.Bit;
   subtype S0CR_PSIZE_Field is STM32_SVD.UInt2;
   subtype S0CR_MSIZE_Field is STM32_SVD.UInt2;
   subtype S0CR_PINCOS_Field is STM32_SVD.Bit;
   subtype S0CR_PL_Field is STM32_SVD.UInt2;
   subtype S0CR_DBM_Field is STM32_SVD.Bit;
   subtype S0CR_CT_Field is STM32_SVD.Bit;
   subtype S0CR_PBURST_Field is STM32_SVD.UInt2;
   subtype S0CR_MBURST_Field is STM32_SVD.UInt2;
   subtype S0CR_CHSEL_Field is STM32_SVD.UInt3;

   --  stream x configuration register
   type S0CR_Register is record
      --  Stream enable / flag stream ready when read low
      EN             : S0CR_EN_Field := 16#0#;
      --  Direct mode error interrupt enable
      DMEIE          : S0CR_DMEIE_Field := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : S0CR_TEIE_Field := 16#0#;
      --  Half transfer interrupt enable
      HTIE           : S0CR_HTIE_Field := 16#0#;
      --  Transfer complete interrupt enable
      TCIE           : S0CR_TCIE_Field := 16#0#;
      --  Peripheral flow controller
      PFCTRL         : S0CR_PFCTRL_Field := 16#0#;
      --  Data transfer direction
      DIR            : S0CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : S0CR_CIRC_Field := 16#0#;
      --  Peripheral increment mode
      PINC           : S0CR_PINC_Field := 16#0#;
      --  Memory increment mode
      MINC           : S0CR_MINC_Field := 16#0#;
      --  Peripheral data size
      PSIZE          : S0CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S0CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : S0CR_PINCOS_Field := 16#0#;
      --  Priority level
      PL             : S0CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : S0CR_DBM_Field := 16#0#;
      --  Current target (only in double buffer mode)
      CT             : S0CR_CT_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : STM32_SVD.Bit := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : S0CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S0CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S0CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   ---------------------
   -- S0NDTR_Register --
   ---------------------

   subtype S0NDTR_NDT_Field is STM32_SVD.Short;

   --  stream x number of data register
   type S0NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S0NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S0NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- S0FCR_Register --
   --------------------

   subtype S0FCR_FTH_Field is STM32_SVD.UInt2;
   subtype S0FCR_DMDIS_Field is STM32_SVD.Bit;
   subtype S0FCR_FS_Field is STM32_SVD.UInt3;
   subtype S0FCR_FEIE_Field is STM32_SVD.Bit;

   --  stream x FIFO control register
   type S0FCR_Register is record
      --  FIFO threshold selection
      FTH           : S0FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : S0FCR_DMDIS_Field := 16#0#;
      --  FIFO status
      FS            : S0FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : STM32_SVD.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : S0FCR_FEIE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   -------------------
   -- S1CR_Register --
   -------------------

   subtype S1CR_EN_Field is STM32_SVD.Bit;
   subtype S1CR_DMEIE_Field is STM32_SVD.Bit;
   subtype S1CR_TEIE_Field is STM32_SVD.Bit;
   subtype S1CR_HTIE_Field is STM32_SVD.Bit;
   subtype S1CR_TCIE_Field is STM32_SVD.Bit;
   subtype S1CR_PFCTRL_Field is STM32_SVD.Bit;
   subtype S1CR_DIR_Field is STM32_SVD.UInt2;
   subtype S1CR_CIRC_Field is STM32_SVD.Bit;
   subtype S1CR_PINC_Field is STM32_SVD.Bit;
   subtype S1CR_MINC_Field is STM32_SVD.Bit;
   subtype S1CR_PSIZE_Field is STM32_SVD.UInt2;
   subtype S1CR_MSIZE_Field is STM32_SVD.UInt2;
   subtype S1CR_PINCOS_Field is STM32_SVD.Bit;
   subtype S1CR_PL_Field is STM32_SVD.UInt2;
   subtype S1CR_DBM_Field is STM32_SVD.Bit;
   subtype S1CR_CT_Field is STM32_SVD.Bit;
   subtype S1CR_ACK_Field is STM32_SVD.Bit;
   subtype S1CR_PBURST_Field is STM32_SVD.UInt2;
   subtype S1CR_MBURST_Field is STM32_SVD.UInt2;
   subtype S1CR_CHSEL_Field is STM32_SVD.UInt3;

   --  stream x configuration register
   type S1CR_Register is record
      --  Stream enable / flag stream ready when read low
      EN             : S1CR_EN_Field := 16#0#;
      --  Direct mode error interrupt enable
      DMEIE          : S1CR_DMEIE_Field := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : S1CR_TEIE_Field := 16#0#;
      --  Half transfer interrupt enable
      HTIE           : S1CR_HTIE_Field := 16#0#;
      --  Transfer complete interrupt enable
      TCIE           : S1CR_TCIE_Field := 16#0#;
      --  Peripheral flow controller
      PFCTRL         : S1CR_PFCTRL_Field := 16#0#;
      --  Data transfer direction
      DIR            : S1CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : S1CR_CIRC_Field := 16#0#;
      --  Peripheral increment mode
      PINC           : S1CR_PINC_Field := 16#0#;
      --  Memory increment mode
      MINC           : S1CR_MINC_Field := 16#0#;
      --  Peripheral data size
      PSIZE          : S1CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S1CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : S1CR_PINCOS_Field := 16#0#;
      --  Priority level
      PL             : S1CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : S1CR_DBM_Field := 16#0#;
      --  Current target (only in double buffer mode)
      CT             : S1CR_CT_Field := 16#0#;
      --  ACK
      ACK            : S1CR_ACK_Field := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : S1CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S1CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S1CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   ---------------------
   -- S1NDTR_Register --
   ---------------------

   subtype S1NDTR_NDT_Field is STM32_SVD.Short;

   --  stream x number of data register
   type S1NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S1NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S1NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- S1FCR_Register --
   --------------------

   subtype S1FCR_FTH_Field is STM32_SVD.UInt2;
   subtype S1FCR_DMDIS_Field is STM32_SVD.Bit;
   subtype S1FCR_FS_Field is STM32_SVD.UInt3;
   subtype S1FCR_FEIE_Field is STM32_SVD.Bit;

   --  stream x FIFO control register
   type S1FCR_Register is record
      --  FIFO threshold selection
      FTH           : S1FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : S1FCR_DMDIS_Field := 16#0#;
      --  FIFO status
      FS            : S1FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : STM32_SVD.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : S1FCR_FEIE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   -------------------
   -- S2CR_Register --
   -------------------

   subtype S2CR_EN_Field is STM32_SVD.Bit;
   subtype S2CR_DMEIE_Field is STM32_SVD.Bit;
   subtype S2CR_TEIE_Field is STM32_SVD.Bit;
   subtype S2CR_HTIE_Field is STM32_SVD.Bit;
   subtype S2CR_TCIE_Field is STM32_SVD.Bit;
   subtype S2CR_PFCTRL_Field is STM32_SVD.Bit;
   subtype S2CR_DIR_Field is STM32_SVD.UInt2;
   subtype S2CR_CIRC_Field is STM32_SVD.Bit;
   subtype S2CR_PINC_Field is STM32_SVD.Bit;
   subtype S2CR_MINC_Field is STM32_SVD.Bit;
   subtype S2CR_PSIZE_Field is STM32_SVD.UInt2;
   subtype S2CR_MSIZE_Field is STM32_SVD.UInt2;
   subtype S2CR_PINCOS_Field is STM32_SVD.Bit;
   subtype S2CR_PL_Field is STM32_SVD.UInt2;
   subtype S2CR_DBM_Field is STM32_SVD.Bit;
   subtype S2CR_CT_Field is STM32_SVD.Bit;
   subtype S2CR_ACK_Field is STM32_SVD.Bit;
   subtype S2CR_PBURST_Field is STM32_SVD.UInt2;
   subtype S2CR_MBURST_Field is STM32_SVD.UInt2;
   subtype S2CR_CHSEL_Field is STM32_SVD.UInt3;

   --  stream x configuration register
   type S2CR_Register is record
      --  Stream enable / flag stream ready when read low
      EN             : S2CR_EN_Field := 16#0#;
      --  Direct mode error interrupt enable
      DMEIE          : S2CR_DMEIE_Field := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : S2CR_TEIE_Field := 16#0#;
      --  Half transfer interrupt enable
      HTIE           : S2CR_HTIE_Field := 16#0#;
      --  Transfer complete interrupt enable
      TCIE           : S2CR_TCIE_Field := 16#0#;
      --  Peripheral flow controller
      PFCTRL         : S2CR_PFCTRL_Field := 16#0#;
      --  Data transfer direction
      DIR            : S2CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : S2CR_CIRC_Field := 16#0#;
      --  Peripheral increment mode
      PINC           : S2CR_PINC_Field := 16#0#;
      --  Memory increment mode
      MINC           : S2CR_MINC_Field := 16#0#;
      --  Peripheral data size
      PSIZE          : S2CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S2CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : S2CR_PINCOS_Field := 16#0#;
      --  Priority level
      PL             : S2CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : S2CR_DBM_Field := 16#0#;
      --  Current target (only in double buffer mode)
      CT             : S2CR_CT_Field := 16#0#;
      --  ACK
      ACK            : S2CR_ACK_Field := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : S2CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S2CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S2CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   ---------------------
   -- S2NDTR_Register --
   ---------------------

   subtype S2NDTR_NDT_Field is STM32_SVD.Short;

   --  stream x number of data register
   type S2NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S2NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S2NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- S2FCR_Register --
   --------------------

   subtype S2FCR_FTH_Field is STM32_SVD.UInt2;
   subtype S2FCR_DMDIS_Field is STM32_SVD.Bit;
   subtype S2FCR_FS_Field is STM32_SVD.UInt3;
   subtype S2FCR_FEIE_Field is STM32_SVD.Bit;

   --  stream x FIFO control register
   type S2FCR_Register is record
      --  FIFO threshold selection
      FTH           : S2FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : S2FCR_DMDIS_Field := 16#0#;
      --  FIFO status
      FS            : S2FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : STM32_SVD.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : S2FCR_FEIE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   -------------------
   -- S3CR_Register --
   -------------------

   subtype S3CR_EN_Field is STM32_SVD.Bit;
   subtype S3CR_DMEIE_Field is STM32_SVD.Bit;
   subtype S3CR_TEIE_Field is STM32_SVD.Bit;
   subtype S3CR_HTIE_Field is STM32_SVD.Bit;
   subtype S3CR_TCIE_Field is STM32_SVD.Bit;
   subtype S3CR_PFCTRL_Field is STM32_SVD.Bit;
   subtype S3CR_DIR_Field is STM32_SVD.UInt2;
   subtype S3CR_CIRC_Field is STM32_SVD.Bit;
   subtype S3CR_PINC_Field is STM32_SVD.Bit;
   subtype S3CR_MINC_Field is STM32_SVD.Bit;
   subtype S3CR_PSIZE_Field is STM32_SVD.UInt2;
   subtype S3CR_MSIZE_Field is STM32_SVD.UInt2;
   subtype S3CR_PINCOS_Field is STM32_SVD.Bit;
   subtype S3CR_PL_Field is STM32_SVD.UInt2;
   subtype S3CR_DBM_Field is STM32_SVD.Bit;
   subtype S3CR_CT_Field is STM32_SVD.Bit;
   subtype S3CR_ACK_Field is STM32_SVD.Bit;
   subtype S3CR_PBURST_Field is STM32_SVD.UInt2;
   subtype S3CR_MBURST_Field is STM32_SVD.UInt2;
   subtype S3CR_CHSEL_Field is STM32_SVD.UInt3;

   --  stream x configuration register
   type S3CR_Register is record
      --  Stream enable / flag stream ready when read low
      EN             : S3CR_EN_Field := 16#0#;
      --  Direct mode error interrupt enable
      DMEIE          : S3CR_DMEIE_Field := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : S3CR_TEIE_Field := 16#0#;
      --  Half transfer interrupt enable
      HTIE           : S3CR_HTIE_Field := 16#0#;
      --  Transfer complete interrupt enable
      TCIE           : S3CR_TCIE_Field := 16#0#;
      --  Peripheral flow controller
      PFCTRL         : S3CR_PFCTRL_Field := 16#0#;
      --  Data transfer direction
      DIR            : S3CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : S3CR_CIRC_Field := 16#0#;
      --  Peripheral increment mode
      PINC           : S3CR_PINC_Field := 16#0#;
      --  Memory increment mode
      MINC           : S3CR_MINC_Field := 16#0#;
      --  Peripheral data size
      PSIZE          : S3CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S3CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : S3CR_PINCOS_Field := 16#0#;
      --  Priority level
      PL             : S3CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : S3CR_DBM_Field := 16#0#;
      --  Current target (only in double buffer mode)
      CT             : S3CR_CT_Field := 16#0#;
      --  ACK
      ACK            : S3CR_ACK_Field := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : S3CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S3CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S3CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   ---------------------
   -- S3NDTR_Register --
   ---------------------

   subtype S3NDTR_NDT_Field is STM32_SVD.Short;

   --  stream x number of data register
   type S3NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S3NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S3NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- S3FCR_Register --
   --------------------

   subtype S3FCR_FTH_Field is STM32_SVD.UInt2;
   subtype S3FCR_DMDIS_Field is STM32_SVD.Bit;
   subtype S3FCR_FS_Field is STM32_SVD.UInt3;
   subtype S3FCR_FEIE_Field is STM32_SVD.Bit;

   --  stream x FIFO control register
   type S3FCR_Register is record
      --  FIFO threshold selection
      FTH           : S3FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : S3FCR_DMDIS_Field := 16#0#;
      --  FIFO status
      FS            : S3FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : STM32_SVD.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : S3FCR_FEIE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   -------------------
   -- S4CR_Register --
   -------------------

   subtype S4CR_EN_Field is STM32_SVD.Bit;
   subtype S4CR_DMEIE_Field is STM32_SVD.Bit;
   subtype S4CR_TEIE_Field is STM32_SVD.Bit;
   subtype S4CR_HTIE_Field is STM32_SVD.Bit;
   subtype S4CR_TCIE_Field is STM32_SVD.Bit;
   subtype S4CR_PFCTRL_Field is STM32_SVD.Bit;
   subtype S4CR_DIR_Field is STM32_SVD.UInt2;
   subtype S4CR_CIRC_Field is STM32_SVD.Bit;
   subtype S4CR_PINC_Field is STM32_SVD.Bit;
   subtype S4CR_MINC_Field is STM32_SVD.Bit;
   subtype S4CR_PSIZE_Field is STM32_SVD.UInt2;
   subtype S4CR_MSIZE_Field is STM32_SVD.UInt2;
   subtype S4CR_PINCOS_Field is STM32_SVD.Bit;
   subtype S4CR_PL_Field is STM32_SVD.UInt2;
   subtype S4CR_DBM_Field is STM32_SVD.Bit;
   subtype S4CR_CT_Field is STM32_SVD.Bit;
   subtype S4CR_ACK_Field is STM32_SVD.Bit;
   subtype S4CR_PBURST_Field is STM32_SVD.UInt2;
   subtype S4CR_MBURST_Field is STM32_SVD.UInt2;
   subtype S4CR_CHSEL_Field is STM32_SVD.UInt3;

   --  stream x configuration register
   type S4CR_Register is record
      --  Stream enable / flag stream ready when read low
      EN             : S4CR_EN_Field := 16#0#;
      --  Direct mode error interrupt enable
      DMEIE          : S4CR_DMEIE_Field := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : S4CR_TEIE_Field := 16#0#;
      --  Half transfer interrupt enable
      HTIE           : S4CR_HTIE_Field := 16#0#;
      --  Transfer complete interrupt enable
      TCIE           : S4CR_TCIE_Field := 16#0#;
      --  Peripheral flow controller
      PFCTRL         : S4CR_PFCTRL_Field := 16#0#;
      --  Data transfer direction
      DIR            : S4CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : S4CR_CIRC_Field := 16#0#;
      --  Peripheral increment mode
      PINC           : S4CR_PINC_Field := 16#0#;
      --  Memory increment mode
      MINC           : S4CR_MINC_Field := 16#0#;
      --  Peripheral data size
      PSIZE          : S4CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S4CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : S4CR_PINCOS_Field := 16#0#;
      --  Priority level
      PL             : S4CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : S4CR_DBM_Field := 16#0#;
      --  Current target (only in double buffer mode)
      CT             : S4CR_CT_Field := 16#0#;
      --  ACK
      ACK            : S4CR_ACK_Field := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : S4CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S4CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S4CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   ---------------------
   -- S4NDTR_Register --
   ---------------------

   subtype S4NDTR_NDT_Field is STM32_SVD.Short;

   --  stream x number of data register
   type S4NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S4NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S4NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- S4FCR_Register --
   --------------------

   subtype S4FCR_FTH_Field is STM32_SVD.UInt2;
   subtype S4FCR_DMDIS_Field is STM32_SVD.Bit;
   subtype S4FCR_FS_Field is STM32_SVD.UInt3;
   subtype S4FCR_FEIE_Field is STM32_SVD.Bit;

   --  stream x FIFO control register
   type S4FCR_Register is record
      --  FIFO threshold selection
      FTH           : S4FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : S4FCR_DMDIS_Field := 16#0#;
      --  FIFO status
      FS            : S4FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : STM32_SVD.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : S4FCR_FEIE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   -------------------
   -- S5CR_Register --
   -------------------

   subtype S5CR_EN_Field is STM32_SVD.Bit;
   subtype S5CR_DMEIE_Field is STM32_SVD.Bit;
   subtype S5CR_TEIE_Field is STM32_SVD.Bit;
   subtype S5CR_HTIE_Field is STM32_SVD.Bit;
   subtype S5CR_TCIE_Field is STM32_SVD.Bit;
   subtype S5CR_PFCTRL_Field is STM32_SVD.Bit;
   subtype S5CR_DIR_Field is STM32_SVD.UInt2;
   subtype S5CR_CIRC_Field is STM32_SVD.Bit;
   subtype S5CR_PINC_Field is STM32_SVD.Bit;
   subtype S5CR_MINC_Field is STM32_SVD.Bit;
   subtype S5CR_PSIZE_Field is STM32_SVD.UInt2;
   subtype S5CR_MSIZE_Field is STM32_SVD.UInt2;
   subtype S5CR_PINCOS_Field is STM32_SVD.Bit;
   subtype S5CR_PL_Field is STM32_SVD.UInt2;
   subtype S5CR_DBM_Field is STM32_SVD.Bit;
   subtype S5CR_CT_Field is STM32_SVD.Bit;
   subtype S5CR_ACK_Field is STM32_SVD.Bit;
   subtype S5CR_PBURST_Field is STM32_SVD.UInt2;
   subtype S5CR_MBURST_Field is STM32_SVD.UInt2;
   subtype S5CR_CHSEL_Field is STM32_SVD.UInt3;

   --  stream x configuration register
   type S5CR_Register is record
      --  Stream enable / flag stream ready when read low
      EN             : S5CR_EN_Field := 16#0#;
      --  Direct mode error interrupt enable
      DMEIE          : S5CR_DMEIE_Field := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : S5CR_TEIE_Field := 16#0#;
      --  Half transfer interrupt enable
      HTIE           : S5CR_HTIE_Field := 16#0#;
      --  Transfer complete interrupt enable
      TCIE           : S5CR_TCIE_Field := 16#0#;
      --  Peripheral flow controller
      PFCTRL         : S5CR_PFCTRL_Field := 16#0#;
      --  Data transfer direction
      DIR            : S5CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : S5CR_CIRC_Field := 16#0#;
      --  Peripheral increment mode
      PINC           : S5CR_PINC_Field := 16#0#;
      --  Memory increment mode
      MINC           : S5CR_MINC_Field := 16#0#;
      --  Peripheral data size
      PSIZE          : S5CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S5CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : S5CR_PINCOS_Field := 16#0#;
      --  Priority level
      PL             : S5CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : S5CR_DBM_Field := 16#0#;
      --  Current target (only in double buffer mode)
      CT             : S5CR_CT_Field := 16#0#;
      --  ACK
      ACK            : S5CR_ACK_Field := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : S5CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S5CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S5CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   ---------------------
   -- S5NDTR_Register --
   ---------------------

   subtype S5NDTR_NDT_Field is STM32_SVD.Short;

   --  stream x number of data register
   type S5NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S5NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S5NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- S5FCR_Register --
   --------------------

   subtype S5FCR_FTH_Field is STM32_SVD.UInt2;
   subtype S5FCR_DMDIS_Field is STM32_SVD.Bit;
   subtype S5FCR_FS_Field is STM32_SVD.UInt3;
   subtype S5FCR_FEIE_Field is STM32_SVD.Bit;

   --  stream x FIFO control register
   type S5FCR_Register is record
      --  FIFO threshold selection
      FTH           : S5FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : S5FCR_DMDIS_Field := 16#0#;
      --  FIFO status
      FS            : S5FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : STM32_SVD.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : S5FCR_FEIE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   -------------------
   -- S6CR_Register --
   -------------------

   subtype S6CR_EN_Field is STM32_SVD.Bit;
   subtype S6CR_DMEIE_Field is STM32_SVD.Bit;
   subtype S6CR_TEIE_Field is STM32_SVD.Bit;
   subtype S6CR_HTIE_Field is STM32_SVD.Bit;
   subtype S6CR_TCIE_Field is STM32_SVD.Bit;
   subtype S6CR_PFCTRL_Field is STM32_SVD.Bit;
   subtype S6CR_DIR_Field is STM32_SVD.UInt2;
   subtype S6CR_CIRC_Field is STM32_SVD.Bit;
   subtype S6CR_PINC_Field is STM32_SVD.Bit;
   subtype S6CR_MINC_Field is STM32_SVD.Bit;
   subtype S6CR_PSIZE_Field is STM32_SVD.UInt2;
   subtype S6CR_MSIZE_Field is STM32_SVD.UInt2;
   subtype S6CR_PINCOS_Field is STM32_SVD.Bit;
   subtype S6CR_PL_Field is STM32_SVD.UInt2;
   subtype S6CR_DBM_Field is STM32_SVD.Bit;
   subtype S6CR_CT_Field is STM32_SVD.Bit;
   subtype S6CR_ACK_Field is STM32_SVD.Bit;
   subtype S6CR_PBURST_Field is STM32_SVD.UInt2;
   subtype S6CR_MBURST_Field is STM32_SVD.UInt2;
   subtype S6CR_CHSEL_Field is STM32_SVD.UInt3;

   --  stream x configuration register
   type S6CR_Register is record
      --  Stream enable / flag stream ready when read low
      EN             : S6CR_EN_Field := 16#0#;
      --  Direct mode error interrupt enable
      DMEIE          : S6CR_DMEIE_Field := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : S6CR_TEIE_Field := 16#0#;
      --  Half transfer interrupt enable
      HTIE           : S6CR_HTIE_Field := 16#0#;
      --  Transfer complete interrupt enable
      TCIE           : S6CR_TCIE_Field := 16#0#;
      --  Peripheral flow controller
      PFCTRL         : S6CR_PFCTRL_Field := 16#0#;
      --  Data transfer direction
      DIR            : S6CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : S6CR_CIRC_Field := 16#0#;
      --  Peripheral increment mode
      PINC           : S6CR_PINC_Field := 16#0#;
      --  Memory increment mode
      MINC           : S6CR_MINC_Field := 16#0#;
      --  Peripheral data size
      PSIZE          : S6CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S6CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : S6CR_PINCOS_Field := 16#0#;
      --  Priority level
      PL             : S6CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : S6CR_DBM_Field := 16#0#;
      --  Current target (only in double buffer mode)
      CT             : S6CR_CT_Field := 16#0#;
      --  ACK
      ACK            : S6CR_ACK_Field := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : S6CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S6CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S6CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   ---------------------
   -- S6NDTR_Register --
   ---------------------

   subtype S6NDTR_NDT_Field is STM32_SVD.Short;

   --  stream x number of data register
   type S6NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S6NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S6NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- S6FCR_Register --
   --------------------

   subtype S6FCR_FTH_Field is STM32_SVD.UInt2;
   subtype S6FCR_DMDIS_Field is STM32_SVD.Bit;
   subtype S6FCR_FS_Field is STM32_SVD.UInt3;
   subtype S6FCR_FEIE_Field is STM32_SVD.Bit;

   --  stream x FIFO control register
   type S6FCR_Register is record
      --  FIFO threshold selection
      FTH           : S6FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : S6FCR_DMDIS_Field := 16#0#;
      --  FIFO status
      FS            : S6FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : STM32_SVD.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : S6FCR_FEIE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   -------------------
   -- S7CR_Register --
   -------------------

   subtype S7CR_EN_Field is STM32_SVD.Bit;
   subtype S7CR_DMEIE_Field is STM32_SVD.Bit;
   subtype S7CR_TEIE_Field is STM32_SVD.Bit;
   subtype S7CR_HTIE_Field is STM32_SVD.Bit;
   subtype S7CR_TCIE_Field is STM32_SVD.Bit;
   subtype S7CR_PFCTRL_Field is STM32_SVD.Bit;
   subtype S7CR_DIR_Field is STM32_SVD.UInt2;
   subtype S7CR_CIRC_Field is STM32_SVD.Bit;
   subtype S7CR_PINC_Field is STM32_SVD.Bit;
   subtype S7CR_MINC_Field is STM32_SVD.Bit;
   subtype S7CR_PSIZE_Field is STM32_SVD.UInt2;
   subtype S7CR_MSIZE_Field is STM32_SVD.UInt2;
   subtype S7CR_PINCOS_Field is STM32_SVD.Bit;
   subtype S7CR_PL_Field is STM32_SVD.UInt2;
   subtype S7CR_DBM_Field is STM32_SVD.Bit;
   subtype S7CR_CT_Field is STM32_SVD.Bit;
   subtype S7CR_ACK_Field is STM32_SVD.Bit;
   subtype S7CR_PBURST_Field is STM32_SVD.UInt2;
   subtype S7CR_MBURST_Field is STM32_SVD.UInt2;
   subtype S7CR_CHSEL_Field is STM32_SVD.UInt3;

   --  stream x configuration register
   type S7CR_Register is record
      --  Stream enable / flag stream ready when read low
      EN             : S7CR_EN_Field := 16#0#;
      --  Direct mode error interrupt enable
      DMEIE          : S7CR_DMEIE_Field := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : S7CR_TEIE_Field := 16#0#;
      --  Half transfer interrupt enable
      HTIE           : S7CR_HTIE_Field := 16#0#;
      --  Transfer complete interrupt enable
      TCIE           : S7CR_TCIE_Field := 16#0#;
      --  Peripheral flow controller
      PFCTRL         : S7CR_PFCTRL_Field := 16#0#;
      --  Data transfer direction
      DIR            : S7CR_DIR_Field := 16#0#;
      --  Circular mode
      CIRC           : S7CR_CIRC_Field := 16#0#;
      --  Peripheral increment mode
      PINC           : S7CR_PINC_Field := 16#0#;
      --  Memory increment mode
      MINC           : S7CR_MINC_Field := 16#0#;
      --  Peripheral data size
      PSIZE          : S7CR_PSIZE_Field := 16#0#;
      --  Memory data size
      MSIZE          : S7CR_MSIZE_Field := 16#0#;
      --  Peripheral increment offset size
      PINCOS         : S7CR_PINCOS_Field := 16#0#;
      --  Priority level
      PL             : S7CR_PL_Field := 16#0#;
      --  Double buffer mode
      DBM            : S7CR_DBM_Field := 16#0#;
      --  Current target (only in double buffer mode)
      CT             : S7CR_CT_Field := 16#0#;
      --  ACK
      ACK            : S7CR_ACK_Field := 16#0#;
      --  Peripheral burst transfer configuration
      PBURST         : S7CR_PBURST_Field := 16#0#;
      --  Memory burst transfer configuration
      MBURST         : S7CR_MBURST_Field := 16#0#;
      --  Channel selection
      CHSEL          : S7CR_CHSEL_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
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

   ---------------------
   -- S7NDTR_Register --
   ---------------------

   subtype S7NDTR_NDT_Field is STM32_SVD.Short;

   --  stream x number of data register
   type S7NDTR_Register is record
      --  Number of data items to transfer
      NDT            : S7NDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for S7NDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- S7FCR_Register --
   --------------------

   subtype S7FCR_FTH_Field is STM32_SVD.UInt2;
   subtype S7FCR_DMDIS_Field is STM32_SVD.Bit;
   subtype S7FCR_FS_Field is STM32_SVD.UInt3;
   subtype S7FCR_FEIE_Field is STM32_SVD.Bit;

   --  stream x FIFO control register
   type S7FCR_Register is record
      --  FIFO threshold selection
      FTH           : S7FCR_FTH_Field := 16#1#;
      --  Direct mode disable
      DMDIS         : S7FCR_DMDIS_Field := 16#0#;
      --  FIFO status
      FS            : S7FCR_FS_Field := 16#4#;
      --  unspecified
      Reserved_6_6  : STM32_SVD.Bit := 16#0#;
      --  FIFO error interrupt enable
      FEIE          : S7FCR_FEIE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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
      LISR   : LISR_Register;
      --  high interrupt status register
      HISR   : HISR_Register;
      --  low interrupt flag clear register
      LIFCR  : LIFCR_Register;
      --  high interrupt flag clear register
      HIFCR  : HIFCR_Register;
      --  stream x configuration register
      S0CR   : S0CR_Register;
      --  stream x number of data register
      S0NDTR : S0NDTR_Register;
      --  stream x peripheral address register
      S0PAR  : STM32_SVD.Word;
      --  stream x memory 0 address register
      S0M0AR : STM32_SVD.Word;
      --  stream x memory 1 address register
      S0M1AR : STM32_SVD.Word;
      --  stream x FIFO control register
      S0FCR  : S0FCR_Register;
      --  stream x configuration register
      S1CR   : S1CR_Register;
      --  stream x number of data register
      S1NDTR : S1NDTR_Register;
      --  stream x peripheral address register
      S1PAR  : STM32_SVD.Word;
      --  stream x memory 0 address register
      S1M0AR : STM32_SVD.Word;
      --  stream x memory 1 address register
      S1M1AR : STM32_SVD.Word;
      --  stream x FIFO control register
      S1FCR  : S1FCR_Register;
      --  stream x configuration register
      S2CR   : S2CR_Register;
      --  stream x number of data register
      S2NDTR : S2NDTR_Register;
      --  stream x peripheral address register
      S2PAR  : STM32_SVD.Word;
      --  stream x memory 0 address register
      S2M0AR : STM32_SVD.Word;
      --  stream x memory 1 address register
      S2M1AR : STM32_SVD.Word;
      --  stream x FIFO control register
      S2FCR  : S2FCR_Register;
      --  stream x configuration register
      S3CR   : S3CR_Register;
      --  stream x number of data register
      S3NDTR : S3NDTR_Register;
      --  stream x peripheral address register
      S3PAR  : STM32_SVD.Word;
      --  stream x memory 0 address register
      S3M0AR : STM32_SVD.Word;
      --  stream x memory 1 address register
      S3M1AR : STM32_SVD.Word;
      --  stream x FIFO control register
      S3FCR  : S3FCR_Register;
      --  stream x configuration register
      S4CR   : S4CR_Register;
      --  stream x number of data register
      S4NDTR : S4NDTR_Register;
      --  stream x peripheral address register
      S4PAR  : STM32_SVD.Word;
      --  stream x memory 0 address register
      S4M0AR : STM32_SVD.Word;
      --  stream x memory 1 address register
      S4M1AR : STM32_SVD.Word;
      --  stream x FIFO control register
      S4FCR  : S4FCR_Register;
      --  stream x configuration register
      S5CR   : S5CR_Register;
      --  stream x number of data register
      S5NDTR : S5NDTR_Register;
      --  stream x peripheral address register
      S5PAR  : STM32_SVD.Word;
      --  stream x memory 0 address register
      S5M0AR : STM32_SVD.Word;
      --  stream x memory 1 address register
      S5M1AR : STM32_SVD.Word;
      --  stream x FIFO control register
      S5FCR  : S5FCR_Register;
      --  stream x configuration register
      S6CR   : S6CR_Register;
      --  stream x number of data register
      S6NDTR : S6NDTR_Register;
      --  stream x peripheral address register
      S6PAR  : STM32_SVD.Word;
      --  stream x memory 0 address register
      S6M0AR : STM32_SVD.Word;
      --  stream x memory 1 address register
      S6M1AR : STM32_SVD.Word;
      --  stream x FIFO control register
      S6FCR  : S6FCR_Register;
      --  stream x configuration register
      S7CR   : S7CR_Register;
      --  stream x number of data register
      S7NDTR : S7NDTR_Register;
      --  stream x peripheral address register
      S7PAR  : STM32_SVD.Word;
      --  stream x memory 0 address register
      S7M0AR : STM32_SVD.Word;
      --  stream x memory 1 address register
      S7M1AR : STM32_SVD.Word;
      --  stream x FIFO control register
      S7FCR  : S7FCR_Register;
   end record
     with Volatile;

   for DMA_Peripheral use record
      LISR   at 0 range 0 .. 31;
      HISR   at 4 range 0 .. 31;
      LIFCR  at 8 range 0 .. 31;
      HIFCR  at 12 range 0 .. 31;
      S0CR   at 16 range 0 .. 31;
      S0NDTR at 20 range 0 .. 31;
      S0PAR  at 24 range 0 .. 31;
      S0M0AR at 28 range 0 .. 31;
      S0M1AR at 32 range 0 .. 31;
      S0FCR  at 36 range 0 .. 31;
      S1CR   at 40 range 0 .. 31;
      S1NDTR at 44 range 0 .. 31;
      S1PAR  at 48 range 0 .. 31;
      S1M0AR at 52 range 0 .. 31;
      S1M1AR at 56 range 0 .. 31;
      S1FCR  at 60 range 0 .. 31;
      S2CR   at 64 range 0 .. 31;
      S2NDTR at 68 range 0 .. 31;
      S2PAR  at 72 range 0 .. 31;
      S2M0AR at 76 range 0 .. 31;
      S2M1AR at 80 range 0 .. 31;
      S2FCR  at 84 range 0 .. 31;
      S3CR   at 88 range 0 .. 31;
      S3NDTR at 92 range 0 .. 31;
      S3PAR  at 96 range 0 .. 31;
      S3M0AR at 100 range 0 .. 31;
      S3M1AR at 104 range 0 .. 31;
      S3FCR  at 108 range 0 .. 31;
      S4CR   at 112 range 0 .. 31;
      S4NDTR at 116 range 0 .. 31;
      S4PAR  at 120 range 0 .. 31;
      S4M0AR at 124 range 0 .. 31;
      S4M1AR at 128 range 0 .. 31;
      S4FCR  at 132 range 0 .. 31;
      S5CR   at 136 range 0 .. 31;
      S5NDTR at 140 range 0 .. 31;
      S5PAR  at 144 range 0 .. 31;
      S5M0AR at 148 range 0 .. 31;
      S5M1AR at 152 range 0 .. 31;
      S5FCR  at 156 range 0 .. 31;
      S6CR   at 160 range 0 .. 31;
      S6NDTR at 164 range 0 .. 31;
      S6PAR  at 168 range 0 .. 31;
      S6M0AR at 172 range 0 .. 31;
      S6M1AR at 176 range 0 .. 31;
      S6FCR  at 180 range 0 .. 31;
      S7CR   at 184 range 0 .. 31;
      S7NDTR at 188 range 0 .. 31;
      S7PAR  at 192 range 0 .. 31;
      S7M0AR at 196 range 0 .. 31;
      S7M1AR at 200 range 0 .. 31;
      S7FCR  at 204 range 0 .. 31;
   end record;

   --  DMA controller
   DMA1_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#40026000#);

   --  DMA controller
   DMA2_Periph : aliased DMA_Peripheral
     with Import, Address => System'To_Address (16#40026400#);

end STM32_SVD.DMA;
