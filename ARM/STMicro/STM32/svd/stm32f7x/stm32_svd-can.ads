--  Automatically generated from STM32F7x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.CAN is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ------------------
   -- MCR_Register --
   ------------------

   subtype MCR_INRQ_Field is STM32_SVD.Bit;
   subtype MCR_SLEEP_Field is STM32_SVD.Bit;
   subtype MCR_TXFP_Field is STM32_SVD.Bit;
   subtype MCR_RFLM_Field is STM32_SVD.Bit;
   subtype MCR_NART_Field is STM32_SVD.Bit;
   subtype MCR_AWUM_Field is STM32_SVD.Bit;
   subtype MCR_ABOM_Field is STM32_SVD.Bit;
   subtype MCR_TTCM_Field is STM32_SVD.Bit;
   subtype MCR_RESET_Field is STM32_SVD.Bit;
   subtype MCR_DBF_Field is STM32_SVD.Bit;

   --  master control register
   type MCR_Register is record
      --  INRQ
      INRQ           : MCR_INRQ_Field := 16#0#;
      --  SLEEP
      SLEEP          : MCR_SLEEP_Field := 16#1#;
      --  TXFP
      TXFP           : MCR_TXFP_Field := 16#0#;
      --  RFLM
      RFLM           : MCR_RFLM_Field := 16#0#;
      --  NART
      NART           : MCR_NART_Field := 16#0#;
      --  AWUM
      AWUM           : MCR_AWUM_Field := 16#0#;
      --  ABOM
      ABOM           : MCR_ABOM_Field := 16#0#;
      --  TTCM
      TTCM           : MCR_TTCM_Field := 16#0#;
      --  unspecified
      Reserved_8_14  : STM32_SVD.UInt7 := 16#0#;
      --  RESET
      RESET          : MCR_RESET_Field := 16#0#;
      --  DBF
      DBF            : MCR_DBF_Field := 16#1#;
      --  unspecified
      Reserved_17_31 : STM32_SVD.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MCR_Register use record
      INRQ           at 0 range 0 .. 0;
      SLEEP          at 0 range 1 .. 1;
      TXFP           at 0 range 2 .. 2;
      RFLM           at 0 range 3 .. 3;
      NART           at 0 range 4 .. 4;
      AWUM           at 0 range 5 .. 5;
      ABOM           at 0 range 6 .. 6;
      TTCM           at 0 range 7 .. 7;
      Reserved_8_14  at 0 range 8 .. 14;
      RESET          at 0 range 15 .. 15;
      DBF            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   ------------------
   -- MSR_Register --
   ------------------

   subtype MSR_INAK_Field is STM32_SVD.Bit;
   subtype MSR_SLAK_Field is STM32_SVD.Bit;
   subtype MSR_ERRI_Field is STM32_SVD.Bit;
   subtype MSR_WKUI_Field is STM32_SVD.Bit;
   subtype MSR_SLAKI_Field is STM32_SVD.Bit;
   subtype MSR_TXM_Field is STM32_SVD.Bit;
   subtype MSR_RXM_Field is STM32_SVD.Bit;
   subtype MSR_SAMP_Field is STM32_SVD.Bit;
   subtype MSR_RX_Field is STM32_SVD.Bit;

   --  master status register
   type MSR_Register is record
      --  INAK
      INAK           : MSR_INAK_Field := 16#0#;
      --  SLAK
      SLAK           : MSR_SLAK_Field := 16#1#;
      --  ERRI
      ERRI           : MSR_ERRI_Field := 16#0#;
      --  WKUI
      WKUI           : MSR_WKUI_Field := 16#0#;
      --  SLAKI
      SLAKI          : MSR_SLAKI_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32_SVD.UInt3 := 16#0#;
      --  TXM
      TXM            : MSR_TXM_Field := 16#0#;
      --  RXM
      RXM            : MSR_RXM_Field := 16#0#;
      --  SAMP
      SAMP           : MSR_SAMP_Field := 16#1#;
      --  RX
      RX             : MSR_RX_Field := 16#1#;
      --  unspecified
      Reserved_12_31 : STM32_SVD.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MSR_Register use record
      INAK           at 0 range 0 .. 0;
      SLAK           at 0 range 1 .. 1;
      ERRI           at 0 range 2 .. 2;
      WKUI           at 0 range 3 .. 3;
      SLAKI          at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TXM            at 0 range 8 .. 8;
      RXM            at 0 range 9 .. 9;
      SAMP           at 0 range 10 .. 10;
      RX             at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   ------------------
   -- TSR_Register --
   ------------------

   subtype TSR_RQCP0_Field is STM32_SVD.Bit;
   subtype TSR_TXOK0_Field is STM32_SVD.Bit;
   subtype TSR_ALST0_Field is STM32_SVD.Bit;
   subtype TSR_TERR0_Field is STM32_SVD.Bit;
   subtype TSR_ABRQ0_Field is STM32_SVD.Bit;
   subtype TSR_RQCP1_Field is STM32_SVD.Bit;
   subtype TSR_TXOK1_Field is STM32_SVD.Bit;
   subtype TSR_ALST1_Field is STM32_SVD.Bit;
   subtype TSR_TERR1_Field is STM32_SVD.Bit;
   subtype TSR_ABRQ1_Field is STM32_SVD.Bit;
   subtype TSR_RQCP2_Field is STM32_SVD.Bit;
   subtype TSR_TXOK2_Field is STM32_SVD.Bit;
   subtype TSR_ALST2_Field is STM32_SVD.Bit;
   subtype TSR_TERR2_Field is STM32_SVD.Bit;
   subtype TSR_ABRQ2_Field is STM32_SVD.Bit;
   subtype TSR_CODE_Field is STM32_SVD.UInt2;

   -------------
   -- TSR.TME --
   -------------

   --  TSR_TME array element
   subtype TSR_TME_Element is STM32_SVD.Bit;

   --  TSR_TME array
   type TSR_TME_Field_Array is array (0 .. 2) of TSR_TME_Element
     with Component_Size => 1, Size => 3;

   --  Type definition for TSR_TME
   type TSR_TME_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TME as a value
            Val : STM32_SVD.UInt3;
         when True =>
            --  TME as an array
            Arr : TSR_TME_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TSR_TME_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   -------------
   -- TSR.LOW --
   -------------

   --  TSR_LOW array element
   subtype TSR_LOW_Element is STM32_SVD.Bit;

   --  TSR_LOW array
   type TSR_LOW_Field_Array is array (0 .. 2) of TSR_LOW_Element
     with Component_Size => 1, Size => 3;

   --  Type definition for TSR_LOW
   type TSR_LOW_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LOW as a value
            Val : STM32_SVD.UInt3;
         when True =>
            --  LOW as an array
            Arr : TSR_LOW_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for TSR_LOW_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  transmit status register
   type TSR_Register is record
      --  RQCP0
      RQCP0          : TSR_RQCP0_Field := 16#0#;
      --  TXOK0
      TXOK0          : TSR_TXOK0_Field := 16#0#;
      --  ALST0
      ALST0          : TSR_ALST0_Field := 16#0#;
      --  TERR0
      TERR0          : TSR_TERR0_Field := 16#0#;
      --  unspecified
      Reserved_4_6   : STM32_SVD.UInt3 := 16#0#;
      --  ABRQ0
      ABRQ0          : TSR_ABRQ0_Field := 16#0#;
      --  RQCP1
      RQCP1          : TSR_RQCP1_Field := 16#0#;
      --  TXOK1
      TXOK1          : TSR_TXOK1_Field := 16#0#;
      --  ALST1
      ALST1          : TSR_ALST1_Field := 16#0#;
      --  TERR1
      TERR1          : TSR_TERR1_Field := 16#0#;
      --  unspecified
      Reserved_12_14 : STM32_SVD.UInt3 := 16#0#;
      --  ABRQ1
      ABRQ1          : TSR_ABRQ1_Field := 16#0#;
      --  RQCP2
      RQCP2          : TSR_RQCP2_Field := 16#0#;
      --  TXOK2
      TXOK2          : TSR_TXOK2_Field := 16#0#;
      --  ALST2
      ALST2          : TSR_ALST2_Field := 16#0#;
      --  TERR2
      TERR2          : TSR_TERR2_Field := 16#0#;
      --  unspecified
      Reserved_20_22 : STM32_SVD.UInt3 := 16#0#;
      --  ABRQ2
      ABRQ2          : TSR_ABRQ2_Field := 16#0#;
      --  CODE
      CODE           : TSR_CODE_Field := 16#0#;
      --  Lowest priority flag for mailbox 0
      TME            : TSR_TME_Field := (As_Array => False, Val => 16#1#);
      --  Lowest priority flag for mailbox 0
      LOW            : TSR_LOW_Field := (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TSR_Register use record
      RQCP0          at 0 range 0 .. 0;
      TXOK0          at 0 range 1 .. 1;
      ALST0          at 0 range 2 .. 2;
      TERR0          at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      ABRQ0          at 0 range 7 .. 7;
      RQCP1          at 0 range 8 .. 8;
      TXOK1          at 0 range 9 .. 9;
      ALST1          at 0 range 10 .. 10;
      TERR1          at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      ABRQ1          at 0 range 15 .. 15;
      RQCP2          at 0 range 16 .. 16;
      TXOK2          at 0 range 17 .. 17;
      ALST2          at 0 range 18 .. 18;
      TERR2          at 0 range 19 .. 19;
      Reserved_20_22 at 0 range 20 .. 22;
      ABRQ2          at 0 range 23 .. 23;
      CODE           at 0 range 24 .. 25;
      TME            at 0 range 26 .. 28;
      LOW            at 0 range 29 .. 31;
   end record;

   -------------------
   -- RF0R_Register --
   -------------------

   subtype RF0R_FMP0_Field is STM32_SVD.UInt2;
   subtype RF0R_FULL0_Field is STM32_SVD.Bit;
   subtype RF0R_FOVR0_Field is STM32_SVD.Bit;
   subtype RF0R_RFOM0_Field is STM32_SVD.Bit;

   --  receive FIFO 0 register
   type RF0R_Register is record
      --  FMP0
      FMP0          : RF0R_FMP0_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : STM32_SVD.Bit := 16#0#;
      --  FULL0
      FULL0         : RF0R_FULL0_Field := 16#0#;
      --  FOVR0
      FOVR0         : RF0R_FOVR0_Field := 16#0#;
      --  RFOM0
      RFOM0         : RF0R_RFOM0_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32_SVD.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RF0R_Register use record
      FMP0          at 0 range 0 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      FULL0         at 0 range 3 .. 3;
      FOVR0         at 0 range 4 .. 4;
      RFOM0         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   -------------------
   -- RF1R_Register --
   -------------------

   subtype RF1R_FMP1_Field is STM32_SVD.UInt2;
   subtype RF1R_FULL1_Field is STM32_SVD.Bit;
   subtype RF1R_FOVR1_Field is STM32_SVD.Bit;
   subtype RF1R_RFOM1_Field is STM32_SVD.Bit;

   --  receive FIFO 1 register
   type RF1R_Register is record
      --  FMP1
      FMP1          : RF1R_FMP1_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : STM32_SVD.Bit := 16#0#;
      --  FULL1
      FULL1         : RF1R_FULL1_Field := 16#0#;
      --  FOVR1
      FOVR1         : RF1R_FOVR1_Field := 16#0#;
      --  RFOM1
      RFOM1         : RF1R_RFOM1_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32_SVD.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RF1R_Register use record
      FMP1          at 0 range 0 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      FULL1         at 0 range 3 .. 3;
      FOVR1         at 0 range 4 .. 4;
      RFOM1         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   ------------------
   -- IER_Register --
   ------------------

   subtype IER_TMEIE_Field is STM32_SVD.Bit;
   subtype IER_FMPIE0_Field is STM32_SVD.Bit;
   subtype IER_FFIE0_Field is STM32_SVD.Bit;
   subtype IER_FOVIE0_Field is STM32_SVD.Bit;
   subtype IER_FMPIE1_Field is STM32_SVD.Bit;
   subtype IER_FFIE1_Field is STM32_SVD.Bit;
   subtype IER_FOVIE1_Field is STM32_SVD.Bit;
   subtype IER_EWGIE_Field is STM32_SVD.Bit;
   subtype IER_EPVIE_Field is STM32_SVD.Bit;
   subtype IER_BOFIE_Field is STM32_SVD.Bit;
   subtype IER_LECIE_Field is STM32_SVD.Bit;
   subtype IER_ERRIE_Field is STM32_SVD.Bit;
   subtype IER_WKUIE_Field is STM32_SVD.Bit;
   subtype IER_SLKIE_Field is STM32_SVD.Bit;

   --  interrupt enable register
   type IER_Register is record
      --  TMEIE
      TMEIE          : IER_TMEIE_Field := 16#0#;
      --  FMPIE0
      FMPIE0         : IER_FMPIE0_Field := 16#0#;
      --  FFIE0
      FFIE0          : IER_FFIE0_Field := 16#0#;
      --  FOVIE0
      FOVIE0         : IER_FOVIE0_Field := 16#0#;
      --  FMPIE1
      FMPIE1         : IER_FMPIE1_Field := 16#0#;
      --  FFIE1
      FFIE1          : IER_FFIE1_Field := 16#0#;
      --  FOVIE1
      FOVIE1         : IER_FOVIE1_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32_SVD.Bit := 16#0#;
      --  EWGIE
      EWGIE          : IER_EWGIE_Field := 16#0#;
      --  EPVIE
      EPVIE          : IER_EPVIE_Field := 16#0#;
      --  BOFIE
      BOFIE          : IER_BOFIE_Field := 16#0#;
      --  LECIE
      LECIE          : IER_LECIE_Field := 16#0#;
      --  unspecified
      Reserved_12_14 : STM32_SVD.UInt3 := 16#0#;
      --  ERRIE
      ERRIE          : IER_ERRIE_Field := 16#0#;
      --  WKUIE
      WKUIE          : IER_WKUIE_Field := 16#0#;
      --  SLKIE
      SLKIE          : IER_SLKIE_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : STM32_SVD.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      TMEIE          at 0 range 0 .. 0;
      FMPIE0         at 0 range 1 .. 1;
      FFIE0          at 0 range 2 .. 2;
      FOVIE0         at 0 range 3 .. 3;
      FMPIE1         at 0 range 4 .. 4;
      FFIE1          at 0 range 5 .. 5;
      FOVIE1         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EWGIE          at 0 range 8 .. 8;
      EPVIE          at 0 range 9 .. 9;
      BOFIE          at 0 range 10 .. 10;
      LECIE          at 0 range 11 .. 11;
      Reserved_12_14 at 0 range 12 .. 14;
      ERRIE          at 0 range 15 .. 15;
      WKUIE          at 0 range 16 .. 16;
      SLKIE          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   ------------------
   -- ESR_Register --
   ------------------

   subtype ESR_EWGF_Field is STM32_SVD.Bit;
   subtype ESR_EPVF_Field is STM32_SVD.Bit;
   subtype ESR_BOFF_Field is STM32_SVD.Bit;
   subtype ESR_LEC_Field is STM32_SVD.UInt3;
   subtype ESR_TEC_Field is STM32_SVD.Byte;
   subtype ESR_REC_Field is STM32_SVD.Byte;

   --  interrupt enable register
   type ESR_Register is record
      --  EWGF
      EWGF          : ESR_EWGF_Field := 16#0#;
      --  EPVF
      EPVF          : ESR_EPVF_Field := 16#0#;
      --  BOFF
      BOFF          : ESR_BOFF_Field := 16#0#;
      --  unspecified
      Reserved_3_3  : STM32_SVD.Bit := 16#0#;
      --  LEC
      LEC           : ESR_LEC_Field := 16#0#;
      --  unspecified
      Reserved_7_15 : STM32_SVD.UInt9 := 16#0#;
      --  TEC
      TEC           : ESR_TEC_Field := 16#0#;
      --  REC
      REC           : ESR_REC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ESR_Register use record
      EWGF          at 0 range 0 .. 0;
      EPVF          at 0 range 1 .. 1;
      BOFF          at 0 range 2 .. 2;
      Reserved_3_3  at 0 range 3 .. 3;
      LEC           at 0 range 4 .. 6;
      Reserved_7_15 at 0 range 7 .. 15;
      TEC           at 0 range 16 .. 23;
      REC           at 0 range 24 .. 31;
   end record;

   ------------------
   -- BTR_Register --
   ------------------

   subtype BTR_BRP_Field is STM32_SVD.UInt10;
   subtype BTR_TS1_Field is STM32_SVD.UInt4;
   subtype BTR_TS2_Field is STM32_SVD.UInt3;
   subtype BTR_SJW_Field is STM32_SVD.UInt2;
   subtype BTR_LBKM_Field is STM32_SVD.Bit;
   subtype BTR_SILM_Field is STM32_SVD.Bit;

   --  bit timing register
   type BTR_Register is record
      --  BRP
      BRP            : BTR_BRP_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : STM32_SVD.UInt6 := 16#0#;
      --  TS1
      TS1            : BTR_TS1_Field := 16#0#;
      --  TS2
      TS2            : BTR_TS2_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32_SVD.Bit := 16#0#;
      --  SJW
      SJW            : BTR_SJW_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : STM32_SVD.UInt4 := 16#0#;
      --  LBKM
      LBKM           : BTR_LBKM_Field := 16#0#;
      --  SILM
      SILM           : BTR_SILM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BTR_Register use record
      BRP            at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      TS1            at 0 range 16 .. 19;
      TS2            at 0 range 20 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      SJW            at 0 range 24 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      LBKM           at 0 range 30 .. 30;
      SILM           at 0 range 31 .. 31;
   end record;

   -------------------
   -- TI0R_Register --
   -------------------

   subtype TI0R_TXRQ_Field is STM32_SVD.Bit;
   subtype TI0R_RTR_Field is STM32_SVD.Bit;
   subtype TI0R_IDE_Field is STM32_SVD.Bit;
   subtype TI0R_EXID_Field is STM32_SVD.UInt18;
   subtype TI0R_STID_Field is STM32_SVD.UInt11;

   --  TX mailbox identifier register
   type TI0R_Register is record
      --  TXRQ
      TXRQ : TI0R_TXRQ_Field := 16#0#;
      --  RTR
      RTR  : TI0R_RTR_Field := 16#0#;
      --  IDE
      IDE  : TI0R_IDE_Field := 16#0#;
      --  EXID
      EXID : TI0R_EXID_Field := 16#0#;
      --  STID
      STID : TI0R_STID_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TI0R_Register use record
      TXRQ at 0 range 0 .. 0;
      RTR  at 0 range 1 .. 1;
      IDE  at 0 range 2 .. 2;
      EXID at 0 range 3 .. 20;
      STID at 0 range 21 .. 31;
   end record;

   --------------------
   -- TDT0R_Register --
   --------------------

   subtype TDT0R_DLC_Field is STM32_SVD.UInt4;
   subtype TDT0R_TGT_Field is STM32_SVD.Bit;
   subtype TDT0R_TIME_Field is STM32_SVD.Short;

   --  mailbox data length control and time stamp register
   type TDT0R_Register is record
      --  DLC
      DLC           : TDT0R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : STM32_SVD.UInt4 := 16#0#;
      --  TGT
      TGT           : TDT0R_TGT_Field := 16#0#;
      --  unspecified
      Reserved_9_15 : STM32_SVD.UInt7 := 16#0#;
      --  TIME
      TIME          : TDT0R_TIME_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TDT0R_Register use record
      DLC           at 0 range 0 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      TGT           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      TIME          at 0 range 16 .. 31;
   end record;

   --------------------
   -- TDL0R_Register --
   --------------------

   --  TDL0R_DATA array element
   subtype TDL0R_DATA_Element is STM32_SVD.Byte;

   --  TDL0R_DATA array
   type TDL0R_DATA_Field_Array is array (0 .. 3) of TDL0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data low register
   type TDL0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : TDL0R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for TDL0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --------------------
   -- TDH0R_Register --
   --------------------

   --  TDH0R_DATA array element
   subtype TDH0R_DATA_Element is STM32_SVD.Byte;

   --  TDH0R_DATA array
   type TDH0R_DATA_Field_Array is array (0 .. 3) of TDH0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type TDH0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : TDH0R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for TDH0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- TI1R_Register --
   -------------------

   subtype TI1R_TXRQ_Field is STM32_SVD.Bit;
   subtype TI1R_RTR_Field is STM32_SVD.Bit;
   subtype TI1R_IDE_Field is STM32_SVD.Bit;
   subtype TI1R_EXID_Field is STM32_SVD.UInt18;
   subtype TI1R_STID_Field is STM32_SVD.UInt11;

   --  mailbox identifier register
   type TI1R_Register is record
      --  TXRQ
      TXRQ : TI1R_TXRQ_Field := 16#0#;
      --  RTR
      RTR  : TI1R_RTR_Field := 16#0#;
      --  IDE
      IDE  : TI1R_IDE_Field := 16#0#;
      --  EXID
      EXID : TI1R_EXID_Field := 16#0#;
      --  STID
      STID : TI1R_STID_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TI1R_Register use record
      TXRQ at 0 range 0 .. 0;
      RTR  at 0 range 1 .. 1;
      IDE  at 0 range 2 .. 2;
      EXID at 0 range 3 .. 20;
      STID at 0 range 21 .. 31;
   end record;

   --------------------
   -- TDT1R_Register --
   --------------------

   subtype TDT1R_DLC_Field is STM32_SVD.UInt4;
   subtype TDT1R_TGT_Field is STM32_SVD.Bit;
   subtype TDT1R_TIME_Field is STM32_SVD.Short;

   --  mailbox data length control and time stamp register
   type TDT1R_Register is record
      --  DLC
      DLC           : TDT1R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : STM32_SVD.UInt4 := 16#0#;
      --  TGT
      TGT           : TDT1R_TGT_Field := 16#0#;
      --  unspecified
      Reserved_9_15 : STM32_SVD.UInt7 := 16#0#;
      --  TIME
      TIME          : TDT1R_TIME_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TDT1R_Register use record
      DLC           at 0 range 0 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      TGT           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      TIME          at 0 range 16 .. 31;
   end record;

   --------------------
   -- TDL1R_Register --
   --------------------

   --  TDL1R_DATA array element
   subtype TDL1R_DATA_Element is STM32_SVD.Byte;

   --  TDL1R_DATA array
   type TDL1R_DATA_Field_Array is array (0 .. 3) of TDL1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data low register
   type TDL1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : TDL1R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for TDL1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --------------------
   -- TDH1R_Register --
   --------------------

   --  TDH1R_DATA array element
   subtype TDH1R_DATA_Element is STM32_SVD.Byte;

   --  TDH1R_DATA array
   type TDH1R_DATA_Field_Array is array (0 .. 3) of TDH1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type TDH1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : TDH1R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for TDH1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- TI2R_Register --
   -------------------

   subtype TI2R_TXRQ_Field is STM32_SVD.Bit;
   subtype TI2R_RTR_Field is STM32_SVD.Bit;
   subtype TI2R_IDE_Field is STM32_SVD.Bit;
   subtype TI2R_EXID_Field is STM32_SVD.UInt18;
   subtype TI2R_STID_Field is STM32_SVD.UInt11;

   --  mailbox identifier register
   type TI2R_Register is record
      --  TXRQ
      TXRQ : TI2R_TXRQ_Field := 16#0#;
      --  RTR
      RTR  : TI2R_RTR_Field := 16#0#;
      --  IDE
      IDE  : TI2R_IDE_Field := 16#0#;
      --  EXID
      EXID : TI2R_EXID_Field := 16#0#;
      --  STID
      STID : TI2R_STID_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TI2R_Register use record
      TXRQ at 0 range 0 .. 0;
      RTR  at 0 range 1 .. 1;
      IDE  at 0 range 2 .. 2;
      EXID at 0 range 3 .. 20;
      STID at 0 range 21 .. 31;
   end record;

   --------------------
   -- TDT2R_Register --
   --------------------

   subtype TDT2R_DLC_Field is STM32_SVD.UInt4;
   subtype TDT2R_TGT_Field is STM32_SVD.Bit;
   subtype TDT2R_TIME_Field is STM32_SVD.Short;

   --  mailbox data length control and time stamp register
   type TDT2R_Register is record
      --  DLC
      DLC           : TDT2R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : STM32_SVD.UInt4 := 16#0#;
      --  TGT
      TGT           : TDT2R_TGT_Field := 16#0#;
      --  unspecified
      Reserved_9_15 : STM32_SVD.UInt7 := 16#0#;
      --  TIME
      TIME          : TDT2R_TIME_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TDT2R_Register use record
      DLC           at 0 range 0 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      TGT           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      TIME          at 0 range 16 .. 31;
   end record;

   --------------------
   -- TDL2R_Register --
   --------------------

   --  TDL2R_DATA array element
   subtype TDL2R_DATA_Element is STM32_SVD.Byte;

   --  TDL2R_DATA array
   type TDL2R_DATA_Field_Array is array (0 .. 3) of TDL2R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data low register
   type TDL2R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : TDL2R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for TDL2R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --------------------
   -- TDH2R_Register --
   --------------------

   --  TDH2R_DATA array element
   subtype TDH2R_DATA_Element is STM32_SVD.Byte;

   --  TDH2R_DATA array
   type TDH2R_DATA_Field_Array is array (0 .. 3) of TDH2R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type TDH2R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : TDH2R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for TDH2R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- RI0R_Register --
   -------------------

   subtype RI0R_RTR_Field is STM32_SVD.Bit;
   subtype RI0R_IDE_Field is STM32_SVD.Bit;
   subtype RI0R_EXID_Field is STM32_SVD.UInt18;
   subtype RI0R_STID_Field is STM32_SVD.UInt11;

   --  receive FIFO mailbox identifier register
   type RI0R_Register is record
      --  unspecified
      Reserved_0_0 : STM32_SVD.Bit := 16#0#;
      --  RTR
      RTR          : RI0R_RTR_Field := 16#0#;
      --  IDE
      IDE          : RI0R_IDE_Field := 16#0#;
      --  EXID
      EXID         : RI0R_EXID_Field := 16#0#;
      --  STID
      STID         : RI0R_STID_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RI0R_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      RTR          at 0 range 1 .. 1;
      IDE          at 0 range 2 .. 2;
      EXID         at 0 range 3 .. 20;
      STID         at 0 range 21 .. 31;
   end record;

   --------------------
   -- RDT0R_Register --
   --------------------

   subtype RDT0R_DLC_Field is STM32_SVD.UInt4;
   subtype RDT0R_FMI_Field is STM32_SVD.Byte;
   subtype RDT0R_TIME_Field is STM32_SVD.Short;

   --  mailbox data high register
   type RDT0R_Register is record
      --  DLC
      DLC          : RDT0R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32_SVD.UInt4 := 16#0#;
      --  FMI
      FMI          : RDT0R_FMI_Field := 16#0#;
      --  TIME
      TIME         : RDT0R_TIME_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RDT0R_Register use record
      DLC          at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      FMI          at 0 range 8 .. 15;
      TIME         at 0 range 16 .. 31;
   end record;

   --------------------
   -- RDL0R_Register --
   --------------------

   --  RDL0R_DATA array element
   subtype RDL0R_DATA_Element is STM32_SVD.Byte;

   --  RDL0R_DATA array
   type RDL0R_DATA_Field_Array is array (0 .. 3) of RDL0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type RDL0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : RDL0R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for RDL0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --------------------
   -- RDH0R_Register --
   --------------------

   --  RDH0R_DATA array element
   subtype RDH0R_DATA_Element is STM32_SVD.Byte;

   --  RDH0R_DATA array
   type RDH0R_DATA_Field_Array is array (0 .. 3) of RDH0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  receive FIFO mailbox data high register
   type RDH0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : RDH0R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for RDH0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- RI1R_Register --
   -------------------

   subtype RI1R_RTR_Field is STM32_SVD.Bit;
   subtype RI1R_IDE_Field is STM32_SVD.Bit;
   subtype RI1R_EXID_Field is STM32_SVD.UInt18;
   subtype RI1R_STID_Field is STM32_SVD.UInt11;

   --  mailbox data high register
   type RI1R_Register is record
      --  unspecified
      Reserved_0_0 : STM32_SVD.Bit := 16#0#;
      --  RTR
      RTR          : RI1R_RTR_Field := 16#0#;
      --  IDE
      IDE          : RI1R_IDE_Field := 16#0#;
      --  EXID
      EXID         : RI1R_EXID_Field := 16#0#;
      --  STID
      STID         : RI1R_STID_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RI1R_Register use record
      Reserved_0_0 at 0 range 0 .. 0;
      RTR          at 0 range 1 .. 1;
      IDE          at 0 range 2 .. 2;
      EXID         at 0 range 3 .. 20;
      STID         at 0 range 21 .. 31;
   end record;

   --------------------
   -- RDT1R_Register --
   --------------------

   subtype RDT1R_DLC_Field is STM32_SVD.UInt4;
   subtype RDT1R_FMI_Field is STM32_SVD.Byte;
   subtype RDT1R_TIME_Field is STM32_SVD.Short;

   --  mailbox data high register
   type RDT1R_Register is record
      --  DLC
      DLC          : RDT1R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32_SVD.UInt4 := 16#0#;
      --  FMI
      FMI          : RDT1R_FMI_Field := 16#0#;
      --  TIME
      TIME         : RDT1R_TIME_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RDT1R_Register use record
      DLC          at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      FMI          at 0 range 8 .. 15;
      TIME         at 0 range 16 .. 31;
   end record;

   --------------------
   -- RDL1R_Register --
   --------------------

   --  RDL1R_DATA array element
   subtype RDL1R_DATA_Element is STM32_SVD.Byte;

   --  RDL1R_DATA array
   type RDL1R_DATA_Field_Array is array (0 .. 3) of RDL1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type RDL1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : RDL1R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for RDL1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --------------------
   -- RDH1R_Register --
   --------------------

   --  RDH1R_DATA array element
   subtype RDH1R_DATA_Element is STM32_SVD.Byte;

   --  RDH1R_DATA array
   type RDH1R_DATA_Field_Array is array (0 .. 3) of RDH1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type RDH1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : RDH1R_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for RDH1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- FMR_Register --
   ------------------

   subtype FMR_FINIT_Field is STM32_SVD.Bit;
   subtype FMR_CAN2SB_Field is STM32_SVD.UInt6;

   --  filter master register
   type FMR_Register is record
      --  FINIT
      FINIT          : FMR_FINIT_Field := 16#1#;
      --  unspecified
      Reserved_1_7   : STM32_SVD.UInt7 := 16#0#;
      --  CAN2SB
      CAN2SB         : FMR_CAN2SB_Field := 16#E#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#A870#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMR_Register use record
      FINIT          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      CAN2SB         at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -------------------
   -- FM1R_Register --
   -------------------

   --------------
   -- FM1R.FBM --
   --------------

   --  FM1R_FBM array element
   subtype FM1R_FBM_Element is STM32_SVD.Bit;

   --  FM1R_FBM array
   type FM1R_FBM_Field_Array is array (0 .. 27) of FM1R_FBM_Element
     with Component_Size => 1, Size => 28;

   --  Type definition for FM1R_FBM
   type FM1R_FBM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FBM as a value
            Val : STM32_SVD.UInt28;
         when True =>
            --  FBM as an array
            Arr : FM1R_FBM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 28;

   for FM1R_FBM_Field use record
      Val at 0 range 0 .. 27;
      Arr at 0 range 0 .. 27;
   end record;

   --  filter mode register
   type FM1R_Register is record
      --  Filter mode
      FBM            : FM1R_FBM_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FM1R_Register use record
      FBM            at 0 range 0 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -------------------
   -- FS1R_Register --
   -------------------

   --------------
   -- FS1R.FSC --
   --------------

   --  FS1R_FSC array element
   subtype FS1R_FSC_Element is STM32_SVD.Bit;

   --  FS1R_FSC array
   type FS1R_FSC_Field_Array is array (0 .. 27) of FS1R_FSC_Element
     with Component_Size => 1, Size => 28;

   --  Type definition for FS1R_FSC
   type FS1R_FSC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FSC as a value
            Val : STM32_SVD.UInt28;
         when True =>
            --  FSC as an array
            Arr : FS1R_FSC_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 28;

   for FS1R_FSC_Field use record
      Val at 0 range 0 .. 27;
      Arr at 0 range 0 .. 27;
   end record;

   --  filter scale register
   type FS1R_Register is record
      --  Filter scale configuration
      FSC            : FS1R_FSC_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS1R_Register use record
      FSC            at 0 range 0 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --------------------
   -- FFA1R_Register --
   --------------------

   ---------------
   -- FFA1R.FFA --
   ---------------

   --  FFA1R_FFA array element
   subtype FFA1R_FFA_Element is STM32_SVD.Bit;

   --  FFA1R_FFA array
   type FFA1R_FFA_Field_Array is array (0 .. 27) of FFA1R_FFA_Element
     with Component_Size => 1, Size => 28;

   --  Type definition for FFA1R_FFA
   type FFA1R_FFA_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FFA as a value
            Val : STM32_SVD.UInt28;
         when True =>
            --  FFA as an array
            Arr : FFA1R_FFA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 28;

   for FFA1R_FFA_Field use record
      Val at 0 range 0 .. 27;
      Arr at 0 range 0 .. 27;
   end record;

   --  filter FIFO assignment register
   type FFA1R_Register is record
      --  Filter FIFO assignment for filter 0
      FFA            : FFA1R_FFA_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FFA1R_Register use record
      FFA            at 0 range 0 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -------------------
   -- FA1R_Register --
   -------------------

   ---------------
   -- FA1R.FACT --
   ---------------

   --  FA1R_FACT array element
   subtype FA1R_FACT_Element is STM32_SVD.Bit;

   --  FA1R_FACT array
   type FA1R_FACT_Field_Array is array (0 .. 27) of FA1R_FACT_Element
     with Component_Size => 1, Size => 28;

   --  Type definition for FA1R_FACT
   type FA1R_FACT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FACT as a value
            Val : STM32_SVD.UInt28;
         when True =>
            --  FACT as an array
            Arr : FA1R_FACT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 28;

   for FA1R_FACT_Field use record
      Val at 0 range 0 .. 27;
      Arr at 0 range 0 .. 27;
   end record;

   --  filter activation register
   type FA1R_Register is record
      --  Filter active
      FACT           : FA1R_FACT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FA1R_Register use record
      FACT           at 0 range 0 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   ------------------
   -- F0R_Register --
   ------------------

   --  F0R1_FB array element
   subtype F0R1_FB_Element is STM32_SVD.Bit;

   --  F0R1_FB array
   type F0R1_FB_Field_Array is array (0 .. 31) of F0R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 0 register 1
   type F0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F0R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- F1R_Register --
   ------------------

   --  F1R1_FB array element
   subtype F1R1_FB_Element is STM32_SVD.Bit;

   --  F1R1_FB array
   type F1R1_FB_Field_Array is array (0 .. 31) of F1R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 1 register 1
   type F1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F1R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- F2R_Register --
   ------------------

   --  F2R1_FB array element
   subtype F2R1_FB_Element is STM32_SVD.Bit;

   --  F2R1_FB array
   type F2R1_FB_Field_Array is array (0 .. 31) of F2R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 2 register 1
   type F2R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F2R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F2R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- F3R_Register --
   ------------------

   --  F3R1_FB array element
   subtype F3R1_FB_Element is STM32_SVD.Bit;

   --  F3R1_FB array
   type F3R1_FB_Field_Array is array (0 .. 31) of F3R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 3 register 1
   type F3R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F3R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F3R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- F4R_Register --
   ------------------

   --  F4R1_FB array element
   subtype F4R1_FB_Element is STM32_SVD.Bit;

   --  F4R1_FB array
   type F4R1_FB_Field_Array is array (0 .. 31) of F4R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 4 register 1
   type F4R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F4R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F4R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- F5R_Register --
   ------------------

   --  F5R1_FB array element
   subtype F5R1_FB_Element is STM32_SVD.Bit;

   --  F5R1_FB array
   type F5R1_FB_Field_Array is array (0 .. 31) of F5R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 5 register 1
   type F5R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F5R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F5R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- F6R_Register --
   ------------------

   --  F6R1_FB array element
   subtype F6R1_FB_Element is STM32_SVD.Bit;

   --  F6R1_FB array
   type F6R1_FB_Field_Array is array (0 .. 31) of F6R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 6 register 1
   type F6R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F6R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F6R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- F7R_Register --
   ------------------

   --  F7R1_FB array element
   subtype F7R1_FB_Element is STM32_SVD.Bit;

   --  F7R1_FB array
   type F7R1_FB_Field_Array is array (0 .. 31) of F7R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 7 register 1
   type F7R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F7R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F7R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- F8R_Register --
   ------------------

   --  F8R1_FB array element
   subtype F8R1_FB_Element is STM32_SVD.Bit;

   --  F8R1_FB array
   type F8R1_FB_Field_Array is array (0 .. 31) of F8R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 8 register 1
   type F8R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F8R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F8R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   ------------------
   -- F9R_Register --
   ------------------

   --  F9R1_FB array element
   subtype F9R1_FB_Element is STM32_SVD.Bit;

   --  F9R1_FB array
   type F9R1_FB_Field_Array is array (0 .. 31) of F9R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 9 register 1
   type F9R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F9R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F9R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F10R_Register --
   -------------------

   --  F10R1_FB array element
   subtype F10R1_FB_Element is STM32_SVD.Bit;

   --  F10R1_FB array
   type F10R1_FB_Field_Array is array (0 .. 31) of F10R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 10 register 1
   type F10R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F10R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F10R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F11R_Register --
   -------------------

   --  F11R1_FB array element
   subtype F11R1_FB_Element is STM32_SVD.Bit;

   --  F11R1_FB array
   type F11R1_FB_Field_Array is array (0 .. 31) of F11R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 11 register 1
   type F11R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F11R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F11R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F12R_Register --
   -------------------

   --  F12R1_FB array element
   subtype F12R1_FB_Element is STM32_SVD.Bit;

   --  F12R1_FB array
   type F12R1_FB_Field_Array is array (0 .. 31) of F12R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 4 register 1
   type F12R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F12R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F12R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F13R_Register --
   -------------------

   --  F13R1_FB array element
   subtype F13R1_FB_Element is STM32_SVD.Bit;

   --  F13R1_FB array
   type F13R1_FB_Field_Array is array (0 .. 31) of F13R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 13 register 1
   type F13R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F13R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F13R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F14R_Register --
   -------------------

   --  F14R1_FB array element
   subtype F14R1_FB_Element is STM32_SVD.Bit;

   --  F14R1_FB array
   type F14R1_FB_Field_Array is array (0 .. 31) of F14R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 14 register 1
   type F14R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F14R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F14R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F15R_Register --
   -------------------

   --  F15R1_FB array element
   subtype F15R1_FB_Element is STM32_SVD.Bit;

   --  F15R1_FB array
   type F15R1_FB_Field_Array is array (0 .. 31) of F15R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 15 register 1
   type F15R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F15R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F15R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F16R_Register --
   -------------------

   --  F16R1_FB array element
   subtype F16R1_FB_Element is STM32_SVD.Bit;

   --  F16R1_FB array
   type F16R1_FB_Field_Array is array (0 .. 31) of F16R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 16 register 1
   type F16R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F16R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F16R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F17R_Register --
   -------------------

   --  F17R1_FB array element
   subtype F17R1_FB_Element is STM32_SVD.Bit;

   --  F17R1_FB array
   type F17R1_FB_Field_Array is array (0 .. 31) of F17R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 17 register 1
   type F17R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F17R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F17R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F18R_Register --
   -------------------

   --  F18R1_FB array element
   subtype F18R1_FB_Element is STM32_SVD.Bit;

   --  F18R1_FB array
   type F18R1_FB_Field_Array is array (0 .. 31) of F18R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 18 register 1
   type F18R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F18R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F18R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F19R_Register --
   -------------------

   --  F19R1_FB array element
   subtype F19R1_FB_Element is STM32_SVD.Bit;

   --  F19R1_FB array
   type F19R1_FB_Field_Array is array (0 .. 31) of F19R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 19 register 1
   type F19R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F19R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F19R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F20R_Register --
   -------------------

   --  F20R1_FB array element
   subtype F20R1_FB_Element is STM32_SVD.Bit;

   --  F20R1_FB array
   type F20R1_FB_Field_Array is array (0 .. 31) of F20R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 20 register 1
   type F20R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F20R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F20R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F21R_Register --
   -------------------

   --  F21R1_FB array element
   subtype F21R1_FB_Element is STM32_SVD.Bit;

   --  F21R1_FB array
   type F21R1_FB_Field_Array is array (0 .. 31) of F21R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 21 register 1
   type F21R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F21R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F21R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F22R_Register --
   -------------------

   --  F22R1_FB array element
   subtype F22R1_FB_Element is STM32_SVD.Bit;

   --  F22R1_FB array
   type F22R1_FB_Field_Array is array (0 .. 31) of F22R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 22 register 1
   type F22R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F22R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F22R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F23R_Register --
   -------------------

   --  F23R1_FB array element
   subtype F23R1_FB_Element is STM32_SVD.Bit;

   --  F23R1_FB array
   type F23R1_FB_Field_Array is array (0 .. 31) of F23R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 23 register 1
   type F23R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F23R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F23R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F24R_Register --
   -------------------

   --  F24R1_FB array element
   subtype F24R1_FB_Element is STM32_SVD.Bit;

   --  F24R1_FB array
   type F24R1_FB_Field_Array is array (0 .. 31) of F24R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 24 register 1
   type F24R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F24R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F24R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F25R_Register --
   -------------------

   --  F25R1_FB array element
   subtype F25R1_FB_Element is STM32_SVD.Bit;

   --  F25R1_FB array
   type F25R1_FB_Field_Array is array (0 .. 31) of F25R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 25 register 1
   type F25R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F25R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F25R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F26R_Register --
   -------------------

   --  F26R1_FB array element
   subtype F26R1_FB_Element is STM32_SVD.Bit;

   --  F26R1_FB array
   type F26R1_FB_Field_Array is array (0 .. 31) of F26R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 26 register 1
   type F26R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F26R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F26R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -------------------
   -- F27R_Register --
   -------------------

   --  F27R1_FB array element
   subtype F27R1_FB_Element is STM32_SVD.Bit;

   --  F27R1_FB array
   type F27R1_FB_Field_Array is array (0 .. 31) of F27R1_FB_Element
     with Component_Size => 1, Size => 32;

   --  Filter bank 27 register 1
   type F27R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : STM32_SVD.Word;
         when True =>
            --  FB as an array
            Arr : F27R1_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F27R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Controller area network
   type CAN_Peripheral is record
      --  master control register
      MCR   : MCR_Register;
      --  master status register
      MSR   : MSR_Register;
      --  transmit status register
      TSR   : TSR_Register;
      --  receive FIFO 0 register
      RF0R  : RF0R_Register;
      --  receive FIFO 1 register
      RF1R  : RF1R_Register;
      --  interrupt enable register
      IER   : IER_Register;
      --  interrupt enable register
      ESR   : ESR_Register;
      --  bit timing register
      BTR   : BTR_Register;
      --  TX mailbox identifier register
      TI0R  : TI0R_Register;
      --  mailbox data length control and time stamp register
      TDT0R : TDT0R_Register;
      --  mailbox data low register
      TDL0R : TDL0R_Register;
      --  mailbox data high register
      TDH0R : TDH0R_Register;
      --  mailbox identifier register
      TI1R  : TI1R_Register;
      --  mailbox data length control and time stamp register
      TDT1R : TDT1R_Register;
      --  mailbox data low register
      TDL1R : TDL1R_Register;
      --  mailbox data high register
      TDH1R : TDH1R_Register;
      --  mailbox identifier register
      TI2R  : TI2R_Register;
      --  mailbox data length control and time stamp register
      TDT2R : TDT2R_Register;
      --  mailbox data low register
      TDL2R : TDL2R_Register;
      --  mailbox data high register
      TDH2R : TDH2R_Register;
      --  receive FIFO mailbox identifier register
      RI0R  : RI0R_Register;
      --  mailbox data high register
      RDT0R : RDT0R_Register;
      --  mailbox data high register
      RDL0R : RDL0R_Register;
      --  receive FIFO mailbox data high register
      RDH0R : RDH0R_Register;
      --  mailbox data high register
      RI1R  : RI1R_Register;
      --  mailbox data high register
      RDT1R : RDT1R_Register;
      --  mailbox data high register
      RDL1R : RDL1R_Register;
      --  mailbox data high register
      RDH1R : RDH1R_Register;
      --  filter master register
      FMR   : FMR_Register;
      --  filter mode register
      FM1R  : FM1R_Register;
      --  filter scale register
      FS1R  : FS1R_Register;
      --  filter FIFO assignment register
      FFA1R : FFA1R_Register;
      --  filter activation register
      FA1R  : FA1R_Register;
      --  Filter bank 0 register 1
      F0R1  : F0R_Register;
      --  Filter bank 0 register 2
      F0R2  : F0R_Register;
      --  Filter bank 1 register 1
      F1R1  : F1R_Register;
      --  Filter bank 1 register 2
      F1R2  : F1R_Register;
      --  Filter bank 2 register 1
      F2R1  : F2R_Register;
      --  Filter bank 2 register 2
      F2R2  : F2R_Register;
      --  Filter bank 3 register 1
      F3R1  : F3R_Register;
      --  Filter bank 3 register 2
      F3R2  : F3R_Register;
      --  Filter bank 4 register 1
      F4R1  : F4R_Register;
      --  Filter bank 4 register 2
      F4R2  : F4R_Register;
      --  Filter bank 5 register 1
      F5R1  : F5R_Register;
      --  Filter bank 5 register 2
      F5R2  : F5R_Register;
      --  Filter bank 6 register 1
      F6R1  : F6R_Register;
      --  Filter bank 6 register 2
      F6R2  : F6R_Register;
      --  Filter bank 7 register 1
      F7R1  : F7R_Register;
      --  Filter bank 7 register 2
      F7R2  : F7R_Register;
      --  Filter bank 8 register 1
      F8R1  : F8R_Register;
      --  Filter bank 8 register 2
      F8R2  : F8R_Register;
      --  Filter bank 9 register 1
      F9R1  : F9R_Register;
      --  Filter bank 9 register 2
      F9R2  : F9R_Register;
      --  Filter bank 10 register 1
      F10R1 : F10R_Register;
      --  Filter bank 10 register 2
      F10R2 : F10R_Register;
      --  Filter bank 11 register 1
      F11R1 : F11R_Register;
      --  Filter bank 11 register 2
      F11R2 : F11R_Register;
      --  Filter bank 4 register 1
      F12R1 : F12R_Register;
      --  Filter bank 12 register 2
      F12R2 : F12R_Register;
      --  Filter bank 13 register 1
      F13R1 : F13R_Register;
      --  Filter bank 13 register 2
      F13R2 : F13R_Register;
      --  Filter bank 14 register 1
      F14R1 : F14R_Register;
      --  Filter bank 14 register 2
      F14R2 : F14R_Register;
      --  Filter bank 15 register 1
      F15R1 : F15R_Register;
      --  Filter bank 15 register 2
      F15R2 : F15R_Register;
      --  Filter bank 16 register 1
      F16R1 : F16R_Register;
      --  Filter bank 16 register 2
      F16R2 : F16R_Register;
      --  Filter bank 17 register 1
      F17R1 : F17R_Register;
      --  Filter bank 17 register 2
      F17R2 : F17R_Register;
      --  Filter bank 18 register 1
      F18R1 : F18R_Register;
      --  Filter bank 18 register 2
      F18R2 : F18R_Register;
      --  Filter bank 19 register 1
      F19R1 : F19R_Register;
      --  Filter bank 19 register 2
      F19R2 : F19R_Register;
      --  Filter bank 20 register 1
      F20R1 : F20R_Register;
      --  Filter bank 20 register 2
      F20R2 : F20R_Register;
      --  Filter bank 21 register 1
      F21R1 : F21R_Register;
      --  Filter bank 21 register 2
      F21R2 : F21R_Register;
      --  Filter bank 22 register 1
      F22R1 : F22R_Register;
      --  Filter bank 22 register 2
      F22R2 : F22R_Register;
      --  Filter bank 23 register 1
      F23R1 : F23R_Register;
      --  Filter bank 23 register 2
      F23R2 : F23R_Register;
      --  Filter bank 24 register 1
      F24R1 : F24R_Register;
      --  Filter bank 24 register 2
      F24R2 : F24R_Register;
      --  Filter bank 25 register 1
      F25R1 : F25R_Register;
      --  Filter bank 25 register 2
      F25R2 : F25R_Register;
      --  Filter bank 26 register 1
      F26R1 : F26R_Register;
      --  Filter bank 26 register 2
      F26R2 : F26R_Register;
      --  Filter bank 27 register 1
      F27R1 : F27R_Register;
      --  Filter bank 27 register 2
      F27R2 : F27R_Register;
   end record
     with Volatile;

   for CAN_Peripheral use record
      MCR   at 0 range 0 .. 31;
      MSR   at 4 range 0 .. 31;
      TSR   at 8 range 0 .. 31;
      RF0R  at 12 range 0 .. 31;
      RF1R  at 16 range 0 .. 31;
      IER   at 20 range 0 .. 31;
      ESR   at 24 range 0 .. 31;
      BTR   at 28 range 0 .. 31;
      TI0R  at 384 range 0 .. 31;
      TDT0R at 388 range 0 .. 31;
      TDL0R at 392 range 0 .. 31;
      TDH0R at 396 range 0 .. 31;
      TI1R  at 400 range 0 .. 31;
      TDT1R at 404 range 0 .. 31;
      TDL1R at 408 range 0 .. 31;
      TDH1R at 412 range 0 .. 31;
      TI2R  at 416 range 0 .. 31;
      TDT2R at 420 range 0 .. 31;
      TDL2R at 424 range 0 .. 31;
      TDH2R at 428 range 0 .. 31;
      RI0R  at 432 range 0 .. 31;
      RDT0R at 436 range 0 .. 31;
      RDL0R at 440 range 0 .. 31;
      RDH0R at 444 range 0 .. 31;
      RI1R  at 448 range 0 .. 31;
      RDT1R at 452 range 0 .. 31;
      RDL1R at 456 range 0 .. 31;
      RDH1R at 460 range 0 .. 31;
      FMR   at 512 range 0 .. 31;
      FM1R  at 516 range 0 .. 31;
      FS1R  at 524 range 0 .. 31;
      FFA1R at 532 range 0 .. 31;
      FA1R  at 540 range 0 .. 31;
      F0R1  at 576 range 0 .. 31;
      F0R2  at 580 range 0 .. 31;
      F1R1  at 584 range 0 .. 31;
      F1R2  at 588 range 0 .. 31;
      F2R1  at 592 range 0 .. 31;
      F2R2  at 596 range 0 .. 31;
      F3R1  at 600 range 0 .. 31;
      F3R2  at 604 range 0 .. 31;
      F4R1  at 608 range 0 .. 31;
      F4R2  at 612 range 0 .. 31;
      F5R1  at 616 range 0 .. 31;
      F5R2  at 620 range 0 .. 31;
      F6R1  at 624 range 0 .. 31;
      F6R2  at 628 range 0 .. 31;
      F7R1  at 632 range 0 .. 31;
      F7R2  at 636 range 0 .. 31;
      F8R1  at 640 range 0 .. 31;
      F8R2  at 644 range 0 .. 31;
      F9R1  at 648 range 0 .. 31;
      F9R2  at 652 range 0 .. 31;
      F10R1 at 656 range 0 .. 31;
      F10R2 at 660 range 0 .. 31;
      F11R1 at 664 range 0 .. 31;
      F11R2 at 668 range 0 .. 31;
      F12R1 at 672 range 0 .. 31;
      F12R2 at 676 range 0 .. 31;
      F13R1 at 680 range 0 .. 31;
      F13R2 at 684 range 0 .. 31;
      F14R1 at 688 range 0 .. 31;
      F14R2 at 692 range 0 .. 31;
      F15R1 at 696 range 0 .. 31;
      F15R2 at 700 range 0 .. 31;
      F16R1 at 704 range 0 .. 31;
      F16R2 at 708 range 0 .. 31;
      F17R1 at 712 range 0 .. 31;
      F17R2 at 716 range 0 .. 31;
      F18R1 at 720 range 0 .. 31;
      F18R2 at 724 range 0 .. 31;
      F19R1 at 728 range 0 .. 31;
      F19R2 at 732 range 0 .. 31;
      F20R1 at 736 range 0 .. 31;
      F20R2 at 740 range 0 .. 31;
      F21R1 at 744 range 0 .. 31;
      F21R2 at 748 range 0 .. 31;
      F22R1 at 752 range 0 .. 31;
      F22R2 at 756 range 0 .. 31;
      F23R1 at 760 range 0 .. 31;
      F23R2 at 764 range 0 .. 31;
      F24R1 at 768 range 0 .. 31;
      F24R2 at 772 range 0 .. 31;
      F25R1 at 776 range 0 .. 31;
      F25R2 at 780 range 0 .. 31;
      F26R1 at 784 range 0 .. 31;
      F26R2 at 788 range 0 .. 31;
      F27R1 at 792 range 0 .. 31;
      F27R2 at 796 range 0 .. 31;
   end record;

   --  Controller area network
   CAN1_Periph : aliased CAN_Peripheral
     with Import, Address => System'To_Address (16#40006400#);

   --  Controller area network
   CAN2_Periph : aliased CAN_Peripheral
     with Import, Address => System'To_Address (16#40006800#);

end STM32_SVD.CAN;
