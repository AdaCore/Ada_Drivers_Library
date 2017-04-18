--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.CAN is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  master control register
   type MCR_Register is record
      --  INRQ
      INRQ           : Boolean := False;
      --  SLEEP
      SLEEP          : Boolean := True;
      --  TXFP
      TXFP           : Boolean := False;
      --  RFLM
      RFLM           : Boolean := False;
      --  NART
      NART           : Boolean := False;
      --  AWUM
      AWUM           : Boolean := False;
      --  ABOM
      ABOM           : Boolean := False;
      --  TTCM
      TTCM           : Boolean := False;
      --  unspecified
      Reserved_8_14  : HAL.UInt7 := 16#0#;
      --  RESET
      RESET          : Boolean := False;
      --  DBF
      DBF            : Boolean := True;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
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

   --  master status register
   type MSR_Register is record
      --  Read-only. INAK
      INAK           : Boolean := False;
      --  Read-only. SLAK
      SLAK           : Boolean := True;
      --  ERRI
      ERRI           : Boolean := False;
      --  WKUI
      WKUI           : Boolean := False;
      --  SLAKI
      SLAKI          : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Read-only. TXM
      TXM            : Boolean := False;
      --  Read-only. RXM
      RXM            : Boolean := False;
      --  Read-only. SAMP
      SAMP           : Boolean := True;
      --  Read-only. RX
      RX             : Boolean := True;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
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

   subtype TSR_CODE_Field is HAL.UInt2;

   --  TSR_TME array
   type TSR_TME_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TSR_TME
   type TSR_TME_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TME as a value
            Val : HAL.UInt3;
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

   --  TSR_LOW array
   type TSR_LOW_Field_Array is array (0 .. 2) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for TSR_LOW
   type TSR_LOW_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LOW as a value
            Val : HAL.UInt3;
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
      RQCP0          : Boolean := False;
      --  TXOK0
      TXOK0          : Boolean := False;
      --  ALST0
      ALST0          : Boolean := False;
      --  TERR0
      TERR0          : Boolean := False;
      --  unspecified
      Reserved_4_6   : HAL.UInt3 := 16#0#;
      --  ABRQ0
      ABRQ0          : Boolean := False;
      --  RQCP1
      RQCP1          : Boolean := False;
      --  TXOK1
      TXOK1          : Boolean := False;
      --  ALST1
      ALST1          : Boolean := False;
      --  TERR1
      TERR1          : Boolean := False;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  ABRQ1
      ABRQ1          : Boolean := False;
      --  RQCP2
      RQCP2          : Boolean := False;
      --  TXOK2
      TXOK2          : Boolean := False;
      --  ALST2
      ALST2          : Boolean := False;
      --  TERR2
      TERR2          : Boolean := False;
      --  unspecified
      Reserved_20_22 : HAL.UInt3 := 16#0#;
      --  ABRQ2
      ABRQ2          : Boolean := False;
      --  Read-only. CODE
      CODE           : TSR_CODE_Field := 16#0#;
      --  Read-only. Lowest priority flag for mailbox 0
      TME            : TSR_TME_Field := (As_Array => False, Val => 16#1#);
      --  Read-only. Lowest priority flag for mailbox 0
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

   subtype RF0R_FMP0_Field is HAL.UInt2;

   --  receive FIFO 0 register
   type RF0R_Register is record
      --  Read-only. FMP0
      FMP0          : RF0R_FMP0_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  FULL0
      FULL0         : Boolean := False;
      --  FOVR0
      FOVR0         : Boolean := False;
      --  RFOM0
      RFOM0         : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
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

   subtype RF1R_FMP1_Field is HAL.UInt2;

   --  receive FIFO 1 register
   type RF1R_Register is record
      --  Read-only. FMP1
      FMP1          : RF1R_FMP1_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  FULL1
      FULL1         : Boolean := False;
      --  FOVR1
      FOVR1         : Boolean := False;
      --  RFOM1
      RFOM1         : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
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

   --  interrupt enable register
   type IER_Register is record
      --  TMEIE
      TMEIE          : Boolean := False;
      --  FMPIE0
      FMPIE0         : Boolean := False;
      --  FFIE0
      FFIE0          : Boolean := False;
      --  FOVIE0
      FOVIE0         : Boolean := False;
      --  FMPIE1
      FMPIE1         : Boolean := False;
      --  FFIE1
      FFIE1          : Boolean := False;
      --  FOVIE1
      FOVIE1         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  EWGIE
      EWGIE          : Boolean := False;
      --  EPVIE
      EPVIE          : Boolean := False;
      --  BOFIE
      BOFIE          : Boolean := False;
      --  LECIE
      LECIE          : Boolean := False;
      --  unspecified
      Reserved_12_14 : HAL.UInt3 := 16#0#;
      --  ERRIE
      ERRIE          : Boolean := False;
      --  WKUIE
      WKUIE          : Boolean := False;
      --  SLKIE
      SLKIE          : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
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

   subtype ESR_LEC_Field is HAL.UInt3;
   subtype ESR_TEC_Field is HAL.UInt8;
   subtype ESR_REC_Field is HAL.UInt8;

   --  interrupt enable register
   type ESR_Register is record
      --  Read-only. EWGF
      EWGF          : Boolean := False;
      --  Read-only. EPVF
      EPVF          : Boolean := False;
      --  Read-only. BOFF
      BOFF          : Boolean := False;
      --  unspecified
      Reserved_3_3  : HAL.Bit := 16#0#;
      --  LEC
      LEC           : ESR_LEC_Field := 16#0#;
      --  unspecified
      Reserved_7_15 : HAL.UInt9 := 16#0#;
      --  Read-only. TEC
      TEC           : ESR_TEC_Field := 16#0#;
      --  Read-only. REC
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

   subtype BTR_BRP_Field is HAL.UInt10;
   subtype BTR_TS1_Field is HAL.UInt4;
   subtype BTR_TS2_Field is HAL.UInt3;
   subtype BTR_SJW_Field is HAL.UInt2;

   --  bit timing register
   type BTR_Register is record
      --  BRP
      BRP            : BTR_BRP_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : HAL.UInt6 := 16#0#;
      --  TS1
      TS1            : BTR_TS1_Field := 16#0#;
      --  TS2
      TS2            : BTR_TS2_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  SJW
      SJW            : BTR_SJW_Field := 16#0#;
      --  unspecified
      Reserved_26_29 : HAL.UInt4 := 16#0#;
      --  LBKM
      LBKM           : Boolean := False;
      --  SILM
      SILM           : Boolean := False;
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

   subtype TI0R_EXID_Field is HAL.UInt18;
   subtype TI0R_STID_Field is HAL.UInt11;

   --  TX mailbox identifier register
   type TI0R_Register is record
      --  TXRQ
      TXRQ : Boolean := False;
      --  RTR
      RTR  : Boolean := False;
      --  IDE
      IDE  : Boolean := False;
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

   subtype TDT0R_DLC_Field is HAL.UInt4;
   subtype TDT0R_TIME_Field is HAL.UInt16;

   --  mailbox data length control and time stamp register
   type TDT0R_Register is record
      --  DLC
      DLC           : TDT0R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  TGT
      TGT           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
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

   --  TDL0R_DATA array element
   subtype TDL0R_DATA_Element is HAL.UInt8;

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
            Val : HAL.UInt32;
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

   --  TDH0R_DATA array element
   subtype TDH0R_DATA_Element is HAL.UInt8;

   --  TDH0R_DATA array
   type TDH0R_DATA_Field_Array is array (4 .. 7) of TDH0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type TDH0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
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

   subtype TI1R_EXID_Field is HAL.UInt18;
   subtype TI1R_STID_Field is HAL.UInt11;

   --  mailbox identifier register
   type TI1R_Register is record
      --  TXRQ
      TXRQ : Boolean := False;
      --  RTR
      RTR  : Boolean := False;
      --  IDE
      IDE  : Boolean := False;
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

   subtype TDT1R_DLC_Field is HAL.UInt4;
   subtype TDT1R_TIME_Field is HAL.UInt16;

   --  mailbox data length control and time stamp register
   type TDT1R_Register is record
      --  DLC
      DLC           : TDT1R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  TGT
      TGT           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
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

   --  TDL1R_DATA array element
   subtype TDL1R_DATA_Element is HAL.UInt8;

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
            Val : HAL.UInt32;
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

   --  TDH1R_DATA array element
   subtype TDH1R_DATA_Element is HAL.UInt8;

   --  TDH1R_DATA array
   type TDH1R_DATA_Field_Array is array (4 .. 7) of TDH1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type TDH1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
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

   subtype TI2R_EXID_Field is HAL.UInt18;
   subtype TI2R_STID_Field is HAL.UInt11;

   --  mailbox identifier register
   type TI2R_Register is record
      --  TXRQ
      TXRQ : Boolean := False;
      --  RTR
      RTR  : Boolean := False;
      --  IDE
      IDE  : Boolean := False;
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

   subtype TDT2R_DLC_Field is HAL.UInt4;
   subtype TDT2R_TIME_Field is HAL.UInt16;

   --  mailbox data length control and time stamp register
   type TDT2R_Register is record
      --  DLC
      DLC           : TDT2R_DLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : HAL.UInt4 := 16#0#;
      --  TGT
      TGT           : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
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

   --  TDL2R_DATA array element
   subtype TDL2R_DATA_Element is HAL.UInt8;

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
            Val : HAL.UInt32;
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

   --  TDH2R_DATA array element
   subtype TDH2R_DATA_Element is HAL.UInt8;

   --  TDH2R_DATA array
   type TDH2R_DATA_Field_Array is array (4 .. 7) of TDH2R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type TDH2R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
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

   subtype RI0R_EXID_Field is HAL.UInt18;
   subtype RI0R_STID_Field is HAL.UInt11;

   --  receive FIFO mailbox identifier register
   type RI0R_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit;
      --  Read-only. RTR
      RTR          : Boolean;
      --  Read-only. IDE
      IDE          : Boolean;
      --  Read-only. EXID
      EXID         : RI0R_EXID_Field;
      --  Read-only. STID
      STID         : RI0R_STID_Field;
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

   subtype RDT0R_DLC_Field is HAL.UInt4;
   subtype RDT0R_FMI_Field is HAL.UInt8;
   subtype RDT0R_TIME_Field is HAL.UInt16;

   --  mailbox data high register
   type RDT0R_Register is record
      --  Read-only. DLC
      DLC          : RDT0R_DLC_Field;
      --  unspecified
      Reserved_4_7 : HAL.UInt4;
      --  Read-only. FMI
      FMI          : RDT0R_FMI_Field;
      --  Read-only. TIME
      TIME         : RDT0R_TIME_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RDT0R_Register use record
      DLC          at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      FMI          at 0 range 8 .. 15;
      TIME         at 0 range 16 .. 31;
   end record;

   --  RDL0R_DATA array element
   subtype RDL0R_DATA_Element is HAL.UInt8;

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
            Val : HAL.UInt32;
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

   --  RDH0R_DATA array element
   subtype RDH0R_DATA_Element is HAL.UInt8;

   --  RDH0R_DATA array
   type RDH0R_DATA_Field_Array is array (4 .. 7) of RDH0R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  receive FIFO mailbox data high register
   type RDH0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
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

   subtype RI1R_EXID_Field is HAL.UInt18;
   subtype RI1R_STID_Field is HAL.UInt11;

   --  mailbox data high register
   type RI1R_Register is record
      --  unspecified
      Reserved_0_0 : HAL.Bit;
      --  Read-only. RTR
      RTR          : Boolean;
      --  Read-only. IDE
      IDE          : Boolean;
      --  Read-only. EXID
      EXID         : RI1R_EXID_Field;
      --  Read-only. STID
      STID         : RI1R_STID_Field;
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

   subtype RDT1R_DLC_Field is HAL.UInt4;
   subtype RDT1R_FMI_Field is HAL.UInt8;
   subtype RDT1R_TIME_Field is HAL.UInt16;

   --  mailbox data high register
   type RDT1R_Register is record
      --  Read-only. DLC
      DLC          : RDT1R_DLC_Field;
      --  unspecified
      Reserved_4_7 : HAL.UInt4;
      --  Read-only. FMI
      FMI          : RDT1R_FMI_Field;
      --  Read-only. TIME
      TIME         : RDT1R_TIME_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RDT1R_Register use record
      DLC          at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      FMI          at 0 range 8 .. 15;
      TIME         at 0 range 16 .. 31;
   end record;

   --  RDL1R_DATA array element
   subtype RDL1R_DATA_Element is HAL.UInt8;

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
            Val : HAL.UInt32;
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

   --  RDH1R_DATA array element
   subtype RDH1R_DATA_Element is HAL.UInt8;

   --  RDH1R_DATA array
   type RDH1R_DATA_Field_Array is array (4 .. 7) of RDH1R_DATA_Element
     with Component_Size => 8, Size => 32;

   --  mailbox data high register
   type RDH1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : HAL.UInt32;
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

   subtype FMR_CAN2SB_Field is HAL.UInt6;

   --  filter master register
   type FMR_Register is record
      --  FINIT
      FINIT          : Boolean := True;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  CAN2SB
      CAN2SB         : FMR_CAN2SB_Field := 16#E#;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#A870#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMR_Register use record
      FINIT          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      CAN2SB         at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  FM1R_FBM array
   type FM1R_FBM_Field_Array is array (0 .. 27) of Boolean
     with Component_Size => 1, Size => 28;

   --  Type definition for FM1R_FBM
   type FM1R_FBM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FBM as a value
            Val : HAL.UInt28;
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
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FM1R_Register use record
      FBM            at 0 range 0 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  FS1R_FSC array
   type FS1R_FSC_Field_Array is array (0 .. 27) of Boolean
     with Component_Size => 1, Size => 28;

   --  Type definition for FS1R_FSC
   type FS1R_FSC_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FSC as a value
            Val : HAL.UInt28;
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
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS1R_Register use record
      FSC            at 0 range 0 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  FFA1R_FFA array
   type FFA1R_FFA_Field_Array is array (0 .. 27) of Boolean
     with Component_Size => 1, Size => 28;

   --  Type definition for FFA1R_FFA
   type FFA1R_FFA_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FFA as a value
            Val : HAL.UInt28;
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
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FFA1R_Register use record
      FFA            at 0 range 0 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  FA1R_FACT array
   type FA1R_FACT_Field_Array is array (0 .. 27) of Boolean
     with Component_Size => 1, Size => 28;

   --  Type definition for FA1R_FACT
   type FA1R_FACT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FACT as a value
            Val : HAL.UInt28;
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
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FA1R_Register use record
      FACT           at 0 range 0 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  F0R_FB array
   type F0R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 0 register 1
   type F0R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F0R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F0R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F1R_FB array
   type F1R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 1 register 1
   type F1R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F1R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F1R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F2R_FB array
   type F2R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 2 register 1
   type F2R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F2R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F2R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F3R_FB array
   type F3R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 3 register 1
   type F3R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F3R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F3R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F4R_FB array
   type F4R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 4 register 1
   type F4R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F4R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F4R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F5R_FB array
   type F5R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 5 register 1
   type F5R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F5R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F5R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F6R_FB array
   type F6R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 6 register 1
   type F6R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F6R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F6R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F7R_FB array
   type F7R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 7 register 1
   type F7R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F7R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F7R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F8R_FB array
   type F8R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 8 register 1
   type F8R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F8R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F8R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F9R_FB array
   type F9R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 9 register 1
   type F9R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F9R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F9R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F10R_FB array
   type F10R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 10 register 1
   type F10R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F10R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F10R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F11R_FB array
   type F11R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 11 register 1
   type F11R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F11R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F11R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F12R_FB array
   type F12R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 4 register 1
   type F12R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F12R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F12R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F13R_FB array
   type F13R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 13 register 1
   type F13R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F13R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F13R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F14R_FB array
   type F14R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 14 register 1
   type F14R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F14R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F14R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F15R_FB array
   type F15R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 15 register 1
   type F15R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F15R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F15R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F16R_FB array
   type F16R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 16 register 1
   type F16R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F16R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F16R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F17R_FB array
   type F17R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 17 register 1
   type F17R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F17R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F17R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F18R_FB array
   type F18R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 18 register 1
   type F18R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F18R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F18R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F19R_FB array
   type F19R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 19 register 1
   type F19R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F19R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F19R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F20R_FB array
   type F20R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 20 register 1
   type F20R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F20R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F20R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F21R_FB array
   type F21R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 21 register 1
   type F21R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F21R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F21R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F22R_FB array
   type F22R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 22 register 1
   type F22R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F22R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F22R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F23R_FB array
   type F23R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 23 register 1
   type F23R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F23R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F23R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F24R_FB array
   type F24R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 24 register 1
   type F24R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F24R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F24R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F25R_FB array
   type F25R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 25 register 1
   type F25R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F25R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F25R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F26R_FB array
   type F26R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 26 register 1
   type F26R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F26R_FB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for F26R_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  F27R_FB array
   type F27R_FB_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  Filter bank 27 register 1
   type F27R_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FB as a value
            Val : HAL.UInt32;
         when True =>
            --  FB as an array
            Arr : F27R_FB_Field_Array;
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
      MCR   : aliased MCR_Register;
      --  master status register
      MSR   : aliased MSR_Register;
      --  transmit status register
      TSR   : aliased TSR_Register;
      --  receive FIFO 0 register
      RF0R  : aliased RF0R_Register;
      --  receive FIFO 1 register
      RF1R  : aliased RF1R_Register;
      --  interrupt enable register
      IER   : aliased IER_Register;
      --  interrupt enable register
      ESR   : aliased ESR_Register;
      --  bit timing register
      BTR   : aliased BTR_Register;
      --  TX mailbox identifier register
      TI0R  : aliased TI0R_Register;
      --  mailbox data length control and time stamp register
      TDT0R : aliased TDT0R_Register;
      --  mailbox data low register
      TDL0R : aliased TDL0R_Register;
      --  mailbox data high register
      TDH0R : aliased TDH0R_Register;
      --  mailbox identifier register
      TI1R  : aliased TI1R_Register;
      --  mailbox data length control and time stamp register
      TDT1R : aliased TDT1R_Register;
      --  mailbox data low register
      TDL1R : aliased TDL1R_Register;
      --  mailbox data high register
      TDH1R : aliased TDH1R_Register;
      --  mailbox identifier register
      TI2R  : aliased TI2R_Register;
      --  mailbox data length control and time stamp register
      TDT2R : aliased TDT2R_Register;
      --  mailbox data low register
      TDL2R : aliased TDL2R_Register;
      --  mailbox data high register
      TDH2R : aliased TDH2R_Register;
      --  receive FIFO mailbox identifier register
      RI0R  : aliased RI0R_Register;
      --  mailbox data high register
      RDT0R : aliased RDT0R_Register;
      --  mailbox data high register
      RDL0R : aliased RDL0R_Register;
      --  receive FIFO mailbox data high register
      RDH0R : aliased RDH0R_Register;
      --  mailbox data high register
      RI1R  : aliased RI1R_Register;
      --  mailbox data high register
      RDT1R : aliased RDT1R_Register;
      --  mailbox data high register
      RDL1R : aliased RDL1R_Register;
      --  mailbox data high register
      RDH1R : aliased RDH1R_Register;
      --  filter master register
      FMR   : aliased FMR_Register;
      --  filter mode register
      FM1R  : aliased FM1R_Register;
      --  filter scale register
      FS1R  : aliased FS1R_Register;
      --  filter FIFO assignment register
      FFA1R : aliased FFA1R_Register;
      --  filter activation register
      FA1R  : aliased FA1R_Register;
      --  Filter bank 0 register 1
      F0R1  : aliased F0R_Register;
      --  Filter bank 0 register 2
      F0R2  : aliased F0R_Register;
      --  Filter bank 1 register 1
      F1R1  : aliased F1R_Register;
      --  Filter bank 1 register 2
      F1R2  : aliased F1R_Register;
      --  Filter bank 2 register 1
      F2R1  : aliased F2R_Register;
      --  Filter bank 2 register 2
      F2R2  : aliased F2R_Register;
      --  Filter bank 3 register 1
      F3R1  : aliased F3R_Register;
      --  Filter bank 3 register 2
      F3R2  : aliased F3R_Register;
      --  Filter bank 4 register 1
      F4R1  : aliased F4R_Register;
      --  Filter bank 4 register 2
      F4R2  : aliased F4R_Register;
      --  Filter bank 5 register 1
      F5R1  : aliased F5R_Register;
      --  Filter bank 5 register 2
      F5R2  : aliased F5R_Register;
      --  Filter bank 6 register 1
      F6R1  : aliased F6R_Register;
      --  Filter bank 6 register 2
      F6R2  : aliased F6R_Register;
      --  Filter bank 7 register 1
      F7R1  : aliased F7R_Register;
      --  Filter bank 7 register 2
      F7R2  : aliased F7R_Register;
      --  Filter bank 8 register 1
      F8R1  : aliased F8R_Register;
      --  Filter bank 8 register 2
      F8R2  : aliased F8R_Register;
      --  Filter bank 9 register 1
      F9R1  : aliased F9R_Register;
      --  Filter bank 9 register 2
      F9R2  : aliased F9R_Register;
      --  Filter bank 10 register 1
      F10R1 : aliased F10R_Register;
      --  Filter bank 10 register 2
      F10R2 : aliased F10R_Register;
      --  Filter bank 11 register 1
      F11R1 : aliased F11R_Register;
      --  Filter bank 11 register 2
      F11R2 : aliased F11R_Register;
      --  Filter bank 4 register 1
      F12R1 : aliased F12R_Register;
      --  Filter bank 12 register 2
      F12R2 : aliased F12R_Register;
      --  Filter bank 13 register 1
      F13R1 : aliased F13R_Register;
      --  Filter bank 13 register 2
      F13R2 : aliased F13R_Register;
      --  Filter bank 14 register 1
      F14R1 : aliased F14R_Register;
      --  Filter bank 14 register 2
      F14R2 : aliased F14R_Register;
      --  Filter bank 15 register 1
      F15R1 : aliased F15R_Register;
      --  Filter bank 15 register 2
      F15R2 : aliased F15R_Register;
      --  Filter bank 16 register 1
      F16R1 : aliased F16R_Register;
      --  Filter bank 16 register 2
      F16R2 : aliased F16R_Register;
      --  Filter bank 17 register 1
      F17R1 : aliased F17R_Register;
      --  Filter bank 17 register 2
      F17R2 : aliased F17R_Register;
      --  Filter bank 18 register 1
      F18R1 : aliased F18R_Register;
      --  Filter bank 18 register 2
      F18R2 : aliased F18R_Register;
      --  Filter bank 19 register 1
      F19R1 : aliased F19R_Register;
      --  Filter bank 19 register 2
      F19R2 : aliased F19R_Register;
      --  Filter bank 20 register 1
      F20R1 : aliased F20R_Register;
      --  Filter bank 20 register 2
      F20R2 : aliased F20R_Register;
      --  Filter bank 21 register 1
      F21R1 : aliased F21R_Register;
      --  Filter bank 21 register 2
      F21R2 : aliased F21R_Register;
      --  Filter bank 22 register 1
      F22R1 : aliased F22R_Register;
      --  Filter bank 22 register 2
      F22R2 : aliased F22R_Register;
      --  Filter bank 23 register 1
      F23R1 : aliased F23R_Register;
      --  Filter bank 23 register 2
      F23R2 : aliased F23R_Register;
      --  Filter bank 24 register 1
      F24R1 : aliased F24R_Register;
      --  Filter bank 24 register 2
      F24R2 : aliased F24R_Register;
      --  Filter bank 25 register 1
      F25R1 : aliased F25R_Register;
      --  Filter bank 25 register 2
      F25R2 : aliased F25R_Register;
      --  Filter bank 26 register 1
      F26R1 : aliased F26R_Register;
      --  Filter bank 26 register 2
      F26R2 : aliased F26R_Register;
      --  Filter bank 27 register 1
      F27R1 : aliased F27R_Register;
      --  Filter bank 27 register 2
      F27R2 : aliased F27R_Register;
   end record
     with Volatile;

   for CAN_Peripheral use record
      MCR   at 16#0# range 0 .. 31;
      MSR   at 16#4# range 0 .. 31;
      TSR   at 16#8# range 0 .. 31;
      RF0R  at 16#C# range 0 .. 31;
      RF1R  at 16#10# range 0 .. 31;
      IER   at 16#14# range 0 .. 31;
      ESR   at 16#18# range 0 .. 31;
      BTR   at 16#1C# range 0 .. 31;
      TI0R  at 16#180# range 0 .. 31;
      TDT0R at 16#184# range 0 .. 31;
      TDL0R at 16#188# range 0 .. 31;
      TDH0R at 16#18C# range 0 .. 31;
      TI1R  at 16#190# range 0 .. 31;
      TDT1R at 16#194# range 0 .. 31;
      TDL1R at 16#198# range 0 .. 31;
      TDH1R at 16#19C# range 0 .. 31;
      TI2R  at 16#1A0# range 0 .. 31;
      TDT2R at 16#1A4# range 0 .. 31;
      TDL2R at 16#1A8# range 0 .. 31;
      TDH2R at 16#1AC# range 0 .. 31;
      RI0R  at 16#1B0# range 0 .. 31;
      RDT0R at 16#1B4# range 0 .. 31;
      RDL0R at 16#1B8# range 0 .. 31;
      RDH0R at 16#1BC# range 0 .. 31;
      RI1R  at 16#1C0# range 0 .. 31;
      RDT1R at 16#1C4# range 0 .. 31;
      RDL1R at 16#1C8# range 0 .. 31;
      RDH1R at 16#1CC# range 0 .. 31;
      FMR   at 16#200# range 0 .. 31;
      FM1R  at 16#204# range 0 .. 31;
      FS1R  at 16#20C# range 0 .. 31;
      FFA1R at 16#214# range 0 .. 31;
      FA1R  at 16#21C# range 0 .. 31;
      F0R1  at 16#240# range 0 .. 31;
      F0R2  at 16#244# range 0 .. 31;
      F1R1  at 16#248# range 0 .. 31;
      F1R2  at 16#24C# range 0 .. 31;
      F2R1  at 16#250# range 0 .. 31;
      F2R2  at 16#254# range 0 .. 31;
      F3R1  at 16#258# range 0 .. 31;
      F3R2  at 16#25C# range 0 .. 31;
      F4R1  at 16#260# range 0 .. 31;
      F4R2  at 16#264# range 0 .. 31;
      F5R1  at 16#268# range 0 .. 31;
      F5R2  at 16#26C# range 0 .. 31;
      F6R1  at 16#270# range 0 .. 31;
      F6R2  at 16#274# range 0 .. 31;
      F7R1  at 16#278# range 0 .. 31;
      F7R2  at 16#27C# range 0 .. 31;
      F8R1  at 16#280# range 0 .. 31;
      F8R2  at 16#284# range 0 .. 31;
      F9R1  at 16#288# range 0 .. 31;
      F9R2  at 16#28C# range 0 .. 31;
      F10R1 at 16#290# range 0 .. 31;
      F10R2 at 16#294# range 0 .. 31;
      F11R1 at 16#298# range 0 .. 31;
      F11R2 at 16#29C# range 0 .. 31;
      F12R1 at 16#2A0# range 0 .. 31;
      F12R2 at 16#2A4# range 0 .. 31;
      F13R1 at 16#2A8# range 0 .. 31;
      F13R2 at 16#2AC# range 0 .. 31;
      F14R1 at 16#2B0# range 0 .. 31;
      F14R2 at 16#2B4# range 0 .. 31;
      F15R1 at 16#2B8# range 0 .. 31;
      F15R2 at 16#2BC# range 0 .. 31;
      F16R1 at 16#2C0# range 0 .. 31;
      F16R2 at 16#2C4# range 0 .. 31;
      F17R1 at 16#2C8# range 0 .. 31;
      F17R2 at 16#2CC# range 0 .. 31;
      F18R1 at 16#2D0# range 0 .. 31;
      F18R2 at 16#2D4# range 0 .. 31;
      F19R1 at 16#2D8# range 0 .. 31;
      F19R2 at 16#2DC# range 0 .. 31;
      F20R1 at 16#2E0# range 0 .. 31;
      F20R2 at 16#2E4# range 0 .. 31;
      F21R1 at 16#2E8# range 0 .. 31;
      F21R2 at 16#2EC# range 0 .. 31;
      F22R1 at 16#2F0# range 0 .. 31;
      F22R2 at 16#2F4# range 0 .. 31;
      F23R1 at 16#2F8# range 0 .. 31;
      F23R2 at 16#2FC# range 0 .. 31;
      F24R1 at 16#300# range 0 .. 31;
      F24R2 at 16#304# range 0 .. 31;
      F25R1 at 16#308# range 0 .. 31;
      F25R2 at 16#30C# range 0 .. 31;
      F26R1 at 16#310# range 0 .. 31;
      F26R2 at 16#314# range 0 .. 31;
      F27R1 at 16#318# range 0 .. 31;
      F27R2 at 16#31C# range 0 .. 31;
   end record;

   --  Controller area network
   CAN1_Periph : aliased CAN_Peripheral
     with Import, Address => System'To_Address (16#40006400#);

   --  Controller area network
   CAN2_Periph : aliased CAN_Peripheral
     with Import, Address => System'To_Address (16#40006800#);

end STM32_SVD.CAN;
