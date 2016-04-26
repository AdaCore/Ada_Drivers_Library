--  This spec has been automatically generated from STM32F429x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.FSMC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -------------------
   -- BCR1_Register --
   -------------------

   subtype BCR1_MTYP_Field is HAL.UInt2;
   subtype BCR1_MWID_Field is HAL.UInt2;

   --  SRAM/NOR-Flash chip-select control register 1
   type BCR1_Register is record
      --  MBKEN
      MBKEN          : Boolean := False;
      --  MUXEN
      MUXEN          : Boolean := False;
      --  MTYP
      MTYP           : BCR1_MTYP_Field := 16#0#;
      --  MWID
      MWID           : BCR1_MWID_Field := 16#1#;
      --  FACCEN
      FACCEN         : Boolean := True;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#1#;
      --  BURSTEN
      BURSTEN        : Boolean := False;
      --  WAITPOL
      WAITPOL        : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  WAITCFG
      WAITCFG        : Boolean := False;
      --  WREN
      WREN           : Boolean := True;
      --  WAITEN
      WAITEN         : Boolean := True;
      --  EXTMOD
      EXTMOD         : Boolean := False;
      --  ASYNCWAIT
      ASYNCWAIT      : Boolean := False;
      --  unspecified
      Reserved_16_18 : HAL.UInt3 := 16#0#;
      --  CBURSTRW
      CBURSTRW       : Boolean := False;
      --  CCLKEN
      CCLKEN         : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BCR1_Register use record
      MBKEN          at 0 range 0 .. 0;
      MUXEN          at 0 range 1 .. 1;
      MTYP           at 0 range 2 .. 3;
      MWID           at 0 range 4 .. 5;
      FACCEN         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      BURSTEN        at 0 range 8 .. 8;
      WAITPOL        at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      WAITCFG        at 0 range 11 .. 11;
      WREN           at 0 range 12 .. 12;
      WAITEN         at 0 range 13 .. 13;
      EXTMOD         at 0 range 14 .. 14;
      ASYNCWAIT      at 0 range 15 .. 15;
      Reserved_16_18 at 0 range 16 .. 18;
      CBURSTRW       at 0 range 19 .. 19;
      CCLKEN         at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   ------------------
   -- BTR_Register --
   ------------------

   subtype BTR1_ADDSET_Field is HAL.UInt4;
   subtype BTR1_ADDHLD_Field is HAL.UInt4;
   subtype BTR1_DATAST_Field is HAL.Byte;
   subtype BTR1_BUSTURN_Field is HAL.UInt4;
   subtype BTR1_CLKDIV_Field is HAL.UInt4;
   subtype BTR1_DATLAT_Field is HAL.UInt4;
   subtype BTR1_ACCMOD_Field is HAL.UInt2;

   --  SRAM/NOR-Flash chip-select timing register 1
   type BTR_Register is record
      --  ADDSET
      ADDSET         : BTR1_ADDSET_Field := 16#F#;
      --  ADDHLD
      ADDHLD         : BTR1_ADDHLD_Field := 16#F#;
      --  DATAST
      DATAST         : BTR1_DATAST_Field := 16#FF#;
      --  BUSTURN
      BUSTURN        : BTR1_BUSTURN_Field := 16#F#;
      --  CLKDIV
      CLKDIV         : BTR1_CLKDIV_Field := 16#F#;
      --  DATLAT
      DATLAT         : BTR1_DATLAT_Field := 16#F#;
      --  ACCMOD
      ACCMOD         : BTR1_ACCMOD_Field := 16#3#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#3#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BTR_Register use record
      ADDSET         at 0 range 0 .. 3;
      ADDHLD         at 0 range 4 .. 7;
      DATAST         at 0 range 8 .. 15;
      BUSTURN        at 0 range 16 .. 19;
      CLKDIV         at 0 range 20 .. 23;
      DATLAT         at 0 range 24 .. 27;
      ACCMOD         at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   ------------------
   -- BCR_Register --
   ------------------

   subtype BCR2_MTYP_Field is HAL.UInt2;
   subtype BCR2_MWID_Field is HAL.UInt2;

   --  SRAM/NOR-Flash chip-select control register 2
   type BCR_Register is record
      --  MBKEN
      MBKEN          : Boolean := False;
      --  MUXEN
      MUXEN          : Boolean := False;
      --  MTYP
      MTYP           : BCR2_MTYP_Field := 16#0#;
      --  MWID
      MWID           : BCR2_MWID_Field := 16#1#;
      --  FACCEN
      FACCEN         : Boolean := True;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#1#;
      --  BURSTEN
      BURSTEN        : Boolean := False;
      --  WAITPOL
      WAITPOL        : Boolean := False;
      --  WRAPMOD
      WRAPMOD        : Boolean := False;
      --  WAITCFG
      WAITCFG        : Boolean := False;
      --  WREN
      WREN           : Boolean := True;
      --  WAITEN
      WAITEN         : Boolean := True;
      --  EXTMOD
      EXTMOD         : Boolean := False;
      --  ASYNCWAIT
      ASYNCWAIT      : Boolean := False;
      --  unspecified
      Reserved_16_18 : HAL.UInt3 := 16#0#;
      --  CBURSTRW
      CBURSTRW       : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BCR_Register use record
      MBKEN          at 0 range 0 .. 0;
      MUXEN          at 0 range 1 .. 1;
      MTYP           at 0 range 2 .. 3;
      MWID           at 0 range 4 .. 5;
      FACCEN         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      BURSTEN        at 0 range 8 .. 8;
      WAITPOL        at 0 range 9 .. 9;
      WRAPMOD        at 0 range 10 .. 10;
      WAITCFG        at 0 range 11 .. 11;
      WREN           at 0 range 12 .. 12;
      WAITEN         at 0 range 13 .. 13;
      EXTMOD         at 0 range 14 .. 14;
      ASYNCWAIT      at 0 range 15 .. 15;
      Reserved_16_18 at 0 range 16 .. 18;
      CBURSTRW       at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   ------------------
   -- PCR_Register --
   ------------------

   subtype PCR2_PWID_Field is HAL.UInt2;
   subtype PCR2_TCLR_Field is HAL.UInt4;
   subtype PCR2_TAR_Field is HAL.UInt4;
   subtype PCR2_ECCPS_Field is HAL.UInt3;

   --  PC Card/NAND Flash control register 2
   type PCR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  PWAITEN
      PWAITEN        : Boolean := False;
      --  PBKEN
      PBKEN          : Boolean := False;
      --  PTYP
      PTYP           : Boolean := True;
      --  PWID
      PWID           : PCR2_PWID_Field := 16#1#;
      --  ECCEN
      ECCEN          : Boolean := False;
      --  unspecified
      Reserved_7_8   : HAL.UInt2 := 16#0#;
      --  TCLR
      TCLR           : PCR2_TCLR_Field := 16#0#;
      --  TAR
      TAR            : PCR2_TAR_Field := 16#0#;
      --  ECCPS
      ECCPS          : PCR2_ECCPS_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PCR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      PWAITEN        at 0 range 1 .. 1;
      PBKEN          at 0 range 2 .. 2;
      PTYP           at 0 range 3 .. 3;
      PWID           at 0 range 4 .. 5;
      ECCEN          at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      TCLR           at 0 range 9 .. 12;
      TAR            at 0 range 13 .. 16;
      ECCPS          at 0 range 17 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   --  FIFO status and interrupt register 2
   type SR_Register is record
      --  IRS
      IRS           : Boolean := False;
      --  ILS
      ILS           : Boolean := False;
      --  IFS
      IFS           : Boolean := False;
      --  IREN
      IREN          : Boolean := False;
      --  ILEN
      ILEN          : Boolean := False;
      --  IFEN
      IFEN          : Boolean := False;
      --  Read-only. FEMPT
      FEMPT         : Boolean := True;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      IRS           at 0 range 0 .. 0;
      ILS           at 0 range 1 .. 1;
      IFS           at 0 range 2 .. 2;
      IREN          at 0 range 3 .. 3;
      ILEN          at 0 range 4 .. 4;
      IFEN          at 0 range 5 .. 5;
      FEMPT         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -------------------
   -- PMEM_Register --
   -------------------

   subtype PMEM2_MEMSETx_Field is HAL.Byte;
   subtype PMEM2_MEMWAITx_Field is HAL.Byte;
   subtype PMEM2_MEMHOLDx_Field is HAL.Byte;
   subtype PMEM2_MEMHIZx_Field is HAL.Byte;

   --  Common memory space timing register 2
   type PMEM_Register is record
      --  MEMSETx
      MEMSETx  : PMEM2_MEMSETx_Field := 16#FC#;
      --  MEMWAITx
      MEMWAITx : PMEM2_MEMWAITx_Field := 16#FC#;
      --  MEMHOLDx
      MEMHOLDx : PMEM2_MEMHOLDx_Field := 16#FC#;
      --  MEMHIZx
      MEMHIZx  : PMEM2_MEMHIZx_Field := 16#FC#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMEM_Register use record
      MEMSETx  at 0 range 0 .. 7;
      MEMWAITx at 0 range 8 .. 15;
      MEMHOLDx at 0 range 16 .. 23;
      MEMHIZx  at 0 range 24 .. 31;
   end record;

   -------------------
   -- PATT_Register --
   -------------------

   subtype PATT2_ATTSETx_Field is HAL.Byte;
   subtype PATT2_ATTWAITx_Field is HAL.Byte;
   subtype PATT2_ATTHOLDx_Field is HAL.Byte;
   subtype PATT2_ATTHIZx_Field is HAL.Byte;

   --  Attribute memory space timing register 2
   type PATT_Register is record
      --  ATTSETx
      ATTSETx  : PATT2_ATTSETx_Field := 16#FC#;
      --  ATTWAITx
      ATTWAITx : PATT2_ATTWAITx_Field := 16#FC#;
      --  ATTHOLDx
      ATTHOLDx : PATT2_ATTHOLDx_Field := 16#FC#;
      --  ATTHIZx
      ATTHIZx  : PATT2_ATTHIZx_Field := 16#FC#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PATT_Register use record
      ATTSETx  at 0 range 0 .. 7;
      ATTWAITx at 0 range 8 .. 15;
      ATTHOLDx at 0 range 16 .. 23;
      ATTHIZx  at 0 range 24 .. 31;
   end record;

   -------------------
   -- PIO4_Register --
   -------------------

   subtype PIO4_IOSETx_Field is HAL.Byte;
   subtype PIO4_IOWAITx_Field is HAL.Byte;
   subtype PIO4_IOHOLDx_Field is HAL.Byte;
   subtype PIO4_IOHIZx_Field is HAL.Byte;

   --  I/O space timing register 4
   type PIO4_Register is record
      --  IOSETx
      IOSETx  : PIO4_IOSETx_Field := 16#FC#;
      --  IOWAITx
      IOWAITx : PIO4_IOWAITx_Field := 16#FC#;
      --  IOHOLDx
      IOHOLDx : PIO4_IOHOLDx_Field := 16#FC#;
      --  IOHIZx
      IOHIZx  : PIO4_IOHIZx_Field := 16#FC#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIO4_Register use record
      IOSETx  at 0 range 0 .. 7;
      IOWAITx at 0 range 8 .. 15;
      IOHOLDx at 0 range 16 .. 23;
      IOHIZx  at 0 range 24 .. 31;
   end record;

   -------------------
   -- BWTR_Register --
   -------------------

   subtype BWTR1_ADDSET_Field is HAL.UInt4;
   subtype BWTR1_ADDHLD_Field is HAL.UInt4;
   subtype BWTR1_DATAST_Field is HAL.Byte;
   subtype BWTR1_CLKDIV_Field is HAL.UInt4;
   subtype BWTR1_DATLAT_Field is HAL.UInt4;
   subtype BWTR1_ACCMOD_Field is HAL.UInt2;

   --  SRAM/NOR-Flash write timing registers 1
   type BWTR_Register is record
      --  ADDSET
      ADDSET         : BWTR1_ADDSET_Field := 16#F#;
      --  ADDHLD
      ADDHLD         : BWTR1_ADDHLD_Field := 16#F#;
      --  DATAST
      DATAST         : BWTR1_DATAST_Field := 16#FF#;
      --  unspecified
      Reserved_16_19 : HAL.UInt4 := 16#F#;
      --  CLKDIV
      CLKDIV         : BWTR1_CLKDIV_Field := 16#F#;
      --  DATLAT
      DATLAT         : BWTR1_DATLAT_Field := 16#F#;
      --  ACCMOD
      ACCMOD         : BWTR1_ACCMOD_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BWTR_Register use record
      ADDSET         at 0 range 0 .. 3;
      ADDHLD         at 0 range 4 .. 7;
      DATAST         at 0 range 8 .. 15;
      Reserved_16_19 at 0 range 16 .. 19;
      CLKDIV         at 0 range 20 .. 23;
      DATLAT         at 0 range 24 .. 27;
      ACCMOD         at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   -------------------
   -- SDCR_Register --
   -------------------

   subtype SDCR1_NC_Field is HAL.UInt2;
   subtype SDCR1_NR_Field is HAL.UInt2;
   subtype SDCR1_MWID_Field is HAL.UInt2;
   subtype SDCR1_CAS_Field is HAL.UInt2;
   subtype SDCR1_SDCLK_Field is HAL.UInt2;
   subtype SDCR1_RPIPE_Field is HAL.UInt2;

   --  SDRAM Control Register 1
   type SDCR_Register is record
      --  Number of column address bits
      NC             : SDCR1_NC_Field := 16#0#;
      --  Number of row address bits
      NR             : SDCR1_NR_Field := 16#0#;
      --  Memory data bus width
      MWID           : SDCR1_MWID_Field := 16#1#;
      --  Number of internal banks
      NB             : Boolean := True;
      --  CAS latency
      CAS            : SDCR1_CAS_Field := 16#1#;
      --  Write protection
      WP             : Boolean := True;
      --  SDRAM clock configuration
      SDCLK          : SDCR1_SDCLK_Field := 16#0#;
      --  Burst read
      RBURST         : Boolean := False;
      --  Read pipe
      RPIPE          : SDCR1_RPIPE_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDCR_Register use record
      NC             at 0 range 0 .. 1;
      NR             at 0 range 2 .. 3;
      MWID           at 0 range 4 .. 5;
      NB             at 0 range 6 .. 6;
      CAS            at 0 range 7 .. 8;
      WP             at 0 range 9 .. 9;
      SDCLK          at 0 range 10 .. 11;
      RBURST         at 0 range 12 .. 12;
      RPIPE          at 0 range 13 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -------------------
   -- SDTR_Register --
   -------------------

   subtype SDTR1_TMRD_Field is HAL.UInt4;
   subtype SDTR1_TXSR_Field is HAL.UInt4;
   subtype SDTR1_TRAS_Field is HAL.UInt4;
   subtype SDTR1_TRC_Field is HAL.UInt4;
   subtype SDTR1_TWR_Field is HAL.UInt4;
   subtype SDTR1_TRP_Field is HAL.UInt4;
   subtype SDTR1_TRCD_Field is HAL.UInt4;

   --  SDRAM Timing register 1
   type SDTR_Register is record
      --  Load Mode Register to Active
      TMRD           : SDTR1_TMRD_Field := 16#F#;
      --  Exit self-refresh delay
      TXSR           : SDTR1_TXSR_Field := 16#F#;
      --  Self refresh time
      TRAS           : SDTR1_TRAS_Field := 16#F#;
      --  Row cycle delay
      TRC            : SDTR1_TRC_Field := 16#F#;
      --  Recovery delay
      TWR            : SDTR1_TWR_Field := 16#F#;
      --  Row precharge delay
      TRP            : SDTR1_TRP_Field := 16#F#;
      --  Row to column delay
      TRCD           : SDTR1_TRCD_Field := 16#F#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDTR_Register use record
      TMRD           at 0 range 0 .. 3;
      TXSR           at 0 range 4 .. 7;
      TRAS           at 0 range 8 .. 11;
      TRC            at 0 range 12 .. 15;
      TWR            at 0 range 16 .. 19;
      TRP            at 0 range 20 .. 23;
      TRCD           at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --------------------
   -- SDCMR_Register --
   --------------------

   subtype SDCMR_MODE_Field is HAL.UInt3;

   ---------------
   -- SDCMR.CTB --
   ---------------

   --  SDCMR_CTB array
   type SDCMR_CTB_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for SDCMR_CTB
   type SDCMR_CTB_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CTB as a value
            Val : HAL.UInt2;
         when True =>
            --  CTB as an array
            Arr : SDCMR_CTB_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for SDCMR_CTB_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype SDCMR_NRFS_Field is HAL.UInt4;
   subtype SDCMR_MRD_Field is HAL.UInt13;

   --  SDRAM Command Mode register
   type SDCMR_Register is record
      --  Write-only. Command mode
      MODE           : SDCMR_MODE_Field := 16#0#;
      --  Write-only. Command target bank 2
      CTB            : SDCMR_CTB_Field := (As_Array => False, Val => 16#0#);
      --  Number of Auto-refresh
      NRFS           : SDCMR_NRFS_Field := 16#0#;
      --  Mode Register definition
      MRD            : SDCMR_MRD_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDCMR_Register use record
      MODE           at 0 range 0 .. 2;
      CTB            at 0 range 3 .. 4;
      NRFS           at 0 range 5 .. 8;
      MRD            at 0 range 9 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --------------------
   -- SDRTR_Register --
   --------------------

   subtype SDRTR_COUNT_Field is HAL.UInt13;

   --  SDRAM Refresh Timer register
   type SDRTR_Register is record
      --  Write-only. Clear Refresh error flag
      CRE            : Boolean := False;
      --  Refresh Timer Count
      COUNT          : SDRTR_COUNT_Field := 16#0#;
      --  RES Interrupt Enable
      REIE           : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDRTR_Register use record
      CRE            at 0 range 0 .. 0;
      COUNT          at 0 range 1 .. 13;
      REIE           at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -------------------
   -- SDSR_Register --
   -------------------

   ----------------
   -- SDSR.MODES --
   ----------------

   --  SDSR_MODES array element
   subtype SDSR_MODES_Element is HAL.UInt2;

   --  SDSR_MODES array
   type SDSR_MODES_Field_Array is array (1 .. 2) of SDSR_MODES_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for SDSR_MODES
   type SDSR_MODES_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MODES as a value
            Val : HAL.UInt4;
         when True =>
            --  MODES as an array
            Arr : SDSR_MODES_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for SDSR_MODES_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  SDRAM Status register
   type SDSR_Register is record
      --  Read-only. Refresh error flag
      RE            : Boolean := False;
      --  Read-only. Status Mode for Bank 1
      MODES         : SDSR_MODES_Field := (As_Array => False, Val => 16#0#);
      --  Read-only. Busy status
      BUSY          : Boolean := False;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDSR_Register use record
      RE            at 0 range 0 .. 0;
      MODES         at 0 range 1 .. 4;
      BUSY          at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Flexible memory controller
   type FMC_Peripheral is record
      --  SRAM/NOR-Flash chip-select control register 1
      BCR1  : BCR1_Register;
      --  SRAM/NOR-Flash chip-select timing register 1
      BTR1  : BTR_Register;
      --  SRAM/NOR-Flash chip-select control register 2
      BCR2  : BCR_Register;
      --  SRAM/NOR-Flash chip-select timing register 2
      BTR2  : BTR_Register;
      --  SRAM/NOR-Flash chip-select control register 3
      BCR3  : BCR_Register;
      --  SRAM/NOR-Flash chip-select timing register 3
      BTR3  : BTR_Register;
      --  SRAM/NOR-Flash chip-select control register 4
      BCR4  : BCR_Register;
      --  SRAM/NOR-Flash chip-select timing register 4
      BTR4  : BTR_Register;
      --  PC Card/NAND Flash control register 2
      PCR2  : PCR_Register;
      --  FIFO status and interrupt register 2
      SR2   : SR_Register;
      --  Common memory space timing register 2
      PMEM2 : PMEM_Register;
      --  Attribute memory space timing register 2
      PATT2 : PATT_Register;
      --  ECC result register 2
      ECCR2 : HAL.Word;
      --  PC Card/NAND Flash control register 3
      PCR3  : PCR_Register;
      --  FIFO status and interrupt register 3
      SR3   : SR_Register;
      --  Common memory space timing register 3
      PMEM3 : PMEM_Register;
      --  Attribute memory space timing register 3
      PATT3 : PATT_Register;
      --  ECC result register 3
      ECCR3 : HAL.Word;
      --  PC Card/NAND Flash control register 4
      PCR4  : PCR_Register;
      --  FIFO status and interrupt register 4
      SR4   : SR_Register;
      --  Common memory space timing register 4
      PMEM4 : PMEM_Register;
      --  Attribute memory space timing register 4
      PATT4 : PATT_Register;
      --  I/O space timing register 4
      PIO4  : PIO4_Register;
      --  SRAM/NOR-Flash write timing registers 1
      BWTR1 : BWTR_Register;
      --  SRAM/NOR-Flash write timing registers 2
      BWTR2 : BWTR_Register;
      --  SRAM/NOR-Flash write timing registers 3
      BWTR3 : BWTR_Register;
      --  SRAM/NOR-Flash write timing registers 4
      BWTR4 : BWTR_Register;
      --  SDRAM Control Register 1
      SDCR1 : SDCR_Register;
      --  SDRAM Control Register 2
      SDCR2 : SDCR_Register;
      --  SDRAM Timing register 1
      SDTR1 : SDTR_Register;
      --  SDRAM Timing register 2
      SDTR2 : SDTR_Register;
      --  SDRAM Command Mode register
      SDCMR : SDCMR_Register;
      --  SDRAM Refresh Timer register
      SDRTR : SDRTR_Register;
      --  SDRAM Status register
      SDSR  : SDSR_Register;
   end record
     with Volatile;

   for FMC_Peripheral use record
      BCR1  at 0 range 0 .. 31;
      BTR1  at 4 range 0 .. 31;
      BCR2  at 8 range 0 .. 31;
      BTR2  at 12 range 0 .. 31;
      BCR3  at 16 range 0 .. 31;
      BTR3  at 20 range 0 .. 31;
      BCR4  at 24 range 0 .. 31;
      BTR4  at 28 range 0 .. 31;
      PCR2  at 96 range 0 .. 31;
      SR2   at 100 range 0 .. 31;
      PMEM2 at 104 range 0 .. 31;
      PATT2 at 108 range 0 .. 31;
      ECCR2 at 116 range 0 .. 31;
      PCR3  at 128 range 0 .. 31;
      SR3   at 132 range 0 .. 31;
      PMEM3 at 136 range 0 .. 31;
      PATT3 at 140 range 0 .. 31;
      ECCR3 at 148 range 0 .. 31;
      PCR4  at 160 range 0 .. 31;
      SR4   at 164 range 0 .. 31;
      PMEM4 at 168 range 0 .. 31;
      PATT4 at 172 range 0 .. 31;
      PIO4  at 176 range 0 .. 31;
      BWTR1 at 260 range 0 .. 31;
      BWTR2 at 268 range 0 .. 31;
      BWTR3 at 276 range 0 .. 31;
      BWTR4 at 284 range 0 .. 31;
      SDCR1 at 320 range 0 .. 31;
      SDCR2 at 324 range 0 .. 31;
      SDTR1 at 328 range 0 .. 31;
      SDTR2 at 332 range 0 .. 31;
      SDCMR at 336 range 0 .. 31;
      SDRTR at 340 range 0 .. 31;
      SDSR  at 344 range 0 .. 31;
   end record;

   --  Flexible memory controller
   FMC_Periph : aliased FMC_Peripheral
     with Import, Address => FMC_Base;

end STM32_SVD.FSMC;
