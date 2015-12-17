--  Automatically generated from CMSIS-SVD description file by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with System;

package STM32_SVD.Ethernet is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype MACCR_RE_Field is STM32_SVD.Bit;

   subtype MACCR_TE_Field is STM32_SVD.Bit;

   subtype MACCR_DC_Field is STM32_SVD.Bit;

   subtype MACCR_BL_Field is STM32_SVD.UInt2;

   subtype MACCR_APCS_Field is STM32_SVD.Bit;

   subtype MACCR_RD_Field is STM32_SVD.Bit;

   subtype MACCR_IPCO_Field is STM32_SVD.Bit;

   subtype MACCR_DM_Field is STM32_SVD.Bit;

   subtype MACCR_LM_Field is STM32_SVD.Bit;

   subtype MACCR_ROD_Field is STM32_SVD.Bit;

   subtype MACCR_FES_Field is STM32_SVD.Bit;

   subtype MACCR_CSD_Field is STM32_SVD.Bit;

   subtype MACCR_IFG_Field is STM32_SVD.UInt3;

   subtype MACCR_JD_Field is STM32_SVD.Bit;

   subtype MACCR_WD_Field is STM32_SVD.Bit;

   subtype MACCR_CSTF_Field is STM32_SVD.Bit;

   --  Ethernet MAC configuration register
   type MACCR_Register is record
      --  unspecified
      Reserved_0_1   : STM32_SVD.UInt2 := 16#0#;
      --  RE
      RE             : MACCR_RE_Field := 16#0#;
      --  TE
      TE             : MACCR_TE_Field := 16#0#;
      --  DC
      DC             : MACCR_DC_Field := 16#0#;
      --  BL
      BL             : MACCR_BL_Field := 16#0#;
      --  APCS
      APCS           : MACCR_APCS_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : STM32_SVD.Bit := 16#0#;
      --  RD
      RD             : MACCR_RD_Field := 16#0#;
      --  IPCO
      IPCO           : MACCR_IPCO_Field := 16#0#;
      --  DM
      DM             : MACCR_DM_Field := 16#0#;
      --  LM
      LM             : MACCR_LM_Field := 16#0#;
      --  ROD
      ROD            : MACCR_ROD_Field := 16#0#;
      --  FES
      FES            : MACCR_FES_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#1#;
      --  CSD
      CSD            : MACCR_CSD_Field := 16#0#;
      --  IFG
      IFG            : MACCR_IFG_Field := 16#0#;
      --  unspecified
      Reserved_20_21 : STM32_SVD.UInt2 := 16#0#;
      --  JD
      JD             : MACCR_JD_Field := 16#0#;
      --  WD
      WD             : MACCR_WD_Field := 16#0#;
      --  unspecified
      Reserved_24_24 : STM32_SVD.Bit := 16#0#;
      --  CSTF
      CSTF           : MACCR_CSTF_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : STM32_SVD.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACCR_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      RE             at 0 range 2 .. 2;
      TE             at 0 range 3 .. 3;
      DC             at 0 range 4 .. 4;
      BL             at 0 range 5 .. 6;
      APCS           at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      RD             at 0 range 9 .. 9;
      IPCO           at 0 range 10 .. 10;
      DM             at 0 range 11 .. 11;
      LM             at 0 range 12 .. 12;
      ROD            at 0 range 13 .. 13;
      FES            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CSD            at 0 range 16 .. 16;
      IFG            at 0 range 17 .. 19;
      Reserved_20_21 at 0 range 20 .. 21;
      JD             at 0 range 22 .. 22;
      WD             at 0 range 23 .. 23;
      Reserved_24_24 at 0 range 24 .. 24;
      CSTF           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MACFFR_PM_Field is STM32_SVD.Bit;

   subtype MACFFR_HU_Field is STM32_SVD.Bit;

   subtype MACFFR_HM_Field is STM32_SVD.Bit;

   subtype MACFFR_DAIF_Field is STM32_SVD.Bit;

   subtype MACFFR_RAM_Field is STM32_SVD.Bit;

   subtype MACFFR_BFD_Field is STM32_SVD.Bit;

   subtype MACFFR_PCF_Field is STM32_SVD.Bit;

   subtype MACFFR_SAIF_Field is STM32_SVD.Bit;

   subtype MACFFR_SAF_Field is STM32_SVD.Bit;

   subtype MACFFR_HPF_Field is STM32_SVD.Bit;

   subtype MACFFR_RA_Field is STM32_SVD.Bit;

   --  Ethernet MAC frame filter register
   type MACFFR_Register is record
      --  no description available
      PM             : MACFFR_PM_Field := 16#0#;
      --  no description available
      HU             : MACFFR_HU_Field := 16#0#;
      --  no description available
      HM             : MACFFR_HM_Field := 16#0#;
      --  no description available
      DAIF           : MACFFR_DAIF_Field := 16#0#;
      --  no description available
      RAM            : MACFFR_RAM_Field := 16#0#;
      --  no description available
      BFD            : MACFFR_BFD_Field := 16#0#;
      --  no description available
      PCF            : MACFFR_PCF_Field := 16#0#;
      --  no description available
      SAIF           : MACFFR_SAIF_Field := 16#0#;
      --  no description available
      SAF            : MACFFR_SAF_Field := 16#0#;
      --  no description available
      HPF            : MACFFR_HPF_Field := 16#0#;
      --  unspecified
      Reserved_10_30 : STM32_SVD.UInt21 := 16#0#;
      --  no description available
      RA             : MACFFR_RA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACFFR_Register use record
      PM             at 0 range 0 .. 0;
      HU             at 0 range 1 .. 1;
      HM             at 0 range 2 .. 2;
      DAIF           at 0 range 3 .. 3;
      RAM            at 0 range 4 .. 4;
      BFD            at 0 range 5 .. 5;
      PCF            at 0 range 6 .. 6;
      SAIF           at 0 range 7 .. 7;
      SAF            at 0 range 8 .. 8;
      HPF            at 0 range 9 .. 9;
      Reserved_10_30 at 0 range 10 .. 30;
      RA             at 0 range 31 .. 31;
   end record;

   subtype MACMIIAR_MB_Field is STM32_SVD.Bit;

   subtype MACMIIAR_MW_Field is STM32_SVD.Bit;

   subtype MACMIIAR_CR_Field is STM32_SVD.UInt3;

   subtype MACMIIAR_MR_Field is STM32_SVD.UInt5;

   subtype MACMIIAR_PA_Field is STM32_SVD.UInt5;

   --  Ethernet MAC MII address register
   type MACMIIAR_Register is record
      --  no description available
      MB             : MACMIIAR_MB_Field := 16#0#;
      --  no description available
      MW             : MACMIIAR_MW_Field := 16#0#;
      --  no description available
      CR             : MACMIIAR_CR_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : STM32_SVD.Bit := 16#0#;
      --  no description available
      MR             : MACMIIAR_MR_Field := 16#0#;
      --  no description available
      PA             : MACMIIAR_PA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACMIIAR_Register use record
      MB             at 0 range 0 .. 0;
      MW             at 0 range 1 .. 1;
      CR             at 0 range 2 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      MR             at 0 range 6 .. 10;
      PA             at 0 range 11 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MACMIIDR_TD_Field is STM32_SVD.Short;

   --  Ethernet MAC MII data register
   type MACMIIDR_Register is record
      --  no description available
      TD             : MACMIIDR_TD_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACMIIDR_Register use record
      TD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MACFCR_FCB_Field is STM32_SVD.Bit;

   subtype MACFCR_TFCE_Field is STM32_SVD.Bit;

   subtype MACFCR_RFCE_Field is STM32_SVD.Bit;

   subtype MACFCR_UPFD_Field is STM32_SVD.Bit;

   subtype MACFCR_PLT_Field is STM32_SVD.UInt2;

   subtype MACFCR_ZQPD_Field is STM32_SVD.Bit;

   subtype MACFCR_PT_Field is STM32_SVD.Short;

   --  Ethernet MAC flow control register
   type MACFCR_Register is record
      --  no description available
      FCB           : MACFCR_FCB_Field := 16#0#;
      --  no description available
      TFCE          : MACFCR_TFCE_Field := 16#0#;
      --  no description available
      RFCE          : MACFCR_RFCE_Field := 16#0#;
      --  no description available
      UPFD          : MACFCR_UPFD_Field := 16#0#;
      --  no description available
      PLT           : MACFCR_PLT_Field := 16#0#;
      --  unspecified
      Reserved_6_6  : STM32_SVD.Bit := 16#0#;
      --  no description available
      ZQPD          : MACFCR_ZQPD_Field := 16#0#;
      --  unspecified
      Reserved_8_15 : STM32_SVD.Byte := 16#0#;
      --  no description available
      PT            : MACFCR_PT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACFCR_Register use record
      FCB           at 0 range 0 .. 0;
      TFCE          at 0 range 1 .. 1;
      RFCE          at 0 range 2 .. 2;
      UPFD          at 0 range 3 .. 3;
      PLT           at 0 range 4 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      ZQPD          at 0 range 7 .. 7;
      Reserved_8_15 at 0 range 8 .. 15;
      PT            at 0 range 16 .. 31;
   end record;

   subtype MACVLANTR_VLANTI_Field is STM32_SVD.Short;

   subtype MACVLANTR_VLANTC_Field is STM32_SVD.Bit;

   --  Ethernet MAC VLAN tag register
   type MACVLANTR_Register is record
      --  no description available
      VLANTI         : MACVLANTR_VLANTI_Field := 16#0#;
      --  no description available
      VLANTC         : MACVLANTR_VLANTC_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : STM32_SVD.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACVLANTR_Register use record
      VLANTI         at 0 range 0 .. 15;
      VLANTC         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MACPMTCSR_PD_Field is STM32_SVD.Bit;

   subtype MACPMTCSR_MPE_Field is STM32_SVD.Bit;

   subtype MACPMTCSR_WFE_Field is STM32_SVD.Bit;

   subtype MACPMTCSR_MPR_Field is STM32_SVD.Bit;

   subtype MACPMTCSR_WFR_Field is STM32_SVD.Bit;

   subtype MACPMTCSR_GU_Field is STM32_SVD.Bit;

   subtype MACPMTCSR_WFFRPR_Field is STM32_SVD.Bit;

   --  Ethernet MAC PMT control and status register
   type MACPMTCSR_Register is record
      --  no description available
      PD             : MACPMTCSR_PD_Field := 16#0#;
      --  no description available
      MPE            : MACPMTCSR_MPE_Field := 16#0#;
      --  no description available
      WFE            : MACPMTCSR_WFE_Field := 16#0#;
      --  unspecified
      Reserved_3_4   : STM32_SVD.UInt2 := 16#0#;
      --  no description available
      MPR            : MACPMTCSR_MPR_Field := 16#0#;
      --  no description available
      WFR            : MACPMTCSR_WFR_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : STM32_SVD.UInt2 := 16#0#;
      --  no description available
      GU             : MACPMTCSR_GU_Field := 16#0#;
      --  unspecified
      Reserved_10_30 : STM32_SVD.UInt21 := 16#0#;
      --  no description available
      WFFRPR         : MACPMTCSR_WFFRPR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACPMTCSR_Register use record
      PD             at 0 range 0 .. 0;
      MPE            at 0 range 1 .. 1;
      WFE            at 0 range 2 .. 2;
      Reserved_3_4   at 0 range 3 .. 4;
      MPR            at 0 range 5 .. 5;
      WFR            at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      GU             at 0 range 9 .. 9;
      Reserved_10_30 at 0 range 10 .. 30;
      WFFRPR         at 0 range 31 .. 31;
   end record;

   subtype MACDBGR_CR_Field is STM32_SVD.Bit;

   subtype MACDBGR_CSR_Field is STM32_SVD.Bit;

   subtype MACDBGR_ROR_Field is STM32_SVD.Bit;

   subtype MACDBGR_MCF_Field is STM32_SVD.Bit;

   subtype MACDBGR_MCP_Field is STM32_SVD.Bit;

   subtype MACDBGR_MCFHP_Field is STM32_SVD.Bit;

   --  Ethernet MAC debug register
   type MACDBGR_Register is record
      --  CR
      CR            : MACDBGR_CR_Field := 16#0#;
      --  CSR
      CSR           : MACDBGR_CSR_Field := 16#0#;
      --  ROR
      ROR           : MACDBGR_ROR_Field := 16#0#;
      --  MCF
      MCF           : MACDBGR_MCF_Field := 16#0#;
      --  MCP
      MCP           : MACDBGR_MCP_Field := 16#0#;
      --  MCFHP
      MCFHP         : MACDBGR_MCFHP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32_SVD.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACDBGR_Register use record
      CR            at 0 range 0 .. 0;
      CSR           at 0 range 1 .. 1;
      ROR           at 0 range 2 .. 2;
      MCF           at 0 range 3 .. 3;
      MCP           at 0 range 4 .. 4;
      MCFHP         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype MACSR_PMTS_Field is STM32_SVD.Bit;

   subtype MACSR_MMCS_Field is STM32_SVD.Bit;

   subtype MACSR_MMCRS_Field is STM32_SVD.Bit;

   subtype MACSR_MMCTS_Field is STM32_SVD.Bit;

   subtype MACSR_TSTS_Field is STM32_SVD.Bit;

   --  Ethernet MAC interrupt status register
   type MACSR_Register is record
      --  unspecified
      Reserved_0_2   : STM32_SVD.UInt3 := 16#0#;
      --  no description available
      PMTS           : MACSR_PMTS_Field := 16#0#;
      --  no description available
      MMCS           : MACSR_MMCS_Field := 16#0#;
      --  no description available
      MMCRS          : MACSR_MMCRS_Field := 16#0#;
      --  no description available
      MMCTS          : MACSR_MMCTS_Field := 16#0#;
      --  unspecified
      Reserved_7_8   : STM32_SVD.UInt2 := 16#0#;
      --  no description available
      TSTS           : MACSR_TSTS_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : STM32_SVD.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACSR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      PMTS           at 0 range 3 .. 3;
      MMCS           at 0 range 4 .. 4;
      MMCRS          at 0 range 5 .. 5;
      MMCTS          at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      TSTS           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype MACIMR_PMTIM_Field is STM32_SVD.Bit;

   subtype MACIMR_TSTIM_Field is STM32_SVD.Bit;

   --  Ethernet MAC interrupt mask register
   type MACIMR_Register is record
      --  unspecified
      Reserved_0_2   : STM32_SVD.UInt3 := 16#0#;
      --  no description available
      PMTIM          : MACIMR_PMTIM_Field := 16#0#;
      --  unspecified
      Reserved_4_8   : STM32_SVD.UInt5 := 16#0#;
      --  no description available
      TSTIM          : MACIMR_TSTIM_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : STM32_SVD.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACIMR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      PMTIM          at 0 range 3 .. 3;
      Reserved_4_8   at 0 range 4 .. 8;
      TSTIM          at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype MACA0HR_MACA0H_Field is STM32_SVD.Short;

   subtype MACA0HR_MO_Field is STM32_SVD.Bit;

   --  Ethernet MAC address 0 high register
   type MACA0HR_Register is record
      --  MAC address0 high
      MACA0H         : MACA0HR_MACA0H_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_30 : STM32_SVD.UInt15 := 16#10#;
      --  Always 1
      MO             : MACA0HR_MO_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACA0HR_Register use record
      MACA0H         at 0 range 0 .. 15;
      Reserved_16_30 at 0 range 16 .. 30;
      MO             at 0 range 31 .. 31;
   end record;

   subtype MACA1HR_MACA1H_Field is STM32_SVD.Short;

   subtype MACA1HR_MBC_Field is STM32_SVD.UInt6;

   subtype MACA1HR_SA_Field is STM32_SVD.Bit;

   subtype MACA1HR_AE_Field is STM32_SVD.Bit;

   --  Ethernet MAC address 1 high register
   type MACA1HR_Register is record
      --  no description available
      MACA1H         : MACA1HR_MACA1H_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_23 : STM32_SVD.Byte := 16#0#;
      --  no description available
      MBC            : MACA1HR_MBC_Field := 16#0#;
      --  no description available
      SA             : MACA1HR_SA_Field := 16#0#;
      --  no description available
      AE             : MACA1HR_AE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACA1HR_Register use record
      MACA1H         at 0 range 0 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      MBC            at 0 range 24 .. 29;
      SA             at 0 range 30 .. 30;
      AE             at 0 range 31 .. 31;
   end record;

   subtype MACA2HR_MAC2AH_Field is STM32_SVD.Short;

   subtype MACA2HR_MBC_Field is STM32_SVD.UInt6;

   subtype MACA2HR_SA_Field is STM32_SVD.Bit;

   subtype MACA2HR_AE_Field is STM32_SVD.Bit;

   --  Ethernet MAC address 2 high register
   type MACA2HR_Register is record
      --  no description available
      MAC2AH         : MACA2HR_MAC2AH_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_23 : STM32_SVD.Byte := 16#0#;
      --  no description available
      MBC            : MACA2HR_MBC_Field := 16#0#;
      --  no description available
      SA             : MACA2HR_SA_Field := 16#0#;
      --  no description available
      AE             : MACA2HR_AE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACA2HR_Register use record
      MAC2AH         at 0 range 0 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      MBC            at 0 range 24 .. 29;
      SA             at 0 range 30 .. 30;
      AE             at 0 range 31 .. 31;
   end record;

   subtype MACA2LR_MACA2L_Field is STM32_SVD.UInt31;

   --  Ethernet MAC address 2 low register
   type MACA2LR_Register is record
      --  no description available
      MACA2L         : MACA2LR_MACA2L_Field := 16#7FFFFFFF#;
      --  unspecified
      Reserved_31_31 : STM32_SVD.Bit := 16#1#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACA2LR_Register use record
      MACA2L         at 0 range 0 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype MACA3HR_MACA3H_Field is STM32_SVD.Short;

   subtype MACA3HR_MBC_Field is STM32_SVD.UInt6;

   subtype MACA3HR_SA_Field is STM32_SVD.Bit;

   subtype MACA3HR_AE_Field is STM32_SVD.Bit;

   --  Ethernet MAC address 3 high register
   type MACA3HR_Register is record
      --  no description available
      MACA3H         : MACA3HR_MACA3H_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_23 : STM32_SVD.Byte := 16#0#;
      --  no description available
      MBC            : MACA3HR_MBC_Field := 16#0#;
      --  no description available
      SA             : MACA3HR_SA_Field := 16#0#;
      --  no description available
      AE             : MACA3HR_AE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MACA3HR_Register use record
      MACA3H         at 0 range 0 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      MBC            at 0 range 24 .. 29;
      SA             at 0 range 30 .. 30;
      AE             at 0 range 31 .. 31;
   end record;

   subtype MMCCR_CR_Field is STM32_SVD.Bit;

   subtype MMCCR_CSR_Field is STM32_SVD.Bit;

   subtype MMCCR_ROR_Field is STM32_SVD.Bit;

   subtype MMCCR_MCF_Field is STM32_SVD.Bit;

   subtype MMCCR_MCP_Field is STM32_SVD.Bit;

   subtype MMCCR_MCFHP_Field is STM32_SVD.Bit;

   --  Ethernet MMC control register
   type MMCCR_Register is record
      --  no description available
      CR            : MMCCR_CR_Field := 16#0#;
      --  no description available
      CSR           : MMCCR_CSR_Field := 16#0#;
      --  no description available
      ROR           : MMCCR_ROR_Field := 16#0#;
      --  no description available
      MCF           : MMCCR_MCF_Field := 16#0#;
      --  no description available
      MCP           : MMCCR_MCP_Field := 16#0#;
      --  no description available
      MCFHP         : MMCCR_MCFHP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32_SVD.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MMCCR_Register use record
      CR            at 0 range 0 .. 0;
      CSR           at 0 range 1 .. 1;
      ROR           at 0 range 2 .. 2;
      MCF           at 0 range 3 .. 3;
      MCP           at 0 range 4 .. 4;
      MCFHP         at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype MMCRIR_RFCES_Field is STM32_SVD.Bit;

   subtype MMCRIR_RFAES_Field is STM32_SVD.Bit;

   subtype MMCRIR_RGUFS_Field is STM32_SVD.Bit;

   --  Ethernet MMC receive interrupt register
   type MMCRIR_Register is record
      --  unspecified
      Reserved_0_4   : STM32_SVD.UInt5 := 16#0#;
      --  no description available
      RFCES          : MMCRIR_RFCES_Field := 16#0#;
      --  no description available
      RFAES          : MMCRIR_RFAES_Field := 16#0#;
      --  unspecified
      Reserved_7_16  : STM32_SVD.UInt10 := 16#0#;
      --  no description available
      RGUFS          : MMCRIR_RGUFS_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : STM32_SVD.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MMCRIR_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      RFCES          at 0 range 5 .. 5;
      RFAES          at 0 range 6 .. 6;
      Reserved_7_16  at 0 range 7 .. 16;
      RGUFS          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype MMCTIR_TGFSCS_Field is STM32_SVD.Bit;

   subtype MMCTIR_TGFMSCS_Field is STM32_SVD.Bit;

   subtype MMCTIR_TGFS_Field is STM32_SVD.Bit;

   --  Ethernet MMC transmit interrupt register
   type MMCTIR_Register is record
      --  unspecified
      Reserved_0_13  : STM32_SVD.UInt14 := 16#0#;
      --  no description available
      TGFSCS         : MMCTIR_TGFSCS_Field := 16#0#;
      --  no description available
      TGFMSCS        : MMCTIR_TGFMSCS_Field := 16#0#;
      --  unspecified
      Reserved_16_20 : STM32_SVD.UInt5 := 16#0#;
      --  no description available
      TGFS           : MMCTIR_TGFS_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : STM32_SVD.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MMCTIR_Register use record
      Reserved_0_13  at 0 range 0 .. 13;
      TGFSCS         at 0 range 14 .. 14;
      TGFMSCS        at 0 range 15 .. 15;
      Reserved_16_20 at 0 range 16 .. 20;
      TGFS           at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype MMCRIMR_RFCEM_Field is STM32_SVD.Bit;

   subtype MMCRIMR_RFAEM_Field is STM32_SVD.Bit;

   subtype MMCRIMR_RGUFM_Field is STM32_SVD.Bit;

   --  Ethernet MMC receive interrupt mask register
   type MMCRIMR_Register is record
      --  unspecified
      Reserved_0_4   : STM32_SVD.UInt5 := 16#0#;
      --  no description available
      RFCEM          : MMCRIMR_RFCEM_Field := 16#0#;
      --  no description available
      RFAEM          : MMCRIMR_RFAEM_Field := 16#0#;
      --  unspecified
      Reserved_7_16  : STM32_SVD.UInt10 := 16#0#;
      --  no description available
      RGUFM          : MMCRIMR_RGUFM_Field := 16#0#;
      --  unspecified
      Reserved_18_31 : STM32_SVD.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MMCRIMR_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      RFCEM          at 0 range 5 .. 5;
      RFAEM          at 0 range 6 .. 6;
      Reserved_7_16  at 0 range 7 .. 16;
      RGUFM          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype MMCTIMR_TGFSCM_Field is STM32_SVD.Bit;

   subtype MMCTIMR_TGFMSCM_Field is STM32_SVD.Bit;

   subtype MMCTIMR_TGFM_Field is STM32_SVD.Bit;

   --  Ethernet MMC transmit interrupt mask register
   type MMCTIMR_Register is record
      --  unspecified
      Reserved_0_13  : STM32_SVD.UInt14 := 16#0#;
      --  no description available
      TGFSCM         : MMCTIMR_TGFSCM_Field := 16#0#;
      --  no description available
      TGFMSCM        : MMCTIMR_TGFMSCM_Field := 16#0#;
      --  no description available
      TGFM           : MMCTIMR_TGFM_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : STM32_SVD.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MMCTIMR_Register use record
      Reserved_0_13  at 0 range 0 .. 13;
      TGFSCM         at 0 range 14 .. 14;
      TGFMSCM        at 0 range 15 .. 15;
      TGFM           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype PTPTSCR_TSE_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSFCU_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSSTI_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSSTU_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSITE_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TTSARU_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSSARFE_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSSSR_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSPTPPSV2E_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSSPTPOEFE_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSSIPV6FE_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSSIPV4FE_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSSEME_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSSMRME_Field is STM32_SVD.Bit;

   subtype PTPTSCR_TSCNT_Field is STM32_SVD.UInt2;

   subtype PTPTSCR_TSPFFMAE_Field is STM32_SVD.Bit;

   --  Ethernet PTP time stamp control register
   type PTPTSCR_Register is record
      --  no description available
      TSE            : PTPTSCR_TSE_Field := 16#0#;
      --  no description available
      TSFCU          : PTPTSCR_TSFCU_Field := 16#0#;
      --  no description available
      TSSTI          : PTPTSCR_TSSTI_Field := 16#0#;
      --  no description available
      TSSTU          : PTPTSCR_TSSTU_Field := 16#0#;
      --  no description available
      TSITE          : PTPTSCR_TSITE_Field := 16#0#;
      --  no description available
      TTSARU         : PTPTSCR_TTSARU_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : STM32_SVD.UInt2 := 16#0#;
      --  no description available
      TSSARFE        : PTPTSCR_TSSARFE_Field := 16#0#;
      --  no description available
      TSSSR          : PTPTSCR_TSSSR_Field := 16#0#;
      --  no description available
      TSPTPPSV2E     : PTPTSCR_TSPTPPSV2E_Field := 16#0#;
      --  no description available
      TSSPTPOEFE     : PTPTSCR_TSSPTPOEFE_Field := 16#0#;
      --  no description available
      TSSIPV6FE      : PTPTSCR_TSSIPV6FE_Field := 16#0#;
      --  no description available
      TSSIPV4FE      : PTPTSCR_TSSIPV4FE_Field := 16#1#;
      --  no description available
      TSSEME         : PTPTSCR_TSSEME_Field := 16#0#;
      --  no description available
      TSSMRME        : PTPTSCR_TSSMRME_Field := 16#0#;
      --  no description available
      TSCNT          : PTPTSCR_TSCNT_Field := 16#0#;
      --  no description available
      TSPFFMAE       : PTPTSCR_TSPFFMAE_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : STM32_SVD.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PTPTSCR_Register use record
      TSE            at 0 range 0 .. 0;
      TSFCU          at 0 range 1 .. 1;
      TSSTI          at 0 range 2 .. 2;
      TSSTU          at 0 range 3 .. 3;
      TSITE          at 0 range 4 .. 4;
      TTSARU         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      TSSARFE        at 0 range 8 .. 8;
      TSSSR          at 0 range 9 .. 9;
      TSPTPPSV2E     at 0 range 10 .. 10;
      TSSPTPOEFE     at 0 range 11 .. 11;
      TSSIPV6FE      at 0 range 12 .. 12;
      TSSIPV4FE      at 0 range 13 .. 13;
      TSSEME         at 0 range 14 .. 14;
      TSSMRME        at 0 range 15 .. 15;
      TSCNT          at 0 range 16 .. 17;
      TSPFFMAE       at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype PTPSSIR_STSSI_Field is STM32_SVD.Byte;

   --  Ethernet PTP subsecond increment register
   type PTPSSIR_Register is record
      --  no description available
      STSSI         : PTPSSIR_STSSI_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PTPSSIR_Register use record
      STSSI         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype PTPTSLR_STSS_Field is STM32_SVD.UInt31;

   subtype PTPTSLR_STPNS_Field is STM32_SVD.Bit;

   --  Ethernet PTP time stamp low register
   type PTPTSLR_Register is record
      --  no description available
      STSS  : PTPTSLR_STSS_Field := 16#0#;
      --  no description available
      STPNS : PTPTSLR_STPNS_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PTPTSLR_Register use record
      STSS  at 0 range 0 .. 30;
      STPNS at 0 range 31 .. 31;
   end record;

   subtype PTPTSLUR_TSUSS_Field is STM32_SVD.UInt31;

   subtype PTPTSLUR_TSUPNS_Field is STM32_SVD.Bit;

   --  Ethernet PTP time stamp low update register
   type PTPTSLUR_Register is record
      --  no description available
      TSUSS  : PTPTSLUR_TSUSS_Field := 16#0#;
      --  no description available
      TSUPNS : PTPTSLUR_TSUPNS_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PTPTSLUR_Register use record
      TSUSS  at 0 range 0 .. 30;
      TSUPNS at 0 range 31 .. 31;
   end record;

   subtype PTPTSSR_TSSO_Field is STM32_SVD.Bit;

   subtype PTPTSSR_TSTTR_Field is STM32_SVD.Bit;

   --  Ethernet PTP time stamp status register
   type PTPTSSR_Register is record
      --  no description available
      TSSO          : PTPTSSR_TSSO_Field := 16#0#;
      --  no description available
      TSTTR         : PTPTSSR_TSTTR_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PTPTSSR_Register use record
      TSSO          at 0 range 0 .. 0;
      TSTTR         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype PTPPPSCR_TSSO_Field is STM32_SVD.Bit;

   subtype PTPPPSCR_TSTTR_Field is STM32_SVD.Bit;

   --  Ethernet PTP PPS control register
   type PTPPPSCR_Register is record
      --  TSSO
      TSSO          : PTPPPSCR_TSSO_Field := 16#0#;
      --  TSTTR
      TSTTR         : PTPPPSCR_TSTTR_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PTPPPSCR_Register use record
      TSSO          at 0 range 0 .. 0;
      TSTTR         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype DMABMR_SR_Field is STM32_SVD.Bit;

   subtype DMABMR_DA_Field is STM32_SVD.Bit;

   subtype DMABMR_DSL_Field is STM32_SVD.UInt5;

   subtype DMABMR_EDFE_Field is STM32_SVD.Bit;

   subtype DMABMR_PBL_Field is STM32_SVD.UInt6;

   subtype DMABMR_RTPR_Field is STM32_SVD.UInt2;

   subtype DMABMR_FB_Field is STM32_SVD.Bit;

   subtype DMABMR_RDP_Field is STM32_SVD.UInt6;

   subtype DMABMR_USP_Field is STM32_SVD.Bit;

   subtype DMABMR_FPM_Field is STM32_SVD.Bit;

   subtype DMABMR_AAB_Field is STM32_SVD.Bit;

   subtype DMABMR_MB_Field is STM32_SVD.Bit;

   --  Ethernet DMA bus mode register
   type DMABMR_Register is record
      --  no description available
      SR             : DMABMR_SR_Field := 16#1#;
      --  no description available
      DA             : DMABMR_DA_Field := 16#0#;
      --  no description available
      DSL            : DMABMR_DSL_Field := 16#0#;
      --  no description available
      EDFE           : DMABMR_EDFE_Field := 16#0#;
      --  no description available
      PBL            : DMABMR_PBL_Field := 16#21#;
      --  no description available
      RTPR           : DMABMR_RTPR_Field := 16#0#;
      --  no description available
      FB             : DMABMR_FB_Field := 16#0#;
      --  no description available
      RDP            : DMABMR_RDP_Field := 16#0#;
      --  no description available
      USP            : DMABMR_USP_Field := 16#0#;
      --  no description available
      FPM            : DMABMR_FPM_Field := 16#0#;
      --  no description available
      AAB            : DMABMR_AAB_Field := 16#0#;
      --  no description available
      MB             : DMABMR_MB_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32_SVD.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMABMR_Register use record
      SR             at 0 range 0 .. 0;
      DA             at 0 range 1 .. 1;
      DSL            at 0 range 2 .. 6;
      EDFE           at 0 range 7 .. 7;
      PBL            at 0 range 8 .. 13;
      RTPR           at 0 range 14 .. 15;
      FB             at 0 range 16 .. 16;
      RDP            at 0 range 17 .. 22;
      USP            at 0 range 23 .. 23;
      FPM            at 0 range 24 .. 24;
      AAB            at 0 range 25 .. 25;
      MB             at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMASR_TS_Field is STM32_SVD.Bit;

   subtype DMASR_TPSS_Field is STM32_SVD.Bit;

   subtype DMASR_TBUS_Field is STM32_SVD.Bit;

   subtype DMASR_TJTS_Field is STM32_SVD.Bit;

   subtype DMASR_ROS_Field is STM32_SVD.Bit;

   subtype DMASR_TUS_Field is STM32_SVD.Bit;

   subtype DMASR_RS_Field is STM32_SVD.Bit;

   subtype DMASR_RBUS_Field is STM32_SVD.Bit;

   subtype DMASR_RPSS_Field is STM32_SVD.Bit;

   subtype DMASR_PWTS_Field is STM32_SVD.Bit;

   subtype DMASR_ETS_Field is STM32_SVD.Bit;

   subtype DMASR_FBES_Field is STM32_SVD.Bit;

   subtype DMASR_ERS_Field is STM32_SVD.Bit;

   subtype DMASR_AIS_Field is STM32_SVD.Bit;

   subtype DMASR_NIS_Field is STM32_SVD.Bit;

   subtype DMASR_RPS_Field is STM32_SVD.UInt3;

   subtype DMASR_TPS_Field is STM32_SVD.UInt3;

   subtype DMASR_EBS_Field is STM32_SVD.UInt3;

   subtype DMASR_MMCS_Field is STM32_SVD.Bit;

   subtype DMASR_PMTS_Field is STM32_SVD.Bit;

   subtype DMASR_TSTS_Field is STM32_SVD.Bit;

   --  Ethernet DMA status register
   type DMASR_Register is record
      --  no description available
      TS             : DMASR_TS_Field := 16#0#;
      --  no description available
      TPSS           : DMASR_TPSS_Field := 16#0#;
      --  no description available
      TBUS           : DMASR_TBUS_Field := 16#0#;
      --  no description available
      TJTS           : DMASR_TJTS_Field := 16#0#;
      --  no description available
      ROS            : DMASR_ROS_Field := 16#0#;
      --  no description available
      TUS            : DMASR_TUS_Field := 16#0#;
      --  no description available
      RS             : DMASR_RS_Field := 16#0#;
      --  no description available
      RBUS           : DMASR_RBUS_Field := 16#0#;
      --  no description available
      RPSS           : DMASR_RPSS_Field := 16#0#;
      --  no description available
      PWTS           : DMASR_PWTS_Field := 16#0#;
      --  no description available
      ETS            : DMASR_ETS_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : STM32_SVD.UInt2 := 16#0#;
      --  no description available
      FBES           : DMASR_FBES_Field := 16#0#;
      --  no description available
      ERS            : DMASR_ERS_Field := 16#0#;
      --  no description available
      AIS            : DMASR_AIS_Field := 16#0#;
      --  no description available
      NIS            : DMASR_NIS_Field := 16#0#;
      --  no description available
      RPS            : DMASR_RPS_Field := 16#0#;
      --  no description available
      TPS            : DMASR_TPS_Field := 16#0#;
      --  no description available
      EBS            : DMASR_EBS_Field := 16#0#;
      --  unspecified
      Reserved_26_26 : STM32_SVD.Bit := 16#0#;
      --  no description available
      MMCS           : DMASR_MMCS_Field := 16#0#;
      --  no description available
      PMTS           : DMASR_PMTS_Field := 16#0#;
      --  no description available
      TSTS           : DMASR_TSTS_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : STM32_SVD.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMASR_Register use record
      TS             at 0 range 0 .. 0;
      TPSS           at 0 range 1 .. 1;
      TBUS           at 0 range 2 .. 2;
      TJTS           at 0 range 3 .. 3;
      ROS            at 0 range 4 .. 4;
      TUS            at 0 range 5 .. 5;
      RS             at 0 range 6 .. 6;
      RBUS           at 0 range 7 .. 7;
      RPSS           at 0 range 8 .. 8;
      PWTS           at 0 range 9 .. 9;
      ETS            at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      FBES           at 0 range 13 .. 13;
      ERS            at 0 range 14 .. 14;
      AIS            at 0 range 15 .. 15;
      NIS            at 0 range 16 .. 16;
      RPS            at 0 range 17 .. 19;
      TPS            at 0 range 20 .. 22;
      EBS            at 0 range 23 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      MMCS           at 0 range 27 .. 27;
      PMTS           at 0 range 28 .. 28;
      TSTS           at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype DMAOMR_SR_Field is STM32_SVD.Bit;

   subtype DMAOMR_OSF_Field is STM32_SVD.Bit;

   subtype DMAOMR_RTC_Field is STM32_SVD.UInt2;

   subtype DMAOMR_FUGF_Field is STM32_SVD.Bit;

   subtype DMAOMR_FEF_Field is STM32_SVD.Bit;

   subtype DMAOMR_ST_Field is STM32_SVD.Bit;

   subtype DMAOMR_TTC_Field is STM32_SVD.UInt3;

   subtype DMAOMR_FTF_Field is STM32_SVD.Bit;

   subtype DMAOMR_TSF_Field is STM32_SVD.Bit;

   subtype DMAOMR_DFRF_Field is STM32_SVD.Bit;

   subtype DMAOMR_RSF_Field is STM32_SVD.Bit;

   subtype DMAOMR_DTCEFD_Field is STM32_SVD.Bit;

   --  Ethernet DMA operation mode register
   type DMAOMR_Register is record
      --  unspecified
      Reserved_0_0   : STM32_SVD.Bit := 16#0#;
      --  SR
      SR             : DMAOMR_SR_Field := 16#0#;
      --  OSF
      OSF            : DMAOMR_OSF_Field := 16#0#;
      --  RTC
      RTC            : DMAOMR_RTC_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : STM32_SVD.Bit := 16#0#;
      --  FUGF
      FUGF           : DMAOMR_FUGF_Field := 16#0#;
      --  FEF
      FEF            : DMAOMR_FEF_Field := 16#0#;
      --  unspecified
      Reserved_8_12  : STM32_SVD.UInt5 := 16#0#;
      --  ST
      ST             : DMAOMR_ST_Field := 16#0#;
      --  TTC
      TTC            : DMAOMR_TTC_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : STM32_SVD.UInt3 := 16#0#;
      --  FTF
      FTF            : DMAOMR_FTF_Field := 16#0#;
      --  TSF
      TSF            : DMAOMR_TSF_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : STM32_SVD.UInt2 := 16#0#;
      --  DFRF
      DFRF           : DMAOMR_DFRF_Field := 16#0#;
      --  RSF
      RSF            : DMAOMR_RSF_Field := 16#0#;
      --  DTCEFD
      DTCEFD         : DMAOMR_DTCEFD_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32_SVD.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAOMR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      SR             at 0 range 1 .. 1;
      OSF            at 0 range 2 .. 2;
      RTC            at 0 range 3 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      FUGF           at 0 range 6 .. 6;
      FEF            at 0 range 7 .. 7;
      Reserved_8_12  at 0 range 8 .. 12;
      ST             at 0 range 13 .. 13;
      TTC            at 0 range 14 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      FTF            at 0 range 20 .. 20;
      TSF            at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      DFRF           at 0 range 24 .. 24;
      RSF            at 0 range 25 .. 25;
      DTCEFD         at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAIER_TIE_Field is STM32_SVD.Bit;

   subtype DMAIER_TPSIE_Field is STM32_SVD.Bit;

   subtype DMAIER_TBUIE_Field is STM32_SVD.Bit;

   subtype DMAIER_TJTIE_Field is STM32_SVD.Bit;

   subtype DMAIER_ROIE_Field is STM32_SVD.Bit;

   subtype DMAIER_TUIE_Field is STM32_SVD.Bit;

   subtype DMAIER_RIE_Field is STM32_SVD.Bit;

   subtype DMAIER_RBUIE_Field is STM32_SVD.Bit;

   subtype DMAIER_RPSIE_Field is STM32_SVD.Bit;

   subtype DMAIER_RWTIE_Field is STM32_SVD.Bit;

   subtype DMAIER_ETIE_Field is STM32_SVD.Bit;

   subtype DMAIER_FBEIE_Field is STM32_SVD.Bit;

   subtype DMAIER_ERIE_Field is STM32_SVD.Bit;

   subtype DMAIER_AISE_Field is STM32_SVD.Bit;

   subtype DMAIER_NISE_Field is STM32_SVD.Bit;

   --  Ethernet DMA interrupt enable register
   type DMAIER_Register is record
      --  no description available
      TIE            : DMAIER_TIE_Field := 16#0#;
      --  no description available
      TPSIE          : DMAIER_TPSIE_Field := 16#0#;
      --  no description available
      TBUIE          : DMAIER_TBUIE_Field := 16#0#;
      --  no description available
      TJTIE          : DMAIER_TJTIE_Field := 16#0#;
      --  no description available
      ROIE           : DMAIER_ROIE_Field := 16#0#;
      --  no description available
      TUIE           : DMAIER_TUIE_Field := 16#0#;
      --  no description available
      RIE            : DMAIER_RIE_Field := 16#0#;
      --  no description available
      RBUIE          : DMAIER_RBUIE_Field := 16#0#;
      --  no description available
      RPSIE          : DMAIER_RPSIE_Field := 16#0#;
      --  no description available
      RWTIE          : DMAIER_RWTIE_Field := 16#0#;
      --  no description available
      ETIE           : DMAIER_ETIE_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : STM32_SVD.UInt2 := 16#0#;
      --  no description available
      FBEIE          : DMAIER_FBEIE_Field := 16#0#;
      --  no description available
      ERIE           : DMAIER_ERIE_Field := 16#0#;
      --  no description available
      AISE           : DMAIER_AISE_Field := 16#0#;
      --  no description available
      NISE           : DMAIER_NISE_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : STM32_SVD.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAIER_Register use record
      TIE            at 0 range 0 .. 0;
      TPSIE          at 0 range 1 .. 1;
      TBUIE          at 0 range 2 .. 2;
      TJTIE          at 0 range 3 .. 3;
      ROIE           at 0 range 4 .. 4;
      TUIE           at 0 range 5 .. 5;
      RIE            at 0 range 6 .. 6;
      RBUIE          at 0 range 7 .. 7;
      RPSIE          at 0 range 8 .. 8;
      RWTIE          at 0 range 9 .. 9;
      ETIE           at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      FBEIE          at 0 range 13 .. 13;
      ERIE           at 0 range 14 .. 14;
      AISE           at 0 range 15 .. 15;
      NISE           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype DMAMFBOCR_MFC_Field is STM32_SVD.Short;

   subtype DMAMFBOCR_OMFC_Field is STM32_SVD.Bit;

   subtype DMAMFBOCR_MFA_Field is STM32_SVD.UInt11;

   subtype DMAMFBOCR_OFOC_Field is STM32_SVD.Bit;

   --  Ethernet DMA missed frame and buffer overflow counter register
   type DMAMFBOCR_Register is record
      --  no description available
      MFC            : DMAMFBOCR_MFC_Field := 16#0#;
      --  no description available
      OMFC           : DMAMFBOCR_OMFC_Field := 16#0#;
      --  no description available
      MFA            : DMAMFBOCR_MFA_Field := 16#0#;
      --  no description available
      OFOC           : DMAMFBOCR_OFOC_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32_SVD.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMFBOCR_Register use record
      MFC            at 0 range 0 .. 15;
      OMFC           at 0 range 16 .. 16;
      MFA            at 0 range 17 .. 27;
      OFOC           at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype DMARSWTR_RSWTC_Field is STM32_SVD.Byte;

   --  Ethernet DMA receive status watchdog timer register
   type DMARSWTR_Register is record
      --  RSWTC
      RSWTC         : DMARSWTR_RSWTC_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMARSWTR_Register use record
      RSWTC         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Ethernet: media access control (MAC)
   type Ethernet_MAC_Peripheral is record
      --  Ethernet MAC configuration register
      MACCR      : MACCR_Register;
      --  Ethernet MAC frame filter register
      MACFFR     : MACFFR_Register;
      --  Ethernet MAC hash table high register
      MACHTHR    : STM32_SVD.Word;
      --  Ethernet MAC hash table low register
      MACHTLR    : STM32_SVD.Word;
      --  Ethernet MAC MII address register
      MACMIIAR   : MACMIIAR_Register;
      --  Ethernet MAC MII data register
      MACMIIDR   : MACMIIDR_Register;
      --  Ethernet MAC flow control register
      MACFCR     : MACFCR_Register;
      --  Ethernet MAC VLAN tag register
      MACVLANTR  : MACVLANTR_Register;
      --  Ethernet MAC PMT control and status register
      MACPMTCSR  : MACPMTCSR_Register;
      --  Ethernet MAC debug register
      MACDBGR    : MACDBGR_Register;
      --  Ethernet MAC interrupt status register
      MACSR      : MACSR_Register;
      --  Ethernet MAC interrupt mask register
      MACIMR     : MACIMR_Register;
      --  Ethernet MAC address 0 high register
      MACA0HR    : MACA0HR_Register;
      --  Ethernet MAC address 0 low register
      MACA0LR    : STM32_SVD.Word;
      --  Ethernet MAC address 1 high register
      MACA1HR    : MACA1HR_Register;
      --  Ethernet MAC address1 low register
      MACA1LR    : STM32_SVD.Word;
      --  Ethernet MAC address 2 high register
      MACA2HR    : MACA2HR_Register;
      --  Ethernet MAC address 2 low register
      MACA2LR    : MACA2LR_Register;
      --  Ethernet MAC address 3 high register
      MACA3HR    : MACA3HR_Register;
      --  Ethernet MAC address 3 low register
      MACA3LR    : STM32_SVD.Word;
      --  Ethernet MAC remote wakeup frame filter register
      MACRWUFFER : STM32_SVD.Word;
   end record
     with Volatile;

   for Ethernet_MAC_Peripheral use record
      MACCR      at 0 range 0 .. 31;
      MACFFR     at 4 range 0 .. 31;
      MACHTHR    at 8 range 0 .. 31;
      MACHTLR    at 12 range 0 .. 31;
      MACMIIAR   at 16 range 0 .. 31;
      MACMIIDR   at 20 range 0 .. 31;
      MACFCR     at 24 range 0 .. 31;
      MACVLANTR  at 28 range 0 .. 31;
      MACPMTCSR  at 44 range 0 .. 31;
      MACDBGR    at 52 range 0 .. 31;
      MACSR      at 56 range 0 .. 31;
      MACIMR     at 60 range 0 .. 31;
      MACA0HR    at 64 range 0 .. 31;
      MACA0LR    at 68 range 0 .. 31;
      MACA1HR    at 72 range 0 .. 31;
      MACA1LR    at 76 range 0 .. 31;
      MACA2HR    at 80 range 0 .. 31;
      MACA2LR    at 84 range 0 .. 31;
      MACA3HR    at 88 range 0 .. 31;
      MACA3LR    at 92 range 0 .. 31;
      MACRWUFFER at 96 range 0 .. 31;
   end record;

   --  Ethernet: media access control (MAC)
   Ethernet_MAC_Periph : aliased Ethernet_MAC_Peripheral
     with Import, Address => System'To_Address(16#40028000#);

   --  Ethernet: MAC management counters
   type Ethernet_MMC_Peripheral is record
      --  Ethernet MMC control register
      MMCCR       : MMCCR_Register;
      --  Ethernet MMC receive interrupt register
      MMCRIR      : MMCRIR_Register;
      --  Ethernet MMC transmit interrupt register
      MMCTIR      : MMCTIR_Register;
      --  Ethernet MMC receive interrupt mask register
      MMCRIMR     : MMCRIMR_Register;
      --  Ethernet MMC transmit interrupt mask register
      MMCTIMR     : MMCTIMR_Register;
      --  Ethernet MMC transmitted good frames after a single collision counter
      MMCTGFSCCR  : STM32_SVD.Word;
      --  Ethernet MMC transmitted good frames after more than a single
      --  collision
      MMCTGFMSCCR : STM32_SVD.Word;
      --  Ethernet MMC transmitted good frames counter register
      MMCTGFCR    : STM32_SVD.Word;
      --  Ethernet MMC received frames with CRC error counter register
      MMCRFCECR   : STM32_SVD.Word;
      --  Ethernet MMC received frames with alignment error counter register
      MMCRFAECR   : STM32_SVD.Word;
      --  MMC received good unicast frames counter register
      MMCRGUFCR   : STM32_SVD.Word;
   end record
     with Volatile;

   for Ethernet_MMC_Peripheral use record
      MMCCR       at 0 range 0 .. 31;
      MMCRIR      at 4 range 0 .. 31;
      MMCTIR      at 8 range 0 .. 31;
      MMCRIMR     at 12 range 0 .. 31;
      MMCTIMR     at 16 range 0 .. 31;
      MMCTGFSCCR  at 76 range 0 .. 31;
      MMCTGFMSCCR at 80 range 0 .. 31;
      MMCTGFCR    at 104 range 0 .. 31;
      MMCRFCECR   at 148 range 0 .. 31;
      MMCRFAECR   at 152 range 0 .. 31;
      MMCRGUFCR   at 196 range 0 .. 31;
   end record;

   --  Ethernet: MAC management counters
   Ethernet_MMC_Periph : aliased Ethernet_MMC_Peripheral
     with Import, Address => System'To_Address(16#40028100#);

   --  Ethernet: Precision time protocol
   type Ethernet_PTP_Peripheral is record
      --  Ethernet PTP time stamp control register
      PTPTSCR  : PTPTSCR_Register;
      --  Ethernet PTP subsecond increment register
      PTPSSIR  : PTPSSIR_Register;
      --  Ethernet PTP time stamp high register
      PTPTSHR  : STM32_SVD.Word;
      --  Ethernet PTP time stamp low register
      PTPTSLR  : PTPTSLR_Register;
      --  Ethernet PTP time stamp high update register
      PTPTSHUR : STM32_SVD.Word;
      --  Ethernet PTP time stamp low update register
      PTPTSLUR : PTPTSLUR_Register;
      --  Ethernet PTP time stamp addend register
      PTPTSAR  : STM32_SVD.Word;
      --  Ethernet PTP target time high register
      PTPTTHR  : STM32_SVD.Word;
      --  Ethernet PTP target time low register
      PTPTTLR  : STM32_SVD.Word;
      --  Ethernet PTP time stamp status register
      PTPTSSR  : PTPTSSR_Register;
      --  Ethernet PTP PPS control register
      PTPPPSCR : PTPPPSCR_Register;
   end record
     with Volatile;

   for Ethernet_PTP_Peripheral use record
      PTPTSCR  at 0 range 0 .. 31;
      PTPSSIR  at 4 range 0 .. 31;
      PTPTSHR  at 8 range 0 .. 31;
      PTPTSLR  at 12 range 0 .. 31;
      PTPTSHUR at 16 range 0 .. 31;
      PTPTSLUR at 20 range 0 .. 31;
      PTPTSAR  at 24 range 0 .. 31;
      PTPTTHR  at 28 range 0 .. 31;
      PTPTTLR  at 32 range 0 .. 31;
      PTPTSSR  at 40 range 0 .. 31;
      PTPPPSCR at 44 range 0 .. 31;
   end record;

   --  Ethernet: Precision time protocol
   Ethernet_PTP_Periph : aliased Ethernet_PTP_Peripheral
     with Import, Address => System'To_Address(16#40028700#);

   --  Ethernet: DMA controller operation
   type Ethernet_DMA_Peripheral is record
      --  Ethernet DMA bus mode register
      DMABMR    : DMABMR_Register;
      --  Ethernet DMA transmit poll demand register
      DMATPDR   : STM32_SVD.Word;
      --  EHERNET DMA receive poll demand register
      DMARPDR   : STM32_SVD.Word;
      --  Ethernet DMA receive descriptor list address register
      DMARDLAR  : STM32_SVD.Word;
      --  Ethernet DMA transmit descriptor list address register
      DMATDLAR  : STM32_SVD.Word;
      --  Ethernet DMA status register
      DMASR     : DMASR_Register;
      --  Ethernet DMA operation mode register
      DMAOMR    : DMAOMR_Register;
      --  Ethernet DMA interrupt enable register
      DMAIER    : DMAIER_Register;
      --  Ethernet DMA missed frame and buffer overflow counter register
      DMAMFBOCR : DMAMFBOCR_Register;
      --  Ethernet DMA receive status watchdog timer register
      DMARSWTR  : DMARSWTR_Register;
      --  Ethernet DMA current host transmit descriptor register
      DMACHTDR  : STM32_SVD.Word;
      --  Ethernet DMA current host receive descriptor register
      DMACHRDR  : STM32_SVD.Word;
      --  Ethernet DMA current host transmit buffer address register
      DMACHTBAR : STM32_SVD.Word;
      --  Ethernet DMA current host receive buffer address register
      DMACHRBAR : STM32_SVD.Word;
   end record
     with Volatile;

   for Ethernet_DMA_Peripheral use record
      DMABMR    at 0 range 0 .. 31;
      DMATPDR   at 4 range 0 .. 31;
      DMARPDR   at 8 range 0 .. 31;
      DMARDLAR  at 12 range 0 .. 31;
      DMATDLAR  at 16 range 0 .. 31;
      DMASR     at 20 range 0 .. 31;
      DMAOMR    at 24 range 0 .. 31;
      DMAIER    at 28 range 0 .. 31;
      DMAMFBOCR at 32 range 0 .. 31;
      DMARSWTR  at 36 range 0 .. 31;
      DMACHTDR  at 72 range 0 .. 31;
      DMACHRDR  at 76 range 0 .. 31;
      DMACHTBAR at 80 range 0 .. 31;
      DMACHRBAR at 84 range 0 .. 31;
   end record;

   --  Ethernet: DMA controller operation
   Ethernet_DMA_Periph : aliased Ethernet_DMA_Peripheral
     with Import, Address => System'To_Address(16#40029000#);

end STM32_SVD.Ethernet;
