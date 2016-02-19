--  Automatically generated from STM32F46_79x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.DSIHOST is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ---------------------
   -- DSI_CR_Register --
   ---------------------

   subtype DSI_CR_EN_Field is STM32_SVD.Bit;

   --  DSI Host Control Register
   type DSI_CR_Register is record
      --  Enable
      EN            : DSI_CR_EN_Field := 16#0#;
      --  unspecified
      Reserved_1_31 : STM32_SVD.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_CR_Register use record
      EN            at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --------------------------
   -- DSIHSOT_CCR_Register --
   --------------------------

   subtype DSIHSOT_CCR_TXECKDIV_Field is STM32_SVD.Byte;
   subtype DSIHSOT_CCR_TOCKDIV_Field is STM32_SVD.Byte;

   --  DSI HOST Clock Control Register
   type DSIHSOT_CCR_Register is record
      --  TXECKDIV
      TXECKDIV       : DSIHSOT_CCR_TXECKDIV_Field := 16#2A#;
      --  TOCKDIV
      TOCKDIV        : DSIHSOT_CCR_TOCKDIV_Field := 16#30#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#3133#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSIHSOT_CCR_Register use record
      TXECKDIV       at 0 range 0 .. 7;
      TOCKDIV        at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------------
   -- DSI_LVCIDR_Register --
   -------------------------

   subtype DSI_LVCIDR_VCID_Field is STM32_SVD.UInt2;

   --  DSI Host LTDC VCID Register
   type DSI_LVCIDR_Register is record
      --  Virtual Channel ID
      VCID          : DSI_LVCIDR_VCID_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_LVCIDR_Register use record
      VCID          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -------------------------
   -- DSI_LCOLCR_Register --
   -------------------------

   subtype DSI_LCOLCR_COLC_Field is STM32_SVD.UInt4;
   subtype DSI_LCOLCR_LPE_Field is STM32_SVD.Bit;

   --  DSI Host LTDC Color Coding Register
   type DSI_LCOLCR_Register is record
      --  Color Coding
      COLC          : DSI_LCOLCR_COLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : STM32_SVD.UInt4 := 16#0#;
      --  Loosely Packet Enable
      LPE           : DSI_LCOLCR_LPE_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : STM32_SVD.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_LCOLCR_Register use record
      COLC          at 0 range 0 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      LPE           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------------
   -- DSI_LPCR_Register --
   -----------------------

   subtype DSI_LPCR_DEP_Field is STM32_SVD.Bit;
   subtype DSI_LPCR_VSP_Field is STM32_SVD.Bit;
   subtype DSI_LPCR_HSP_Field is STM32_SVD.Bit;

   --  DSI Host LTDC Polarity Configuration Register
   type DSI_LPCR_Register is record
      --  Data Enable Polarity
      DEP           : DSI_LPCR_DEP_Field := 16#0#;
      --  VSYNC Polarity
      VSP           : DSI_LPCR_VSP_Field := 16#0#;
      --  HSYNC Polarity
      HSP           : DSI_LPCR_HSP_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : STM32_SVD.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_LPCR_Register use record
      DEP           at 0 range 0 .. 0;
      VSP           at 0 range 1 .. 1;
      HSP           at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   ------------------------
   -- DSI_LPMCR_Register --
   ------------------------

   subtype DSI_LPMCR_VLPSIZE_Field is STM32_SVD.Byte;
   subtype DSI_LPMCR_LPSIZE_Field is STM32_SVD.Byte;

   --  DSI Host Low-Power Mode Configuration Register
   type DSI_LPMCR_Register is record
      --  VACT Largest Packet Size
      VLPSIZE        : DSI_LPMCR_VLPSIZE_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : STM32_SVD.Byte := 16#0#;
      --  Largest Packet Size
      LPSIZE         : DSI_LPMCR_LPSIZE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_LPMCR_Register use record
      VLPSIZE        at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      LPSIZE         at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ----------------------
   -- DSI_PCR_Register --
   ----------------------

   subtype DSI_PCR_ETTXE_Field is STM32_SVD.Bit;
   subtype DSI_PCR_ETRXE_Field is STM32_SVD.Bit;
   subtype DSI_PCR_BTAE_Field is STM32_SVD.Bit;
   subtype DSI_PCR_ECCRXE_Field is STM32_SVD.Bit;
   subtype DSI_PCR_CRCRXE_Field is STM32_SVD.Bit;

   --  DSI Host Protocol Configuration Register
   type DSI_PCR_Register is record
      --  EoTp Transmission Enable
      ETTXE         : DSI_PCR_ETTXE_Field := 16#0#;
      --  EoTp Reception Enable
      ETRXE         : DSI_PCR_ETRXE_Field := 16#0#;
      --  Bus Turn Around Enable
      BTAE          : DSI_PCR_BTAE_Field := 16#0#;
      --  ECC Reception Enable
      ECCRXE        : DSI_PCR_ECCRXE_Field := 16#0#;
      --  CRC Reception Enable
      CRCRXE        : DSI_PCR_CRCRXE_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : STM32_SVD.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_PCR_Register use record
      ETTXE         at 0 range 0 .. 0;
      ETRXE         at 0 range 1 .. 1;
      BTAE          at 0 range 2 .. 2;
      ECCRXE        at 0 range 3 .. 3;
      CRCRXE        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   -------------------------
   -- DSI_GVCIDR_Register --
   -------------------------

   subtype DSI_GVCIDR_VCID_Field is STM32_SVD.UInt2;

   --  DSI Host Generic VCID Register
   type DSI_GVCIDR_Register is record
      --  Virtual Channel ID
      VCID          : DSI_GVCIDR_VCID_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_GVCIDR_Register use record
      VCID          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   ----------------------
   -- DSI_MCR_Register --
   ----------------------

   subtype DSI_MCR_CMDM_Field is STM32_SVD.Bit;

   --  DSI Host Mode Configuration Register
   type DSI_MCR_Register is record
      --  Command mode
      CMDM          : DSI_MCR_CMDM_Field := 16#1#;
      --  unspecified
      Reserved_1_31 : STM32_SVD.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_MCR_Register use record
      CMDM          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------------
   -- DSI_VMCR_Register --
   -----------------------

   subtype DSI_VMCR_VMT_Field is STM32_SVD.UInt2;
   subtype DSI_VMCR_LPVSAE_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_LPVBPE_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_LPVFPE_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_LVAE_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_LPHBPE_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_LPHFE_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_FBTAAE_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_LPCE_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_PGE_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_PGM_Field is STM32_SVD.Bit;
   subtype DSI_VMCR_PGO_Field is STM32_SVD.Bit;

   --  DSI Host Video mode Configuration Register
   type DSI_VMCR_Register is record
      --  Video mode Type
      VMT            : DSI_VMCR_VMT_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : STM32_SVD.UInt6 := 16#0#;
      --  Low-Power Vertical Sync Active Enable
      LPVSAE         : DSI_VMCR_LPVSAE_Field := 16#0#;
      --  Low-power Vertical Back-Porch Enable
      LPVBPE         : DSI_VMCR_LPVBPE_Field := 16#0#;
      --  Low-power Vertical Front-porch Enable
      LPVFPE         : DSI_VMCR_LPVFPE_Field := 16#0#;
      --  Low-Power Vertical Active Enable
      LVAE           : DSI_VMCR_LVAE_Field := 16#0#;
      --  Low-Power Horizontal Back-Porch Enable
      LPHBPE         : DSI_VMCR_LPHBPE_Field := 16#0#;
      --  Low-Power Horizontal Front-Porch Enable
      LPHFE          : DSI_VMCR_LPHFE_Field := 16#0#;
      --  Frame Bus-Turn-Around Acknowledge Enable
      FBTAAE         : DSI_VMCR_FBTAAE_Field := 16#0#;
      --  Low-Power Command Enable
      LPCE           : DSI_VMCR_LPCE_Field := 16#0#;
      --  Pattern Generator Enable
      PGE            : DSI_VMCR_PGE_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : STM32_SVD.UInt3 := 16#0#;
      --  Pattern Generator Mode
      PGM            : DSI_VMCR_PGM_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : STM32_SVD.UInt3 := 16#0#;
      --  Pattern Generator Orientation
      PGO            : DSI_VMCR_PGO_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VMCR_Register use record
      VMT            at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      LPVSAE         at 0 range 8 .. 8;
      LPVBPE         at 0 range 9 .. 9;
      LPVFPE         at 0 range 10 .. 10;
      LVAE           at 0 range 11 .. 11;
      LPHBPE         at 0 range 12 .. 12;
      LPHFE          at 0 range 13 .. 13;
      FBTAAE         at 0 range 14 .. 14;
      LPCE           at 0 range 15 .. 15;
      PGE            at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      PGM            at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      PGO            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------------
   -- DSI_VPCR_Register --
   -----------------------

   subtype DSI_VPCR_VPSIZE_Field is STM32_SVD.UInt15;

   --  DSI Host Video Packet Configuration Register
   type DSI_VPCR_Register is record
      --  Video Packet Size
      VPSIZE         : DSI_VPCR_VPSIZE_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : STM32_SVD.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VPCR_Register use record
      VPSIZE         at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -----------------------
   -- DSI_VCCR_Register --
   -----------------------

   subtype DSI_VCCR_NUMC_Field is STM32_SVD.UInt14;

   --  DSI Host Video Chunks Configuration Register
   type DSI_VCCR_Register is record
      --  Number of Chunks
      NUMC           : DSI_VCCR_NUMC_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VCCR_Register use record
      NUMC           at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   ------------------------
   -- DSI_VNPCR_Register --
   ------------------------

   subtype DSI_VNPCR_NPSIZE_Field is STM32_SVD.UInt14;

   --  DSI Host Video Null Packet Configuration Register
   type DSI_VNPCR_Register is record
      --  Null Packet Size
      NPSIZE         : DSI_VNPCR_NPSIZE_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VNPCR_Register use record
      NPSIZE         at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -------------------------
   -- DSI_VHSACR_Register --
   -------------------------

   subtype DSI_VHSACR_HSA_Field is STM32_SVD.UInt13;

   --  DSI Host Video HSA Configuration Register
   type DSI_VHSACR_Register is record
      --  Horizontal Synchronism Active duration
      HSA            : DSI_VHSACR_HSA_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VHSACR_Register use record
      HSA            at 0 range 0 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -------------------------
   -- DSI_VHBPCR_Register --
   -------------------------

   subtype DSI_VHBPCR_HBP_Field is STM32_SVD.UInt13;

   --  DSI Host Video HBP Configuration Register
   type DSI_VHBPCR_Register is record
      --  Horizontal Back-Porch duration
      HBP            : DSI_VHBPCR_HBP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VHBPCR_Register use record
      HBP            at 0 range 0 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------------
   -- DSI_VLCR_Register --
   -----------------------

   subtype DSI_VLCR_HLINE_Field is STM32_SVD.UInt15;

   --  DSI Host Video Line Configuration Register
   type DSI_VLCR_Register is record
      --  Horizontal Line duration
      HLINE          : DSI_VLCR_HLINE_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : STM32_SVD.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VLCR_Register use record
      HLINE          at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -------------------------
   -- DSI_VVSACR_Register --
   -------------------------

   subtype DSI_VVSACR_VSA_Field is STM32_SVD.UInt10;

   --  DSI Host Video VSA Configuration Register
   type DSI_VVSACR_Register is record
      --  Vertical Synchronism Active duration
      VSA            : DSI_VVSACR_VSA_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : STM32_SVD.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VVSACR_Register use record
      VSA            at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -------------------------
   -- DSI_VVBPCR_Register --
   -------------------------

   subtype DSI_VVBPCR_VBP_Field is STM32_SVD.UInt10;

   --  DSI Host Video VBP Configuration Register
   type DSI_VVBPCR_Register is record
      --  Vertical Back-Porch duration
      VBP            : DSI_VVBPCR_VBP_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : STM32_SVD.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VVBPCR_Register use record
      VBP            at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -------------------------
   -- DSI_VVFPCR_Register --
   -------------------------

   subtype DSI_VVFPCR_VFP_Field is STM32_SVD.UInt10;

   --  DSI Host Video VFP Configuration Register
   type DSI_VVFPCR_Register is record
      --  Vertical Front-Porch duration
      VFP            : DSI_VVFPCR_VFP_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : STM32_SVD.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VVFPCR_Register use record
      VFP            at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   ------------------------
   -- DSI_VVACR_Register --
   ------------------------

   subtype DSI_VVACR_VA_Field is STM32_SVD.UInt14;

   --  DSI Host Video VA Configuration Register
   type DSI_VVACR_Register is record
      --  Vertical Active duration
      VA             : DSI_VVACR_VA_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VVACR_Register use record
      VA             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -----------------------
   -- DSI_LCCR_Register --
   -----------------------

   subtype DSI_LCCR_CMDSIZE_Field is STM32_SVD.Short;

   --  DSI Host LTDC Command Configuration Register
   type DSI_LCCR_Register is record
      --  Command Size
      CMDSIZE        : DSI_LCCR_CMDSIZE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_LCCR_Register use record
      CMDSIZE        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------------
   -- DSI_CMCR_Register --
   -----------------------

   subtype DSI_CMCR_TEARE_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_ARE_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_GSW0TX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_GSW1TX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_GSW2TX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_GSR0TX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_GSR1TX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_GSR2TX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_GLWTX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_DSW0TX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_DSW1TX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_DSR0TX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_DLWTX_Field is STM32_SVD.Bit;
   subtype DSI_CMCR_MRDPS_Field is STM32_SVD.Bit;

   --  DSI Host Command mode Configuration Register
   type DSI_CMCR_Register is record
      --  Tearing Effect Acknowledge Request Enable
      TEARE          : DSI_CMCR_TEARE_Field := 16#0#;
      --  Acknowledge Request Enable
      ARE            : DSI_CMCR_ARE_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : STM32_SVD.UInt6 := 16#0#;
      --  Generic Short Write Zero parameters Transmission
      GSW0TX         : DSI_CMCR_GSW0TX_Field := 16#0#;
      --  Generic Short Write One parameters Transmission
      GSW1TX         : DSI_CMCR_GSW1TX_Field := 16#0#;
      --  Generic Short Write Two parameters Transmission
      GSW2TX         : DSI_CMCR_GSW2TX_Field := 16#0#;
      --  Generic Short Read Zero parameters Transmission
      GSR0TX         : DSI_CMCR_GSR0TX_Field := 16#0#;
      --  Generic Short Read One parameters Transmission
      GSR1TX         : DSI_CMCR_GSR1TX_Field := 16#0#;
      --  Generic Short Read Two parameters Transmission
      GSR2TX         : DSI_CMCR_GSR2TX_Field := 16#0#;
      --  Generic Long Write Transmission
      GLWTX          : DSI_CMCR_GLWTX_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#0#;
      --  DCS Short Write Zero parameter Transmission
      DSW0TX         : DSI_CMCR_DSW0TX_Field := 16#0#;
      --  DCS Short Read One parameter Transmission
      DSW1TX         : DSI_CMCR_DSW1TX_Field := 16#0#;
      --  DCS Short Read Zero parameter Transmission
      DSR0TX         : DSI_CMCR_DSR0TX_Field := 16#0#;
      --  DCS Long Write Transmission
      DLWTX          : DSI_CMCR_DLWTX_Field := 16#0#;
      --  unspecified
      Reserved_20_23 : STM32_SVD.UInt4 := 16#0#;
      --  Maximum Read Packet Size
      MRDPS          : DSI_CMCR_MRDPS_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_CMCR_Register use record
      TEARE          at 0 range 0 .. 0;
      ARE            at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      GSW0TX         at 0 range 8 .. 8;
      GSW1TX         at 0 range 9 .. 9;
      GSW2TX         at 0 range 10 .. 10;
      GSR0TX         at 0 range 11 .. 11;
      GSR1TX         at 0 range 12 .. 12;
      GSR2TX         at 0 range 13 .. 13;
      GLWTX          at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DSW0TX         at 0 range 16 .. 16;
      DSW1TX         at 0 range 17 .. 17;
      DSR0TX         at 0 range 18 .. 18;
      DLWTX          at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      MRDPS          at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------------
   -- DSI_GHCR_Register --
   -----------------------

   subtype DSI_GHCR_DT_Field is STM32_SVD.UInt6;
   subtype DSI_GHCR_VCID_Field is STM32_SVD.UInt2;
   subtype DSI_GHCR_WCLSB_Field is STM32_SVD.Byte;
   subtype DSI_GHCR_WCMSB_Field is STM32_SVD.Byte;

   --  DSI Host Generic Header Configuration Register
   type DSI_GHCR_Register is record
      --  Type
      DT             : DSI_GHCR_DT_Field := 16#0#;
      --  Channel
      VCID           : DSI_GHCR_VCID_Field := 16#0#;
      --  WordCount LSB
      WCLSB          : DSI_GHCR_WCLSB_Field := 16#0#;
      --  WordCount MSB
      WCMSB          : DSI_GHCR_WCMSB_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_GHCR_Register use record
      DT             at 0 range 0 .. 5;
      VCID           at 0 range 6 .. 7;
      WCLSB          at 0 range 8 .. 15;
      WCMSB          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -----------------------
   -- DSI_GPDR_Register --
   -----------------------

   --  DSI_GPDR_DATA array element
   subtype DSI_GPDR_DATA_Element is STM32_SVD.Byte;

   --  DSI_GPDR_DATA array
   type DSI_GPDR_DATA_Field_Array is array (0 .. 3) of DSI_GPDR_DATA_Element
     with Component_Size => 8, Size => 32;

   --  DSI Host Generic Payload Data Register
   type DSI_GPDR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DATA as a value
            Val : STM32_SVD.Word;
         when True =>
            --  DATA as an array
            Arr : DSI_GPDR_DATA_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DSI_GPDR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------------
   -- DSI_GPSR_Register --
   -----------------------

   subtype DSI_GPSR_CMDFE_Field is STM32_SVD.Bit;
   subtype DSI_GPSR_CMDFF_Field is STM32_SVD.Bit;
   subtype DSI_GPSR_PWRFE_Field is STM32_SVD.Bit;
   subtype DSI_GPSR_PWRFF_Field is STM32_SVD.Bit;
   subtype DSI_GPSR_PRDFE_Field is STM32_SVD.Bit;
   subtype DSI_GPSR_PRDFF_Field is STM32_SVD.Bit;
   subtype DSI_GPSR_RCB_Field is STM32_SVD.Bit;

   --  DSI Host Generic Packet Status Register
   type DSI_GPSR_Register is record
      --  Tearing Effect Acknowledge Request Enable
      CMDFE         : DSI_GPSR_CMDFE_Field := 16#0#;
      --  Acknowledge Request Enable
      CMDFF         : DSI_GPSR_CMDFF_Field := 16#0#;
      --  PWRFE
      PWRFE         : DSI_GPSR_PWRFE_Field := 16#0#;
      --  PWRFF
      PWRFF         : DSI_GPSR_PWRFF_Field := 16#0#;
      --  PRDFE
      PRDFE         : DSI_GPSR_PRDFE_Field := 16#0#;
      --  PRDFF
      PRDFF         : DSI_GPSR_PRDFF_Field := 16#0#;
      --  RCB
      RCB           : DSI_GPSR_RCB_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : STM32_SVD.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_GPSR_Register use record
      CMDFE         at 0 range 0 .. 0;
      CMDFF         at 0 range 1 .. 1;
      PWRFE         at 0 range 2 .. 2;
      PWRFF         at 0 range 3 .. 3;
      PRDFE         at 0 range 4 .. 4;
      PRDFF         at 0 range 5 .. 5;
      RCB           at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   ------------------------
   -- DSI_TCCR1_Register --
   ------------------------

   subtype DSI_TCCR1_LPRX_TOCNT_Field is STM32_SVD.Short;
   subtype DSI_TCCR1_HSTX_TOCNT_Field is STM32_SVD.Short;

   --  DSI Host Timeout Counter Configuration Register1
   type DSI_TCCR1_Register is record
      --  Low-power Reception Timeout Counter
      LPRX_TOCNT : DSI_TCCR1_LPRX_TOCNT_Field := 16#0#;
      --  High-Speed Transmission Timeout Counter
      HSTX_TOCNT : DSI_TCCR1_HSTX_TOCNT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_TCCR1_Register use record
      LPRX_TOCNT at 0 range 0 .. 15;
      HSTX_TOCNT at 0 range 16 .. 31;
   end record;

   -----------------------
   -- DSI_TCCR_Register --
   -----------------------

   subtype DSI_TCCR2_HSRD_TOCNT_Field is STM32_SVD.Short;

   --  DSI Host Timeout Counter Configuration Register2
   type DSI_TCCR_Register is record
      --  High-Speed Read Timeout Counter
      HSRD_TOCNT     : DSI_TCCR2_HSRD_TOCNT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_TCCR_Register use record
      HSRD_TOCNT     at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------------
   -- DSI_TCCR4_Register --
   ------------------------

   subtype DSI_TCCR4_HSWR_TOCNT_Field is STM32_SVD.Short;
   subtype DSI_TCCR4_PM_Field is STM32_SVD.Bit;

   --  DSI Host Timeout Counter Configuration Register4
   type DSI_TCCR4_Register is record
      --  High-Speed Write Timeout Counter
      HSWR_TOCNT     : DSI_TCCR4_HSWR_TOCNT_Field := 16#0#;
      --  unspecified
      Reserved_16_23 : STM32_SVD.Byte := 16#0#;
      --  Presp Mode
      PM             : DSI_TCCR4_PM_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_TCCR4_Register use record
      HSWR_TOCNT     at 0 range 0 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      PM             at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------------
   -- DSI_CLCR_Register --
   -----------------------

   subtype DSI_CLCR_DPCC_Field is STM32_SVD.Bit;
   subtype DSI_CLCR_ACR_Field is STM32_SVD.Bit;

   --  DSI Host Clock Lane Configuration Register
   type DSI_CLCR_Register is record
      --  D-PHY Clock Control
      DPCC          : DSI_CLCR_DPCC_Field := 16#0#;
      --  Automatic Clock lane Control
      ACR           : DSI_CLCR_ACR_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_CLCR_Register use record
      DPCC          at 0 range 0 .. 0;
      ACR           at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   ------------------------
   -- DSI_CLTCR_Register --
   ------------------------

   subtype DSI_CLTCR_LP2HS_TIME_Field is STM32_SVD.UInt10;
   subtype DSI_CLTCR_HS2LP_TIME_Field is STM32_SVD.UInt10;

   --  DSI Host Clock Lane Timer Configuration Register
   type DSI_CLTCR_Register is record
      --  Low-Power to High-Speed Time
      LP2HS_TIME     : DSI_CLTCR_LP2HS_TIME_Field := 16#0#;
      --  unspecified
      Reserved_10_15 : STM32_SVD.UInt6 := 16#0#;
      --  High-Speed to Low-Power Time
      HS2LP_TIME     : DSI_CLTCR_HS2LP_TIME_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : STM32_SVD.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_CLTCR_Register use record
      LP2HS_TIME     at 0 range 0 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      HS2LP_TIME     at 0 range 16 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   ------------------------
   -- DSI_DLTCR_Register --
   ------------------------

   subtype DSI_DLTCR_MRD_TIME_Field is STM32_SVD.UInt15;
   subtype DSI_DLTCR_LP2HS_TIME_Field is STM32_SVD.Byte;
   subtype DSI_DLTCR_HS2LP_TIME_Field is STM32_SVD.Byte;

   --  DSI Host Data Lane Timer Configuration Register
   type DSI_DLTCR_Register is record
      --  Maximum Read Time
      MRD_TIME       : DSI_DLTCR_MRD_TIME_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#0#;
      --  Low-Power To High-Speed Time
      LP2HS_TIME     : DSI_DLTCR_LP2HS_TIME_Field := 16#0#;
      --  High-Speed To Low-Power Time
      HS2LP_TIME     : DSI_DLTCR_HS2LP_TIME_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_DLTCR_Register use record
      MRD_TIME       at 0 range 0 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      LP2HS_TIME     at 0 range 16 .. 23;
      HS2LP_TIME     at 0 range 24 .. 31;
   end record;

   ------------------------
   -- DSI_PCTLR_Register --
   ------------------------

   subtype DSI_PCTLR_DEN_Field is STM32_SVD.Bit;
   subtype DSI_PCTLR_CKE_Field is STM32_SVD.Bit;

   --  DSI Host PHY Control Register
   type DSI_PCTLR_Register is record
      --  unspecified
      Reserved_0_0  : STM32_SVD.Bit := 16#0#;
      --  Digital Enable
      DEN           : DSI_PCTLR_DEN_Field := 16#0#;
      --  Clock Enable
      CKE           : DSI_PCTLR_CKE_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : STM32_SVD.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_PCTLR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      DEN           at 0 range 1 .. 1;
      CKE           at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --------------------------
   -- DSI_PCCONFR_Register --
   --------------------------

   subtype DSI_PCCONFR_NL_Field is STM32_SVD.UInt2;
   subtype DSI_PCCONFR_SW_TIME_Field is STM32_SVD.Byte;

   --  DSI Host PHY Configuration Register
   type DSI_PCCONFR_Register is record
      --  NL
      NL             : DSI_PCCONFR_NL_Field := 16#2#;
      --  unspecified
      Reserved_2_7   : STM32_SVD.UInt6 := 16#A#;
      --  SW_TIME
      SW_TIME        : DSI_PCCONFR_SW_TIME_Field := 16#30#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#3133#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_PCCONFR_Register use record
      NL             at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      SW_TIME        at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------------
   -- DSI_PUCR_Register --
   -----------------------

   subtype DSI_PUCR_URCL_Field is STM32_SVD.Bit;
   subtype DSI_PUCR_UECL_Field is STM32_SVD.Bit;
   subtype DSI_PUCR_URDL_Field is STM32_SVD.Bit;
   subtype DSI_PUCR_UEDL_Field is STM32_SVD.Bit;

   --  DSI Host PHY ULPS Control Register
   type DSI_PUCR_Register is record
      --  ULPS Request on Clock Lane
      URCL          : DSI_PUCR_URCL_Field := 16#0#;
      --  ULPS Exit on Clock Lane
      UECL          : DSI_PUCR_UECL_Field := 16#0#;
      --  ULPS Request on Data Lane
      URDL          : DSI_PUCR_URDL_Field := 16#0#;
      --  ULPS Exit on Data Lane
      UEDL          : DSI_PUCR_UEDL_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : STM32_SVD.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_PUCR_Register use record
      URCL          at 0 range 0 .. 0;
      UECL          at 0 range 1 .. 1;
      URDL          at 0 range 2 .. 2;
      UEDL          at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   ------------------------
   -- DSI_PTTCR_Register --
   ------------------------

   subtype DSI_PTTCR_TX_TRIG_Field is STM32_SVD.UInt4;

   --  DSI Host PHY TX Triggers Configuration Register
   type DSI_PTTCR_Register is record
      --  Transmission Trigger
      TX_TRIG       : DSI_PTTCR_TX_TRIG_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : STM32_SVD.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_PTTCR_Register use record
      TX_TRIG       at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   ----------------------
   -- DSI_PSR_Register --
   ----------------------

   subtype DSI_PSR_PD_Field is STM32_SVD.Bit;
   subtype DSI_PSR_PSSC_Field is STM32_SVD.Bit;
   subtype DSI_PSR_UANC_Field is STM32_SVD.Bit;
   subtype DSI_PSR_PSS0_Field is STM32_SVD.Bit;
   subtype DSI_PSR_UAN0_Field is STM32_SVD.Bit;
   subtype DSI_PSR_RUE0_Field is STM32_SVD.Bit;
   subtype DSI_PSR_PSS1_Field is STM32_SVD.Bit;
   subtype DSI_PSR_UAN1_Field is STM32_SVD.Bit;

   --  DSI Host PHY Status Register
   type DSI_PSR_Register is record
      --  unspecified
      Reserved_0_0  : STM32_SVD.Bit := 16#0#;
      --  PHY Direction
      PD            : DSI_PSR_PD_Field := 16#0#;
      --  PHY Stop State Clock lane
      PSSC          : DSI_PSR_PSSC_Field := 16#0#;
      --  ULPS Active Not Clock lane
      UANC          : DSI_PSR_UANC_Field := 16#1#;
      --  PHY Stop State lane 0
      PSS0          : DSI_PSR_PSS0_Field := 16#0#;
      --  ULPS Active Not lane 1
      UAN0          : DSI_PSR_UAN0_Field := 16#1#;
      --  RX ULPS Escape lane 0
      RUE0          : DSI_PSR_RUE0_Field := 16#0#;
      --  PHY Stop State lane 1
      PSS1          : DSI_PSR_PSS1_Field := 16#0#;
      --  ULPS Active Not lane 1
      UAN1          : DSI_PSR_UAN1_Field := 16#1#;
      --  unspecified
      Reserved_9_31 : STM32_SVD.UInt23 := 16#A#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_PSR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      PD            at 0 range 1 .. 1;
      PSSC          at 0 range 2 .. 2;
      UANC          at 0 range 3 .. 3;
      PSS0          at 0 range 4 .. 4;
      UAN0          at 0 range 5 .. 5;
      RUE0          at 0 range 6 .. 6;
      PSS1          at 0 range 7 .. 7;
      UAN1          at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -----------------------
   -- DSI_ISR0_Register --
   -----------------------

   -----------------
   -- DSI_ISR0.AE --
   -----------------

   --  DSI_ISR0_AE array element
   subtype DSI_ISR0_AE_Element is STM32_SVD.Bit;

   --  DSI_ISR0_AE array
   type DSI_ISR0_AE_Field_Array is array (0 .. 15) of DSI_ISR0_AE_Element
     with Component_Size => 1, Size => 16;

   --  Type definition for DSI_ISR0_AE
   type DSI_ISR0_AE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AE as a value
            Val : STM32_SVD.Short;
         when True =>
            --  AE as an array
            Arr : DSI_ISR0_AE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for DSI_ISR0_AE_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   -----------------
   -- DSI_ISR0.PE --
   -----------------

   --  DSI_ISR0_PE array element
   subtype DSI_ISR0_PE_Element is STM32_SVD.Bit;

   --  DSI_ISR0_PE array
   type DSI_ISR0_PE_Field_Array is array (0 .. 4) of DSI_ISR0_PE_Element
     with Component_Size => 1, Size => 5;

   --  Type definition for DSI_ISR0_PE
   type DSI_ISR0_PE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PE as a value
            Val : STM32_SVD.UInt5;
         when True =>
            --  PE as an array
            Arr : DSI_ISR0_PE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for DSI_ISR0_PE_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  DSI Host Interrupt & Status Register 0
   type DSI_ISR0_Register is record
      --  Acknowledge Error 0
      AE             : DSI_ISR0_AE_Field;
      --  PHY Error 0
      PE             : DSI_ISR0_PE_Field;
      --  unspecified
      Reserved_21_31 : STM32_SVD.UInt11;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_ISR0_Register use record
      AE             at 0 range 0 .. 15;
      PE             at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   -----------------------
   -- DSI_ISR1_Register --
   -----------------------

   subtype DSI_ISR1_TOHSTX_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_TOLPRX_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_ECCSE_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_ECCME_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_CRCE_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_PSE_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_EOTPE_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_LPWRE_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_GCWRE_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_GPWRE_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_GPTXE_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_GPRDE_Field is STM32_SVD.Bit;
   subtype DSI_ISR1_GPRXE_Field is STM32_SVD.Bit;

   --  DSI Host Interrupt & Status Register 1
   type DSI_ISR1_Register is record
      --  Timeout High-Speed Transmission
      TOHSTX         : DSI_ISR1_TOHSTX_Field;
      --  Timeout Low-Power Reception
      TOLPRX         : DSI_ISR1_TOLPRX_Field;
      --  ECC Single-bit Error
      ECCSE          : DSI_ISR1_ECCSE_Field;
      --  ECC Multi-bit Error
      ECCME          : DSI_ISR1_ECCME_Field;
      --  CRC Error
      CRCE           : DSI_ISR1_CRCE_Field;
      --  Packet Size Error
      PSE            : DSI_ISR1_PSE_Field;
      --  EoTp Error
      EOTPE          : DSI_ISR1_EOTPE_Field;
      --  LTDC Payload Write Error
      LPWRE          : DSI_ISR1_LPWRE_Field;
      --  Generic Command Write Error
      GCWRE          : DSI_ISR1_GCWRE_Field;
      --  Generic Payload Write Error
      GPWRE          : DSI_ISR1_GPWRE_Field;
      --  Generic Payload Transmit Error
      GPTXE          : DSI_ISR1_GPTXE_Field;
      --  Generic Payload Read Error
      GPRDE          : DSI_ISR1_GPRDE_Field;
      --  Generic Payload Receive Error
      GPRXE          : DSI_ISR1_GPRXE_Field;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_ISR1_Register use record
      TOHSTX         at 0 range 0 .. 0;
      TOLPRX         at 0 range 1 .. 1;
      ECCSE          at 0 range 2 .. 2;
      ECCME          at 0 range 3 .. 3;
      CRCE           at 0 range 4 .. 4;
      PSE            at 0 range 5 .. 5;
      EOTPE          at 0 range 6 .. 6;
      LPWRE          at 0 range 7 .. 7;
      GCWRE          at 0 range 8 .. 8;
      GPWRE          at 0 range 9 .. 9;
      GPTXE          at 0 range 10 .. 10;
      GPRDE          at 0 range 11 .. 11;
      GPRXE          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------------
   -- DSI_IER0_Register --
   -----------------------

   subtype DSI_IER0_AE0IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE1IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE2IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE3IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE4IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE5IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE6IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE7IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE8IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE9IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE10IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE11IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE12IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE13IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE14IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_AE15IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_PE0IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_PE1IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_PE2IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_PE3IE_Field is STM32_SVD.Bit;
   subtype DSI_IER0_PE4IE_Field is STM32_SVD.Bit;

   --  DSI Host Interrupt Enable Register 0
   type DSI_IER0_Register is record
      --  Acknowledge Error 0 Interrupt Enable
      AE0IE          : DSI_IER0_AE0IE_Field := 16#0#;
      --  Acknowledge Error 1 Interrupt Enable
      AE1IE          : DSI_IER0_AE1IE_Field := 16#0#;
      --  Acknowledge Error 2 Interrupt Enable
      AE2IE          : DSI_IER0_AE2IE_Field := 16#0#;
      --  Acknowledge Error 3 Interrupt Enable
      AE3IE          : DSI_IER0_AE3IE_Field := 16#0#;
      --  Acknowledge Error 4 Interrupt Enable
      AE4IE          : DSI_IER0_AE4IE_Field := 16#0#;
      --  Acknowledge Error 5 Interrupt Enable
      AE5IE          : DSI_IER0_AE5IE_Field := 16#0#;
      --  Acknowledge Error 6 Interrupt Enable
      AE6IE          : DSI_IER0_AE6IE_Field := 16#0#;
      --  Acknowledge Error 7 Interrupt Enable
      AE7IE          : DSI_IER0_AE7IE_Field := 16#0#;
      --  Acknowledge Error 8 Interrupt Enable
      AE8IE          : DSI_IER0_AE8IE_Field := 16#0#;
      --  Acknowledge Error 9 Interrupt Enable
      AE9IE          : DSI_IER0_AE9IE_Field := 16#0#;
      --  Acknowledge Error 10 Interrupt Enable
      AE10IE         : DSI_IER0_AE10IE_Field := 16#0#;
      --  Acknowledge Error 11 Interrupt Enable
      AE11IE         : DSI_IER0_AE11IE_Field := 16#0#;
      --  Acknowledge Error 12 Interrupt Enable
      AE12IE         : DSI_IER0_AE12IE_Field := 16#0#;
      --  Acknowledge Error 13 Interrupt Enable
      AE13IE         : DSI_IER0_AE13IE_Field := 16#0#;
      --  Acknowledge Error 14 Interrupt Enable
      AE14IE         : DSI_IER0_AE14IE_Field := 16#0#;
      --  Acknowledge Error 15 Interrupt Enable
      AE15IE         : DSI_IER0_AE15IE_Field := 16#0#;
      --  PHY Error 0 Interrupt Enable
      PE0IE          : DSI_IER0_PE0IE_Field := 16#0#;
      --  PHY Error 1 Interrupt Enable
      PE1IE          : DSI_IER0_PE1IE_Field := 16#0#;
      --  PHY Error 2 Interrupt Enable
      PE2IE          : DSI_IER0_PE2IE_Field := 16#0#;
      --  PHY Error 3 Interrupt Enable
      PE3IE          : DSI_IER0_PE3IE_Field := 16#0#;
      --  PHY Error 4 Interrupt Enable
      PE4IE          : DSI_IER0_PE4IE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : STM32_SVD.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_IER0_Register use record
      AE0IE          at 0 range 0 .. 0;
      AE1IE          at 0 range 1 .. 1;
      AE2IE          at 0 range 2 .. 2;
      AE3IE          at 0 range 3 .. 3;
      AE4IE          at 0 range 4 .. 4;
      AE5IE          at 0 range 5 .. 5;
      AE6IE          at 0 range 6 .. 6;
      AE7IE          at 0 range 7 .. 7;
      AE8IE          at 0 range 8 .. 8;
      AE9IE          at 0 range 9 .. 9;
      AE10IE         at 0 range 10 .. 10;
      AE11IE         at 0 range 11 .. 11;
      AE12IE         at 0 range 12 .. 12;
      AE13IE         at 0 range 13 .. 13;
      AE14IE         at 0 range 14 .. 14;
      AE15IE         at 0 range 15 .. 15;
      PE0IE          at 0 range 16 .. 16;
      PE1IE          at 0 range 17 .. 17;
      PE2IE          at 0 range 18 .. 18;
      PE3IE          at 0 range 19 .. 19;
      PE4IE          at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   -----------------------
   -- DSI_IER1_Register --
   -----------------------

   subtype DSI_IER1_TOHSTXIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_TOLPRXIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_ECCSEIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_ECCMEIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_CRCEIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_PSEIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_EOTPEIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_LPWREIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_GCWREIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_GPWREIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_GPTXEIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_GPRDEIE_Field is STM32_SVD.Bit;
   subtype DSI_IER1_GPRXEIE_Field is STM32_SVD.Bit;

   --  DSI Host Interrupt Enable Register 1
   type DSI_IER1_Register is record
      --  Timeout High-Speed Transmission Interrupt Enable
      TOHSTXIE       : DSI_IER1_TOHSTXIE_Field := 16#0#;
      --  Timeout Low-Power Reception Interrupt Enable
      TOLPRXIE       : DSI_IER1_TOLPRXIE_Field := 16#0#;
      --  ECC Single-bit Error Interrupt Enable
      ECCSEIE        : DSI_IER1_ECCSEIE_Field := 16#0#;
      --  ECC Multi-bit Error Interrupt Enable
      ECCMEIE        : DSI_IER1_ECCMEIE_Field := 16#0#;
      --  CRC Error Interrupt Enable
      CRCEIE         : DSI_IER1_CRCEIE_Field := 16#0#;
      --  Packet Size Error Interrupt Enable
      PSEIE          : DSI_IER1_PSEIE_Field := 16#0#;
      --  EoTp Error Interrupt Enable
      EOTPEIE        : DSI_IER1_EOTPEIE_Field := 16#0#;
      --  LTDC Payload Write Error Interrupt Enable
      LPWREIE        : DSI_IER1_LPWREIE_Field := 16#0#;
      --  Generic Command Write Error Interrupt Enable
      GCWREIE        : DSI_IER1_GCWREIE_Field := 16#0#;
      --  Generic Payload Write Error Interrupt Enable
      GPWREIE        : DSI_IER1_GPWREIE_Field := 16#0#;
      --  Generic Payload Transmit Error Interrupt Enable
      GPTXEIE        : DSI_IER1_GPTXEIE_Field := 16#0#;
      --  Generic Payload Read Error Interrupt Enable
      GPRDEIE        : DSI_IER1_GPRDEIE_Field := 16#0#;
      --  Generic Payload Receive Error Interrupt Enable
      GPRXEIE        : DSI_IER1_GPRXEIE_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_IER1_Register use record
      TOHSTXIE       at 0 range 0 .. 0;
      TOLPRXIE       at 0 range 1 .. 1;
      ECCSEIE        at 0 range 2 .. 2;
      ECCMEIE        at 0 range 3 .. 3;
      CRCEIE         at 0 range 4 .. 4;
      PSEIE          at 0 range 5 .. 5;
      EOTPEIE        at 0 range 6 .. 6;
      LPWREIE        at 0 range 7 .. 7;
      GCWREIE        at 0 range 8 .. 8;
      GPWREIE        at 0 range 9 .. 9;
      GPTXEIE        at 0 range 10 .. 10;
      GPRDEIE        at 0 range 11 .. 11;
      GPRXEIE        at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------------
   -- DSI_FIR0_Register --
   -----------------------

   ------------------
   -- DSI_FIR0.FAE --
   ------------------

   --  DSI_FIR0_FAE array element
   subtype DSI_FIR0_FAE_Element is STM32_SVD.Bit;

   --  DSI_FIR0_FAE array
   type DSI_FIR0_FAE_Field_Array is array (0 .. 15) of DSI_FIR0_FAE_Element
     with Component_Size => 1, Size => 16;

   --  Type definition for DSI_FIR0_FAE
   type DSI_FIR0_FAE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FAE as a value
            Val : STM32_SVD.Short;
         when True =>
            --  FAE as an array
            Arr : DSI_FIR0_FAE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for DSI_FIR0_FAE_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   ------------------
   -- DSI_FIR0.FPE --
   ------------------

   --  DSI_FIR0_FPE array element
   subtype DSI_FIR0_FPE_Element is STM32_SVD.Bit;

   --  DSI_FIR0_FPE array
   type DSI_FIR0_FPE_Field_Array is array (0 .. 4) of DSI_FIR0_FPE_Element
     with Component_Size => 1, Size => 5;

   --  Type definition for DSI_FIR0_FPE
   type DSI_FIR0_FPE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  FPE as a value
            Val : STM32_SVD.UInt5;
         when True =>
            --  FPE as an array
            Arr : DSI_FIR0_FPE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for DSI_FIR0_FPE_Field use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  DSI Host Force Interrupt Register 0
   type DSI_FIR0_Register is record
      --  Force Acknowledge Error 0
      FAE            : DSI_FIR0_FAE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Force PHY Error 0
      FPE            : DSI_FIR0_FPE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_21_31 : STM32_SVD.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_FIR0_Register use record
      FAE            at 0 range 0 .. 15;
      FPE            at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   -----------------------
   -- DSI_FIR1_Register --
   -----------------------

   subtype DSI_FIR1_FTOHSTX_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FTOLPRX_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FECCSE_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FECCME_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FCRCE_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FPSE_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FEOTPE_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FLPWRE_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FGCWRE_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FGPWRE_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FGPTXE_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FGPRDE_Field is STM32_SVD.Bit;
   subtype DSI_FIR1_FGPRXE_Field is STM32_SVD.Bit;

   --  DSI Host Force Interrupt Register 1
   type DSI_FIR1_Register is record
      --  Force Timeout High-Speed Transmission
      FTOHSTX        : DSI_FIR1_FTOHSTX_Field := 16#0#;
      --  Force Timeout Low-Power Reception
      FTOLPRX        : DSI_FIR1_FTOLPRX_Field := 16#0#;
      --  Force ECC Single-bit Error
      FECCSE         : DSI_FIR1_FECCSE_Field := 16#0#;
      --  Force ECC Multi-bit Error
      FECCME         : DSI_FIR1_FECCME_Field := 16#0#;
      --  Force CRC Error
      FCRCE          : DSI_FIR1_FCRCE_Field := 16#0#;
      --  Force Packet Size Error
      FPSE           : DSI_FIR1_FPSE_Field := 16#0#;
      --  Force EoTp Error
      FEOTPE         : DSI_FIR1_FEOTPE_Field := 16#0#;
      --  Force LTDC Payload Write Error
      FLPWRE         : DSI_FIR1_FLPWRE_Field := 16#0#;
      --  Force Generic Command Write Error
      FGCWRE         : DSI_FIR1_FGCWRE_Field := 16#0#;
      --  Force Generic Payload Write Error
      FGPWRE         : DSI_FIR1_FGPWRE_Field := 16#0#;
      --  Force Generic Payload Transmit Error
      FGPTXE         : DSI_FIR1_FGPTXE_Field := 16#0#;
      --  Force Generic Payload Read Error
      FGPRDE         : DSI_FIR1_FGPRDE_Field := 16#0#;
      --  Force Generic Payload Receive Error
      FGPRXE         : DSI_FIR1_FGPRXE_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_FIR1_Register use record
      FTOHSTX        at 0 range 0 .. 0;
      FTOLPRX        at 0 range 1 .. 1;
      FECCSE         at 0 range 2 .. 2;
      FECCME         at 0 range 3 .. 3;
      FCRCE          at 0 range 4 .. 4;
      FPSE           at 0 range 5 .. 5;
      FEOTPE         at 0 range 6 .. 6;
      FLPWRE         at 0 range 7 .. 7;
      FGCWRE         at 0 range 8 .. 8;
      FGPWRE         at 0 range 9 .. 9;
      FGPTXE         at 0 range 10 .. 10;
      FGPRDE         at 0 range 11 .. 11;
      FGPRXE         at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------------
   -- DSI_VSCR_Register --
   -----------------------

   subtype DSI_VSCR_EN_Field is STM32_SVD.Bit;
   subtype DSI_VSCR_UR_Field is STM32_SVD.Bit;

   --  DSI Host Video Shadow Control Register
   type DSI_VSCR_Register is record
      --  Enable
      EN            : DSI_VSCR_EN_Field := 16#0#;
      --  unspecified
      Reserved_1_7  : STM32_SVD.UInt7 := 16#0#;
      --  Update Register
      UR            : DSI_VSCR_UR_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : STM32_SVD.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VSCR_Register use record
      EN            at 0 range 0 .. 0;
      Reserved_1_7  at 0 range 1 .. 7;
      UR            at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --------------------------
   -- DSI_LCVCIDR_Register --
   --------------------------

   subtype DSI_LCVCIDR_VCID_Field is STM32_SVD.UInt2;

   --  DSI Host LTDC Current VCID Register
   type DSI_LCVCIDR_Register is record
      --  Virtual Channel ID
      VCID          : DSI_LCVCIDR_VCID_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_LCVCIDR_Register use record
      VCID          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   ------------------------
   -- DSI_LCCCR_Register --
   ------------------------

   subtype DSI_LCCCR_COLC_Field is STM32_SVD.UInt4;
   subtype DSI_LCCCR_LPE_Field is STM32_SVD.Bit;

   --  DSI Host LTDC Current Color Coding Register
   type DSI_LCCCR_Register is record
      --  Color Coding
      COLC          : DSI_LCCCR_COLC_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : STM32_SVD.UInt4 := 16#0#;
      --  Loosely Packed Enable
      LPE           : DSI_LCCCR_LPE_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : STM32_SVD.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_LCCCR_Register use record
      COLC          at 0 range 0 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      LPE           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -------------------------
   -- DSI_LPMCCR_Register --
   -------------------------

   subtype DSI_LPMCCR_VLPSIZE_Field is STM32_SVD.Byte;
   subtype DSI_LPMCCR_LPSIZE_Field is STM32_SVD.Byte;

   --  DSI Host Low-power Mode Current Configuration Register
   type DSI_LPMCCR_Register is record
      --  VACT Largest Packet Size
      VLPSIZE        : DSI_LPMCCR_VLPSIZE_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : STM32_SVD.Byte := 16#0#;
      --  Largest Packet Size
      LPSIZE         : DSI_LPMCCR_LPSIZE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_LPMCCR_Register use record
      VLPSIZE        at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      LPSIZE         at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ------------------------
   -- DSI_VMCCR_Register --
   ------------------------

   subtype DSI_VMCCR_VMT_Field is STM32_SVD.UInt2;
   subtype DSI_VMCCR_LPVSAE_Field is STM32_SVD.Bit;
   subtype DSI_VMCCR_LPVBPE_Field is STM32_SVD.Bit;
   subtype DSI_VMCCR_LPVFPE_Field is STM32_SVD.Bit;
   subtype DSI_VMCCR_LVAE_Field is STM32_SVD.Bit;
   subtype DSI_VMCCR_LPHBPE_Field is STM32_SVD.Bit;
   subtype DSI_VMCCR_LPHFE_Field is STM32_SVD.Bit;
   subtype DSI_VMCCR_FBTAAE_Field is STM32_SVD.Bit;
   subtype DSI_VMCCR_LPCE_Field is STM32_SVD.Bit;

   --  DSI Host Video mode Current Configuration Register
   type DSI_VMCCR_Register is record
      --  Video mode Type
      VMT            : DSI_VMCCR_VMT_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : STM32_SVD.UInt6 := 16#0#;
      --  Low-Power Vertical Sync time Enable
      LPVSAE         : DSI_VMCCR_LPVSAE_Field := 16#0#;
      --  Low-power Vertical Back-Porch Enable
      LPVBPE         : DSI_VMCCR_LPVBPE_Field := 16#0#;
      --  Low-power Vertical Front-Porch Enable
      LPVFPE         : DSI_VMCCR_LPVFPE_Field := 16#0#;
      --  Low-Power Vertical Active Enable
      LVAE           : DSI_VMCCR_LVAE_Field := 16#0#;
      --  Low-power Horizontal Back-Porch Enable
      LPHBPE         : DSI_VMCCR_LPHBPE_Field := 16#0#;
      --  Low-Power Horizontal Front-Porch Enable
      LPHFE          : DSI_VMCCR_LPHFE_Field := 16#0#;
      --  Frame BTA Acknowledge Enable
      FBTAAE         : DSI_VMCCR_FBTAAE_Field := 16#0#;
      --  Low-Power Command Enable
      LPCE           : DSI_VMCCR_LPCE_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VMCCR_Register use record
      VMT            at 0 range 0 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      LPVSAE         at 0 range 8 .. 8;
      LPVBPE         at 0 range 9 .. 9;
      LPVFPE         at 0 range 10 .. 10;
      LVAE           at 0 range 11 .. 11;
      LPHBPE         at 0 range 12 .. 12;
      LPHFE          at 0 range 13 .. 13;
      FBTAAE         at 0 range 14 .. 14;
      LPCE           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------------
   -- DSI_VPCCR_Register --
   ------------------------

   subtype DSI_VPCCR_VPSIZE_Field is STM32_SVD.UInt15;

   --  DSI Host Video Packet Current Configuration Register
   type DSI_VPCCR_Register is record
      --  Video Packet Size
      VPSIZE         : DSI_VPCCR_VPSIZE_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : STM32_SVD.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VPCCR_Register use record
      VPSIZE         at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   ------------------------
   -- DSI_VCCCR_Register --
   ------------------------

   subtype DSI_VCCCR_NUMC_Field is STM32_SVD.UInt14;

   --  DSI Host Video Chunks Current Configuration Register
   type DSI_VCCCR_Register is record
      --  Number of Chunks
      NUMC           : DSI_VCCCR_NUMC_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VCCCR_Register use record
      NUMC           at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -------------------------
   -- DSI_VNPCCR_Register --
   -------------------------

   subtype DSI_VNPCCR_NPSIZE_Field is STM32_SVD.UInt14;

   --  DSI Host Video Null Packet Current Configuration Register
   type DSI_VNPCCR_Register is record
      --  Null Packet Size
      NPSIZE         : DSI_VNPCCR_NPSIZE_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VNPCCR_Register use record
      NPSIZE         at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --------------------------
   -- DSI_VHSACCR_Register --
   --------------------------

   subtype DSI_VHSACCR_HSA_Field is STM32_SVD.UInt13;

   --  DSI Host Video HSA Current Configuration Register
   type DSI_VHSACCR_Register is record
      --  Horizontal Synchronism Active duration
      HSA            : DSI_VHSACCR_HSA_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VHSACCR_Register use record
      HSA            at 0 range 0 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --------------------------
   -- DSI_VHBPCCR_Register --
   --------------------------

   subtype DSI_VHBPCCR_HBP_Field is STM32_SVD.UInt13;

   --  DSI Host Video HBP Current Configuration Register
   type DSI_VHBPCCR_Register is record
      --  Horizontal Back-Porch duration
      HBP            : DSI_VHBPCCR_HBP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VHBPCCR_Register use record
      HBP            at 0 range 0 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   ------------------------
   -- DSI_VLCCR_Register --
   ------------------------

   subtype DSI_VLCCR_HLINE_Field is STM32_SVD.UInt15;

   --  DSI Host Video Line Current Configuration Register
   type DSI_VLCCR_Register is record
      --  Horizontal Line duration
      HLINE          : DSI_VLCCR_HLINE_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : STM32_SVD.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VLCCR_Register use record
      HLINE          at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --------------------------
   -- DSI_VVSACCR_Register --
   --------------------------

   subtype DSI_VVSACCR_VSA_Field is STM32_SVD.UInt10;

   --  DSI Host Video VSA Current Configuration Register
   type DSI_VVSACCR_Register is record
      --  Vertical Synchronism Active duration
      VSA            : DSI_VVSACCR_VSA_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : STM32_SVD.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VVSACCR_Register use record
      VSA            at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --------------------------
   -- DSI_VVBPCCR_Register --
   --------------------------

   subtype DSI_VVBPCCR_VBP_Field is STM32_SVD.UInt10;

   --  DSI Host Video VBP Current Configuration Register
   type DSI_VVBPCCR_Register is record
      --  Vertical Back-Porch duration
      VBP            : DSI_VVBPCCR_VBP_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : STM32_SVD.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VVBPCCR_Register use record
      VBP            at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --------------------------
   -- DSI_VVFPCCR_Register --
   --------------------------

   subtype DSI_VVFPCCR_VFP_Field is STM32_SVD.UInt10;

   --  DSI Host Video VFP Current Configuration Register
   type DSI_VVFPCCR_Register is record
      --  Vertical Front-Porch duration
      VFP            : DSI_VVFPCCR_VFP_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : STM32_SVD.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VVFPCCR_Register use record
      VFP            at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -------------------------
   -- DSI_VVACCR_Register --
   -------------------------

   subtype DSI_VVACCR_VA_Field is STM32_SVD.UInt14;

   --  DSI Host Video VA Current Configuration Register
   type DSI_VVACCR_Register is record
      --  Vertical Active duration
      VA             : DSI_VVACCR_VA_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_VVACCR_Register use record
      VA             at 0 range 0 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   ------------------------
   -- DSI_WCFGR_Register --
   ------------------------

   subtype DSI_WCFGR_DSIM_Field is STM32_SVD.Bit;
   subtype DSI_WCFGR_COLMUX_Field is STM32_SVD.UInt3;
   subtype DSI_WCFGR_TESRC_Field is STM32_SVD.Bit;
   subtype DSI_WCFGR_TEPOL_Field is STM32_SVD.Bit;
   subtype DSI_WCFGR_AR_Field is STM32_SVD.Bit;
   subtype DSI_WCFGR_VSPOL_Field is STM32_SVD.Bit;

   --  DSI Wrapper Configuration Register
   type DSI_WCFGR_Register is record
      --  DSI Mode
      DSIM          : DSI_WCFGR_DSIM_Field := 16#0#;
      --  Color Multiplexing
      COLMUX        : DSI_WCFGR_COLMUX_Field := 16#0#;
      --  TE Source
      TESRC         : DSI_WCFGR_TESRC_Field := 16#0#;
      --  TE Polarity
      TEPOL         : DSI_WCFGR_TEPOL_Field := 16#0#;
      --  Automatic Refresh
      AR            : DSI_WCFGR_AR_Field := 16#0#;
      --  VSync Polarity
      VSPOL         : DSI_WCFGR_VSPOL_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WCFGR_Register use record
      DSIM          at 0 range 0 .. 0;
      COLMUX        at 0 range 1 .. 3;
      TESRC         at 0 range 4 .. 4;
      TEPOL         at 0 range 5 .. 5;
      AR            at 0 range 6 .. 6;
      VSPOL         at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   ----------------------
   -- DSI_WCR_Register --
   ----------------------

   subtype DSI_WCR_COLM_Field is STM32_SVD.Bit;
   subtype DSI_WCR_SHTDN_Field is STM32_SVD.Bit;
   subtype DSI_WCR_LTDCEN_Field is STM32_SVD.Bit;
   subtype DSI_WCR_DSIEN_Field is STM32_SVD.Bit;

   --  DSI Wrapper Control Register
   type DSI_WCR_Register is record
      --  Color Mode
      COLM          : DSI_WCR_COLM_Field := 16#0#;
      --  Shutdown
      SHTDN         : DSI_WCR_SHTDN_Field := 16#0#;
      --  LTDC Enable
      LTDCEN        : DSI_WCR_LTDCEN_Field := 16#0#;
      --  DSI Enable
      DSIEN         : DSI_WCR_DSIEN_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : STM32_SVD.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WCR_Register use record
      COLM          at 0 range 0 .. 0;
      SHTDN         at 0 range 1 .. 1;
      LTDCEN        at 0 range 2 .. 2;
      DSIEN         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------------
   -- DSI_WIER_Register --
   -----------------------

   subtype DSI_WIER_TEIE_Field is STM32_SVD.Bit;
   subtype DSI_WIER_ERIE_Field is STM32_SVD.Bit;
   subtype DSI_WIER_PLLLIE_Field is STM32_SVD.Bit;
   subtype DSI_WIER_PLLUIE_Field is STM32_SVD.Bit;
   subtype DSI_WIER_RRIE_Field is STM32_SVD.Bit;

   --  DSI Wrapper Interrupt Enable Register
   type DSI_WIER_Register is record
      --  Tearing Effect Interrupt Enable
      TEIE           : DSI_WIER_TEIE_Field := 16#0#;
      --  End of Refresh Interrupt Enable
      ERIE           : DSI_WIER_ERIE_Field := 16#0#;
      --  unspecified
      Reserved_2_8   : STM32_SVD.UInt7 := 16#0#;
      --  PLL Lock Interrupt Enable
      PLLLIE         : DSI_WIER_PLLLIE_Field := 16#0#;
      --  PLL Unlock Interrupt Enable
      PLLUIE         : DSI_WIER_PLLUIE_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : STM32_SVD.UInt2 := 16#0#;
      --  Regulator Ready Interrupt Enable
      RRIE           : DSI_WIER_RRIE_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WIER_Register use record
      TEIE           at 0 range 0 .. 0;
      ERIE           at 0 range 1 .. 1;
      Reserved_2_8   at 0 range 2 .. 8;
      PLLLIE         at 0 range 9 .. 9;
      PLLUIE         at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      RRIE           at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -----------------------
   -- DSI_WISR_Register --
   -----------------------

   subtype DSI_WISR_TEIF_Field is STM32_SVD.Bit;
   subtype DSI_WISR_ERIF_Field is STM32_SVD.Bit;
   subtype DSI_WISR_BUSY_Field is STM32_SVD.Bit;
   subtype DSI_WISR_PLLLS_Field is STM32_SVD.Bit;
   subtype DSI_WISR_PLLLIF_Field is STM32_SVD.Bit;
   subtype DSI_WISR_PLLUIF_Field is STM32_SVD.Bit;
   subtype DSI_WISR_RRS_Field is STM32_SVD.Bit;
   subtype DSI_WISR_RRIF_Field is STM32_SVD.Bit;

   --  DSI Wrapper Interrupt & Status Register
   type DSI_WISR_Register is record
      --  Tearing Effect Interrupt Flag
      TEIF           : DSI_WISR_TEIF_Field;
      --  End of Refresh Interrupt Flag
      ERIF           : DSI_WISR_ERIF_Field;
      --  Busy Flag
      BUSY           : DSI_WISR_BUSY_Field;
      --  unspecified
      Reserved_3_7   : STM32_SVD.UInt5;
      --  PLL Lock Status
      PLLLS          : DSI_WISR_PLLLS_Field;
      --  PLL Lock Interrupt Flag
      PLLLIF         : DSI_WISR_PLLLIF_Field;
      --  PLL Unlock Interrupt Flag
      PLLUIF         : DSI_WISR_PLLUIF_Field;
      --  unspecified
      Reserved_11_11 : STM32_SVD.Bit;
      --  Regulator Ready Status
      RRS            : DSI_WISR_RRS_Field;
      --  Regulator Ready Interrupt Flag
      RRIF           : DSI_WISR_RRIF_Field;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WISR_Register use record
      TEIF           at 0 range 0 .. 0;
      ERIF           at 0 range 1 .. 1;
      BUSY           at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      PLLLS          at 0 range 8 .. 8;
      PLLLIF         at 0 range 9 .. 9;
      PLLUIF         at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      RRS            at 0 range 12 .. 12;
      RRIF           at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   ------------------------
   -- DSI_WIFCR_Register --
   ------------------------

   subtype DSI_WIFCR_CTEIF_Field is STM32_SVD.Bit;
   subtype DSI_WIFCR_CERIF_Field is STM32_SVD.Bit;
   subtype DSI_WIFCR_CPLLLIF_Field is STM32_SVD.Bit;
   subtype DSI_WIFCR_CPLLUIF_Field is STM32_SVD.Bit;
   subtype DSI_WIFCR_CRRIF_Field is STM32_SVD.Bit;

   --  DSI Wrapper Interrupt Flag Clear Register
   type DSI_WIFCR_Register is record
      --  Clear Tearing Effect Interrupt Flag
      CTEIF          : DSI_WIFCR_CTEIF_Field := 16#0#;
      --  Clear End of Refresh Interrupt Flag
      CERIF          : DSI_WIFCR_CERIF_Field := 16#0#;
      --  unspecified
      Reserved_2_8   : STM32_SVD.UInt7 := 16#0#;
      --  Clear PLL Lock Interrupt Flag
      CPLLLIF        : DSI_WIFCR_CPLLLIF_Field := 16#0#;
      --  Clear PLL Unlock Interrupt Flag
      CPLLUIF        : DSI_WIFCR_CPLLUIF_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : STM32_SVD.UInt2 := 16#0#;
      --  Clear Regulator Ready Interrupt Flag
      CRRIF          : DSI_WIFCR_CRRIF_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WIFCR_Register use record
      CTEIF          at 0 range 0 .. 0;
      CERIF          at 0 range 1 .. 1;
      Reserved_2_8   at 0 range 2 .. 8;
      CPLLLIF        at 0 range 9 .. 9;
      CPLLUIF        at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      CRRIF          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   ------------------------
   -- DSI_WPCR1_Register --
   ------------------------

   subtype DSI_WPCR1_UIX4_Field is STM32_SVD.UInt6;
   subtype DSI_WPCR1_SWCL_Field is STM32_SVD.Bit;

   --------------------
   -- DSI_WPCR1.SWDL --
   --------------------

   --  DSI_WPCR1_SWDL array element
   subtype DSI_WPCR1_SWDL_Element is STM32_SVD.Bit;

   --  DSI_WPCR1_SWDL array
   type DSI_WPCR1_SWDL_Field_Array is array (0 .. 1)
     of DSI_WPCR1_SWDL_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for DSI_WPCR1_SWDL
   type DSI_WPCR1_SWDL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWDL as a value
            Val : STM32_SVD.UInt2;
         when True =>
            --  SWDL as an array
            Arr : DSI_WPCR1_SWDL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DSI_WPCR1_SWDL_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype DSI_WPCR1_HSICL_Field is STM32_SVD.Bit;

   ---------------------
   -- DSI_WPCR1.HSIDL --
   ---------------------

   --  DSI_WPCR1_HSIDL array element
   subtype DSI_WPCR1_HSIDL_Element is STM32_SVD.Bit;

   --  DSI_WPCR1_HSIDL array
   type DSI_WPCR1_HSIDL_Field_Array is array (0 .. 1)
     of DSI_WPCR1_HSIDL_Element
     with Component_Size => 1, Size => 2;

   --  Type definition for DSI_WPCR1_HSIDL
   type DSI_WPCR1_HSIDL_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  HSIDL as a value
            Val : STM32_SVD.UInt2;
         when True =>
            --  HSIDL as an array
            Arr : DSI_WPCR1_HSIDL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for DSI_WPCR1_HSIDL_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   subtype DSI_WPCR1_FTXSMCL_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_FTXSMDL_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_CDOFFDL_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_TDDL_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_PDEN_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_TCLKPREPEN_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_TCLKZEROEN_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_THSPREPEN_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_THSTRAILEN_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_THSZEROEN_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_TLPXDEN_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_THSEXITEN_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_TLPXCEN_Field is STM32_SVD.Bit;
   subtype DSI_WPCR1_TCLKPOSTEN_Field is STM32_SVD.Bit;

   --  DSI Wrapper PHY Configuration Register 1
   type DSI_WPCR1_Register is record
      --  Unit Interval multiplied by 4
      UIX4           : DSI_WPCR1_UIX4_Field := 16#0#;
      --  Swap Clock Lane pins
      SWCL           : DSI_WPCR1_SWCL_Field := 16#0#;
      --  Swap Data Lane 0 pins
      SWDL           : DSI_WPCR1_SWDL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Invert Hight-Speed data signal on Clock Lane
      HSICL          : DSI_WPCR1_HSICL_Field := 16#0#;
      --  Invert the Hight-Speed data signal on Data Lane 0
      HSIDL          : DSI_WPCR1_HSIDL_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Force in TX Stop Mode the Clock Lane
      FTXSMCL        : DSI_WPCR1_FTXSMCL_Field := 16#0#;
      --  Force in TX Stop Mode the Data Lanes
      FTXSMDL        : DSI_WPCR1_FTXSMDL_Field := 16#0#;
      --  Contention Detection OFF on Data Lanes
      CDOFFDL        : DSI_WPCR1_CDOFFDL_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#0#;
      --  Turn Disable Data Lanes
      TDDL           : DSI_WPCR1_TDDL_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32_SVD.Bit := 16#0#;
      --  Pull-Down Enable
      PDEN           : DSI_WPCR1_PDEN_Field := 16#0#;
      --  custom time for tCLK-PREPARE Enable
      TCLKPREPEN     : DSI_WPCR1_TCLKPREPEN_Field := 16#0#;
      --  custom time for tCLK-ZERO Enable
      TCLKZEROEN     : DSI_WPCR1_TCLKZEROEN_Field := 16#0#;
      --  custom time for tHS-PREPARE Enable
      THSPREPEN      : DSI_WPCR1_THSPREPEN_Field := 16#0#;
      --  custom time for tHS-TRAIL Enable
      THSTRAILEN     : DSI_WPCR1_THSTRAILEN_Field := 16#0#;
      --  custom time for tHS-ZERO Enable
      THSZEROEN      : DSI_WPCR1_THSZEROEN_Field := 16#0#;
      --  custom time for tLPX for Data lanes Enable
      TLPXDEN        : DSI_WPCR1_TLPXDEN_Field := 16#0#;
      --  custom time for tHS-EXIT Enable
      THSEXITEN      : DSI_WPCR1_THSEXITEN_Field := 16#0#;
      --  custom time for tLPX for Clock lane Enable
      TLPXCEN        : DSI_WPCR1_TLPXCEN_Field := 16#0#;
      --  custom time for tCLK-POST Enable
      TCLKPOSTEN     : DSI_WPCR1_TCLKPOSTEN_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32_SVD.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WPCR1_Register use record
      UIX4           at 0 range 0 .. 5;
      SWCL           at 0 range 6 .. 6;
      SWDL           at 0 range 7 .. 8;
      HSICL          at 0 range 9 .. 9;
      HSIDL          at 0 range 10 .. 11;
      FTXSMCL        at 0 range 12 .. 12;
      FTXSMDL        at 0 range 13 .. 13;
      CDOFFDL        at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      TDDL           at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      PDEN           at 0 range 18 .. 18;
      TCLKPREPEN     at 0 range 19 .. 19;
      TCLKZEROEN     at 0 range 20 .. 20;
      THSPREPEN      at 0 range 21 .. 21;
      THSTRAILEN     at 0 range 22 .. 22;
      THSZEROEN      at 0 range 23 .. 23;
      TLPXDEN        at 0 range 24 .. 24;
      THSEXITEN      at 0 range 25 .. 25;
      TLPXCEN        at 0 range 26 .. 26;
      TCLKPOSTEN     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   ------------------------
   -- DSI_WPCR2_Register --
   ------------------------

   subtype DSI_WPCR2_HSTXDCL_Field is STM32_SVD.UInt2;
   subtype DSI_WPCR2_HSTXDLL_Field is STM32_SVD.UInt2;
   subtype DSI_WPCR2_LPSRCL_Field is STM32_SVD.UInt2;
   subtype DSI_WPCR2_LPSRDL_Field is STM32_SVD.UInt2;
   subtype DSI_WPCR2_SDCC_Field is STM32_SVD.Bit;
   subtype DSI_WPCR2_HSTXSRCCL_Field is STM32_SVD.UInt2;
   subtype DSI_WPCR2_HSTXSRCDL_Field is STM32_SVD.UInt2;
   subtype DSI_WPCR2_FLPRXLPM_Field is STM32_SVD.Bit;
   subtype DSI_WPCR2_LPRXFT_Field is STM32_SVD.UInt2;

   --  DSI Wrapper PHY Configuration Register 2
   type DSI_WPCR2_Register is record
      --  High-Speed Transmission Delay on Clock Lane
      HSTXDCL        : DSI_WPCR2_HSTXDCL_Field := 16#0#;
      --  High-Speed Transmission Delay on Data Lanes
      HSTXDLL        : DSI_WPCR2_HSTXDLL_Field := 16#0#;
      --  unspecified
      Reserved_4_5   : STM32_SVD.UInt2 := 16#0#;
      --  Low-Power transmission Slew Rate Compensation on Clock Lane
      LPSRCL         : DSI_WPCR2_LPSRCL_Field := 16#0#;
      --  Low-Power transmission Slew Rate Compensation on Data Lanes
      LPSRDL         : DSI_WPCR2_LPSRDL_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : STM32_SVD.UInt2 := 16#0#;
      --  SDD Control
      SDCC           : DSI_WPCR2_SDCC_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : STM32_SVD.UInt3 := 16#0#;
      --  High-Speed Transmission Slew Rate Control on Clock Lane
      HSTXSRCCL      : DSI_WPCR2_HSTXSRCCL_Field := 16#0#;
      --  High-Speed Transmission Slew Rate Control on Data Lanes
      HSTXSRCDL      : DSI_WPCR2_HSTXSRCDL_Field := 16#0#;
      --  unspecified
      Reserved_20_21 : STM32_SVD.UInt2 := 16#0#;
      --  Forces LP Receiver in Low-Power Mode
      FLPRXLPM       : DSI_WPCR2_FLPRXLPM_Field := 16#0#;
      --  unspecified
      Reserved_23_24 : STM32_SVD.UInt2 := 16#0#;
      --  Low-Power RX low-pass Filtering Tuning
      LPRXFT         : DSI_WPCR2_LPRXFT_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32_SVD.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WPCR2_Register use record
      HSTXDCL        at 0 range 0 .. 1;
      HSTXDLL        at 0 range 2 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      LPSRCL         at 0 range 6 .. 7;
      LPSRDL         at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      SDCC           at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      HSTXSRCCL      at 0 range 16 .. 17;
      HSTXSRCDL      at 0 range 18 .. 19;
      Reserved_20_21 at 0 range 20 .. 21;
      FLPRXLPM       at 0 range 22 .. 22;
      Reserved_23_24 at 0 range 23 .. 24;
      LPRXFT         at 0 range 25 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   -----------------------
   -- DSI_WPCR_Register --
   -----------------------

   subtype DSI_WPCR3_TCLKPREP_Field is STM32_SVD.Byte;
   subtype DSI_WPCR3_TCLKZEO_Field is STM32_SVD.Byte;
   subtype DSI_WPCR3_THSPREP_Field is STM32_SVD.Byte;
   subtype DSI_WPCR3_THSTRAIL_Field is STM32_SVD.Byte;

   --  DSI Wrapper PHY Configuration Register 3
   type DSI_WPCR_Register is record
      --  tCLK-PREPARE
      TCLKPREP : DSI_WPCR3_TCLKPREP_Field := 16#0#;
      --  tCLK-ZERO
      TCLKZEO  : DSI_WPCR3_TCLKZEO_Field := 16#0#;
      --  tHS-PREPARE
      THSPREP  : DSI_WPCR3_THSPREP_Field := 16#0#;
      --  tHSTRAIL
      THSTRAIL : DSI_WPCR3_THSTRAIL_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WPCR_Register use record
      TCLKPREP at 0 range 0 .. 7;
      TCLKZEO  at 0 range 8 .. 15;
      THSPREP  at 0 range 16 .. 23;
      THSTRAIL at 0 range 24 .. 31;
   end record;

   ------------------------
   -- DSI_WPCR5_Register --
   ------------------------

   subtype DSI_WPCR5_THSZERO_Field is STM32_SVD.Byte;

   --  DSI Wrapper PHY Configuration Register 5
   type DSI_WPCR5_Register is record
      --  tCLK-POST
      THSZERO       : DSI_WPCR5_THSZERO_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WPCR5_Register use record
      THSZERO       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   ------------------------
   -- DSI_WRPCR_Register --
   ------------------------

   subtype DSI_WRPCR_PLLEN_Field is STM32_SVD.Bit;
   subtype DSI_WRPCR_NDIV_Field is STM32_SVD.UInt7;
   subtype DSI_WRPCR_IDF_Field is STM32_SVD.UInt4;
   subtype DSI_WRPCR_ODF_Field is STM32_SVD.UInt2;
   subtype DSI_WRPCR_REGEN_Field is STM32_SVD.Bit;

   --  DSI Wrapper Regulator and PLL Control Register
   type DSI_WRPCR_Register is record
      --  PLL Enable
      PLLEN          : DSI_WRPCR_PLLEN_Field := 16#0#;
      --  unspecified
      Reserved_1_1   : STM32_SVD.Bit := 16#0#;
      --  PLL Loop Division Factor
      NDIV           : DSI_WRPCR_NDIV_Field := 16#0#;
      --  unspecified
      Reserved_9_10  : STM32_SVD.UInt2 := 16#0#;
      --  PLL Input Division Factor
      IDF            : DSI_WRPCR_IDF_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32_SVD.Bit := 16#0#;
      --  PLL Output Division Factor
      ODF            : DSI_WRPCR_ODF_Field := 16#0#;
      --  unspecified
      Reserved_18_23 : STM32_SVD.UInt6 := 16#0#;
      --  Regulator Enable
      REGEN          : DSI_WRPCR_REGEN_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DSI_WRPCR_Register use record
      PLLEN          at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      NDIV           at 0 range 2 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      IDF            at 0 range 11 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      ODF            at 0 range 16 .. 17;
      Reserved_18_23 at 0 range 18 .. 23;
      REGEN          at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DSI Host
   type DSIHOST_Peripheral is record
      --  DSI Host Version Register
      DSI_VR      : STM32_SVD.Word;
      --  DSI Host Control Register
      DSI_CR      : DSI_CR_Register;
      --  DSI HOST Clock Control Register
      DSIHSOT_CCR : DSIHSOT_CCR_Register;
      --  DSI Host LTDC VCID Register
      DSI_LVCIDR  : DSI_LVCIDR_Register;
      --  DSI Host LTDC Color Coding Register
      DSI_LCOLCR  : DSI_LCOLCR_Register;
      --  DSI Host LTDC Polarity Configuration Register
      DSI_LPCR    : DSI_LPCR_Register;
      --  DSI Host Low-Power Mode Configuration Register
      DSI_LPMCR   : DSI_LPMCR_Register;
      --  DSI Host Protocol Configuration Register
      DSI_PCR     : DSI_PCR_Register;
      --  DSI Host Generic VCID Register
      DSI_GVCIDR  : DSI_GVCIDR_Register;
      --  DSI Host Mode Configuration Register
      DSI_MCR     : DSI_MCR_Register;
      --  DSI Host Video mode Configuration Register
      DSI_VMCR    : DSI_VMCR_Register;
      --  DSI Host Video Packet Configuration Register
      DSI_VPCR    : DSI_VPCR_Register;
      --  DSI Host Video Chunks Configuration Register
      DSI_VCCR    : DSI_VCCR_Register;
      --  DSI Host Video Null Packet Configuration Register
      DSI_VNPCR   : DSI_VNPCR_Register;
      --  DSI Host Video HSA Configuration Register
      DSI_VHSACR  : DSI_VHSACR_Register;
      --  DSI Host Video HBP Configuration Register
      DSI_VHBPCR  : DSI_VHBPCR_Register;
      --  DSI Host Video Line Configuration Register
      DSI_VLCR    : DSI_VLCR_Register;
      --  DSI Host Video VSA Configuration Register
      DSI_VVSACR  : DSI_VVSACR_Register;
      --  DSI Host Video VBP Configuration Register
      DSI_VVBPCR  : DSI_VVBPCR_Register;
      --  DSI Host Video VFP Configuration Register
      DSI_VVFPCR  : DSI_VVFPCR_Register;
      --  DSI Host Video VA Configuration Register
      DSI_VVACR   : DSI_VVACR_Register;
      --  DSI Host LTDC Command Configuration Register
      DSI_LCCR    : DSI_LCCR_Register;
      --  DSI Host Command mode Configuration Register
      DSI_CMCR    : DSI_CMCR_Register;
      --  DSI Host Generic Header Configuration Register
      DSI_GHCR    : DSI_GHCR_Register;
      --  DSI Host Generic Payload Data Register
      DSI_GPDR    : DSI_GPDR_Register;
      --  DSI Host Generic Packet Status Register
      DSI_GPSR    : DSI_GPSR_Register;
      --  DSI Host Timeout Counter Configuration Register1
      DSI_TCCR1   : DSI_TCCR1_Register;
      --  DSI Host Timeout Counter Configuration Register2
      DSI_TCCR2   : DSI_TCCR_Register;
      --  DSI Host Timeout Counter Configuration Register3
      DSI_TCCR3   : DSI_TCCR_Register;
      --  DSI Host Timeout Counter Configuration Register4
      DSI_TCCR4   : DSI_TCCR4_Register;
      --  DSI Host Timeout Counter Configuration Register5
      DSI_TCCR5   : DSI_TCCR_Register;
      --  DSI Host Timeout Counter Configuration Register6
      DSI_TCCR6   : DSI_TCCR_Register;
      --  DSI Host Clock Lane Configuration Register
      DSI_CLCR    : DSI_CLCR_Register;
      --  DSI Host Clock Lane Timer Configuration Register
      DSI_CLTCR   : DSI_CLTCR_Register;
      --  DSI Host Data Lane Timer Configuration Register
      DSI_DLTCR   : DSI_DLTCR_Register;
      --  DSI Host PHY Control Register
      DSI_PCTLR   : DSI_PCTLR_Register;
      --  DSI Host PHY Configuration Register
      DSI_PCCONFR : DSI_PCCONFR_Register;
      --  DSI Host PHY ULPS Control Register
      DSI_PUCR    : DSI_PUCR_Register;
      --  DSI Host PHY TX Triggers Configuration Register
      DSI_PTTCR   : DSI_PTTCR_Register;
      --  DSI Host PHY Status Register
      DSI_PSR     : DSI_PSR_Register;
      --  DSI Host Interrupt & Status Register 0
      DSI_ISR0    : DSI_ISR0_Register;
      --  DSI Host Interrupt & Status Register 1
      DSI_ISR1    : DSI_ISR1_Register;
      --  DSI Host Interrupt Enable Register 0
      DSI_IER0    : DSI_IER0_Register;
      --  DSI Host Interrupt Enable Register 1
      DSI_IER1    : DSI_IER1_Register;
      --  DSI Host Force Interrupt Register 0
      DSI_FIR0    : DSI_FIR0_Register;
      --  DSI Host Force Interrupt Register 1
      DSI_FIR1    : DSI_FIR1_Register;
      --  DSI Host Video Shadow Control Register
      DSI_VSCR    : DSI_VSCR_Register;
      --  DSI Host LTDC Current VCID Register
      DSI_LCVCIDR : DSI_LCVCIDR_Register;
      --  DSI Host LTDC Current Color Coding Register
      DSI_LCCCR   : DSI_LCCCR_Register;
      --  DSI Host Low-power Mode Current Configuration Register
      DSI_LPMCCR  : DSI_LPMCCR_Register;
      --  DSI Host Video mode Current Configuration Register
      DSI_VMCCR   : DSI_VMCCR_Register;
      --  DSI Host Video Packet Current Configuration Register
      DSI_VPCCR   : DSI_VPCCR_Register;
      --  DSI Host Video Chunks Current Configuration Register
      DSI_VCCCR   : DSI_VCCCR_Register;
      --  DSI Host Video Null Packet Current Configuration Register
      DSI_VNPCCR  : DSI_VNPCCR_Register;
      --  DSI Host Video HSA Current Configuration Register
      DSI_VHSACCR : DSI_VHSACCR_Register;
      --  DSI Host Video HBP Current Configuration Register
      DSI_VHBPCCR : DSI_VHBPCCR_Register;
      --  DSI Host Video Line Current Configuration Register
      DSI_VLCCR   : DSI_VLCCR_Register;
      --  DSI Host Video VSA Current Configuration Register
      DSI_VVSACCR : DSI_VVSACCR_Register;
      --  DSI Host Video VBP Current Configuration Register
      DSI_VVBPCCR : DSI_VVBPCCR_Register;
      --  DSI Host Video VFP Current Configuration Register
      DSI_VVFPCCR : DSI_VVFPCCR_Register;
      --  DSI Host Video VA Current Configuration Register
      DSI_VVACCR  : DSI_VVACCR_Register;
      --  DSI Wrapper Configuration Register
      DSI_WCFGR   : DSI_WCFGR_Register;
      --  DSI Wrapper Control Register
      DSI_WCR     : DSI_WCR_Register;
      --  DSI Wrapper Interrupt Enable Register
      DSI_WIER    : DSI_WIER_Register;
      --  DSI Wrapper Interrupt & Status Register
      DSI_WISR    : DSI_WISR_Register;
      --  DSI Wrapper Interrupt Flag Clear Register
      DSI_WIFCR   : DSI_WIFCR_Register;
      --  DSI Wrapper PHY Configuration Register 1
      DSI_WPCR1   : DSI_WPCR1_Register;
      --  DSI Wrapper PHY Configuration Register 2
      DSI_WPCR2   : DSI_WPCR2_Register;
      --  DSI Wrapper PHY Configuration Register 3
      DSI_WPCR3   : DSI_WPCR_Register;
      --  DSI_WPCR4
      DSI_WPCR4   : DSI_WPCR_Register;
      --  DSI Wrapper PHY Configuration Register 5
      DSI_WPCR5   : DSI_WPCR5_Register;
      --  DSI Wrapper Regulator and PLL Control Register
      DSI_WRPCR   : DSI_WRPCR_Register;
   end record
     with Volatile;

   for DSIHOST_Peripheral use record
      DSI_VR      at 0 range 0 .. 31;
      DSI_CR      at 4 range 0 .. 31;
      DSIHSOT_CCR at 8 range 0 .. 31;
      DSI_LVCIDR  at 12 range 0 .. 31;
      DSI_LCOLCR  at 16 range 0 .. 31;
      DSI_LPCR    at 20 range 0 .. 31;
      DSI_LPMCR   at 24 range 0 .. 31;
      DSI_PCR     at 44 range 0 .. 31;
      DSI_GVCIDR  at 48 range 0 .. 31;
      DSI_MCR     at 52 range 0 .. 31;
      DSI_VMCR    at 56 range 0 .. 31;
      DSI_VPCR    at 60 range 0 .. 31;
      DSI_VCCR    at 64 range 0 .. 31;
      DSI_VNPCR   at 68 range 0 .. 31;
      DSI_VHSACR  at 72 range 0 .. 31;
      DSI_VHBPCR  at 76 range 0 .. 31;
      DSI_VLCR    at 80 range 0 .. 31;
      DSI_VVSACR  at 84 range 0 .. 31;
      DSI_VVBPCR  at 88 range 0 .. 31;
      DSI_VVFPCR  at 92 range 0 .. 31;
      DSI_VVACR   at 96 range 0 .. 31;
      DSI_LCCR    at 100 range 0 .. 31;
      DSI_CMCR    at 104 range 0 .. 31;
      DSI_GHCR    at 108 range 0 .. 31;
      DSI_GPDR    at 112 range 0 .. 31;
      DSI_GPSR    at 116 range 0 .. 31;
      DSI_TCCR1   at 120 range 0 .. 31;
      DSI_TCCR2   at 124 range 0 .. 31;
      DSI_TCCR3   at 128 range 0 .. 31;
      DSI_TCCR4   at 132 range 0 .. 31;
      DSI_TCCR5   at 136 range 0 .. 31;
      DSI_TCCR6   at 140 range 0 .. 31;
      DSI_CLCR    at 148 range 0 .. 31;
      DSI_CLTCR   at 152 range 0 .. 31;
      DSI_DLTCR   at 156 range 0 .. 31;
      DSI_PCTLR   at 160 range 0 .. 31;
      DSI_PCCONFR at 164 range 0 .. 31;
      DSI_PUCR    at 168 range 0 .. 31;
      DSI_PTTCR   at 172 range 0 .. 31;
      DSI_PSR     at 176 range 0 .. 31;
      DSI_ISR0    at 188 range 0 .. 31;
      DSI_ISR1    at 192 range 0 .. 31;
      DSI_IER0    at 196 range 0 .. 31;
      DSI_IER1    at 200 range 0 .. 31;
      DSI_FIR0    at 216 range 0 .. 31;
      DSI_FIR1    at 220 range 0 .. 31;
      DSI_VSCR    at 256 range 0 .. 31;
      DSI_LCVCIDR at 268 range 0 .. 31;
      DSI_LCCCR   at 272 range 0 .. 31;
      DSI_LPMCCR  at 280 range 0 .. 31;
      DSI_VMCCR   at 312 range 0 .. 31;
      DSI_VPCCR   at 316 range 0 .. 31;
      DSI_VCCCR   at 320 range 0 .. 31;
      DSI_VNPCCR  at 324 range 0 .. 31;
      DSI_VHSACCR at 328 range 0 .. 31;
      DSI_VHBPCCR at 332 range 0 .. 31;
      DSI_VLCCR   at 336 range 0 .. 31;
      DSI_VVSACCR at 340 range 0 .. 31;
      DSI_VVBPCCR at 344 range 0 .. 31;
      DSI_VVFPCCR at 348 range 0 .. 31;
      DSI_VVACCR  at 352 range 0 .. 31;
      DSI_WCFGR   at 1024 range 0 .. 31;
      DSI_WCR     at 1028 range 0 .. 31;
      DSI_WIER    at 1032 range 0 .. 31;
      DSI_WISR    at 1036 range 0 .. 31;
      DSI_WIFCR   at 1040 range 0 .. 31;
      DSI_WPCR1   at 1048 range 0 .. 31;
      DSI_WPCR2   at 1052 range 0 .. 31;
      DSI_WPCR3   at 1056 range 0 .. 31;
      DSI_WPCR4   at 1060 range 0 .. 31;
      DSI_WPCR5   at 1064 range 0 .. 31;
      DSI_WRPCR   at 1072 range 0 .. 31;
   end record;

   --  DSI Host
   DSIHOST_Periph : aliased DSIHOST_Peripheral
     with Import, Address => System'To_Address (16#40016C00#);

end STM32_SVD.DSIHOST;
