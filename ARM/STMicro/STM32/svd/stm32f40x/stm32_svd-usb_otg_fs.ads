--  Automatically generated from STM32F40x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.USB_OTG_FS is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -------------------------
   -- FS_GOTGCTL_Register --
   -------------------------

   subtype FS_GOTGCTL_SRQSCS_Field is STM32_SVD.Bit;
   subtype FS_GOTGCTL_SRQ_Field is STM32_SVD.Bit;
   subtype FS_GOTGCTL_HNGSCS_Field is STM32_SVD.Bit;
   subtype FS_GOTGCTL_HNPRQ_Field is STM32_SVD.Bit;
   subtype FS_GOTGCTL_HSHNPEN_Field is STM32_SVD.Bit;
   subtype FS_GOTGCTL_DHNPEN_Field is STM32_SVD.Bit;
   subtype FS_GOTGCTL_CIDSTS_Field is STM32_SVD.Bit;
   subtype FS_GOTGCTL_DBCT_Field is STM32_SVD.Bit;
   subtype FS_GOTGCTL_ASVLD_Field is STM32_SVD.Bit;
   subtype FS_GOTGCTL_BSVLD_Field is STM32_SVD.Bit;

   --  OTG_FS control and status register (OTG_FS_GOTGCTL)
   type FS_GOTGCTL_Register is record
      --  Session request success
      SRQSCS         : FS_GOTGCTL_SRQSCS_Field := 16#0#;
      --  Session request
      SRQ            : FS_GOTGCTL_SRQ_Field := 16#0#;
      --  unspecified
      Reserved_2_7   : STM32_SVD.UInt6 := 16#0#;
      --  Host negotiation success
      HNGSCS         : FS_GOTGCTL_HNGSCS_Field := 16#0#;
      --  HNP request
      HNPRQ          : FS_GOTGCTL_HNPRQ_Field := 16#0#;
      --  Host set HNP enable
      HSHNPEN        : FS_GOTGCTL_HSHNPEN_Field := 16#0#;
      --  Device HNP enabled
      DHNPEN         : FS_GOTGCTL_DHNPEN_Field := 16#1#;
      --  unspecified
      Reserved_12_15 : STM32_SVD.UInt4 := 16#0#;
      --  Connector ID status
      CIDSTS         : FS_GOTGCTL_CIDSTS_Field := 16#0#;
      --  Long/short debounce time
      DBCT           : FS_GOTGCTL_DBCT_Field := 16#0#;
      --  A-session valid
      ASVLD          : FS_GOTGCTL_ASVLD_Field := 16#0#;
      --  B-session valid
      BSVLD          : FS_GOTGCTL_BSVLD_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32_SVD.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GOTGCTL_Register use record
      SRQSCS         at 0 range 0 .. 0;
      SRQ            at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      HNGSCS         at 0 range 8 .. 8;
      HNPRQ          at 0 range 9 .. 9;
      HSHNPEN        at 0 range 10 .. 10;
      DHNPEN         at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      CIDSTS         at 0 range 16 .. 16;
      DBCT           at 0 range 17 .. 17;
      ASVLD          at 0 range 18 .. 18;
      BSVLD          at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   -------------------------
   -- FS_GOTGINT_Register --
   -------------------------

   subtype FS_GOTGINT_SEDET_Field is STM32_SVD.Bit;
   subtype FS_GOTGINT_SRSSCHG_Field is STM32_SVD.Bit;
   subtype FS_GOTGINT_HNSSCHG_Field is STM32_SVD.Bit;
   subtype FS_GOTGINT_HNGDET_Field is STM32_SVD.Bit;
   subtype FS_GOTGINT_ADTOCHG_Field is STM32_SVD.Bit;
   subtype FS_GOTGINT_DBCDNE_Field is STM32_SVD.Bit;

   --  OTG_FS interrupt register (OTG_FS_GOTGINT)
   type FS_GOTGINT_Register is record
      --  unspecified
      Reserved_0_1   : STM32_SVD.UInt2 := 16#0#;
      --  Session end detected
      SEDET          : FS_GOTGINT_SEDET_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : STM32_SVD.UInt5 := 16#0#;
      --  Session request success status change
      SRSSCHG        : FS_GOTGINT_SRSSCHG_Field := 16#0#;
      --  Host negotiation success status change
      HNSSCHG        : FS_GOTGINT_HNSSCHG_Field := 16#0#;
      --  unspecified
      Reserved_10_16 : STM32_SVD.UInt7 := 16#0#;
      --  Host negotiation detected
      HNGDET         : FS_GOTGINT_HNGDET_Field := 16#0#;
      --  A-device timeout change
      ADTOCHG        : FS_GOTGINT_ADTOCHG_Field := 16#0#;
      --  Debounce done
      DBCDNE         : FS_GOTGINT_DBCDNE_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32_SVD.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GOTGINT_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      SEDET          at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      SRSSCHG        at 0 range 8 .. 8;
      HNSSCHG        at 0 range 9 .. 9;
      Reserved_10_16 at 0 range 10 .. 16;
      HNGDET         at 0 range 17 .. 17;
      ADTOCHG        at 0 range 18 .. 18;
      DBCDNE         at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   -------------------------
   -- FS_GAHBCFG_Register --
   -------------------------

   subtype FS_GAHBCFG_GINT_Field is STM32_SVD.Bit;
   subtype FS_GAHBCFG_TXFELVL_Field is STM32_SVD.Bit;
   subtype FS_GAHBCFG_PTXFELVL_Field is STM32_SVD.Bit;

   --  OTG_FS AHB configuration register (OTG_FS_GAHBCFG)
   type FS_GAHBCFG_Register is record
      --  Global interrupt mask
      GINT          : FS_GAHBCFG_GINT_Field := 16#0#;
      --  unspecified
      Reserved_1_6  : STM32_SVD.UInt6 := 16#0#;
      --  TxFIFO empty level
      TXFELVL       : FS_GAHBCFG_TXFELVL_Field := 16#0#;
      --  Periodic TxFIFO empty level
      PTXFELVL      : FS_GAHBCFG_PTXFELVL_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : STM32_SVD.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GAHBCFG_Register use record
      GINT          at 0 range 0 .. 0;
      Reserved_1_6  at 0 range 1 .. 6;
      TXFELVL       at 0 range 7 .. 7;
      PTXFELVL      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   -------------------------
   -- FS_GUSBCFG_Register --
   -------------------------

   subtype FS_GUSBCFG_TOCAL_Field is STM32_SVD.UInt3;
   subtype FS_GUSBCFG_PHYSEL_Field is STM32_SVD.Bit;
   subtype FS_GUSBCFG_SRPCAP_Field is STM32_SVD.Bit;
   subtype FS_GUSBCFG_HNPCAP_Field is STM32_SVD.Bit;
   subtype FS_GUSBCFG_TRDT_Field is STM32_SVD.UInt4;
   subtype FS_GUSBCFG_FHMOD_Field is STM32_SVD.Bit;
   subtype FS_GUSBCFG_FDMOD_Field is STM32_SVD.Bit;
   subtype FS_GUSBCFG_CTXPKT_Field is STM32_SVD.Bit;

   --  OTG_FS USB configuration register (OTG_FS_GUSBCFG)
   type FS_GUSBCFG_Register is record
      --  FS timeout calibration
      TOCAL          : FS_GUSBCFG_TOCAL_Field := 16#0#;
      --  unspecified
      Reserved_3_5   : STM32_SVD.UInt3 := 16#0#;
      --  Full Speed serial transceiver select
      PHYSEL         : FS_GUSBCFG_PHYSEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32_SVD.Bit := 16#0#;
      --  SRP-capable
      SRPCAP         : FS_GUSBCFG_SRPCAP_Field := 16#0#;
      --  HNP-capable
      HNPCAP         : FS_GUSBCFG_HNPCAP_Field := 16#1#;
      --  USB turnaround time
      TRDT           : FS_GUSBCFG_TRDT_Field := 16#2#;
      --  unspecified
      Reserved_14_28 : STM32_SVD.UInt15 := 16#0#;
      --  Force host mode
      FHMOD          : FS_GUSBCFG_FHMOD_Field := 16#0#;
      --  Force device mode
      FDMOD          : FS_GUSBCFG_FDMOD_Field := 16#0#;
      --  Corrupt Tx packet
      CTXPKT         : FS_GUSBCFG_CTXPKT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GUSBCFG_Register use record
      TOCAL          at 0 range 0 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      PHYSEL         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SRPCAP         at 0 range 8 .. 8;
      HNPCAP         at 0 range 9 .. 9;
      TRDT           at 0 range 10 .. 13;
      Reserved_14_28 at 0 range 14 .. 28;
      FHMOD          at 0 range 29 .. 29;
      FDMOD          at 0 range 30 .. 30;
      CTXPKT         at 0 range 31 .. 31;
   end record;

   -------------------------
   -- FS_GRSTCTL_Register --
   -------------------------

   subtype FS_GRSTCTL_CSRST_Field is STM32_SVD.Bit;
   subtype FS_GRSTCTL_HSRST_Field is STM32_SVD.Bit;
   subtype FS_GRSTCTL_FCRST_Field is STM32_SVD.Bit;
   subtype FS_GRSTCTL_RXFFLSH_Field is STM32_SVD.Bit;
   subtype FS_GRSTCTL_TXFFLSH_Field is STM32_SVD.Bit;
   subtype FS_GRSTCTL_TXFNUM_Field is STM32_SVD.UInt5;
   subtype FS_GRSTCTL_AHBIDL_Field is STM32_SVD.Bit;

   --  OTG_FS reset register (OTG_FS_GRSTCTL)
   type FS_GRSTCTL_Register is record
      --  Core soft reset
      CSRST          : FS_GRSTCTL_CSRST_Field := 16#0#;
      --  HCLK soft reset
      HSRST          : FS_GRSTCTL_HSRST_Field := 16#0#;
      --  Host frame counter reset
      FCRST          : FS_GRSTCTL_FCRST_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : STM32_SVD.Bit := 16#0#;
      --  RxFIFO flush
      RXFFLSH        : FS_GRSTCTL_RXFFLSH_Field := 16#0#;
      --  TxFIFO flush
      TXFFLSH        : FS_GRSTCTL_TXFFLSH_Field := 16#0#;
      --  TxFIFO number
      TXFNUM         : FS_GRSTCTL_TXFNUM_Field := 16#0#;
      --  unspecified
      Reserved_11_30 : STM32_SVD.UInt20 := 16#40000#;
      --  AHB master idle
      AHBIDL         : FS_GRSTCTL_AHBIDL_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GRSTCTL_Register use record
      CSRST          at 0 range 0 .. 0;
      HSRST          at 0 range 1 .. 1;
      FCRST          at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      RXFFLSH        at 0 range 4 .. 4;
      TXFFLSH        at 0 range 5 .. 5;
      TXFNUM         at 0 range 6 .. 10;
      Reserved_11_30 at 0 range 11 .. 30;
      AHBIDL         at 0 range 31 .. 31;
   end record;

   -------------------------
   -- FS_GINTSTS_Register --
   -------------------------

   subtype FS_GINTSTS_CMOD_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_MMIS_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_OTGINT_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_SOF_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_RXFLVL_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_NPTXFE_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_GINAKEFF_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_GOUTNAKEFF_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_ESUSP_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_USBSUSP_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_USBRST_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_ENUMDNE_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_ISOODRP_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_EOPF_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_IEPINT_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_OEPINT_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_IISOIXFR_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_IPXFR_INCOMPISOOUT_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_HPRTINT_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_HCINT_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_PTXFE_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_CIDSCHG_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_DISCINT_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_SRQINT_Field is STM32_SVD.Bit;
   subtype FS_GINTSTS_WKUPINT_Field is STM32_SVD.Bit;

   --  OTG_FS core interrupt register (OTG_FS_GINTSTS)
   type FS_GINTSTS_Register is record
      --  Current mode of operation
      CMOD               : FS_GINTSTS_CMOD_Field := 16#0#;
      --  Mode mismatch interrupt
      MMIS               : FS_GINTSTS_MMIS_Field := 16#0#;
      --  OTG interrupt
      OTGINT             : FS_GINTSTS_OTGINT_Field := 16#0#;
      --  Start of frame
      SOF                : FS_GINTSTS_SOF_Field := 16#0#;
      --  RxFIFO non-empty
      RXFLVL             : FS_GINTSTS_RXFLVL_Field := 16#0#;
      --  Non-periodic TxFIFO empty
      NPTXFE             : FS_GINTSTS_NPTXFE_Field := 16#1#;
      --  Global IN non-periodic NAK effective
      GINAKEFF           : FS_GINTSTS_GINAKEFF_Field := 16#0#;
      --  Global OUT NAK effective
      GOUTNAKEFF         : FS_GINTSTS_GOUTNAKEFF_Field := 16#0#;
      --  unspecified
      Reserved_8_9       : STM32_SVD.UInt2 := 16#0#;
      --  Early suspend
      ESUSP              : FS_GINTSTS_ESUSP_Field := 16#0#;
      --  USB suspend
      USBSUSP            : FS_GINTSTS_USBSUSP_Field := 16#0#;
      --  USB reset
      USBRST             : FS_GINTSTS_USBRST_Field := 16#0#;
      --  Enumeration done
      ENUMDNE            : FS_GINTSTS_ENUMDNE_Field := 16#0#;
      --  Isochronous OUT packet dropped interrupt
      ISOODRP            : FS_GINTSTS_ISOODRP_Field := 16#0#;
      --  End of periodic frame interrupt
      EOPF               : FS_GINTSTS_EOPF_Field := 16#0#;
      --  unspecified
      Reserved_16_17     : STM32_SVD.UInt2 := 16#0#;
      --  IN endpoint interrupt
      IEPINT             : FS_GINTSTS_IEPINT_Field := 16#0#;
      --  OUT endpoint interrupt
      OEPINT             : FS_GINTSTS_OEPINT_Field := 16#0#;
      --  Incomplete isochronous IN transfer
      IISOIXFR           : FS_GINTSTS_IISOIXFR_Field := 16#0#;
      --  Incomplete periodic transfer(Host mode)/Incomplete isochronous OUT
      --  transfer(Device mode)
      IPXFR_INCOMPISOOUT : FS_GINTSTS_IPXFR_INCOMPISOOUT_Field := 16#0#;
      --  unspecified
      Reserved_22_23     : STM32_SVD.UInt2 := 16#0#;
      --  Host port interrupt
      HPRTINT            : FS_GINTSTS_HPRTINT_Field := 16#0#;
      --  Host channels interrupt
      HCINT              : FS_GINTSTS_HCINT_Field := 16#0#;
      --  Periodic TxFIFO empty
      PTXFE              : FS_GINTSTS_PTXFE_Field := 16#1#;
      --  unspecified
      Reserved_27_27     : STM32_SVD.Bit := 16#0#;
      --  Connector ID status change
      CIDSCHG            : FS_GINTSTS_CIDSCHG_Field := 16#0#;
      --  Disconnect detected interrupt
      DISCINT            : FS_GINTSTS_DISCINT_Field := 16#0#;
      --  Session request/new session detected interrupt
      SRQINT             : FS_GINTSTS_SRQINT_Field := 16#0#;
      --  Resume/remote wakeup detected interrupt
      WKUPINT            : FS_GINTSTS_WKUPINT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GINTSTS_Register use record
      CMOD               at 0 range 0 .. 0;
      MMIS               at 0 range 1 .. 1;
      OTGINT             at 0 range 2 .. 2;
      SOF                at 0 range 3 .. 3;
      RXFLVL             at 0 range 4 .. 4;
      NPTXFE             at 0 range 5 .. 5;
      GINAKEFF           at 0 range 6 .. 6;
      GOUTNAKEFF         at 0 range 7 .. 7;
      Reserved_8_9       at 0 range 8 .. 9;
      ESUSP              at 0 range 10 .. 10;
      USBSUSP            at 0 range 11 .. 11;
      USBRST             at 0 range 12 .. 12;
      ENUMDNE            at 0 range 13 .. 13;
      ISOODRP            at 0 range 14 .. 14;
      EOPF               at 0 range 15 .. 15;
      Reserved_16_17     at 0 range 16 .. 17;
      IEPINT             at 0 range 18 .. 18;
      OEPINT             at 0 range 19 .. 19;
      IISOIXFR           at 0 range 20 .. 20;
      IPXFR_INCOMPISOOUT at 0 range 21 .. 21;
      Reserved_22_23     at 0 range 22 .. 23;
      HPRTINT            at 0 range 24 .. 24;
      HCINT              at 0 range 25 .. 25;
      PTXFE              at 0 range 26 .. 26;
      Reserved_27_27     at 0 range 27 .. 27;
      CIDSCHG            at 0 range 28 .. 28;
      DISCINT            at 0 range 29 .. 29;
      SRQINT             at 0 range 30 .. 30;
      WKUPINT            at 0 range 31 .. 31;
   end record;

   -------------------------
   -- FS_GINTMSK_Register --
   -------------------------

   subtype FS_GINTMSK_MMISM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_OTGINT_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_SOFM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_RXFLVLM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_NPTXFEM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_GINAKEFFM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_GONAKEFFM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_ESUSPM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_USBSUSPM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_USBRST_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_ENUMDNEM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_ISOODRPM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_EOPFM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_EPMISM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_IEPINT_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_OEPINT_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_IISOIXFRM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_IPXFRM_IISOOXFRM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_PRTIM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_HCIM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_PTXFEM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_CIDSCHGM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_DISCINT_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_SRQIM_Field is STM32_SVD.Bit;
   subtype FS_GINTMSK_WUIM_Field is STM32_SVD.Bit;

   --  OTG_FS interrupt mask register (OTG_FS_GINTMSK)
   type FS_GINTMSK_Register is record
      --  unspecified
      Reserved_0_0     : STM32_SVD.Bit := 16#0#;
      --  Mode mismatch interrupt mask
      MMISM            : FS_GINTMSK_MMISM_Field := 16#0#;
      --  OTG interrupt mask
      OTGINT           : FS_GINTMSK_OTGINT_Field := 16#0#;
      --  Start of frame mask
      SOFM             : FS_GINTMSK_SOFM_Field := 16#0#;
      --  Receive FIFO non-empty mask
      RXFLVLM          : FS_GINTMSK_RXFLVLM_Field := 16#0#;
      --  Non-periodic TxFIFO empty mask
      NPTXFEM          : FS_GINTMSK_NPTXFEM_Field := 16#0#;
      --  Global non-periodic IN NAK effective mask
      GINAKEFFM        : FS_GINTMSK_GINAKEFFM_Field := 16#0#;
      --  Global OUT NAK effective mask
      GONAKEFFM        : FS_GINTMSK_GONAKEFFM_Field := 16#0#;
      --  unspecified
      Reserved_8_9     : STM32_SVD.UInt2 := 16#0#;
      --  Early suspend mask
      ESUSPM           : FS_GINTMSK_ESUSPM_Field := 16#0#;
      --  USB suspend mask
      USBSUSPM         : FS_GINTMSK_USBSUSPM_Field := 16#0#;
      --  USB reset mask
      USBRST           : FS_GINTMSK_USBRST_Field := 16#0#;
      --  Enumeration done mask
      ENUMDNEM         : FS_GINTMSK_ENUMDNEM_Field := 16#0#;
      --  Isochronous OUT packet dropped interrupt mask
      ISOODRPM         : FS_GINTMSK_ISOODRPM_Field := 16#0#;
      --  End of periodic frame interrupt mask
      EOPFM            : FS_GINTMSK_EOPFM_Field := 16#0#;
      --  unspecified
      Reserved_16_16   : STM32_SVD.Bit := 16#0#;
      --  Endpoint mismatch interrupt mask
      EPMISM           : FS_GINTMSK_EPMISM_Field := 16#0#;
      --  IN endpoints interrupt mask
      IEPINT           : FS_GINTMSK_IEPINT_Field := 16#0#;
      --  OUT endpoints interrupt mask
      OEPINT           : FS_GINTMSK_OEPINT_Field := 16#0#;
      --  Incomplete isochronous IN transfer mask
      IISOIXFRM        : FS_GINTMSK_IISOIXFRM_Field := 16#0#;
      --  Incomplete periodic transfer mask(Host mode)/Incomplete isochronous
      --  OUT transfer mask(Device mode)
      IPXFRM_IISOOXFRM : FS_GINTMSK_IPXFRM_IISOOXFRM_Field := 16#0#;
      --  unspecified
      Reserved_22_23   : STM32_SVD.UInt2 := 16#0#;
      --  Host port interrupt mask
      PRTIM            : FS_GINTMSK_PRTIM_Field := 16#0#;
      --  Host channels interrupt mask
      HCIM             : FS_GINTMSK_HCIM_Field := 16#0#;
      --  Periodic TxFIFO empty mask
      PTXFEM           : FS_GINTMSK_PTXFEM_Field := 16#0#;
      --  unspecified
      Reserved_27_27   : STM32_SVD.Bit := 16#0#;
      --  Connector ID status change mask
      CIDSCHGM         : FS_GINTMSK_CIDSCHGM_Field := 16#0#;
      --  Disconnect detected interrupt mask
      DISCINT          : FS_GINTMSK_DISCINT_Field := 16#0#;
      --  Session request/new session detected interrupt mask
      SRQIM            : FS_GINTMSK_SRQIM_Field := 16#0#;
      --  Resume/remote wakeup detected interrupt mask
      WUIM             : FS_GINTMSK_WUIM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GINTMSK_Register use record
      Reserved_0_0     at 0 range 0 .. 0;
      MMISM            at 0 range 1 .. 1;
      OTGINT           at 0 range 2 .. 2;
      SOFM             at 0 range 3 .. 3;
      RXFLVLM          at 0 range 4 .. 4;
      NPTXFEM          at 0 range 5 .. 5;
      GINAKEFFM        at 0 range 6 .. 6;
      GONAKEFFM        at 0 range 7 .. 7;
      Reserved_8_9     at 0 range 8 .. 9;
      ESUSPM           at 0 range 10 .. 10;
      USBSUSPM         at 0 range 11 .. 11;
      USBRST           at 0 range 12 .. 12;
      ENUMDNEM         at 0 range 13 .. 13;
      ISOODRPM         at 0 range 14 .. 14;
      EOPFM            at 0 range 15 .. 15;
      Reserved_16_16   at 0 range 16 .. 16;
      EPMISM           at 0 range 17 .. 17;
      IEPINT           at 0 range 18 .. 18;
      OEPINT           at 0 range 19 .. 19;
      IISOIXFRM        at 0 range 20 .. 20;
      IPXFRM_IISOOXFRM at 0 range 21 .. 21;
      Reserved_22_23   at 0 range 22 .. 23;
      PRTIM            at 0 range 24 .. 24;
      HCIM             at 0 range 25 .. 25;
      PTXFEM           at 0 range 26 .. 26;
      Reserved_27_27   at 0 range 27 .. 27;
      CIDSCHGM         at 0 range 28 .. 28;
      DISCINT          at 0 range 29 .. 29;
      SRQIM            at 0 range 30 .. 30;
      WUIM             at 0 range 31 .. 31;
   end record;

   --------------------------------
   -- FS_GRXSTSR_Device_Register --
   --------------------------------

   subtype FS_GRXSTSR_Device_EPNUM_Field is STM32_SVD.UInt4;
   subtype FS_GRXSTSR_Device_BCNT_Field is STM32_SVD.UInt11;
   subtype FS_GRXSTSR_Device_DPID_Field is STM32_SVD.UInt2;
   subtype FS_GRXSTSR_Device_PKTSTS_Field is STM32_SVD.UInt4;
   subtype FS_GRXSTSR_Device_FRMNUM_Field is STM32_SVD.UInt4;

   --  OTG_FS Receive status debug read(Device mode)
   type FS_GRXSTSR_Device_Register is record
      --  Endpoint number
      EPNUM          : FS_GRXSTSR_Device_EPNUM_Field;
      --  Byte count
      BCNT           : FS_GRXSTSR_Device_BCNT_Field;
      --  Data PID
      DPID           : FS_GRXSTSR_Device_DPID_Field;
      --  Packet status
      PKTSTS         : FS_GRXSTSR_Device_PKTSTS_Field;
      --  Frame number
      FRMNUM         : FS_GRXSTSR_Device_FRMNUM_Field;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GRXSTSR_Device_Register use record
      EPNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      FRMNUM         at 0 range 21 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   ------------------------------
   -- FS_GRXSTSR_Host_Register --
   ------------------------------

   subtype FS_GRXSTSR_Host_EPNUM_Field is STM32_SVD.UInt4;
   subtype FS_GRXSTSR_Host_BCNT_Field is STM32_SVD.UInt11;
   subtype FS_GRXSTSR_Host_DPID_Field is STM32_SVD.UInt2;
   subtype FS_GRXSTSR_Host_PKTSTS_Field is STM32_SVD.UInt4;
   subtype FS_GRXSTSR_Host_FRMNUM_Field is STM32_SVD.UInt4;

   --  OTG_FS Receive status debug read(Host mode)
   type FS_GRXSTSR_Host_Register is record
      --  Endpoint number
      EPNUM          : FS_GRXSTSR_Host_EPNUM_Field;
      --  Byte count
      BCNT           : FS_GRXSTSR_Host_BCNT_Field;
      --  Data PID
      DPID           : FS_GRXSTSR_Host_DPID_Field;
      --  Packet status
      PKTSTS         : FS_GRXSTSR_Host_PKTSTS_Field;
      --  Frame number
      FRMNUM         : FS_GRXSTSR_Host_FRMNUM_Field;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GRXSTSR_Host_Register use record
      EPNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      FRMNUM         at 0 range 21 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -------------------------
   -- FS_GRXFSIZ_Register --
   -------------------------

   subtype FS_GRXFSIZ_RXFD_Field is STM32_SVD.Short;

   --  OTG_FS Receive FIFO size register (OTG_FS_GRXFSIZ)
   type FS_GRXFSIZ_Register is record
      --  RxFIFO depth
      RXFD           : FS_GRXFSIZ_RXFD_Field := 16#200#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GRXFSIZ_Register use record
      RXFD           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ----------------------------------
   -- FS_GNPTXFSIZ_Device_Register --
   ----------------------------------

   subtype FS_GNPTXFSIZ_Device_TX0FSA_Field is STM32_SVD.Short;
   subtype FS_GNPTXFSIZ_Device_TX0FD_Field is STM32_SVD.Short;

   --  OTG_FS non-periodic transmit FIFO size register (Device mode)
   type FS_GNPTXFSIZ_Device_Register is record
      --  Endpoint 0 transmit RAM start address
      TX0FSA : FS_GNPTXFSIZ_Device_TX0FSA_Field := 16#200#;
      --  Endpoint 0 TxFIFO depth
      TX0FD  : FS_GNPTXFSIZ_Device_TX0FD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GNPTXFSIZ_Device_Register use record
      TX0FSA at 0 range 0 .. 15;
      TX0FD  at 0 range 16 .. 31;
   end record;

   --------------------------------
   -- FS_GNPTXFSIZ_Host_Register --
   --------------------------------

   subtype FS_GNPTXFSIZ_Host_NPTXFSA_Field is STM32_SVD.Short;
   subtype FS_GNPTXFSIZ_Host_NPTXFD_Field is STM32_SVD.Short;

   --  OTG_FS non-periodic transmit FIFO size register (Host mode)
   type FS_GNPTXFSIZ_Host_Register is record
      --  Non-periodic transmit RAM start address
      NPTXFSA : FS_GNPTXFSIZ_Host_NPTXFSA_Field := 16#200#;
      --  Non-periodic TxFIFO depth
      NPTXFD  : FS_GNPTXFSIZ_Host_NPTXFD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GNPTXFSIZ_Host_Register use record
      NPTXFSA at 0 range 0 .. 15;
      NPTXFD  at 0 range 16 .. 31;
   end record;

   --------------------------
   -- FS_GNPTXSTS_Register --
   --------------------------

   subtype FS_GNPTXSTS_NPTXFSAV_Field is STM32_SVD.Short;
   subtype FS_GNPTXSTS_NPTQXSAV_Field is STM32_SVD.Byte;
   subtype FS_GNPTXSTS_NPTXQTOP_Field is STM32_SVD.UInt7;

   --  OTG_FS non-periodic transmit FIFO/queue status register
   --  (OTG_FS_GNPTXSTS)
   type FS_GNPTXSTS_Register is record
      --  Non-periodic TxFIFO space available
      NPTXFSAV       : FS_GNPTXSTS_NPTXFSAV_Field;
      --  Non-periodic transmit request queue space available
      NPTQXSAV       : FS_GNPTXSTS_NPTQXSAV_Field;
      --  Top of the non-periodic transmit request queue
      NPTXQTOP       : FS_GNPTXSTS_NPTXQTOP_Field;
      --  unspecified
      Reserved_31_31 : STM32_SVD.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GNPTXSTS_Register use record
      NPTXFSAV       at 0 range 0 .. 15;
      NPTQXSAV       at 0 range 16 .. 23;
      NPTXQTOP       at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   -----------------------
   -- FS_GCCFG_Register --
   -----------------------

   subtype FS_GCCFG_PWRDWN_Field is STM32_SVD.Bit;
   subtype FS_GCCFG_VBUSASEN_Field is STM32_SVD.Bit;
   subtype FS_GCCFG_VBUSBSEN_Field is STM32_SVD.Bit;
   subtype FS_GCCFG_SOFOUTEN_Field is STM32_SVD.Bit;

   --  OTG_FS general core configuration register (OTG_FS_GCCFG)
   type FS_GCCFG_Register is record
      --  unspecified
      Reserved_0_15  : STM32_SVD.Short := 16#0#;
      --  Power down
      PWRDWN         : FS_GCCFG_PWRDWN_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32_SVD.Bit := 16#0#;
      --  Enable the VBUS sensing device
      VBUSASEN       : FS_GCCFG_VBUSASEN_Field := 16#0#;
      --  Enable the VBUS sensing device
      VBUSBSEN       : FS_GCCFG_VBUSBSEN_Field := 16#0#;
      --  SOF output enable
      SOFOUTEN       : FS_GCCFG_SOFOUTEN_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : STM32_SVD.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GCCFG_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      PWRDWN         at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      VBUSASEN       at 0 range 18 .. 18;
      VBUSBSEN       at 0 range 19 .. 19;
      SOFOUTEN       at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   --------------------------
   -- FS_HPTXFSIZ_Register --
   --------------------------

   subtype FS_HPTXFSIZ_PTXSA_Field is STM32_SVD.Short;
   subtype FS_HPTXFSIZ_PTXFSIZ_Field is STM32_SVD.Short;

   --  OTG_FS Host periodic transmit FIFO size register (OTG_FS_HPTXFSIZ)
   type FS_HPTXFSIZ_Register is record
      --  Host periodic TxFIFO start address
      PTXSA   : FS_HPTXFSIZ_PTXSA_Field := 16#600#;
      --  Host periodic TxFIFO depth
      PTXFSIZ : FS_HPTXFSIZ_PTXFSIZ_Field := 16#200#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HPTXFSIZ_Register use record
      PTXSA   at 0 range 0 .. 15;
      PTXFSIZ at 0 range 16 .. 31;
   end record;

   -------------------------
   -- FS_DIEPTXF_Register --
   -------------------------

   subtype FS_DIEPTXF1_INEPTXSA_Field is STM32_SVD.Short;
   subtype FS_DIEPTXF1_INEPTXFD_Field is STM32_SVD.Short;

   --  OTG_FS device IN endpoint transmit FIFO size register (OTG_FS_DIEPTXF2)
   type FS_DIEPTXF_Register is record
      --  IN endpoint FIFO2 transmit RAM start address
      INEPTXSA : FS_DIEPTXF1_INEPTXSA_Field := 16#400#;
      --  IN endpoint TxFIFO depth
      INEPTXFD : FS_DIEPTXF1_INEPTXFD_Field := 16#200#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DIEPTXF_Register use record
      INEPTXSA at 0 range 0 .. 15;
      INEPTXFD at 0 range 16 .. 31;
   end record;

   ----------------------
   -- FS_HCFG_Register --
   ----------------------

   subtype FS_HCFG_FSLSPCS_Field is STM32_SVD.UInt2;
   subtype FS_HCFG_FSLSS_Field is STM32_SVD.Bit;

   --  OTG_FS host configuration register (OTG_FS_HCFG)
   type FS_HCFG_Register is record
      --  FS/LS PHY clock select
      FSLSPCS       : FS_HCFG_FSLSPCS_Field := 16#0#;
      --  FS- and LS-only support
      FSLSS         : FS_HCFG_FSLSS_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : STM32_SVD.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HCFG_Register use record
      FSLSPCS       at 0 range 0 .. 1;
      FSLSS         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   -------------------
   -- HFIR_Register --
   -------------------

   subtype HFIR_FRIVL_Field is STM32_SVD.Short;

   --  OTG_FS Host frame interval register
   type HFIR_Register is record
      --  Frame interval
      FRIVL          : HFIR_FRIVL_Field := 16#EA60#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFIR_Register use record
      FRIVL          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------------
   -- FS_HFNUM_Register --
   -----------------------

   subtype FS_HFNUM_FRNUM_Field is STM32_SVD.Short;
   subtype FS_HFNUM_FTREM_Field is STM32_SVD.Short;

   --  OTG_FS host frame number/frame time remaining register (OTG_FS_HFNUM)
   type FS_HFNUM_Register is record
      --  Frame number
      FRNUM : FS_HFNUM_FRNUM_Field;
      --  Frame time remaining
      FTREM : FS_HFNUM_FTREM_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HFNUM_Register use record
      FRNUM at 0 range 0 .. 15;
      FTREM at 0 range 16 .. 31;
   end record;

   -------------------------
   -- FS_HPTXSTS_Register --
   -------------------------

   subtype FS_HPTXSTS_PTXFSAVL_Field is STM32_SVD.Short;
   subtype FS_HPTXSTS_PTXQSAV_Field is STM32_SVD.Byte;
   subtype FS_HPTXSTS_PTXQTOP_Field is STM32_SVD.Byte;

   --  OTG_FS_Host periodic transmit FIFO/queue status register
   --  (OTG_FS_HPTXSTS)
   type FS_HPTXSTS_Register is record
      --  Periodic transmit data FIFO space available
      PTXFSAVL : FS_HPTXSTS_PTXFSAVL_Field := 16#100#;
      --  Periodic transmit request queue space available
      PTXQSAV  : FS_HPTXSTS_PTXQSAV_Field := 16#8#;
      --  Top of the periodic transmit request queue
      PTXQTOP  : FS_HPTXSTS_PTXQTOP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HPTXSTS_Register use record
      PTXFSAVL at 0 range 0 .. 15;
      PTXQSAV  at 0 range 16 .. 23;
      PTXQTOP  at 0 range 24 .. 31;
   end record;

   --------------------
   -- HAINT_Register --
   --------------------

   subtype HAINT_HAINT_Field is STM32_SVD.Short;

   --  OTG_FS Host all channels interrupt register
   type HAINT_Register is record
      --  Channel interrupts
      HAINT          : HAINT_HAINT_Field;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HAINT_Register use record
      HAINT          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------------
   -- HAINTMSK_Register --
   -----------------------

   subtype HAINTMSK_HAINTM_Field is STM32_SVD.Short;

   --  OTG_FS host all channels interrupt mask register
   type HAINTMSK_Register is record
      --  Channel interrupt mask
      HAINTM         : HAINTMSK_HAINTM_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HAINTMSK_Register use record
      HAINTM         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ----------------------
   -- FS_HPRT_Register --
   ----------------------

   subtype FS_HPRT_PCSTS_Field is STM32_SVD.Bit;
   subtype FS_HPRT_PCDET_Field is STM32_SVD.Bit;
   subtype FS_HPRT_PENA_Field is STM32_SVD.Bit;
   subtype FS_HPRT_PENCHNG_Field is STM32_SVD.Bit;
   subtype FS_HPRT_POCA_Field is STM32_SVD.Bit;
   subtype FS_HPRT_POCCHNG_Field is STM32_SVD.Bit;
   subtype FS_HPRT_PRES_Field is STM32_SVD.Bit;
   subtype FS_HPRT_PSUSP_Field is STM32_SVD.Bit;
   subtype FS_HPRT_PRST_Field is STM32_SVD.Bit;
   subtype FS_HPRT_PLSTS_Field is STM32_SVD.UInt2;
   subtype FS_HPRT_PPWR_Field is STM32_SVD.Bit;
   subtype FS_HPRT_PTCTL_Field is STM32_SVD.UInt4;
   subtype FS_HPRT_PSPD_Field is STM32_SVD.UInt2;

   --  OTG_FS host port control and status register (OTG_FS_HPRT)
   type FS_HPRT_Register is record
      --  Port connect status
      PCSTS          : FS_HPRT_PCSTS_Field := 16#0#;
      --  Port connect detected
      PCDET          : FS_HPRT_PCDET_Field := 16#0#;
      --  Port enable
      PENA           : FS_HPRT_PENA_Field := 16#0#;
      --  Port enable/disable change
      PENCHNG        : FS_HPRT_PENCHNG_Field := 16#0#;
      --  Port overcurrent active
      POCA           : FS_HPRT_POCA_Field := 16#0#;
      --  Port overcurrent change
      POCCHNG        : FS_HPRT_POCCHNG_Field := 16#0#;
      --  Port resume
      PRES           : FS_HPRT_PRES_Field := 16#0#;
      --  Port suspend
      PSUSP          : FS_HPRT_PSUSP_Field := 16#0#;
      --  Port reset
      PRST           : FS_HPRT_PRST_Field := 16#0#;
      --  unspecified
      Reserved_9_9   : STM32_SVD.Bit := 16#0#;
      --  Port line status
      PLSTS          : FS_HPRT_PLSTS_Field := 16#0#;
      --  Port power
      PPWR           : FS_HPRT_PPWR_Field := 16#0#;
      --  Port test control
      PTCTL          : FS_HPRT_PTCTL_Field := 16#0#;
      --  Port speed
      PSPD           : FS_HPRT_PSPD_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : STM32_SVD.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HPRT_Register use record
      PCSTS          at 0 range 0 .. 0;
      PCDET          at 0 range 1 .. 1;
      PENA           at 0 range 2 .. 2;
      PENCHNG        at 0 range 3 .. 3;
      POCA           at 0 range 4 .. 4;
      POCCHNG        at 0 range 5 .. 5;
      PRES           at 0 range 6 .. 6;
      PSUSP          at 0 range 7 .. 7;
      PRST           at 0 range 8 .. 8;
      Reserved_9_9   at 0 range 9 .. 9;
      PLSTS          at 0 range 10 .. 11;
      PPWR           at 0 range 12 .. 12;
      PTCTL          at 0 range 13 .. 16;
      PSPD           at 0 range 17 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   ------------------------
   -- FS_HCCHAR_Register --
   ------------------------

   subtype FS_HCCHAR0_MPSIZ_Field is STM32_SVD.UInt11;
   subtype FS_HCCHAR0_EPNUM_Field is STM32_SVD.UInt4;
   subtype FS_HCCHAR0_EPDIR_Field is STM32_SVD.Bit;
   subtype FS_HCCHAR0_LSDEV_Field is STM32_SVD.Bit;
   subtype FS_HCCHAR0_EPTYP_Field is STM32_SVD.UInt2;
   subtype FS_HCCHAR0_MCNT_Field is STM32_SVD.UInt2;
   subtype FS_HCCHAR0_DAD_Field is STM32_SVD.UInt7;
   subtype FS_HCCHAR0_ODDFRM_Field is STM32_SVD.Bit;
   subtype FS_HCCHAR0_CHDIS_Field is STM32_SVD.Bit;
   subtype FS_HCCHAR0_CHENA_Field is STM32_SVD.Bit;

   --  OTG_FS host channel-0 characteristics register (OTG_FS_HCCHAR0)
   type FS_HCCHAR_Register is record
      --  Maximum packet size
      MPSIZ          : FS_HCCHAR0_MPSIZ_Field := 16#0#;
      --  Endpoint number
      EPNUM          : FS_HCCHAR0_EPNUM_Field := 16#0#;
      --  Endpoint direction
      EPDIR          : FS_HCCHAR0_EPDIR_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : STM32_SVD.Bit := 16#0#;
      --  Low-speed device
      LSDEV          : FS_HCCHAR0_LSDEV_Field := 16#0#;
      --  Endpoint type
      EPTYP          : FS_HCCHAR0_EPTYP_Field := 16#0#;
      --  Multicount
      MCNT           : FS_HCCHAR0_MCNT_Field := 16#0#;
      --  Device address
      DAD            : FS_HCCHAR0_DAD_Field := 16#0#;
      --  Odd frame
      ODDFRM         : FS_HCCHAR0_ODDFRM_Field := 16#0#;
      --  Channel disable
      CHDIS          : FS_HCCHAR0_CHDIS_Field := 16#0#;
      --  Channel enable
      CHENA          : FS_HCCHAR0_CHENA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HCCHAR_Register use record
      MPSIZ          at 0 range 0 .. 10;
      EPNUM          at 0 range 11 .. 14;
      EPDIR          at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      LSDEV          at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      MCNT           at 0 range 20 .. 21;
      DAD            at 0 range 22 .. 28;
      ODDFRM         at 0 range 29 .. 29;
      CHDIS          at 0 range 30 .. 30;
      CHENA          at 0 range 31 .. 31;
   end record;

   -----------------------
   -- FS_HCINT_Register --
   -----------------------

   subtype FS_HCINT0_XFRC_Field is STM32_SVD.Bit;
   subtype FS_HCINT0_CHH_Field is STM32_SVD.Bit;
   subtype FS_HCINT0_STALL_Field is STM32_SVD.Bit;
   subtype FS_HCINT0_NAK_Field is STM32_SVD.Bit;
   subtype FS_HCINT0_ACK_Field is STM32_SVD.Bit;
   subtype FS_HCINT0_TXERR_Field is STM32_SVD.Bit;
   subtype FS_HCINT0_BBERR_Field is STM32_SVD.Bit;
   subtype FS_HCINT0_FRMOR_Field is STM32_SVD.Bit;
   subtype FS_HCINT0_DTERR_Field is STM32_SVD.Bit;

   --  OTG_FS host channel-0 interrupt register (OTG_FS_HCINT0)
   type FS_HCINT_Register is record
      --  Transfer completed
      XFRC           : FS_HCINT0_XFRC_Field := 16#0#;
      --  Channel halted
      CHH            : FS_HCINT0_CHH_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : STM32_SVD.Bit := 16#0#;
      --  STALL response received interrupt
      STALL          : FS_HCINT0_STALL_Field := 16#0#;
      --  NAK response received interrupt
      NAK            : FS_HCINT0_NAK_Field := 16#0#;
      --  ACK response received/transmitted interrupt
      ACK            : FS_HCINT0_ACK_Field := 16#0#;
      --  unspecified
      Reserved_6_6   : STM32_SVD.Bit := 16#0#;
      --  Transaction error
      TXERR          : FS_HCINT0_TXERR_Field := 16#0#;
      --  Babble error
      BBERR          : FS_HCINT0_BBERR_Field := 16#0#;
      --  Frame overrun
      FRMOR          : FS_HCINT0_FRMOR_Field := 16#0#;
      --  Data toggle error
      DTERR          : FS_HCINT0_DTERR_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : STM32_SVD.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HCINT_Register use record
      XFRC           at 0 range 0 .. 0;
      CHH            at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      STALL          at 0 range 3 .. 3;
      NAK            at 0 range 4 .. 4;
      ACK            at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      TXERR          at 0 range 7 .. 7;
      BBERR          at 0 range 8 .. 8;
      FRMOR          at 0 range 9 .. 9;
      DTERR          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --------------------------
   -- FS_HCINTMSK_Register --
   --------------------------

   subtype FS_HCINTMSK0_XFRCM_Field is STM32_SVD.Bit;
   subtype FS_HCINTMSK0_CHHM_Field is STM32_SVD.Bit;
   subtype FS_HCINTMSK0_STALLM_Field is STM32_SVD.Bit;
   subtype FS_HCINTMSK0_NAKM_Field is STM32_SVD.Bit;
   subtype FS_HCINTMSK0_ACKM_Field is STM32_SVD.Bit;
   subtype FS_HCINTMSK0_NYET_Field is STM32_SVD.Bit;
   subtype FS_HCINTMSK0_TXERRM_Field is STM32_SVD.Bit;
   subtype FS_HCINTMSK0_BBERRM_Field is STM32_SVD.Bit;
   subtype FS_HCINTMSK0_FRMORM_Field is STM32_SVD.Bit;
   subtype FS_HCINTMSK0_DTERRM_Field is STM32_SVD.Bit;

   --  OTG_FS host channel-0 mask register (OTG_FS_HCINTMSK0)
   type FS_HCINTMSK_Register is record
      --  Transfer completed mask
      XFRCM          : FS_HCINTMSK0_XFRCM_Field := 16#0#;
      --  Channel halted mask
      CHHM           : FS_HCINTMSK0_CHHM_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : STM32_SVD.Bit := 16#0#;
      --  STALL response received interrupt mask
      STALLM         : FS_HCINTMSK0_STALLM_Field := 16#0#;
      --  NAK response received interrupt mask
      NAKM           : FS_HCINTMSK0_NAKM_Field := 16#0#;
      --  ACK response received/transmitted interrupt mask
      ACKM           : FS_HCINTMSK0_ACKM_Field := 16#0#;
      --  response received interrupt mask
      NYET           : FS_HCINTMSK0_NYET_Field := 16#0#;
      --  Transaction error mask
      TXERRM         : FS_HCINTMSK0_TXERRM_Field := 16#0#;
      --  Babble error mask
      BBERRM         : FS_HCINTMSK0_BBERRM_Field := 16#0#;
      --  Frame overrun mask
      FRMORM         : FS_HCINTMSK0_FRMORM_Field := 16#0#;
      --  Data toggle error mask
      DTERRM         : FS_HCINTMSK0_DTERRM_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : STM32_SVD.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HCINTMSK_Register use record
      XFRCM          at 0 range 0 .. 0;
      CHHM           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      STALLM         at 0 range 3 .. 3;
      NAKM           at 0 range 4 .. 4;
      ACKM           at 0 range 5 .. 5;
      NYET           at 0 range 6 .. 6;
      TXERRM         at 0 range 7 .. 7;
      BBERRM         at 0 range 8 .. 8;
      FRMORM         at 0 range 9 .. 9;
      DTERRM         at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   ------------------------
   -- FS_HCTSIZ_Register --
   ------------------------

   subtype FS_HCTSIZ0_XFRSIZ_Field is STM32_SVD.UInt19;
   subtype FS_HCTSIZ0_PKTCNT_Field is STM32_SVD.UInt10;
   subtype FS_HCTSIZ0_DPID_Field is STM32_SVD.UInt2;

   --  OTG_FS host channel-0 transfer size register
   type FS_HCTSIZ_Register is record
      --  Transfer size
      XFRSIZ         : FS_HCTSIZ0_XFRSIZ_Field := 16#0#;
      --  Packet count
      PKTCNT         : FS_HCTSIZ0_PKTCNT_Field := 16#0#;
      --  Data PID
      DPID           : FS_HCTSIZ0_DPID_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : STM32_SVD.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HCTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      DPID           at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   ----------------------
   -- FS_DCFG_Register --
   ----------------------

   subtype FS_DCFG_DSPD_Field is STM32_SVD.UInt2;
   subtype FS_DCFG_NZLSOHSK_Field is STM32_SVD.Bit;
   subtype FS_DCFG_DAD_Field is STM32_SVD.UInt7;
   subtype FS_DCFG_PFIVL_Field is STM32_SVD.UInt2;

   --  OTG_FS device configuration register (OTG_FS_DCFG)
   type FS_DCFG_Register is record
      --  Device speed
      DSPD           : FS_DCFG_DSPD_Field := 16#0#;
      --  Non-zero-length status OUT handshake
      NZLSOHSK       : FS_DCFG_NZLSOHSK_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : STM32_SVD.Bit := 16#0#;
      --  Device address
      DAD            : FS_DCFG_DAD_Field := 16#0#;
      --  Periodic frame interval
      PFIVL          : FS_DCFG_PFIVL_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19 := 16#1100#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DCFG_Register use record
      DSPD           at 0 range 0 .. 1;
      NZLSOHSK       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DAD            at 0 range 4 .. 10;
      PFIVL          at 0 range 11 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   ----------------------
   -- FS_DCTL_Register --
   ----------------------

   subtype FS_DCTL_RWUSIG_Field is STM32_SVD.Bit;
   subtype FS_DCTL_SDIS_Field is STM32_SVD.Bit;
   subtype FS_DCTL_GINSTS_Field is STM32_SVD.Bit;
   subtype FS_DCTL_GONSTS_Field is STM32_SVD.Bit;
   subtype FS_DCTL_TCTL_Field is STM32_SVD.UInt3;
   subtype FS_DCTL_SGINAK_Field is STM32_SVD.Bit;
   subtype FS_DCTL_CGINAK_Field is STM32_SVD.Bit;
   subtype FS_DCTL_SGONAK_Field is STM32_SVD.Bit;
   subtype FS_DCTL_CGONAK_Field is STM32_SVD.Bit;
   subtype FS_DCTL_POPRGDNE_Field is STM32_SVD.Bit;

   --  OTG_FS device control register (OTG_FS_DCTL)
   type FS_DCTL_Register is record
      --  Remote wakeup signaling
      RWUSIG         : FS_DCTL_RWUSIG_Field := 16#0#;
      --  Soft disconnect
      SDIS           : FS_DCTL_SDIS_Field := 16#0#;
      --  Global IN NAK status
      GINSTS         : FS_DCTL_GINSTS_Field := 16#0#;
      --  Global OUT NAK status
      GONSTS         : FS_DCTL_GONSTS_Field := 16#0#;
      --  Test control
      TCTL           : FS_DCTL_TCTL_Field := 16#0#;
      --  Set global IN NAK
      SGINAK         : FS_DCTL_SGINAK_Field := 16#0#;
      --  Clear global IN NAK
      CGINAK         : FS_DCTL_CGINAK_Field := 16#0#;
      --  Set global OUT NAK
      SGONAK         : FS_DCTL_SGONAK_Field := 16#0#;
      --  Clear global OUT NAK
      CGONAK         : FS_DCTL_CGONAK_Field := 16#0#;
      --  Power-on programming done
      POPRGDNE       : FS_DCTL_POPRGDNE_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : STM32_SVD.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DCTL_Register use record
      RWUSIG         at 0 range 0 .. 0;
      SDIS           at 0 range 1 .. 1;
      GINSTS         at 0 range 2 .. 2;
      GONSTS         at 0 range 3 .. 3;
      TCTL           at 0 range 4 .. 6;
      SGINAK         at 0 range 7 .. 7;
      CGINAK         at 0 range 8 .. 8;
      SGONAK         at 0 range 9 .. 9;
      CGONAK         at 0 range 10 .. 10;
      POPRGDNE       at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   ----------------------
   -- FS_DSTS_Register --
   ----------------------

   subtype FS_DSTS_SUSPSTS_Field is STM32_SVD.Bit;
   subtype FS_DSTS_ENUMSPD_Field is STM32_SVD.UInt2;
   subtype FS_DSTS_EERR_Field is STM32_SVD.Bit;
   subtype FS_DSTS_FNSOF_Field is STM32_SVD.UInt14;

   --  OTG_FS device status register (OTG_FS_DSTS)
   type FS_DSTS_Register is record
      --  Suspend status
      SUSPSTS        : FS_DSTS_SUSPSTS_Field;
      --  Enumerated speed
      ENUMSPD        : FS_DSTS_ENUMSPD_Field;
      --  Erratic error
      EERR           : FS_DSTS_EERR_Field;
      --  unspecified
      Reserved_4_7   : STM32_SVD.UInt4;
      --  Frame number of the received SOF
      FNSOF          : FS_DSTS_FNSOF_Field;
      --  unspecified
      Reserved_22_31 : STM32_SVD.UInt10;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DSTS_Register use record
      SUSPSTS        at 0 range 0 .. 0;
      ENUMSPD        at 0 range 1 .. 2;
      EERR           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      FNSOF          at 0 range 8 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   -------------------------
   -- FS_DIEPMSK_Register --
   -------------------------

   subtype FS_DIEPMSK_XFRCM_Field is STM32_SVD.Bit;
   subtype FS_DIEPMSK_EPDM_Field is STM32_SVD.Bit;
   subtype FS_DIEPMSK_TOM_Field is STM32_SVD.Bit;
   subtype FS_DIEPMSK_ITTXFEMSK_Field is STM32_SVD.Bit;
   subtype FS_DIEPMSK_INEPNMM_Field is STM32_SVD.Bit;
   subtype FS_DIEPMSK_INEPNEM_Field is STM32_SVD.Bit;

   --  OTG_FS device IN endpoint common interrupt mask register
   --  (OTG_FS_DIEPMSK)
   type FS_DIEPMSK_Register is record
      --  Transfer completed interrupt mask
      XFRCM         : FS_DIEPMSK_XFRCM_Field := 16#0#;
      --  Endpoint disabled interrupt mask
      EPDM          : FS_DIEPMSK_EPDM_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : STM32_SVD.Bit := 16#0#;
      --  Timeout condition mask (Non-isochronous endpoints)
      TOM           : FS_DIEPMSK_TOM_Field := 16#0#;
      --  IN token received when TxFIFO empty mask
      ITTXFEMSK     : FS_DIEPMSK_ITTXFEMSK_Field := 16#0#;
      --  IN token received with EP mismatch mask
      INEPNMM       : FS_DIEPMSK_INEPNMM_Field := 16#0#;
      --  IN endpoint NAK effective mask
      INEPNEM       : FS_DIEPMSK_INEPNEM_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : STM32_SVD.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DIEPMSK_Register use record
      XFRCM         at 0 range 0 .. 0;
      EPDM          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      TOM           at 0 range 3 .. 3;
      ITTXFEMSK     at 0 range 4 .. 4;
      INEPNMM       at 0 range 5 .. 5;
      INEPNEM       at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -------------------------
   -- FS_DOEPMSK_Register --
   -------------------------

   subtype FS_DOEPMSK_XFRCM_Field is STM32_SVD.Bit;
   subtype FS_DOEPMSK_EPDM_Field is STM32_SVD.Bit;
   subtype FS_DOEPMSK_STUPM_Field is STM32_SVD.Bit;
   subtype FS_DOEPMSK_OTEPDM_Field is STM32_SVD.Bit;

   --  OTG_FS device OUT endpoint common interrupt mask register
   --  (OTG_FS_DOEPMSK)
   type FS_DOEPMSK_Register is record
      --  Transfer completed interrupt mask
      XFRCM         : FS_DOEPMSK_XFRCM_Field := 16#0#;
      --  Endpoint disabled interrupt mask
      EPDM          : FS_DOEPMSK_EPDM_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : STM32_SVD.Bit := 16#0#;
      --  SETUP phase done mask
      STUPM         : FS_DOEPMSK_STUPM_Field := 16#0#;
      --  OUT token received when endpoint disabled mask
      OTEPDM        : FS_DOEPMSK_OTEPDM_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : STM32_SVD.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DOEPMSK_Register use record
      XFRCM         at 0 range 0 .. 0;
      EPDM          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      STUPM         at 0 range 3 .. 3;
      OTEPDM        at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   -----------------------
   -- FS_DAINT_Register --
   -----------------------

   subtype FS_DAINT_IEPINT_Field is STM32_SVD.Short;
   subtype FS_DAINT_OEPINT_Field is STM32_SVD.Short;

   --  OTG_FS device all endpoints interrupt register (OTG_FS_DAINT)
   type FS_DAINT_Register is record
      --  IN endpoint interrupt bits
      IEPINT : FS_DAINT_IEPINT_Field;
      --  OUT endpoint interrupt bits
      OEPINT : FS_DAINT_OEPINT_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DAINT_Register use record
      IEPINT at 0 range 0 .. 15;
      OEPINT at 0 range 16 .. 31;
   end record;

   --------------------------
   -- FS_DAINTMSK_Register --
   --------------------------

   subtype FS_DAINTMSK_IEPM_Field is STM32_SVD.Short;
   subtype FS_DAINTMSK_OEPINT_Field is STM32_SVD.Short;

   --  OTG_FS all endpoints interrupt mask register (OTG_FS_DAINTMSK)
   type FS_DAINTMSK_Register is record
      --  IN EP interrupt mask bits
      IEPM   : FS_DAINTMSK_IEPM_Field := 16#0#;
      --  OUT endpoint interrupt bits
      OEPINT : FS_DAINTMSK_OEPINT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DAINTMSK_Register use record
      IEPM   at 0 range 0 .. 15;
      OEPINT at 0 range 16 .. 31;
   end record;

   -----------------------
   -- DVBUSDIS_Register --
   -----------------------

   subtype DVBUSDIS_VBUSDT_Field is STM32_SVD.Short;

   --  OTG_FS device VBUS discharge time register
   type DVBUSDIS_Register is record
      --  Device VBUS discharge time
      VBUSDT         : DVBUSDIS_VBUSDT_Field := 16#17D7#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DVBUSDIS_Register use record
      VBUSDT         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------------
   -- DVBUSPULSE_Register --
   -------------------------

   subtype DVBUSPULSE_DVBUSP_Field is STM32_SVD.UInt12;

   --  OTG_FS device VBUS pulsing time register
   type DVBUSPULSE_Register is record
      --  Device VBUS pulsing time
      DVBUSP         : DVBUSPULSE_DVBUSP_Field := 16#5B8#;
      --  unspecified
      Reserved_12_31 : STM32_SVD.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DVBUSPULSE_Register use record
      DVBUSP         at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   -------------------------
   -- DIEPEMPMSK_Register --
   -------------------------

   subtype DIEPEMPMSK_INEPTXFEM_Field is STM32_SVD.Short;

   --  OTG_FS device IN endpoint FIFO empty interrupt mask register
   type DIEPEMPMSK_Register is record
      --  IN EP Tx FIFO empty interrupt mask bits
      INEPTXFEM      : DIEPEMPMSK_INEPTXFEM_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPEMPMSK_Register use record
      INEPTXFEM      at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------------
   -- FS_DIEPCTL0_Register --
   --------------------------

   subtype FS_DIEPCTL0_MPSIZ_Field is STM32_SVD.UInt2;
   subtype FS_DIEPCTL0_USBAEP_Field is STM32_SVD.Bit;
   subtype FS_DIEPCTL0_NAKSTS_Field is STM32_SVD.Bit;
   subtype FS_DIEPCTL0_EPTYP_Field is STM32_SVD.UInt2;
   subtype FS_DIEPCTL0_STALL_Field is STM32_SVD.Bit;
   subtype FS_DIEPCTL0_TXFNUM_Field is STM32_SVD.UInt4;
   subtype FS_DIEPCTL0_CNAK_Field is STM32_SVD.Bit;
   subtype FS_DIEPCTL0_SNAK_Field is STM32_SVD.Bit;
   subtype FS_DIEPCTL0_EPDIS_Field is STM32_SVD.Bit;
   subtype FS_DIEPCTL0_EPENA_Field is STM32_SVD.Bit;

   --  OTG_FS device control IN endpoint 0 control register (OTG_FS_DIEPCTL0)
   type FS_DIEPCTL0_Register is record
      --  Maximum packet size
      MPSIZ          : FS_DIEPCTL0_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_2_14  : STM32_SVD.UInt13 := 16#0#;
      --  USB active endpoint
      USBAEP         : FS_DIEPCTL0_USBAEP_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : STM32_SVD.Bit := 16#0#;
      --  NAK status
      NAKSTS         : FS_DIEPCTL0_NAKSTS_Field := 16#0#;
      --  Endpoint type
      EPTYP          : FS_DIEPCTL0_EPTYP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : STM32_SVD.Bit := 16#0#;
      --  STALL handshake
      STALL          : FS_DIEPCTL0_STALL_Field := 16#0#;
      --  TxFIFO number
      TXFNUM         : FS_DIEPCTL0_TXFNUM_Field := 16#0#;
      --  Clear NAK
      CNAK           : FS_DIEPCTL0_CNAK_Field := 16#0#;
      --  Set NAK
      SNAK           : FS_DIEPCTL0_SNAK_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : STM32_SVD.UInt2 := 16#0#;
      --  Endpoint disable
      EPDIS          : FS_DIEPCTL0_EPDIS_Field := 16#0#;
      --  Endpoint enable
      EPENA          : FS_DIEPCTL0_EPENA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DIEPCTL0_Register use record
      MPSIZ          at 0 range 0 .. 1;
      Reserved_2_14  at 0 range 2 .. 14;
      USBAEP         at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      NAKSTS         at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      STALL          at 0 range 21 .. 21;
      TXFNUM         at 0 range 22 .. 25;
      CNAK           at 0 range 26 .. 26;
      SNAK           at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      EPDIS          at 0 range 30 .. 30;
      EPENA          at 0 range 31 .. 31;
   end record;

   ----------------------
   -- DIEPINT_Register --
   ----------------------

   subtype DIEPINT0_XFRC_Field is STM32_SVD.Bit;
   subtype DIEPINT0_EPDISD_Field is STM32_SVD.Bit;
   subtype DIEPINT0_TOC_Field is STM32_SVD.Bit;
   subtype DIEPINT0_ITTXFE_Field is STM32_SVD.Bit;
   subtype DIEPINT0_INEPNE_Field is STM32_SVD.Bit;
   subtype DIEPINT0_TXFE_Field is STM32_SVD.Bit;

   --  device endpoint-x interrupt register
   type DIEPINT_Register is record
      --  XFRC
      XFRC          : DIEPINT0_XFRC_Field := 16#0#;
      --  EPDISD
      EPDISD        : DIEPINT0_EPDISD_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : STM32_SVD.Bit := 16#0#;
      --  TOC
      TOC           : DIEPINT0_TOC_Field := 16#0#;
      --  ITTXFE
      ITTXFE        : DIEPINT0_ITTXFE_Field := 16#0#;
      --  unspecified
      Reserved_5_5  : STM32_SVD.Bit := 16#0#;
      --  INEPNE
      INEPNE        : DIEPINT0_INEPNE_Field := 16#0#;
      --  TXFE
      TXFE          : DIEPINT0_TXFE_Field := 16#1#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPINT_Register use record
      XFRC          at 0 range 0 .. 0;
      EPDISD        at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      TOC           at 0 range 3 .. 3;
      ITTXFE        at 0 range 4 .. 4;
      Reserved_5_5  at 0 range 5 .. 5;
      INEPNE        at 0 range 6 .. 6;
      TXFE          at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   ------------------------
   -- DIEPTSIZ0_Register --
   ------------------------

   subtype DIEPTSIZ0_XFRSIZ_Field is STM32_SVD.UInt7;
   subtype DIEPTSIZ0_PKTCNT_Field is STM32_SVD.UInt2;

   --  device endpoint-0 transfer size register
   type DIEPTSIZ0_Register is record
      --  Transfer size
      XFRSIZ         : DIEPTSIZ0_XFRSIZ_Field := 16#0#;
      --  unspecified
      Reserved_7_18  : STM32_SVD.UInt12 := 16#0#;
      --  Packet count
      PKTCNT         : DIEPTSIZ0_PKTCNT_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : STM32_SVD.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPTSIZ0_Register use record
      XFRSIZ         at 0 range 0 .. 6;
      Reserved_7_18  at 0 range 7 .. 18;
      PKTCNT         at 0 range 19 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   ----------------------
   -- DTXFSTS_Register --
   ----------------------

   subtype DTXFSTS0_INEPTFSAV_Field is STM32_SVD.Short;

   --  OTG_FS device IN endpoint transmit FIFO status register
   type DTXFSTS_Register is record
      --  IN endpoint TxFIFO space available
      INEPTFSAV      : DTXFSTS0_INEPTFSAV_Field;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DTXFSTS_Register use record
      INEPTFSAV      at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ----------------------
   -- DIEPCTL_Register --
   ----------------------

   subtype DIEPCTL1_MPSIZ_Field is STM32_SVD.UInt11;
   subtype DIEPCTL1_USBAEP_Field is STM32_SVD.Bit;
   subtype DIEPCTL1_EONUM_DPID_Field is STM32_SVD.Bit;
   subtype DIEPCTL1_NAKSTS_Field is STM32_SVD.Bit;
   subtype DIEPCTL1_EPTYP_Field is STM32_SVD.UInt2;
   subtype DIEPCTL1_Stall_Field is STM32_SVD.Bit;
   subtype DIEPCTL1_TXFNUM_Field is STM32_SVD.UInt4;
   subtype DIEPCTL1_CNAK_Field is STM32_SVD.Bit;
   subtype DIEPCTL1_SNAK_Field is STM32_SVD.Bit;
   subtype DIEPCTL1_SD0PID_SEVNFRM_Field is STM32_SVD.Bit;
   subtype DIEPCTL1_SODDFRM_SD1PID_Field is STM32_SVD.Bit;
   subtype DIEPCTL1_EPDIS_Field is STM32_SVD.Bit;
   subtype DIEPCTL1_EPENA_Field is STM32_SVD.Bit;

   --  OTG device endpoint-1 control register
   type DIEPCTL_Register is record
      --  MPSIZ
      MPSIZ          : DIEPCTL1_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : STM32_SVD.UInt4 := 16#0#;
      --  USBAEP
      USBAEP         : DIEPCTL1_USBAEP_Field := 16#0#;
      --  EONUM/DPID
      EONUM_DPID     : DIEPCTL1_EONUM_DPID_Field := 16#0#;
      --  NAKSTS
      NAKSTS         : DIEPCTL1_NAKSTS_Field := 16#0#;
      --  EPTYP
      EPTYP          : DIEPCTL1_EPTYP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : STM32_SVD.Bit := 16#0#;
      --  Stall
      Stall          : DIEPCTL1_Stall_Field := 16#0#;
      --  TXFNUM
      TXFNUM         : DIEPCTL1_TXFNUM_Field := 16#0#;
      --  CNAK
      CNAK           : DIEPCTL1_CNAK_Field := 16#0#;
      --  SNAK
      SNAK           : DIEPCTL1_SNAK_Field := 16#0#;
      --  SD0PID/SEVNFRM
      SD0PID_SEVNFRM : DIEPCTL1_SD0PID_SEVNFRM_Field := 16#0#;
      --  SODDFRM/SD1PID
      SODDFRM_SD1PID : DIEPCTL1_SODDFRM_SD1PID_Field := 16#0#;
      --  EPDIS
      EPDIS          : DIEPCTL1_EPDIS_Field := 16#0#;
      --  EPENA
      EPENA          : DIEPCTL1_EPENA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPCTL_Register use record
      MPSIZ          at 0 range 0 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      USBAEP         at 0 range 15 .. 15;
      EONUM_DPID     at 0 range 16 .. 16;
      NAKSTS         at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      Stall          at 0 range 21 .. 21;
      TXFNUM         at 0 range 22 .. 25;
      CNAK           at 0 range 26 .. 26;
      SNAK           at 0 range 27 .. 27;
      SD0PID_SEVNFRM at 0 range 28 .. 28;
      SODDFRM_SD1PID at 0 range 29 .. 29;
      EPDIS          at 0 range 30 .. 30;
      EPENA          at 0 range 31 .. 31;
   end record;

   -----------------------
   -- DIEPTSIZ_Register --
   -----------------------

   subtype DIEPTSIZ1_XFRSIZ_Field is STM32_SVD.UInt19;
   subtype DIEPTSIZ1_PKTCNT_Field is STM32_SVD.UInt10;
   subtype DIEPTSIZ1_MCNT_Field is STM32_SVD.UInt2;

   --  device endpoint-1 transfer size register
   type DIEPTSIZ_Register is record
      --  Transfer size
      XFRSIZ         : DIEPTSIZ1_XFRSIZ_Field := 16#0#;
      --  Packet count
      PKTCNT         : DIEPTSIZ1_PKTCNT_Field := 16#0#;
      --  Multi count
      MCNT           : DIEPTSIZ1_MCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : STM32_SVD.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      MCNT           at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   -----------------------
   -- DOEPCTL0_Register --
   -----------------------

   subtype DOEPCTL0_MPSIZ_Field is STM32_SVD.UInt2;
   subtype DOEPCTL0_USBAEP_Field is STM32_SVD.Bit;
   subtype DOEPCTL0_NAKSTS_Field is STM32_SVD.Bit;
   subtype DOEPCTL0_EPTYP_Field is STM32_SVD.UInt2;
   subtype DOEPCTL0_SNPM_Field is STM32_SVD.Bit;
   subtype DOEPCTL0_Stall_Field is STM32_SVD.Bit;
   subtype DOEPCTL0_CNAK_Field is STM32_SVD.Bit;
   subtype DOEPCTL0_SNAK_Field is STM32_SVD.Bit;
   subtype DOEPCTL0_EPDIS_Field is STM32_SVD.Bit;
   subtype DOEPCTL0_EPENA_Field is STM32_SVD.Bit;

   --  device endpoint-0 control register
   type DOEPCTL0_Register is record
      --  MPSIZ
      MPSIZ          : DOEPCTL0_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_2_14  : STM32_SVD.UInt13 := 16#0#;
      --  USBAEP
      USBAEP         : DOEPCTL0_USBAEP_Field := 16#1#;
      --  unspecified
      Reserved_16_16 : STM32_SVD.Bit := 16#0#;
      --  NAKSTS
      NAKSTS         : DOEPCTL0_NAKSTS_Field := 16#0#;
      --  EPTYP
      EPTYP          : DOEPCTL0_EPTYP_Field := 16#0#;
      --  SNPM
      SNPM           : DOEPCTL0_SNPM_Field := 16#0#;
      --  Stall
      Stall          : DOEPCTL0_Stall_Field := 16#0#;
      --  unspecified
      Reserved_22_25 : STM32_SVD.UInt4 := 16#0#;
      --  CNAK
      CNAK           : DOEPCTL0_CNAK_Field := 16#0#;
      --  SNAK
      SNAK           : DOEPCTL0_SNAK_Field := 16#0#;
      --  unspecified
      Reserved_28_29 : STM32_SVD.UInt2 := 16#0#;
      --  EPDIS
      EPDIS          : DOEPCTL0_EPDIS_Field := 16#0#;
      --  EPENA
      EPENA          : DOEPCTL0_EPENA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPCTL0_Register use record
      MPSIZ          at 0 range 0 .. 1;
      Reserved_2_14  at 0 range 2 .. 14;
      USBAEP         at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      NAKSTS         at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      SNPM           at 0 range 20 .. 20;
      Stall          at 0 range 21 .. 21;
      Reserved_22_25 at 0 range 22 .. 25;
      CNAK           at 0 range 26 .. 26;
      SNAK           at 0 range 27 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      EPDIS          at 0 range 30 .. 30;
      EPENA          at 0 range 31 .. 31;
   end record;

   ----------------------
   -- DOEPINT_Register --
   ----------------------

   subtype DOEPINT0_XFRC_Field is STM32_SVD.Bit;
   subtype DOEPINT0_EPDISD_Field is STM32_SVD.Bit;
   subtype DOEPINT0_STUP_Field is STM32_SVD.Bit;
   subtype DOEPINT0_OTEPDIS_Field is STM32_SVD.Bit;
   subtype DOEPINT0_B2BSTUP_Field is STM32_SVD.Bit;

   --  device endpoint-0 interrupt register
   type DOEPINT_Register is record
      --  XFRC
      XFRC          : DOEPINT0_XFRC_Field := 16#0#;
      --  EPDISD
      EPDISD        : DOEPINT0_EPDISD_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : STM32_SVD.Bit := 16#0#;
      --  STUP
      STUP          : DOEPINT0_STUP_Field := 16#0#;
      --  OTEPDIS
      OTEPDIS       : DOEPINT0_OTEPDIS_Field := 16#0#;
      --  unspecified
      Reserved_5_5  : STM32_SVD.Bit := 16#0#;
      --  B2BSTUP
      B2BSTUP       : DOEPINT0_B2BSTUP_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : STM32_SVD.UInt25 := 16#1#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPINT_Register use record
      XFRC          at 0 range 0 .. 0;
      EPDISD        at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      STUP          at 0 range 3 .. 3;
      OTEPDIS       at 0 range 4 .. 4;
      Reserved_5_5  at 0 range 5 .. 5;
      B2BSTUP       at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   ------------------------
   -- DOEPTSIZ0_Register --
   ------------------------

   subtype DOEPTSIZ0_XFRSIZ_Field is STM32_SVD.UInt7;
   subtype DOEPTSIZ0_PKTCNT_Field is STM32_SVD.Bit;
   subtype DOEPTSIZ0_STUPCNT_Field is STM32_SVD.UInt2;

   --  device OUT endpoint-0 transfer size register
   type DOEPTSIZ0_Register is record
      --  Transfer size
      XFRSIZ         : DOEPTSIZ0_XFRSIZ_Field := 16#0#;
      --  unspecified
      Reserved_7_18  : STM32_SVD.UInt12 := 16#0#;
      --  Packet count
      PKTCNT         : DOEPTSIZ0_PKTCNT_Field := 16#0#;
      --  unspecified
      Reserved_20_28 : STM32_SVD.UInt9 := 16#0#;
      --  SETUP packet count
      STUPCNT        : DOEPTSIZ0_STUPCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : STM32_SVD.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPTSIZ0_Register use record
      XFRSIZ         at 0 range 0 .. 6;
      Reserved_7_18  at 0 range 7 .. 18;
      PKTCNT         at 0 range 19 .. 19;
      Reserved_20_28 at 0 range 20 .. 28;
      STUPCNT        at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   ----------------------
   -- DOEPCTL_Register --
   ----------------------

   subtype DOEPCTL1_MPSIZ_Field is STM32_SVD.UInt11;
   subtype DOEPCTL1_USBAEP_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_EONUM_DPID_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_NAKSTS_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_EPTYP_Field is STM32_SVD.UInt2;
   subtype DOEPCTL1_SNPM_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_Stall_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_CNAK_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_SNAK_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_SD0PID_SEVNFRM_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_SODDFRM_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_EPDIS_Field is STM32_SVD.Bit;
   subtype DOEPCTL1_EPENA_Field is STM32_SVD.Bit;

   --  device endpoint-1 control register
   type DOEPCTL_Register is record
      --  MPSIZ
      MPSIZ          : DOEPCTL1_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : STM32_SVD.UInt4 := 16#0#;
      --  USBAEP
      USBAEP         : DOEPCTL1_USBAEP_Field := 16#0#;
      --  EONUM/DPID
      EONUM_DPID     : DOEPCTL1_EONUM_DPID_Field := 16#0#;
      --  NAKSTS
      NAKSTS         : DOEPCTL1_NAKSTS_Field := 16#0#;
      --  EPTYP
      EPTYP          : DOEPCTL1_EPTYP_Field := 16#0#;
      --  SNPM
      SNPM           : DOEPCTL1_SNPM_Field := 16#0#;
      --  Stall
      Stall          : DOEPCTL1_Stall_Field := 16#0#;
      --  unspecified
      Reserved_22_25 : STM32_SVD.UInt4 := 16#0#;
      --  CNAK
      CNAK           : DOEPCTL1_CNAK_Field := 16#0#;
      --  SNAK
      SNAK           : DOEPCTL1_SNAK_Field := 16#0#;
      --  SD0PID/SEVNFRM
      SD0PID_SEVNFRM : DOEPCTL1_SD0PID_SEVNFRM_Field := 16#0#;
      --  SODDFRM
      SODDFRM        : DOEPCTL1_SODDFRM_Field := 16#0#;
      --  EPDIS
      EPDIS          : DOEPCTL1_EPDIS_Field := 16#0#;
      --  EPENA
      EPENA          : DOEPCTL1_EPENA_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPCTL_Register use record
      MPSIZ          at 0 range 0 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      USBAEP         at 0 range 15 .. 15;
      EONUM_DPID     at 0 range 16 .. 16;
      NAKSTS         at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      SNPM           at 0 range 20 .. 20;
      Stall          at 0 range 21 .. 21;
      Reserved_22_25 at 0 range 22 .. 25;
      CNAK           at 0 range 26 .. 26;
      SNAK           at 0 range 27 .. 27;
      SD0PID_SEVNFRM at 0 range 28 .. 28;
      SODDFRM        at 0 range 29 .. 29;
      EPDIS          at 0 range 30 .. 30;
      EPENA          at 0 range 31 .. 31;
   end record;

   -----------------------
   -- DOEPTSIZ_Register --
   -----------------------

   subtype DOEPTSIZ1_XFRSIZ_Field is STM32_SVD.UInt19;
   subtype DOEPTSIZ1_PKTCNT_Field is STM32_SVD.UInt10;
   subtype DOEPTSIZ1_RXDPID_STUPCNT_Field is STM32_SVD.UInt2;

   --  device OUT endpoint-1 transfer size register
   type DOEPTSIZ_Register is record
      --  Transfer size
      XFRSIZ         : DOEPTSIZ1_XFRSIZ_Field := 16#0#;
      --  Packet count
      PKTCNT         : DOEPTSIZ1_PKTCNT_Field := 16#0#;
      --  Received data PID/SETUP packet count
      RXDPID_STUPCNT : DOEPTSIZ1_RXDPID_STUPCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : STM32_SVD.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      RXDPID_STUPCNT at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   -------------------------
   -- FS_PCGCCTL_Register --
   -------------------------

   subtype FS_PCGCCTL_STPPCLK_Field is STM32_SVD.Bit;
   subtype FS_PCGCCTL_GATEHCLK_Field is STM32_SVD.Bit;
   subtype FS_PCGCCTL_PHYSUSP_Field is STM32_SVD.Bit;

   --  OTG_FS power and clock gating control register
   type FS_PCGCCTL_Register is record
      --  Stop PHY clock
      STPPCLK       : FS_PCGCCTL_STPPCLK_Field := 16#0#;
      --  Gate HCLK
      GATEHCLK      : FS_PCGCCTL_GATEHCLK_Field := 16#0#;
      --  unspecified
      Reserved_2_3  : STM32_SVD.UInt2 := 16#0#;
      --  PHY Suspended
      PHYSUSP       : FS_PCGCCTL_PHYSUSP_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : STM32_SVD.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_PCGCCTL_Register use record
      STPPCLK       at 0 range 0 .. 0;
      GATEHCLK      at 0 range 1 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      PHYSUSP       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type OTG_FS_Mode is
     (Host,
      Device);

   --  USB on the go full speed
   type OTG_FS_GLOBAL_Peripheral
     (Mode : OTG_FS_Mode := Host)
   is record
      --  OTG_FS control and status register (OTG_FS_GOTGCTL)
      FS_GOTGCTL          : FS_GOTGCTL_Register;
      --  OTG_FS interrupt register (OTG_FS_GOTGINT)
      FS_GOTGINT          : FS_GOTGINT_Register;
      --  OTG_FS AHB configuration register (OTG_FS_GAHBCFG)
      FS_GAHBCFG          : FS_GAHBCFG_Register;
      --  OTG_FS USB configuration register (OTG_FS_GUSBCFG)
      FS_GUSBCFG          : FS_GUSBCFG_Register;
      --  OTG_FS reset register (OTG_FS_GRSTCTL)
      FS_GRSTCTL          : FS_GRSTCTL_Register;
      --  OTG_FS core interrupt register (OTG_FS_GINTSTS)
      FS_GINTSTS          : FS_GINTSTS_Register;
      --  OTG_FS interrupt mask register (OTG_FS_GINTMSK)
      FS_GINTMSK          : FS_GINTMSK_Register;
      --  OTG_FS Receive FIFO size register (OTG_FS_GRXFSIZ)
      FS_GRXFSIZ          : FS_GRXFSIZ_Register;
      --  OTG_FS non-periodic transmit FIFO/queue status register
      --  (OTG_FS_GNPTXSTS)
      FS_GNPTXSTS         : FS_GNPTXSTS_Register;
      --  OTG_FS general core configuration register (OTG_FS_GCCFG)
      FS_GCCFG            : FS_GCCFG_Register;
      --  core ID register
      FS_CID              : STM32_SVD.Word;
      --  OTG_FS Host periodic transmit FIFO size register (OTG_FS_HPTXFSIZ)
      FS_HPTXFSIZ         : FS_HPTXFSIZ_Register;
      --  OTG_FS device IN endpoint transmit FIFO size register
      --  (OTG_FS_DIEPTXF2)
      FS_DIEPTXF1         : FS_DIEPTXF_Register;
      --  OTG_FS device IN endpoint transmit FIFO size register
      --  (OTG_FS_DIEPTXF3)
      FS_DIEPTXF2         : FS_DIEPTXF_Register;
      --  OTG_FS device IN endpoint transmit FIFO size register
      --  (OTG_FS_DIEPTXF4)
      FS_DIEPTXF3         : FS_DIEPTXF_Register;
      case Mode is
         when Host =>
            --  OTG_FS Receive status debug read(Host mode)
            FS_GRXSTSR_Host : FS_GRXSTSR_Host_Register;
            --  OTG_FS non-periodic transmit FIFO size register (Host mode)
            FS_GNPTXFSIZ_Host : FS_GNPTXFSIZ_Host_Register;
         when Device =>
            --  OTG_FS Receive status debug read(Device mode)
            FS_GRXSTSR_Device : FS_GRXSTSR_Device_Register;
            --  OTG_FS non-periodic transmit FIFO size register (Device mode)
            FS_GNPTXFSIZ_Device : FS_GNPTXFSIZ_Device_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for OTG_FS_GLOBAL_Peripheral use record
      FS_GOTGCTL          at 0 range 0 .. 31;
      FS_GOTGINT          at 4 range 0 .. 31;
      FS_GAHBCFG          at 8 range 0 .. 31;
      FS_GUSBCFG          at 12 range 0 .. 31;
      FS_GRSTCTL          at 16 range 0 .. 31;
      FS_GINTSTS          at 20 range 0 .. 31;
      FS_GINTMSK          at 24 range 0 .. 31;
      FS_GRXFSIZ          at 36 range 0 .. 31;
      FS_GNPTXSTS         at 44 range 0 .. 31;
      FS_GCCFG            at 56 range 0 .. 31;
      FS_CID              at 60 range 0 .. 31;
      FS_HPTXFSIZ         at 256 range 0 .. 31;
      FS_DIEPTXF1         at 260 range 0 .. 31;
      FS_DIEPTXF2         at 264 range 0 .. 31;
      FS_DIEPTXF3         at 268 range 0 .. 31;
      FS_GRXSTSR_Host     at 28 range 0 .. 31;
      FS_GNPTXFSIZ_Host   at 40 range 0 .. 31;
      FS_GRXSTSR_Device   at 28 range 0 .. 31;
      FS_GNPTXFSIZ_Device at 40 range 0 .. 31;
   end record;

   --  USB on the go full speed
   OTG_FS_GLOBAL_Periph : aliased OTG_FS_GLOBAL_Peripheral
     with Import, Address => System'To_Address (16#50000000#);

   --  USB on the go full speed
   type OTG_FS_HOST_Peripheral is record
      --  OTG_FS host configuration register (OTG_FS_HCFG)
      FS_HCFG      : FS_HCFG_Register;
      --  OTG_FS Host frame interval register
      HFIR         : HFIR_Register;
      --  OTG_FS host frame number/frame time remaining register (OTG_FS_HFNUM)
      FS_HFNUM     : FS_HFNUM_Register;
      --  OTG_FS_Host periodic transmit FIFO/queue status register
      --  (OTG_FS_HPTXSTS)
      FS_HPTXSTS   : FS_HPTXSTS_Register;
      --  OTG_FS Host all channels interrupt register
      HAINT        : HAINT_Register;
      --  OTG_FS host all channels interrupt mask register
      HAINTMSK     : HAINTMSK_Register;
      --  OTG_FS host port control and status register (OTG_FS_HPRT)
      FS_HPRT      : FS_HPRT_Register;
      --  OTG_FS host channel-0 characteristics register (OTG_FS_HCCHAR0)
      FS_HCCHAR0   : FS_HCCHAR_Register;
      --  OTG_FS host channel-0 interrupt register (OTG_FS_HCINT0)
      FS_HCINT0    : FS_HCINT_Register;
      --  OTG_FS host channel-0 mask register (OTG_FS_HCINTMSK0)
      FS_HCINTMSK0 : FS_HCINTMSK_Register;
      --  OTG_FS host channel-0 transfer size register
      FS_HCTSIZ0   : FS_HCTSIZ_Register;
      --  OTG_FS host channel-1 characteristics register (OTG_FS_HCCHAR1)
      FS_HCCHAR1   : FS_HCCHAR_Register;
      --  OTG_FS host channel-1 interrupt register (OTG_FS_HCINT1)
      FS_HCINT1    : FS_HCINT_Register;
      --  OTG_FS host channel-1 mask register (OTG_FS_HCINTMSK1)
      FS_HCINTMSK1 : FS_HCINTMSK_Register;
      --  OTG_FS host channel-1 transfer size register
      FS_HCTSIZ1   : FS_HCTSIZ_Register;
      --  OTG_FS host channel-2 characteristics register (OTG_FS_HCCHAR2)
      FS_HCCHAR2   : FS_HCCHAR_Register;
      --  OTG_FS host channel-2 interrupt register (OTG_FS_HCINT2)
      FS_HCINT2    : FS_HCINT_Register;
      --  OTG_FS host channel-2 mask register (OTG_FS_HCINTMSK2)
      FS_HCINTMSK2 : FS_HCINTMSK_Register;
      --  OTG_FS host channel-2 transfer size register
      FS_HCTSIZ2   : FS_HCTSIZ_Register;
      --  OTG_FS host channel-3 characteristics register (OTG_FS_HCCHAR3)
      FS_HCCHAR3   : FS_HCCHAR_Register;
      --  OTG_FS host channel-3 interrupt register (OTG_FS_HCINT3)
      FS_HCINT3    : FS_HCINT_Register;
      --  OTG_FS host channel-3 mask register (OTG_FS_HCINTMSK3)
      FS_HCINTMSK3 : FS_HCINTMSK_Register;
      --  OTG_FS host channel-3 transfer size register
      FS_HCTSIZ3   : FS_HCTSIZ_Register;
      --  OTG_FS host channel-4 characteristics register (OTG_FS_HCCHAR4)
      FS_HCCHAR4   : FS_HCCHAR_Register;
      --  OTG_FS host channel-4 interrupt register (OTG_FS_HCINT4)
      FS_HCINT4    : FS_HCINT_Register;
      --  OTG_FS host channel-4 mask register (OTG_FS_HCINTMSK4)
      FS_HCINTMSK4 : FS_HCINTMSK_Register;
      --  OTG_FS host channel-x transfer size register
      FS_HCTSIZ4   : FS_HCTSIZ_Register;
      --  OTG_FS host channel-5 characteristics register (OTG_FS_HCCHAR5)
      FS_HCCHAR5   : FS_HCCHAR_Register;
      --  OTG_FS host channel-5 interrupt register (OTG_FS_HCINT5)
      FS_HCINT5    : FS_HCINT_Register;
      --  OTG_FS host channel-5 mask register (OTG_FS_HCINTMSK5)
      FS_HCINTMSK5 : FS_HCINTMSK_Register;
      --  OTG_FS host channel-5 transfer size register
      FS_HCTSIZ5   : FS_HCTSIZ_Register;
      --  OTG_FS host channel-6 characteristics register (OTG_FS_HCCHAR6)
      FS_HCCHAR6   : FS_HCCHAR_Register;
      --  OTG_FS host channel-6 interrupt register (OTG_FS_HCINT6)
      FS_HCINT6    : FS_HCINT_Register;
      --  OTG_FS host channel-6 mask register (OTG_FS_HCINTMSK6)
      FS_HCINTMSK6 : FS_HCINTMSK_Register;
      --  OTG_FS host channel-6 transfer size register
      FS_HCTSIZ6   : FS_HCTSIZ_Register;
      --  OTG_FS host channel-7 characteristics register (OTG_FS_HCCHAR7)
      FS_HCCHAR7   : FS_HCCHAR_Register;
      --  OTG_FS host channel-7 interrupt register (OTG_FS_HCINT7)
      FS_HCINT7    : FS_HCINT_Register;
      --  OTG_FS host channel-7 mask register (OTG_FS_HCINTMSK7)
      FS_HCINTMSK7 : FS_HCINTMSK_Register;
      --  OTG_FS host channel-7 transfer size register
      FS_HCTSIZ7   : FS_HCTSIZ_Register;
   end record
     with Volatile;

   for OTG_FS_HOST_Peripheral use record
      FS_HCFG      at 0 range 0 .. 31;
      HFIR         at 4 range 0 .. 31;
      FS_HFNUM     at 8 range 0 .. 31;
      FS_HPTXSTS   at 16 range 0 .. 31;
      HAINT        at 20 range 0 .. 31;
      HAINTMSK     at 24 range 0 .. 31;
      FS_HPRT      at 64 range 0 .. 31;
      FS_HCCHAR0   at 256 range 0 .. 31;
      FS_HCINT0    at 264 range 0 .. 31;
      FS_HCINTMSK0 at 268 range 0 .. 31;
      FS_HCTSIZ0   at 272 range 0 .. 31;
      FS_HCCHAR1   at 288 range 0 .. 31;
      FS_HCINT1    at 296 range 0 .. 31;
      FS_HCINTMSK1 at 300 range 0 .. 31;
      FS_HCTSIZ1   at 304 range 0 .. 31;
      FS_HCCHAR2   at 320 range 0 .. 31;
      FS_HCINT2    at 328 range 0 .. 31;
      FS_HCINTMSK2 at 332 range 0 .. 31;
      FS_HCTSIZ2   at 336 range 0 .. 31;
      FS_HCCHAR3   at 352 range 0 .. 31;
      FS_HCINT3    at 360 range 0 .. 31;
      FS_HCINTMSK3 at 364 range 0 .. 31;
      FS_HCTSIZ3   at 368 range 0 .. 31;
      FS_HCCHAR4   at 384 range 0 .. 31;
      FS_HCINT4    at 392 range 0 .. 31;
      FS_HCINTMSK4 at 396 range 0 .. 31;
      FS_HCTSIZ4   at 400 range 0 .. 31;
      FS_HCCHAR5   at 416 range 0 .. 31;
      FS_HCINT5    at 424 range 0 .. 31;
      FS_HCINTMSK5 at 428 range 0 .. 31;
      FS_HCTSIZ5   at 432 range 0 .. 31;
      FS_HCCHAR6   at 448 range 0 .. 31;
      FS_HCINT6    at 456 range 0 .. 31;
      FS_HCINTMSK6 at 460 range 0 .. 31;
      FS_HCTSIZ6   at 464 range 0 .. 31;
      FS_HCCHAR7   at 480 range 0 .. 31;
      FS_HCINT7    at 488 range 0 .. 31;
      FS_HCINTMSK7 at 492 range 0 .. 31;
      FS_HCTSIZ7   at 496 range 0 .. 31;
   end record;

   --  USB on the go full speed
   OTG_FS_HOST_Periph : aliased OTG_FS_HOST_Peripheral
     with Import, Address => System'To_Address (16#50000400#);

   --  USB on the go full speed
   type OTG_FS_DEVICE_Peripheral is record
      --  OTG_FS device configuration register (OTG_FS_DCFG)
      FS_DCFG     : FS_DCFG_Register;
      --  OTG_FS device control register (OTG_FS_DCTL)
      FS_DCTL     : FS_DCTL_Register;
      --  OTG_FS device status register (OTG_FS_DSTS)
      FS_DSTS     : FS_DSTS_Register;
      --  OTG_FS device IN endpoint common interrupt mask register
      --  (OTG_FS_DIEPMSK)
      FS_DIEPMSK  : FS_DIEPMSK_Register;
      --  OTG_FS device OUT endpoint common interrupt mask register
      --  (OTG_FS_DOEPMSK)
      FS_DOEPMSK  : FS_DOEPMSK_Register;
      --  OTG_FS device all endpoints interrupt register (OTG_FS_DAINT)
      FS_DAINT    : FS_DAINT_Register;
      --  OTG_FS all endpoints interrupt mask register (OTG_FS_DAINTMSK)
      FS_DAINTMSK : FS_DAINTMSK_Register;
      --  OTG_FS device VBUS discharge time register
      DVBUSDIS    : DVBUSDIS_Register;
      --  OTG_FS device VBUS pulsing time register
      DVBUSPULSE  : DVBUSPULSE_Register;
      --  OTG_FS device IN endpoint FIFO empty interrupt mask register
      DIEPEMPMSK  : DIEPEMPMSK_Register;
      --  OTG_FS device control IN endpoint 0 control register
      --  (OTG_FS_DIEPCTL0)
      FS_DIEPCTL0 : FS_DIEPCTL0_Register;
      --  device endpoint-x interrupt register
      DIEPINT0    : DIEPINT_Register;
      --  device endpoint-0 transfer size register
      DIEPTSIZ0   : DIEPTSIZ0_Register;
      --  OTG_FS device IN endpoint transmit FIFO status register
      DTXFSTS0    : DTXFSTS_Register;
      --  OTG device endpoint-1 control register
      DIEPCTL1    : DIEPCTL_Register;
      --  device endpoint-1 interrupt register
      DIEPINT1    : DIEPINT_Register;
      --  device endpoint-1 transfer size register
      DIEPTSIZ1   : DIEPTSIZ_Register;
      --  OTG_FS device IN endpoint transmit FIFO status register
      DTXFSTS1    : DTXFSTS_Register;
      --  OTG device endpoint-2 control register
      DIEPCTL2    : DIEPCTL_Register;
      --  device endpoint-2 interrupt register
      DIEPINT2    : DIEPINT_Register;
      --  device endpoint-2 transfer size register
      DIEPTSIZ2   : DIEPTSIZ_Register;
      --  OTG_FS device IN endpoint transmit FIFO status register
      DTXFSTS2    : DTXFSTS_Register;
      --  OTG device endpoint-3 control register
      DIEPCTL3    : DIEPCTL_Register;
      --  device endpoint-3 interrupt register
      DIEPINT3    : DIEPINT_Register;
      --  device endpoint-3 transfer size register
      DIEPTSIZ3   : DIEPTSIZ_Register;
      --  OTG_FS device IN endpoint transmit FIFO status register
      DTXFSTS3    : DTXFSTS_Register;
      --  device endpoint-0 control register
      DOEPCTL0    : DOEPCTL0_Register;
      --  device endpoint-0 interrupt register
      DOEPINT0    : DOEPINT_Register;
      --  device OUT endpoint-0 transfer size register
      DOEPTSIZ0   : DOEPTSIZ0_Register;
      --  device endpoint-1 control register
      DOEPCTL1    : DOEPCTL_Register;
      --  device endpoint-1 interrupt register
      DOEPINT1    : DOEPINT_Register;
      --  device OUT endpoint-1 transfer size register
      DOEPTSIZ1   : DOEPTSIZ_Register;
      --  device endpoint-2 control register
      DOEPCTL2    : DOEPCTL_Register;
      --  device endpoint-2 interrupt register
      DOEPINT2    : DOEPINT_Register;
      --  device OUT endpoint-2 transfer size register
      DOEPTSIZ2   : DOEPTSIZ_Register;
      --  device endpoint-3 control register
      DOEPCTL3    : DOEPCTL_Register;
      --  device endpoint-3 interrupt register
      DOEPINT3    : DOEPINT_Register;
      --  device OUT endpoint-3 transfer size register
      DOEPTSIZ3   : DOEPTSIZ_Register;
   end record
     with Volatile;

   for OTG_FS_DEVICE_Peripheral use record
      FS_DCFG     at 0 range 0 .. 31;
      FS_DCTL     at 4 range 0 .. 31;
      FS_DSTS     at 8 range 0 .. 31;
      FS_DIEPMSK  at 16 range 0 .. 31;
      FS_DOEPMSK  at 20 range 0 .. 31;
      FS_DAINT    at 24 range 0 .. 31;
      FS_DAINTMSK at 28 range 0 .. 31;
      DVBUSDIS    at 40 range 0 .. 31;
      DVBUSPULSE  at 44 range 0 .. 31;
      DIEPEMPMSK  at 52 range 0 .. 31;
      FS_DIEPCTL0 at 256 range 0 .. 31;
      DIEPINT0    at 264 range 0 .. 31;
      DIEPTSIZ0   at 272 range 0 .. 31;
      DTXFSTS0    at 280 range 0 .. 31;
      DIEPCTL1    at 288 range 0 .. 31;
      DIEPINT1    at 296 range 0 .. 31;
      DIEPTSIZ1   at 304 range 0 .. 31;
      DTXFSTS1    at 312 range 0 .. 31;
      DIEPCTL2    at 320 range 0 .. 31;
      DIEPINT2    at 328 range 0 .. 31;
      DIEPTSIZ2   at 336 range 0 .. 31;
      DTXFSTS2    at 344 range 0 .. 31;
      DIEPCTL3    at 352 range 0 .. 31;
      DIEPINT3    at 360 range 0 .. 31;
      DIEPTSIZ3   at 368 range 0 .. 31;
      DTXFSTS3    at 376 range 0 .. 31;
      DOEPCTL0    at 768 range 0 .. 31;
      DOEPINT0    at 776 range 0 .. 31;
      DOEPTSIZ0   at 784 range 0 .. 31;
      DOEPCTL1    at 800 range 0 .. 31;
      DOEPINT1    at 808 range 0 .. 31;
      DOEPTSIZ1   at 816 range 0 .. 31;
      DOEPCTL2    at 832 range 0 .. 31;
      DOEPINT2    at 840 range 0 .. 31;
      DOEPTSIZ2   at 848 range 0 .. 31;
      DOEPCTL3    at 864 range 0 .. 31;
      DOEPINT3    at 872 range 0 .. 31;
      DOEPTSIZ3   at 880 range 0 .. 31;
   end record;

   --  USB on the go full speed
   OTG_FS_DEVICE_Periph : aliased OTG_FS_DEVICE_Peripheral
     with Import, Address => System'To_Address (16#50000800#);

   --  USB on the go full speed
   type OTG_FS_PWRCLK_Peripheral is record
      --  OTG_FS power and clock gating control register
      FS_PCGCCTL : FS_PCGCCTL_Register;
   end record
     with Volatile;

   for OTG_FS_PWRCLK_Peripheral use record
      FS_PCGCCTL at 0 range 0 .. 31;
   end record;

   --  USB on the go full speed
   OTG_FS_PWRCLK_Periph : aliased OTG_FS_PWRCLK_Peripheral
     with Import, Address => System'To_Address (16#50000E00#);

end STM32_SVD.USB_OTG_FS;
