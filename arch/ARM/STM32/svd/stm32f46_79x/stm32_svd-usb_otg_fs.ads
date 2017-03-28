--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.USB_OTG_FS is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype FS_DCFG_DSPD_Field is HAL.UInt2;
   subtype FS_DCFG_DAD_Field is HAL.UInt7;
   subtype FS_DCFG_PFIVL_Field is HAL.UInt2;

   --  OTG_FS device configuration register (OTG_FS_DCFG)
   type FS_DCFG_Register is record
      --  Device speed
      DSPD           : FS_DCFG_DSPD_Field := 16#0#;
      --  Non-zero-length status OUT handshake
      NZLSOHSK       : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Device address
      DAD            : FS_DCFG_DAD_Field := 16#0#;
      --  Periodic frame interval
      PFIVL          : FS_DCFG_PFIVL_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#1100#;
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

   subtype FS_DCTL_TCTL_Field is HAL.UInt3;

   --  OTG_FS device control register (OTG_FS_DCTL)
   type FS_DCTL_Register is record
      --  Remote wakeup signaling
      RWUSIG         : Boolean := False;
      --  Soft disconnect
      SDIS           : Boolean := False;
      --  Read-only. Global IN NAK status
      GINSTS         : Boolean := False;
      --  Read-only. Global OUT NAK status
      GONSTS         : Boolean := False;
      --  Test control
      TCTL           : FS_DCTL_TCTL_Field := 16#0#;
      --  Set global IN NAK
      SGINAK         : Boolean := False;
      --  Clear global IN NAK
      CGINAK         : Boolean := False;
      --  Set global OUT NAK
      SGONAK         : Boolean := False;
      --  Clear global OUT NAK
      CGONAK         : Boolean := False;
      --  Power-on programming done
      POPRGDNE       : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
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

   subtype FS_DSTS_ENUMSPD_Field is HAL.UInt2;
   subtype FS_DSTS_FNSOF_Field is HAL.UInt14;

   --  OTG_FS device status register (OTG_FS_DSTS)
   type FS_DSTS_Register is record
      --  Read-only. Suspend status
      SUSPSTS        : Boolean;
      --  Read-only. Enumerated speed
      ENUMSPD        : FS_DSTS_ENUMSPD_Field;
      --  Read-only. Erratic error
      EERR           : Boolean;
      --  unspecified
      Reserved_4_7   : HAL.UInt4;
      --  Read-only. Frame number of the received SOF
      FNSOF          : FS_DSTS_FNSOF_Field;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
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

   --  OTG_FS device IN endpoint common interrupt mask register
   --  (OTG_FS_DIEPMSK)
   type FS_DIEPMSK_Register is record
      --  Transfer completed interrupt mask
      XFRCM         : Boolean := False;
      --  Endpoint disabled interrupt mask
      EPDM          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Timeout condition mask (Non-isochronous endpoints)
      TOM           : Boolean := False;
      --  IN token received when TxFIFO empty mask
      ITTXFEMSK     : Boolean := False;
      --  IN token received with EP mismatch mask
      INEPNMM       : Boolean := False;
      --  IN endpoint NAK effective mask
      INEPNEM       : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
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

   --  OTG_FS device OUT endpoint common interrupt mask register
   --  (OTG_FS_DOEPMSK)
   type FS_DOEPMSK_Register is record
      --  Transfer completed interrupt mask
      XFRCM         : Boolean := False;
      --  Endpoint disabled interrupt mask
      EPDM          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  SETUP phase done mask
      STUPM         : Boolean := False;
      --  OUT token received when endpoint disabled mask
      OTEPDM        : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
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

   subtype FS_DAINT_IEPINT_Field is HAL.UInt16;
   subtype FS_DAINT_OEPINT_Field is HAL.UInt16;

   --  OTG_FS device all endpoints interrupt register (OTG_FS_DAINT)
   type FS_DAINT_Register is record
      --  Read-only. IN endpoint interrupt bits
      IEPINT : FS_DAINT_IEPINT_Field;
      --  Read-only. OUT endpoint interrupt bits
      OEPINT : FS_DAINT_OEPINT_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DAINT_Register use record
      IEPINT at 0 range 0 .. 15;
      OEPINT at 0 range 16 .. 31;
   end record;

   subtype FS_DAINTMSK_IEPM_Field is HAL.UInt16;
   subtype FS_DAINTMSK_OEPM_Field is HAL.UInt16;

   --  OTG_FS all endpoints interrupt mask register (OTG_FS_DAINTMSK)
   type FS_DAINTMSK_Register is record
      --  IN EP interrupt mask bits
      IEPM : FS_DAINTMSK_IEPM_Field := 16#0#;
      --  OUT EP interrupt mask bits
      OEPM : FS_DAINTMSK_OEPM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DAINTMSK_Register use record
      IEPM at 0 range 0 .. 15;
      OEPM at 0 range 16 .. 31;
   end record;

   subtype DVBUSDIS_VBUSDT_Field is HAL.UInt16;

   --  OTG_FS device VBUS discharge time register
   type DVBUSDIS_Register is record
      --  Device VBUS discharge time
      VBUSDT         : DVBUSDIS_VBUSDT_Field := 16#17D7#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DVBUSDIS_Register use record
      VBUSDT         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DVBUSPULSE_DVBUSP_Field is HAL.UInt12;

   --  OTG_FS device VBUS pulsing time register
   type DVBUSPULSE_Register is record
      --  Device VBUS pulsing time
      DVBUSP         : DVBUSPULSE_DVBUSP_Field := 16#5B8#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DVBUSPULSE_Register use record
      DVBUSP         at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype DIEPEMPMSK_INEPTXFEM_Field is HAL.UInt16;

   --  OTG_FS device IN endpoint FIFO empty interrupt mask register
   type DIEPEMPMSK_Register is record
      --  IN EP Tx FIFO empty interrupt mask bits
      INEPTXFEM      : DIEPEMPMSK_INEPTXFEM_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPEMPMSK_Register use record
      INEPTXFEM      at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FS_DIEPCTL0_MPSIZ_Field is HAL.UInt2;
   subtype FS_DIEPCTL0_EPTYP_Field is HAL.UInt2;
   subtype FS_DIEPCTL0_TXFNUM_Field is HAL.UInt4;

   --  OTG_FS device control IN endpoint 0 control register (OTG_FS_DIEPCTL0)
   type FS_DIEPCTL0_Register is record
      --  Maximum packet size
      MPSIZ          : FS_DIEPCTL0_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_2_14  : HAL.UInt13 := 16#0#;
      --  Read-only. USB active endpoint
      USBAEP         : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Read-only. NAK status
      NAKSTS         : Boolean := False;
      --  Read-only. Endpoint type
      EPTYP          : FS_DIEPCTL0_EPTYP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  STALL handshake
      STALL          : Boolean := False;
      --  TxFIFO number
      TXFNUM         : FS_DIEPCTL0_TXFNUM_Field := 16#0#;
      --  Write-only. Clear NAK
      CNAK           : Boolean := False;
      --  Write-only. Set NAK
      SNAK           : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. Endpoint disable
      EPDIS          : Boolean := False;
      --  Read-only. Endpoint enable
      EPENA          : Boolean := False;
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

   --  device endpoint-x interrupt register
   type DIEPINT_Register is record
      --  XFRC
      XFRC          : Boolean := False;
      --  EPDISD
      EPDISD        : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  TOC
      TOC           : Boolean := False;
      --  ITTXFE
      ITTXFE        : Boolean := False;
      --  unspecified
      Reserved_5_5  : HAL.Bit := 16#0#;
      --  INEPNE
      INEPNE        : Boolean := False;
      --  Read-only. TXFE
      TXFE          : Boolean := True;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
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

   subtype DIEPTSIZ0_XFRSIZ_Field is HAL.UInt7;
   subtype DIEPTSIZ0_PKTCNT_Field is HAL.UInt2;

   --  device endpoint-0 transfer size register
   type DIEPTSIZ0_Register is record
      --  Transfer size
      XFRSIZ         : DIEPTSIZ0_XFRSIZ_Field := 16#0#;
      --  unspecified
      Reserved_7_18  : HAL.UInt12 := 16#0#;
      --  Packet count
      PKTCNT         : DIEPTSIZ0_PKTCNT_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPTSIZ0_Register use record
      XFRSIZ         at 0 range 0 .. 6;
      Reserved_7_18  at 0 range 7 .. 18;
      PKTCNT         at 0 range 19 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype DTXFSTS_INEPTFSAV_Field is HAL.UInt16;

   --  OTG_FS device IN endpoint transmit FIFO status register
   type DTXFSTS_Register is record
      --  Read-only. IN endpoint TxFIFO space available
      INEPTFSAV      : DTXFSTS_INEPTFSAV_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DTXFSTS_Register use record
      INEPTFSAV      at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DIEPCTL1_MPSIZ_Field is HAL.UInt11;
   subtype DIEPCTL1_EPTYP_Field is HAL.UInt2;
   subtype DIEPCTL1_TXFNUM_Field is HAL.UInt4;

   --  OTG device endpoint-1 control register
   type DIEPCTL1_Register is record
      --  MPSIZ
      MPSIZ          : DIEPCTL1_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  USBAEP
      USBAEP         : Boolean := False;
      --  Read-only. EONUM/DPID
      EONUM_DPID     : Boolean := False;
      --  Read-only. NAKSTS
      NAKSTS         : Boolean := False;
      --  EPTYP
      EPTYP          : DIEPCTL1_EPTYP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  Stall
      Stall          : Boolean := False;
      --  TXFNUM
      TXFNUM         : DIEPCTL1_TXFNUM_Field := 16#0#;
      --  Write-only. CNAK
      CNAK           : Boolean := False;
      --  Write-only. SNAK
      SNAK           : Boolean := False;
      --  Write-only. SD0PID/SEVNFRM
      SD0PID_SEVNFRM : Boolean := False;
      --  Write-only. SODDFRM/SD1PID
      SODDFRM_SD1PID : Boolean := False;
      --  EPDIS
      EPDIS          : Boolean := False;
      --  EPENA
      EPENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPCTL1_Register use record
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

   subtype DIEPTSIZ_XFRSIZ_Field is HAL.UInt19;
   subtype DIEPTSIZ_PKTCNT_Field is HAL.UInt10;
   subtype DIEPTSIZ_MCNT_Field is HAL.UInt2;

   --  device endpoint-1 transfer size register
   type DIEPTSIZ_Register is record
      --  Transfer size
      XFRSIZ         : DIEPTSIZ_XFRSIZ_Field := 16#0#;
      --  Packet count
      PKTCNT         : DIEPTSIZ_PKTCNT_Field := 16#0#;
      --  Multi count
      MCNT           : DIEPTSIZ_MCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIEPTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      MCNT           at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype DIEPCTL_MPSIZ_Field is HAL.UInt11;
   subtype DIEPCTL_EPTYP_Field is HAL.UInt2;
   subtype DIEPCTL_TXFNUM_Field is HAL.UInt4;

   --  OTG device endpoint-2 control register
   type DIEPCTL_Register is record
      --  MPSIZ
      MPSIZ          : DIEPCTL_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  USBAEP
      USBAEP         : Boolean := False;
      --  Read-only. EONUM/DPID
      EONUM_DPID     : Boolean := False;
      --  Read-only. NAKSTS
      NAKSTS         : Boolean := False;
      --  EPTYP
      EPTYP          : DIEPCTL_EPTYP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  Stall
      Stall          : Boolean := False;
      --  TXFNUM
      TXFNUM         : DIEPCTL_TXFNUM_Field := 16#0#;
      --  Write-only. CNAK
      CNAK           : Boolean := False;
      --  Write-only. SNAK
      SNAK           : Boolean := False;
      --  Write-only. SD0PID/SEVNFRM
      SD0PID_SEVNFRM : Boolean := False;
      --  Write-only. SODDFRM
      SODDFRM        : Boolean := False;
      --  EPDIS
      EPDIS          : Boolean := False;
      --  EPENA
      EPENA          : Boolean := False;
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
      SODDFRM        at 0 range 29 .. 29;
      EPDIS          at 0 range 30 .. 30;
      EPENA          at 0 range 31 .. 31;
   end record;

   subtype DOEPCTL0_MPSIZ_Field is HAL.UInt2;
   subtype DOEPCTL0_EPTYP_Field is HAL.UInt2;

   --  device endpoint-0 control register
   type DOEPCTL0_Register is record
      --  Read-only. MPSIZ
      MPSIZ          : DOEPCTL0_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_2_14  : HAL.UInt13 := 16#0#;
      --  Read-only. USBAEP
      USBAEP         : Boolean := True;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Read-only. NAKSTS
      NAKSTS         : Boolean := False;
      --  Read-only. EPTYP
      EPTYP          : DOEPCTL0_EPTYP_Field := 16#0#;
      --  SNPM
      SNPM           : Boolean := False;
      --  Stall
      Stall          : Boolean := False;
      --  unspecified
      Reserved_22_25 : HAL.UInt4 := 16#0#;
      --  Write-only. CNAK
      CNAK           : Boolean := False;
      --  Write-only. SNAK
      SNAK           : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. EPDIS
      EPDIS          : Boolean := False;
      --  Write-only. EPENA
      EPENA          : Boolean := False;
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

   --  device endpoint-0 interrupt register
   type DOEPINT_Register is record
      --  XFRC
      XFRC          : Boolean := False;
      --  EPDISD
      EPDISD        : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  STUP
      STUP          : Boolean := False;
      --  OTEPDIS
      OTEPDIS       : Boolean := False;
      --  unspecified
      Reserved_5_5  : HAL.Bit := 16#0#;
      --  B2BSTUP
      B2BSTUP       : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#1#;
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

   subtype DOEPTSIZ0_XFRSIZ_Field is HAL.UInt7;
   subtype DOEPTSIZ0_STUPCNT_Field is HAL.UInt2;

   --  device OUT endpoint-0 transfer size register
   type DOEPTSIZ0_Register is record
      --  Transfer size
      XFRSIZ         : DOEPTSIZ0_XFRSIZ_Field := 16#0#;
      --  unspecified
      Reserved_7_18  : HAL.UInt12 := 16#0#;
      --  Packet count
      PKTCNT         : Boolean := False;
      --  unspecified
      Reserved_20_28 : HAL.UInt9 := 16#0#;
      --  SETUP packet count
      STUPCNT        : DOEPTSIZ0_STUPCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
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

   subtype DOEPCTL_MPSIZ_Field is HAL.UInt11;
   subtype DOEPCTL_EPTYP_Field is HAL.UInt2;

   --  device endpoint-1 control register
   type DOEPCTL_Register is record
      --  MPSIZ
      MPSIZ          : DOEPCTL_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  USBAEP
      USBAEP         : Boolean := False;
      --  Read-only. EONUM/DPID
      EONUM_DPID     : Boolean := False;
      --  Read-only. NAKSTS
      NAKSTS         : Boolean := False;
      --  EPTYP
      EPTYP          : DOEPCTL_EPTYP_Field := 16#0#;
      --  SNPM
      SNPM           : Boolean := False;
      --  Stall
      Stall          : Boolean := False;
      --  unspecified
      Reserved_22_25 : HAL.UInt4 := 16#0#;
      --  Write-only. CNAK
      CNAK           : Boolean := False;
      --  Write-only. SNAK
      SNAK           : Boolean := False;
      --  Write-only. SD0PID/SEVNFRM
      SD0PID_SEVNFRM : Boolean := False;
      --  Write-only. SODDFRM
      SODDFRM        : Boolean := False;
      --  EPDIS
      EPDIS          : Boolean := False;
      --  EPENA
      EPENA          : Boolean := False;
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

   subtype DOEPTSIZ_XFRSIZ_Field is HAL.UInt19;
   subtype DOEPTSIZ_PKTCNT_Field is HAL.UInt10;
   subtype DOEPTSIZ_RXDPID_STUPCNT_Field is HAL.UInt2;

   --  device OUT endpoint-1 transfer size register
   type DOEPTSIZ_Register is record
      --  Transfer size
      XFRSIZ         : DOEPTSIZ_XFRSIZ_Field := 16#0#;
      --  Packet count
      PKTCNT         : DOEPTSIZ_PKTCNT_Field := 16#0#;
      --  Received data PID/SETUP packet count
      RXDPID_STUPCNT : DOEPTSIZ_RXDPID_STUPCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOEPTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      RXDPID_STUPCNT at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  OTG_FS control and status register (OTG_FS_GOTGCTL)
   type FS_GOTGCTL_Register is record
      --  Read-only. Session request success
      SRQSCS         : Boolean := False;
      --  Session request
      SRQ            : Boolean := False;
      --  unspecified
      Reserved_2_7   : HAL.UInt6 := 16#0#;
      --  Read-only. Host negotiation success
      HNGSCS         : Boolean := False;
      --  HNP request
      HNPRQ          : Boolean := False;
      --  Host set HNP enable
      HSHNPEN        : Boolean := False;
      --  Device HNP enabled
      DHNPEN         : Boolean := True;
      --  unspecified
      Reserved_12_15 : HAL.UInt4 := 16#0#;
      --  Read-only. Connector ID status
      CIDSTS         : Boolean := False;
      --  Read-only. Long/short debounce time
      DBCT           : Boolean := False;
      --  Read-only. A-session valid
      ASVLD          : Boolean := False;
      --  Read-only. B-session valid
      BSVLD          : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
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

   --  OTG_FS interrupt register (OTG_FS_GOTGINT)
   type FS_GOTGINT_Register is record
      --  unspecified
      Reserved_0_1   : HAL.UInt2 := 16#0#;
      --  Session end detected
      SEDET          : Boolean := False;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Session request success status change
      SRSSCHG        : Boolean := False;
      --  Host negotiation success status change
      HNSSCHG        : Boolean := False;
      --  unspecified
      Reserved_10_16 : HAL.UInt7 := 16#0#;
      --  Host negotiation detected
      HNGDET         : Boolean := False;
      --  A-device timeout change
      ADTOCHG        : Boolean := False;
      --  Debounce done
      DBCDNE         : Boolean := False;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
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

   --  OTG_FS AHB configuration register (OTG_FS_GAHBCFG)
   type FS_GAHBCFG_Register is record
      --  Global interrupt mask
      GINT          : Boolean := False;
      --  unspecified
      Reserved_1_6  : HAL.UInt6 := 16#0#;
      --  TxFIFO empty level
      TXFELVL       : Boolean := False;
      --  Periodic TxFIFO empty level
      PTXFELVL      : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
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

   subtype FS_GUSBCFG_TOCAL_Field is HAL.UInt3;
   subtype FS_GUSBCFG_TRDT_Field is HAL.UInt4;

   --  OTG_FS USB configuration register (OTG_FS_GUSBCFG)
   type FS_GUSBCFG_Register is record
      --  FS timeout calibration
      TOCAL          : FS_GUSBCFG_TOCAL_Field := 16#0#;
      --  unspecified
      Reserved_3_5   : HAL.UInt3 := 16#0#;
      --  Write-only. Full Speed serial transceiver select
      PHYSEL         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  SRP-capable
      SRPCAP         : Boolean := False;
      --  HNP-capable
      HNPCAP         : Boolean := True;
      --  USB turnaround time
      TRDT           : FS_GUSBCFG_TRDT_Field := 16#2#;
      --  unspecified
      Reserved_14_28 : HAL.UInt15 := 16#0#;
      --  Force host mode
      FHMOD          : Boolean := False;
      --  Force device mode
      FDMOD          : Boolean := False;
      --  Corrupt Tx packet
      CTXPKT         : Boolean := False;
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

   subtype FS_GRSTCTL_TXFNUM_Field is HAL.UInt5;

   --  OTG_FS reset register (OTG_FS_GRSTCTL)
   type FS_GRSTCTL_Register is record
      --  Core soft reset
      CSRST          : Boolean := False;
      --  HCLK soft reset
      HSRST          : Boolean := False;
      --  Host frame counter reset
      FCRST          : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  RxFIFO flush
      RXFFLSH        : Boolean := False;
      --  TxFIFO flush
      TXFFLSH        : Boolean := False;
      --  TxFIFO number
      TXFNUM         : FS_GRSTCTL_TXFNUM_Field := 16#0#;
      --  unspecified
      Reserved_11_30 : HAL.UInt20 := 16#40000#;
      --  Read-only. AHB master idle
      AHBIDL         : Boolean := False;
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

   --  OTG_FS core interrupt register (OTG_FS_GINTSTS)
   type FS_GINTSTS_Register is record
      --  Read-only. Current mode of operation
      CMOD               : Boolean := False;
      --  Mode mismatch interrupt
      MMIS               : Boolean := False;
      --  Read-only. OTG interrupt
      OTGINT             : Boolean := False;
      --  Start of frame
      SOF                : Boolean := False;
      --  Read-only. RxFIFO non-empty
      RXFLVL             : Boolean := False;
      --  Read-only. Non-periodic TxFIFO empty
      NPTXFE             : Boolean := True;
      --  Read-only. Global IN non-periodic NAK effective
      GINAKEFF           : Boolean := False;
      --  Read-only. Global OUT NAK effective
      GOUTNAKEFF         : Boolean := False;
      --  unspecified
      Reserved_8_9       : HAL.UInt2 := 16#0#;
      --  Early suspend
      ESUSP              : Boolean := False;
      --  USB suspend
      USBSUSP            : Boolean := False;
      --  USB reset
      USBRST             : Boolean := False;
      --  Enumeration done
      ENUMDNE            : Boolean := False;
      --  Isochronous OUT packet dropped interrupt
      ISOODRP            : Boolean := False;
      --  End of periodic frame interrupt
      EOPF               : Boolean := False;
      --  unspecified
      Reserved_16_17     : HAL.UInt2 := 16#0#;
      --  Read-only. IN endpoint interrupt
      IEPINT             : Boolean := False;
      --  Read-only. OUT endpoint interrupt
      OEPINT             : Boolean := False;
      --  Incomplete isochronous IN transfer
      IISOIXFR           : Boolean := False;
      --  Incomplete periodic transfer(Host mode)/Incomplete isochronous OUT
      --  transfer(Device mode)
      IPXFR_INCOMPISOOUT : Boolean := False;
      --  unspecified
      Reserved_22_23     : HAL.UInt2 := 16#0#;
      --  Read-only. Host port interrupt
      HPRTINT            : Boolean := False;
      --  Read-only. Host channels interrupt
      HCINT              : Boolean := False;
      --  Read-only. Periodic TxFIFO empty
      PTXFE              : Boolean := True;
      --  unspecified
      Reserved_27_27     : HAL.Bit := 16#0#;
      --  Connector ID status change
      CIDSCHG            : Boolean := False;
      --  Disconnect detected interrupt
      DISCINT            : Boolean := False;
      --  Session request/new session detected interrupt
      SRQINT             : Boolean := False;
      --  Resume/remote wakeup detected interrupt
      WKUPINT            : Boolean := False;
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

   --  OTG_FS interrupt mask register (OTG_FS_GINTMSK)
   type FS_GINTMSK_Register is record
      --  unspecified
      Reserved_0_0     : HAL.Bit := 16#0#;
      --  Mode mismatch interrupt mask
      MMISM            : Boolean := False;
      --  OTG interrupt mask
      OTGINT           : Boolean := False;
      --  Start of frame mask
      SOFM             : Boolean := False;
      --  Receive FIFO non-empty mask
      RXFLVLM          : Boolean := False;
      --  Non-periodic TxFIFO empty mask
      NPTXFEM          : Boolean := False;
      --  Global non-periodic IN NAK effective mask
      GINAKEFFM        : Boolean := False;
      --  Global OUT NAK effective mask
      GONAKEFFM        : Boolean := False;
      --  unspecified
      Reserved_8_9     : HAL.UInt2 := 16#0#;
      --  Early suspend mask
      ESUSPM           : Boolean := False;
      --  USB suspend mask
      USBSUSPM         : Boolean := False;
      --  USB reset mask
      USBRST           : Boolean := False;
      --  Enumeration done mask
      ENUMDNEM         : Boolean := False;
      --  Isochronous OUT packet dropped interrupt mask
      ISOODRPM         : Boolean := False;
      --  End of periodic frame interrupt mask
      EOPFM            : Boolean := False;
      --  unspecified
      Reserved_16_16   : HAL.Bit := 16#0#;
      --  Endpoint mismatch interrupt mask
      EPMISM           : Boolean := False;
      --  IN endpoints interrupt mask
      IEPINT           : Boolean := False;
      --  OUT endpoints interrupt mask
      OEPINT           : Boolean := False;
      --  Incomplete isochronous IN transfer mask
      IISOIXFRM        : Boolean := False;
      --  Incomplete periodic transfer mask(Host mode)/Incomplete isochronous
      --  OUT transfer mask(Device mode)
      IPXFRM_IISOOXFRM : Boolean := False;
      --  unspecified
      Reserved_22_23   : HAL.UInt2 := 16#0#;
      --  Read-only. Host port interrupt mask
      PRTIM            : Boolean := False;
      --  Host channels interrupt mask
      HCIM             : Boolean := False;
      --  Periodic TxFIFO empty mask
      PTXFEM           : Boolean := False;
      --  unspecified
      Reserved_27_27   : HAL.Bit := 16#0#;
      --  Connector ID status change mask
      CIDSCHGM         : Boolean := False;
      --  Disconnect detected interrupt mask
      DISCINT          : Boolean := False;
      --  Session request/new session detected interrupt mask
      SRQIM            : Boolean := False;
      --  Resume/remote wakeup detected interrupt mask
      WUIM             : Boolean := False;
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

   subtype FS_GRXSTSR_Device_EPNUM_Field is HAL.UInt4;
   subtype FS_GRXSTSR_Device_BCNT_Field is HAL.UInt11;
   subtype FS_GRXSTSR_Device_DPID_Field is HAL.UInt2;
   subtype FS_GRXSTSR_Device_PKTSTS_Field is HAL.UInt4;
   subtype FS_GRXSTSR_Device_FRMNUM_Field is HAL.UInt4;

   --  OTG_FS Receive status debug read(Device mode)
   type FS_GRXSTSR_Device_Register is record
      --  Read-only. Endpoint number
      EPNUM          : FS_GRXSTSR_Device_EPNUM_Field;
      --  Read-only. Byte count
      BCNT           : FS_GRXSTSR_Device_BCNT_Field;
      --  Read-only. Data PID
      DPID           : FS_GRXSTSR_Device_DPID_Field;
      --  Read-only. Packet status
      PKTSTS         : FS_GRXSTSR_Device_PKTSTS_Field;
      --  Read-only. Frame number
      FRMNUM         : FS_GRXSTSR_Device_FRMNUM_Field;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
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

   subtype FS_GRXSTSR_Host_EPNUM_Field is HAL.UInt4;
   subtype FS_GRXSTSR_Host_BCNT_Field is HAL.UInt11;
   subtype FS_GRXSTSR_Host_DPID_Field is HAL.UInt2;
   subtype FS_GRXSTSR_Host_PKTSTS_Field is HAL.UInt4;
   subtype FS_GRXSTSR_Host_FRMNUM_Field is HAL.UInt4;

   --  OTG_FS Receive status debug read(Hostmode)
   type FS_GRXSTSR_Host_Register is record
      --  Read-only. Endpoint number
      EPNUM          : FS_GRXSTSR_Host_EPNUM_Field;
      --  Read-only. Byte count
      BCNT           : FS_GRXSTSR_Host_BCNT_Field;
      --  Read-only. Data PID
      DPID           : FS_GRXSTSR_Host_DPID_Field;
      --  Read-only. Packet status
      PKTSTS         : FS_GRXSTSR_Host_PKTSTS_Field;
      --  Read-only. Frame number
      FRMNUM         : FS_GRXSTSR_Host_FRMNUM_Field;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
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

   subtype FS_GRXFSIZ_RXFD_Field is HAL.UInt16;

   --  OTG_FS Receive FIFO size register (OTG_FS_GRXFSIZ)
   type FS_GRXFSIZ_Register is record
      --  RxFIFO depth
      RXFD           : FS_GRXFSIZ_RXFD_Field := 16#200#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GRXFSIZ_Register use record
      RXFD           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FS_GNPTXFSIZ_Device_TX0FSA_Field is HAL.UInt16;
   subtype FS_GNPTXFSIZ_Device_TX0FD_Field is HAL.UInt16;

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

   subtype FS_GNPTXFSIZ_Host_NPTXFSA_Field is HAL.UInt16;
   subtype FS_GNPTXFSIZ_Host_NPTXFD_Field is HAL.UInt16;

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

   subtype FS_GNPTXSTS_NPTXFSAV_Field is HAL.UInt16;
   subtype FS_GNPTXSTS_NPTQXSAV_Field is HAL.UInt8;
   subtype FS_GNPTXSTS_NPTXQTOP_Field is HAL.UInt7;

   --  OTG_FS non-periodic transmit FIFO/queue status register
   --  (OTG_FS_GNPTXSTS)
   type FS_GNPTXSTS_Register is record
      --  Read-only. Non-periodic TxFIFO space available
      NPTXFSAV       : FS_GNPTXSTS_NPTXFSAV_Field;
      --  Read-only. Non-periodic transmit request queue space available
      NPTQXSAV       : FS_GNPTXSTS_NPTQXSAV_Field;
      --  Read-only. Top of the non-periodic transmit request queue
      NPTXQTOP       : FS_GNPTXSTS_NPTXQTOP_Field;
      --  unspecified
      Reserved_31_31 : HAL.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_GNPTXSTS_Register use record
      NPTXFSAV       at 0 range 0 .. 15;
      NPTQXSAV       at 0 range 16 .. 23;
      NPTXQTOP       at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  OTG_FS general core configuration register (OTG_FS_GCCFG)
   type FS_GCCFG_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  Power down
      PWRDWN         : Boolean := False;
      --  unspecified
      Reserved_17_17 : HAL.Bit := 16#0#;
      --  Enable the VBUS sensing device
      VBUSASEN       : Boolean := False;
      --  Enable the VBUS sensing device
      VBUSBSEN       : Boolean := False;
      --  SOF output enable
      SOFOUTEN       : Boolean := False;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
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

   subtype FS_HPTXFSIZ_PTXSA_Field is HAL.UInt16;
   subtype FS_HPTXFSIZ_PTXFSIZ_Field is HAL.UInt16;

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

   subtype FS_DIEPTXF_INEPTXSA_Field is HAL.UInt16;
   subtype FS_DIEPTXF_INEPTXFD_Field is HAL.UInt16;

   --  OTG_FS device IN endpoint transmit FIFO size register (OTG_FS_DIEPTXF1)
   type FS_DIEPTXF_Register is record
      --  IN endpoint FIFO2 transmit RAM start address
      INEPTXSA : FS_DIEPTXF_INEPTXSA_Field := 16#400#;
      --  IN endpoint TxFIFO depth
      INEPTXFD : FS_DIEPTXF_INEPTXFD_Field := 16#200#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_DIEPTXF_Register use record
      INEPTXSA at 0 range 0 .. 15;
      INEPTXFD at 0 range 16 .. 31;
   end record;

   subtype FS_HCFG_FSLSPCS_Field is HAL.UInt2;

   --  OTG_FS host configuration register (OTG_FS_HCFG)
   type FS_HCFG_Register is record
      --  FS/LS PHY clock select
      FSLSPCS       : FS_HCFG_FSLSPCS_Field := 16#0#;
      --  Read-only. FS- and LS-only support
      FSLSS         : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HCFG_Register use record
      FSLSPCS       at 0 range 0 .. 1;
      FSLSS         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype HFIR_FRIVL_Field is HAL.UInt16;

   --  OTG_FS Host frame interval register
   type HFIR_Register is record
      --  Frame interval
      FRIVL          : HFIR_FRIVL_Field := 16#EA60#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HFIR_Register use record
      FRIVL          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FS_HFNUM_FRNUM_Field is HAL.UInt16;
   subtype FS_HFNUM_FTREM_Field is HAL.UInt16;

   --  OTG_FS host frame number/frame time remaining register (OTG_FS_HFNUM)
   type FS_HFNUM_Register is record
      --  Read-only. Frame number
      FRNUM : FS_HFNUM_FRNUM_Field;
      --  Read-only. Frame time remaining
      FTREM : FS_HFNUM_FTREM_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HFNUM_Register use record
      FRNUM at 0 range 0 .. 15;
      FTREM at 0 range 16 .. 31;
   end record;

   subtype FS_HPTXSTS_PTXFSAVL_Field is HAL.UInt16;
   subtype FS_HPTXSTS_PTXQSAV_Field is HAL.UInt8;
   subtype FS_HPTXSTS_PTXQTOP_Field is HAL.UInt8;

   --  OTG_FS_Host periodic transmit FIFO/queue status register
   --  (OTG_FS_HPTXSTS)
   type FS_HPTXSTS_Register is record
      --  Periodic transmit data FIFO space available
      PTXFSAVL : FS_HPTXSTS_PTXFSAVL_Field := 16#100#;
      --  Read-only. Periodic transmit request queue space available
      PTXQSAV  : FS_HPTXSTS_PTXQSAV_Field := 16#8#;
      --  Read-only. Top of the periodic transmit request queue
      PTXQTOP  : FS_HPTXSTS_PTXQTOP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HPTXSTS_Register use record
      PTXFSAVL at 0 range 0 .. 15;
      PTXQSAV  at 0 range 16 .. 23;
      PTXQTOP  at 0 range 24 .. 31;
   end record;

   subtype HAINT_HAINT_Field is HAL.UInt16;

   --  OTG_FS Host all channels interrupt register
   type HAINT_Register is record
      --  Read-only. Channel interrupts
      HAINT          : HAINT_HAINT_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HAINT_Register use record
      HAINT          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype HAINTMSK_HAINTM_Field is HAL.UInt16;

   --  OTG_FS host all channels interrupt mask register
   type HAINTMSK_Register is record
      --  Channel interrupt mask
      HAINTM         : HAINTMSK_HAINTM_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HAINTMSK_Register use record
      HAINTM         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FS_HPRT_PLSTS_Field is HAL.UInt2;
   subtype FS_HPRT_PTCTL_Field is HAL.UInt4;
   subtype FS_HPRT_PSPD_Field is HAL.UInt2;

   --  OTG_FS host port control and status register (OTG_FS_HPRT)
   type FS_HPRT_Register is record
      --  Read-only. Port connect status
      PCSTS          : Boolean := False;
      --  Port connect detected
      PCDET          : Boolean := False;
      --  Port enable
      PENA           : Boolean := False;
      --  Port enable/disable change
      PENCHNG        : Boolean := False;
      --  Read-only. Port overcurrent active
      POCA           : Boolean := False;
      --  Port overcurrent change
      POCCHNG        : Boolean := False;
      --  Port resume
      PRES           : Boolean := False;
      --  Port suspend
      PSUSP          : Boolean := False;
      --  Port reset
      PRST           : Boolean := False;
      --  unspecified
      Reserved_9_9   : HAL.Bit := 16#0#;
      --  Read-only. Port line status
      PLSTS          : FS_HPRT_PLSTS_Field := 16#0#;
      --  Port power
      PPWR           : Boolean := False;
      --  Port test control
      PTCTL          : FS_HPRT_PTCTL_Field := 16#0#;
      --  Read-only. Port speed
      PSPD           : FS_HPRT_PSPD_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
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

   subtype FS_HCCHAR_MPSIZ_Field is HAL.UInt11;
   subtype FS_HCCHAR_EPNUM_Field is HAL.UInt4;
   subtype FS_HCCHAR_EPTYP_Field is HAL.UInt2;
   subtype FS_HCCHAR_MCNT_Field is HAL.UInt2;
   subtype FS_HCCHAR_DAD_Field is HAL.UInt7;

   --  OTG_FS host channel-0 characteristics register (OTG_FS_HCCHAR0)
   type FS_HCCHAR_Register is record
      --  Maximum packet size
      MPSIZ          : FS_HCCHAR_MPSIZ_Field := 16#0#;
      --  Endpoint number
      EPNUM          : FS_HCCHAR_EPNUM_Field := 16#0#;
      --  Endpoint direction
      EPDIR          : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Low-speed device
      LSDEV          : Boolean := False;
      --  Endpoint type
      EPTYP          : FS_HCCHAR_EPTYP_Field := 16#0#;
      --  Multicount
      MCNT           : FS_HCCHAR_MCNT_Field := 16#0#;
      --  Device address
      DAD            : FS_HCCHAR_DAD_Field := 16#0#;
      --  Odd frame
      ODDFRM         : Boolean := False;
      --  Channel disable
      CHDIS          : Boolean := False;
      --  Channel enable
      CHENA          : Boolean := False;
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

   --  OTG_FS host channel-0 interrupt register (OTG_FS_HCINT0)
   type FS_HCINT_Register is record
      --  Transfer completed
      XFRC           : Boolean := False;
      --  Channel halted
      CHH            : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  STALL response received interrupt
      STALL          : Boolean := False;
      --  NAK response received interrupt
      NAK            : Boolean := False;
      --  ACK response received/transmitted interrupt
      ACK            : Boolean := False;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  Transaction error
      TXERR          : Boolean := False;
      --  Babble error
      BBERR          : Boolean := False;
      --  Frame overrun
      FRMOR          : Boolean := False;
      --  Data toggle error
      DTERR          : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
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

   --  OTG_FS host channel-0 mask register (OTG_FS_HCINTMSK0)
   type FS_HCINTMSK_Register is record
      --  Transfer completed mask
      XFRCM          : Boolean := False;
      --  Channel halted mask
      CHHM           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  STALL response received interrupt mask
      STALLM         : Boolean := False;
      --  NAK response received interrupt mask
      NAKM           : Boolean := False;
      --  ACK response received/transmitted interrupt mask
      ACKM           : Boolean := False;
      --  response received interrupt mask
      NYET           : Boolean := False;
      --  Transaction error mask
      TXERRM         : Boolean := False;
      --  Babble error mask
      BBERRM         : Boolean := False;
      --  Frame overrun mask
      FRMORM         : Boolean := False;
      --  Data toggle error mask
      DTERRM         : Boolean := False;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
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

   subtype FS_HCTSIZ_XFRSIZ_Field is HAL.UInt19;
   subtype FS_HCTSIZ_PKTCNT_Field is HAL.UInt10;
   subtype FS_HCTSIZ_DPID_Field is HAL.UInt2;

   --  OTG_FS host channel-0 transfer size register
   type FS_HCTSIZ_Register is record
      --  Transfer size
      XFRSIZ         : FS_HCTSIZ_XFRSIZ_Field := 16#0#;
      --  Packet count
      PKTCNT         : FS_HCTSIZ_PKTCNT_Field := 16#0#;
      --  Data PID
      DPID           : FS_HCTSIZ_DPID_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FS_HCTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      DPID           at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  OTG_FS power and clock gating control register (OTG_FS_PCGCCTL)
   type FS_PCGCCTL_Register is record
      --  Stop PHY clock
      STPPCLK       : Boolean := False;
      --  Gate HCLK
      GATEHCLK      : Boolean := False;
      --  unspecified
      Reserved_2_3  : HAL.UInt2 := 16#0#;
      --  PHY Suspended
      PHYSUSP       : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
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

   --  USB on the go full speed
   type OTG_FS_DEVICE_Peripheral is record
      --  OTG_FS device configuration register (OTG_FS_DCFG)
      FS_DCFG     : aliased FS_DCFG_Register;
      --  OTG_FS device control register (OTG_FS_DCTL)
      FS_DCTL     : aliased FS_DCTL_Register;
      --  OTG_FS device status register (OTG_FS_DSTS)
      FS_DSTS     : aliased FS_DSTS_Register;
      --  OTG_FS device IN endpoint common interrupt mask register
      --  (OTG_FS_DIEPMSK)
      FS_DIEPMSK  : aliased FS_DIEPMSK_Register;
      --  OTG_FS device OUT endpoint common interrupt mask register
      --  (OTG_FS_DOEPMSK)
      FS_DOEPMSK  : aliased FS_DOEPMSK_Register;
      --  OTG_FS device all endpoints interrupt register (OTG_FS_DAINT)
      FS_DAINT    : aliased FS_DAINT_Register;
      --  OTG_FS all endpoints interrupt mask register (OTG_FS_DAINTMSK)
      FS_DAINTMSK : aliased FS_DAINTMSK_Register;
      --  OTG_FS device VBUS discharge time register
      DVBUSDIS    : aliased DVBUSDIS_Register;
      --  OTG_FS device VBUS pulsing time register
      DVBUSPULSE  : aliased DVBUSPULSE_Register;
      --  OTG_FS device IN endpoint FIFO empty interrupt mask register
      DIEPEMPMSK  : aliased DIEPEMPMSK_Register;
      --  OTG_FS device control IN endpoint 0 control register
      --  (OTG_FS_DIEPCTL0)
      FS_DIEPCTL0 : aliased FS_DIEPCTL0_Register;
      --  device endpoint-x interrupt register
      DIEPINT0    : aliased DIEPINT_Register;
      --  device endpoint-0 transfer size register
      DIEPTSIZ0   : aliased DIEPTSIZ0_Register;
      --  OTG_FS device IN endpoint transmit FIFO status register
      DTXFSTS0    : aliased DTXFSTS_Register;
      --  OTG device endpoint-1 control register
      DIEPCTL1    : aliased DIEPCTL1_Register;
      --  device endpoint-1 interrupt register
      DIEPINT1    : aliased DIEPINT_Register;
      --  device endpoint-1 transfer size register
      DIEPTSIZ1   : aliased DIEPTSIZ_Register;
      --  OTG_FS device IN endpoint transmit FIFO status register
      DTXFSTS1    : aliased DTXFSTS_Register;
      --  OTG device endpoint-2 control register
      DIEPCTL2    : aliased DIEPCTL_Register;
      --  device endpoint-2 interrupt register
      DIEPINT2    : aliased DIEPINT_Register;
      --  device endpoint-2 transfer size register
      DIEPTSIZ2   : aliased DIEPTSIZ_Register;
      --  OTG_FS device IN endpoint transmit FIFO status register
      DTXFSTS2    : aliased DTXFSTS_Register;
      --  OTG device endpoint-3 control register
      DIEPCTL3    : aliased DIEPCTL_Register;
      --  device endpoint-3 interrupt register
      DIEPINT3    : aliased DIEPINT_Register;
      --  device endpoint-3 transfer size register
      DIEPTSIZ3   : aliased DIEPTSIZ_Register;
      --  OTG_FS device IN endpoint transmit FIFO status register
      DTXFSTS3    : aliased DTXFSTS_Register;
      --  device endpoint-0 control register
      DOEPCTL0    : aliased DOEPCTL0_Register;
      --  device endpoint-0 interrupt register
      DOEPINT0    : aliased DOEPINT_Register;
      --  device OUT endpoint-0 transfer size register
      DOEPTSIZ0   : aliased DOEPTSIZ0_Register;
      --  device endpoint-1 control register
      DOEPCTL1    : aliased DOEPCTL_Register;
      --  device endpoint-1 interrupt register
      DOEPINT1    : aliased DOEPINT_Register;
      --  device OUT endpoint-1 transfer size register
      DOEPTSIZ1   : aliased DOEPTSIZ_Register;
      --  device endpoint-2 control register
      DOEPCTL2    : aliased DOEPCTL_Register;
      --  device endpoint-2 interrupt register
      DOEPINT2    : aliased DOEPINT_Register;
      --  device OUT endpoint-2 transfer size register
      DOEPTSIZ2   : aliased DOEPTSIZ_Register;
      --  device endpoint-3 control register
      DOEPCTL3    : aliased DOEPCTL_Register;
      --  device endpoint-3 interrupt register
      DOEPINT3    : aliased DOEPINT_Register;
      --  device OUT endpoint-3 transfer size register
      DOEPTSIZ3   : aliased DOEPTSIZ_Register;
   end record
     with Volatile;

   for OTG_FS_DEVICE_Peripheral use record
      FS_DCFG     at 16#0# range 0 .. 31;
      FS_DCTL     at 16#4# range 0 .. 31;
      FS_DSTS     at 16#8# range 0 .. 31;
      FS_DIEPMSK  at 16#10# range 0 .. 31;
      FS_DOEPMSK  at 16#14# range 0 .. 31;
      FS_DAINT    at 16#18# range 0 .. 31;
      FS_DAINTMSK at 16#1C# range 0 .. 31;
      DVBUSDIS    at 16#28# range 0 .. 31;
      DVBUSPULSE  at 16#2C# range 0 .. 31;
      DIEPEMPMSK  at 16#34# range 0 .. 31;
      FS_DIEPCTL0 at 16#100# range 0 .. 31;
      DIEPINT0    at 16#108# range 0 .. 31;
      DIEPTSIZ0   at 16#110# range 0 .. 31;
      DTXFSTS0    at 16#118# range 0 .. 31;
      DIEPCTL1    at 16#120# range 0 .. 31;
      DIEPINT1    at 16#128# range 0 .. 31;
      DIEPTSIZ1   at 16#130# range 0 .. 31;
      DTXFSTS1    at 16#138# range 0 .. 31;
      DIEPCTL2    at 16#140# range 0 .. 31;
      DIEPINT2    at 16#148# range 0 .. 31;
      DIEPTSIZ2   at 16#150# range 0 .. 31;
      DTXFSTS2    at 16#158# range 0 .. 31;
      DIEPCTL3    at 16#160# range 0 .. 31;
      DIEPINT3    at 16#168# range 0 .. 31;
      DIEPTSIZ3   at 16#170# range 0 .. 31;
      DTXFSTS3    at 16#178# range 0 .. 31;
      DOEPCTL0    at 16#300# range 0 .. 31;
      DOEPINT0    at 16#308# range 0 .. 31;
      DOEPTSIZ0   at 16#310# range 0 .. 31;
      DOEPCTL1    at 16#320# range 0 .. 31;
      DOEPINT1    at 16#328# range 0 .. 31;
      DOEPTSIZ1   at 16#330# range 0 .. 31;
      DOEPCTL2    at 16#340# range 0 .. 31;
      DOEPINT2    at 16#348# range 0 .. 31;
      DOEPTSIZ2   at 16#350# range 0 .. 31;
      DOEPCTL3    at 16#360# range 0 .. 31;
      DOEPINT3    at 16#368# range 0 .. 31;
      DOEPTSIZ3   at 16#370# range 0 .. 31;
   end record;

   --  USB on the go full speed
   OTG_FS_DEVICE_Periph : aliased OTG_FS_DEVICE_Peripheral
     with Import, Address => System'To_Address (16#50000800#);

   type OTG_FS_GLOBAL_Disc is
     (
      Device,
      Host);

   --  USB on the go full speed
   type OTG_FS_GLOBAL_Peripheral
     (Discriminent : OTG_FS_GLOBAL_Disc := Device)
   is record
      --  OTG_FS control and status register (OTG_FS_GOTGCTL)
      FS_GOTGCTL          : aliased FS_GOTGCTL_Register;
      --  OTG_FS interrupt register (OTG_FS_GOTGINT)
      FS_GOTGINT          : aliased FS_GOTGINT_Register;
      --  OTG_FS AHB configuration register (OTG_FS_GAHBCFG)
      FS_GAHBCFG          : aliased FS_GAHBCFG_Register;
      --  OTG_FS USB configuration register (OTG_FS_GUSBCFG)
      FS_GUSBCFG          : aliased FS_GUSBCFG_Register;
      --  OTG_FS reset register (OTG_FS_GRSTCTL)
      FS_GRSTCTL          : aliased FS_GRSTCTL_Register;
      --  OTG_FS core interrupt register (OTG_FS_GINTSTS)
      FS_GINTSTS          : aliased FS_GINTSTS_Register;
      --  OTG_FS interrupt mask register (OTG_FS_GINTMSK)
      FS_GINTMSK          : aliased FS_GINTMSK_Register;
      --  OTG_FS Receive FIFO size register (OTG_FS_GRXFSIZ)
      FS_GRXFSIZ          : aliased FS_GRXFSIZ_Register;
      --  OTG_FS non-periodic transmit FIFO/queue status register
      --  (OTG_FS_GNPTXSTS)
      FS_GNPTXSTS         : aliased FS_GNPTXSTS_Register;
      --  OTG_FS general core configuration register (OTG_FS_GCCFG)
      FS_GCCFG            : aliased FS_GCCFG_Register;
      --  core ID register
      FS_CID              : aliased HAL.UInt32;
      --  OTG_FS Host periodic transmit FIFO size register (OTG_FS_HPTXFSIZ)
      FS_HPTXFSIZ         : aliased FS_HPTXFSIZ_Register;
      --  OTG_FS device IN endpoint transmit FIFO size register
      --  (OTG_FS_DIEPTXF1)
      FS_DIEPTXF1         : aliased FS_DIEPTXF_Register;
      --  OTG_FS device IN endpoint transmit FIFO size register
      --  (OTG_FS_DIEPTXF2)
      FS_DIEPTXF2         : aliased FS_DIEPTXF_Register;
      --  OTG_FS device IN endpoint transmit FIFO size register
      --  (OTG_FS_DIEPTXF3)
      FS_DIEPTXF3         : aliased FS_DIEPTXF_Register;
      --  OTG_FS device IN endpoint transmit FIFO size register
      --  (OTG_FS_DIEPTXF4)
      FS_DIEPTXF4         : aliased HAL.UInt32;
      --  OTG_FS device IN endpoint transmit FIFO size register
      --  (OTG_FS_DIEPTXF5)
      FS_DIEPTXF5         : aliased HAL.UInt32;
      case Discriminent is
         when Device =>
            --  OTG_FS Receive status debug read(Device mode)
            FS_GRXSTSR_Device : aliased FS_GRXSTSR_Device_Register;
            --  OTG_FS non-periodic transmit FIFO size register (Device mode)
            FS_GNPTXFSIZ_Device : aliased FS_GNPTXFSIZ_Device_Register;
         when Host =>
            --  OTG_FS Receive status debug read(Hostmode)
            FS_GRXSTSR_Host : aliased FS_GRXSTSR_Host_Register;
            --  OTG_FS non-periodic transmit FIFO size register (Host mode)
            FS_GNPTXFSIZ_Host : aliased FS_GNPTXFSIZ_Host_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for OTG_FS_GLOBAL_Peripheral use record
      FS_GOTGCTL          at 16#0# range 0 .. 31;
      FS_GOTGINT          at 16#4# range 0 .. 31;
      FS_GAHBCFG          at 16#8# range 0 .. 31;
      FS_GUSBCFG          at 16#C# range 0 .. 31;
      FS_GRSTCTL          at 16#10# range 0 .. 31;
      FS_GINTSTS          at 16#14# range 0 .. 31;
      FS_GINTMSK          at 16#18# range 0 .. 31;
      FS_GRXFSIZ          at 16#24# range 0 .. 31;
      FS_GNPTXSTS         at 16#2C# range 0 .. 31;
      FS_GCCFG            at 16#38# range 0 .. 31;
      FS_CID              at 16#3C# range 0 .. 31;
      FS_HPTXFSIZ         at 16#100# range 0 .. 31;
      FS_DIEPTXF1         at 16#104# range 0 .. 31;
      FS_DIEPTXF2         at 16#108# range 0 .. 31;
      FS_DIEPTXF3         at 16#10C# range 0 .. 31;
      FS_DIEPTXF4         at 16#110# range 0 .. 31;
      FS_DIEPTXF5         at 16#114# range 0 .. 31;
      FS_GRXSTSR_Device   at 16#1C# range 0 .. 31;
      FS_GNPTXFSIZ_Device at 16#28# range 0 .. 31;
      FS_GRXSTSR_Host     at 16#1C# range 0 .. 31;
      FS_GNPTXFSIZ_Host   at 16#28# range 0 .. 31;
   end record;

   --  USB on the go full speed
   OTG_FS_GLOBAL_Periph : aliased OTG_FS_GLOBAL_Peripheral
     with Import, Address => System'To_Address (16#50000000#);

   --  USB on the go full speed
   type OTG_FS_HOST_Peripheral is record
      --  OTG_FS host configuration register (OTG_FS_HCFG)
      FS_HCFG      : aliased FS_HCFG_Register;
      --  OTG_FS Host frame interval register
      HFIR         : aliased HFIR_Register;
      --  OTG_FS host frame number/frame time remaining register (OTG_FS_HFNUM)
      FS_HFNUM     : aliased FS_HFNUM_Register;
      --  OTG_FS_Host periodic transmit FIFO/queue status register
      --  (OTG_FS_HPTXSTS)
      FS_HPTXSTS   : aliased FS_HPTXSTS_Register;
      --  OTG_FS Host all channels interrupt register
      HAINT        : aliased HAINT_Register;
      --  OTG_FS host all channels interrupt mask register
      HAINTMSK     : aliased HAINTMSK_Register;
      --  OTG_FS host port control and status register (OTG_FS_HPRT)
      FS_HPRT      : aliased FS_HPRT_Register;
      --  OTG_FS host channel-0 characteristics register (OTG_FS_HCCHAR0)
      FS_HCCHAR0   : aliased FS_HCCHAR_Register;
      --  OTG_FS host channel-0 interrupt register (OTG_FS_HCINT0)
      FS_HCINT0    : aliased FS_HCINT_Register;
      --  OTG_FS host channel-0 mask register (OTG_FS_HCINTMSK0)
      FS_HCINTMSK0 : aliased FS_HCINTMSK_Register;
      --  OTG_FS host channel-0 transfer size register
      FS_HCTSIZ0   : aliased FS_HCTSIZ_Register;
      --  OTG_FS host channel-1 characteristics register (OTG_FS_HCCHAR1)
      FS_HCCHAR1   : aliased FS_HCCHAR_Register;
      --  OTG_FS host channel-1 interrupt register (OTG_FS_HCINT1)
      FS_HCINT1    : aliased FS_HCINT_Register;
      --  OTG_FS host channel-1 mask register (OTG_FS_HCINTMSK1)
      FS_HCINTMSK1 : aliased FS_HCINTMSK_Register;
      --  OTG_FS host channel-1 transfer size register
      FS_HCTSIZ1   : aliased FS_HCTSIZ_Register;
      --  OTG_FS host channel-2 characteristics register (OTG_FS_HCCHAR2)
      FS_HCCHAR2   : aliased FS_HCCHAR_Register;
      --  OTG_FS host channel-2 interrupt register (OTG_FS_HCINT2)
      FS_HCINT2    : aliased FS_HCINT_Register;
      --  OTG_FS host channel-2 mask register (OTG_FS_HCINTMSK2)
      FS_HCINTMSK2 : aliased FS_HCINTMSK_Register;
      --  OTG_FS host channel-2 transfer size register
      FS_HCTSIZ2   : aliased FS_HCTSIZ_Register;
      --  OTG_FS host channel-3 characteristics register (OTG_FS_HCCHAR3)
      FS_HCCHAR3   : aliased FS_HCCHAR_Register;
      --  OTG_FS host channel-3 interrupt register (OTG_FS_HCINT3)
      FS_HCINT3    : aliased FS_HCINT_Register;
      --  OTG_FS host channel-3 mask register (OTG_FS_HCINTMSK3)
      FS_HCINTMSK3 : aliased FS_HCINTMSK_Register;
      --  OTG_FS host channel-3 transfer size register
      FS_HCTSIZ3   : aliased FS_HCTSIZ_Register;
      --  OTG_FS host channel-4 characteristics register (OTG_FS_HCCHAR4)
      FS_HCCHAR4   : aliased FS_HCCHAR_Register;
      --  OTG_FS host channel-4 interrupt register (OTG_FS_HCINT4)
      FS_HCINT4    : aliased FS_HCINT_Register;
      --  OTG_FS host channel-4 mask register (OTG_FS_HCINTMSK4)
      FS_HCINTMSK4 : aliased FS_HCINTMSK_Register;
      --  OTG_FS host channel-x transfer size register
      FS_HCTSIZ4   : aliased FS_HCTSIZ_Register;
      --  OTG_FS host channel-5 characteristics register (OTG_FS_HCCHAR5)
      FS_HCCHAR5   : aliased FS_HCCHAR_Register;
      --  OTG_FS host channel-5 interrupt register (OTG_FS_HCINT5)
      FS_HCINT5    : aliased FS_HCINT_Register;
      --  OTG_FS host channel-5 mask register (OTG_FS_HCINTMSK5)
      FS_HCINTMSK5 : aliased FS_HCINTMSK_Register;
      --  OTG_FS host channel-5 transfer size register
      FS_HCTSIZ5   : aliased FS_HCTSIZ_Register;
      --  OTG_FS host channel-6 characteristics register (OTG_FS_HCCHAR6)
      FS_HCCHAR6   : aliased FS_HCCHAR_Register;
      --  OTG_FS host channel-6 interrupt register (OTG_FS_HCINT6)
      FS_HCINT6    : aliased FS_HCINT_Register;
      --  OTG_FS host channel-6 mask register (OTG_FS_HCINTMSK6)
      FS_HCINTMSK6 : aliased FS_HCINTMSK_Register;
      --  OTG_FS host channel-6 transfer size register
      FS_HCTSIZ6   : aliased FS_HCTSIZ_Register;
      --  OTG_FS host channel-7 characteristics register (OTG_FS_HCCHAR7)
      FS_HCCHAR7   : aliased FS_HCCHAR_Register;
      --  OTG_FS host channel-7 interrupt register (OTG_FS_HCINT7)
      FS_HCINT7    : aliased FS_HCINT_Register;
      --  OTG_FS host channel-7 mask register (OTG_FS_HCINTMSK7)
      FS_HCINTMSK7 : aliased FS_HCINTMSK_Register;
      --  OTG_FS host channel-7 transfer size register
      FS_HCTSIZ7   : aliased FS_HCTSIZ_Register;
   end record
     with Volatile;

   for OTG_FS_HOST_Peripheral use record
      FS_HCFG      at 16#0# range 0 .. 31;
      HFIR         at 16#4# range 0 .. 31;
      FS_HFNUM     at 16#8# range 0 .. 31;
      FS_HPTXSTS   at 16#10# range 0 .. 31;
      HAINT        at 16#14# range 0 .. 31;
      HAINTMSK     at 16#18# range 0 .. 31;
      FS_HPRT      at 16#40# range 0 .. 31;
      FS_HCCHAR0   at 16#100# range 0 .. 31;
      FS_HCINT0    at 16#108# range 0 .. 31;
      FS_HCINTMSK0 at 16#10C# range 0 .. 31;
      FS_HCTSIZ0   at 16#110# range 0 .. 31;
      FS_HCCHAR1   at 16#120# range 0 .. 31;
      FS_HCINT1    at 16#128# range 0 .. 31;
      FS_HCINTMSK1 at 16#12C# range 0 .. 31;
      FS_HCTSIZ1   at 16#130# range 0 .. 31;
      FS_HCCHAR2   at 16#140# range 0 .. 31;
      FS_HCINT2    at 16#148# range 0 .. 31;
      FS_HCINTMSK2 at 16#14C# range 0 .. 31;
      FS_HCTSIZ2   at 16#150# range 0 .. 31;
      FS_HCCHAR3   at 16#160# range 0 .. 31;
      FS_HCINT3    at 16#168# range 0 .. 31;
      FS_HCINTMSK3 at 16#16C# range 0 .. 31;
      FS_HCTSIZ3   at 16#170# range 0 .. 31;
      FS_HCCHAR4   at 16#180# range 0 .. 31;
      FS_HCINT4    at 16#188# range 0 .. 31;
      FS_HCINTMSK4 at 16#18C# range 0 .. 31;
      FS_HCTSIZ4   at 16#190# range 0 .. 31;
      FS_HCCHAR5   at 16#1A0# range 0 .. 31;
      FS_HCINT5    at 16#1A8# range 0 .. 31;
      FS_HCINTMSK5 at 16#1AC# range 0 .. 31;
      FS_HCTSIZ5   at 16#1B0# range 0 .. 31;
      FS_HCCHAR6   at 16#1C0# range 0 .. 31;
      FS_HCINT6    at 16#1C8# range 0 .. 31;
      FS_HCINTMSK6 at 16#1CC# range 0 .. 31;
      FS_HCTSIZ6   at 16#1D0# range 0 .. 31;
      FS_HCCHAR7   at 16#1E0# range 0 .. 31;
      FS_HCINT7    at 16#1E8# range 0 .. 31;
      FS_HCINTMSK7 at 16#1EC# range 0 .. 31;
      FS_HCTSIZ7   at 16#1F0# range 0 .. 31;
   end record;

   --  USB on the go full speed
   OTG_FS_HOST_Periph : aliased OTG_FS_HOST_Peripheral
     with Import, Address => System'To_Address (16#50000400#);

   --  USB on the go full speed
   type OTG_FS_PWRCLK_Peripheral is record
      --  OTG_FS power and clock gating control register (OTG_FS_PCGCCTL)
      FS_PCGCCTL : aliased FS_PCGCCTL_Register;
   end record
     with Volatile;

   for OTG_FS_PWRCLK_Peripheral use record
      FS_PCGCCTL at 0 range 0 .. 31;
   end record;

   --  USB on the go full speed
   OTG_FS_PWRCLK_Periph : aliased OTG_FS_PWRCLK_Peripheral
     with Import, Address => System'To_Address (16#50000E00#);

end STM32_SVD.USB_OTG_FS;
