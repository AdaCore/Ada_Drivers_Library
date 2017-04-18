--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.USB_OTG_HS is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype OTG_HS_DCFG_DSPD_Field is HAL.UInt2;
   subtype OTG_HS_DCFG_DAD_Field is HAL.UInt7;
   subtype OTG_HS_DCFG_PFIVL_Field is HAL.UInt2;
   subtype OTG_HS_DCFG_PERSCHIVL_Field is HAL.UInt2;

   --  OTG_HS device configuration register
   type OTG_HS_DCFG_Register is record
      --  Device speed
      DSPD           : OTG_HS_DCFG_DSPD_Field := 16#0#;
      --  Nonzero-length status OUT handshake
      NZLSOHSK       : Boolean := False;
      --  unspecified
      Reserved_3_3   : HAL.Bit := 16#0#;
      --  Device address
      DAD            : OTG_HS_DCFG_DAD_Field := 16#0#;
      --  Periodic (micro)frame interval
      PFIVL          : OTG_HS_DCFG_PFIVL_Field := 16#0#;
      --  unspecified
      Reserved_13_23 : HAL.UInt11 := 16#100#;
      --  Periodic scheduling interval
      PERSCHIVL      : OTG_HS_DCFG_PERSCHIVL_Field := 16#2#;
      --  unspecified
      Reserved_26_31 : HAL.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DCFG_Register use record
      DSPD           at 0 range 0 .. 1;
      NZLSOHSK       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      DAD            at 0 range 4 .. 10;
      PFIVL          at 0 range 11 .. 12;
      Reserved_13_23 at 0 range 13 .. 23;
      PERSCHIVL      at 0 range 24 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype OTG_HS_DCTL_TCTL_Field is HAL.UInt3;

   --  OTG_HS device control register
   type OTG_HS_DCTL_Register is record
      --  Remote wakeup signaling
      RWUSIG         : Boolean := False;
      --  Soft disconnect
      SDIS           : Boolean := False;
      --  Read-only. Global IN NAK status
      GINSTS         : Boolean := False;
      --  Read-only. Global OUT NAK status
      GONSTS         : Boolean := False;
      --  Test control
      TCTL           : OTG_HS_DCTL_TCTL_Field := 16#0#;
      --  Write-only. Set global IN NAK
      SGINAK         : Boolean := False;
      --  Write-only. Clear global IN NAK
      CGINAK         : Boolean := False;
      --  Write-only. Set global OUT NAK
      SGONAK         : Boolean := False;
      --  Write-only. Clear global OUT NAK
      CGONAK         : Boolean := False;
      --  Power-on programming done
      POPRGDNE       : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DCTL_Register use record
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

   subtype OTG_HS_DSTS_ENUMSPD_Field is HAL.UInt2;
   subtype OTG_HS_DSTS_FNSOF_Field is HAL.UInt14;

   --  OTG_HS device status register
   type OTG_HS_DSTS_Register is record
      --  Read-only. Suspend status
      SUSPSTS        : Boolean;
      --  Read-only. Enumerated speed
      ENUMSPD        : OTG_HS_DSTS_ENUMSPD_Field;
      --  Read-only. Erratic error
      EERR           : Boolean;
      --  unspecified
      Reserved_4_7   : HAL.UInt4;
      --  Read-only. Frame number of the received SOF
      FNSOF          : OTG_HS_DSTS_FNSOF_Field;
      --  unspecified
      Reserved_22_31 : HAL.UInt10;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DSTS_Register use record
      SUSPSTS        at 0 range 0 .. 0;
      ENUMSPD        at 0 range 1 .. 2;
      EERR           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      FNSOF          at 0 range 8 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  OTG_HS device IN endpoint common interrupt mask register
   type OTG_HS_DIEPMSK_Register is record
      --  Transfer completed interrupt mask
      XFRCM          : Boolean := False;
      --  Endpoint disabled interrupt mask
      EPDM           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Timeout condition mask (nonisochronous endpoints)
      TOM            : Boolean := False;
      --  IN token received when TxFIFO empty mask
      ITTXFEMSK      : Boolean := False;
      --  IN token received with EP mismatch mask
      INEPNMM        : Boolean := False;
      --  IN endpoint NAK effective mask
      INEPNEM        : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  FIFO underrun mask
      TXFURM         : Boolean := False;
      --  BNA interrupt mask
      BIM            : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DIEPMSK_Register use record
      XFRCM          at 0 range 0 .. 0;
      EPDM           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      TOM            at 0 range 3 .. 3;
      ITTXFEMSK      at 0 range 4 .. 4;
      INEPNMM        at 0 range 5 .. 5;
      INEPNEM        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TXFURM         at 0 range 8 .. 8;
      BIM            at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  OTG_HS device OUT endpoint common interrupt mask register
   type OTG_HS_DOEPMSK_Register is record
      --  Transfer completed interrupt mask
      XFRCM          : Boolean := False;
      --  Endpoint disabled interrupt mask
      EPDM           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  SETUP phase done mask
      STUPM          : Boolean := False;
      --  OUT token received when endpoint disabled mask
      OTEPDM         : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Back-to-back SETUP packets received mask
      B2BSTUP        : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  OUT packet error mask
      OPEM           : Boolean := False;
      --  BNA interrupt mask
      BOIM           : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DOEPMSK_Register use record
      XFRCM          at 0 range 0 .. 0;
      EPDM           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      STUPM          at 0 range 3 .. 3;
      OTEPDM         at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      B2BSTUP        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      OPEM           at 0 range 8 .. 8;
      BOIM           at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype OTG_HS_DAINT_IEPINT_Field is HAL.UInt16;
   subtype OTG_HS_DAINT_OEPINT_Field is HAL.UInt16;

   --  OTG_HS device all endpoints interrupt register
   type OTG_HS_DAINT_Register is record
      --  Read-only. IN endpoint interrupt bits
      IEPINT : OTG_HS_DAINT_IEPINT_Field;
      --  Read-only. OUT endpoint interrupt bits
      OEPINT : OTG_HS_DAINT_OEPINT_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DAINT_Register use record
      IEPINT at 0 range 0 .. 15;
      OEPINT at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_DAINTMSK_IEPM_Field is HAL.UInt16;
   subtype OTG_HS_DAINTMSK_OEPM_Field is HAL.UInt16;

   --  OTG_HS all endpoints interrupt mask register
   type OTG_HS_DAINTMSK_Register is record
      --  IN EP interrupt mask bits
      IEPM : OTG_HS_DAINTMSK_IEPM_Field := 16#0#;
      --  OUT EP interrupt mask bits
      OEPM : OTG_HS_DAINTMSK_OEPM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DAINTMSK_Register use record
      IEPM at 0 range 0 .. 15;
      OEPM at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_DVBUSDIS_VBUSDT_Field is HAL.UInt16;

   --  OTG_HS device VBUS discharge time register
   type OTG_HS_DVBUSDIS_Register is record
      --  Device VBUS discharge time
      VBUSDT         : OTG_HS_DVBUSDIS_VBUSDT_Field := 16#17D7#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DVBUSDIS_Register use record
      VBUSDT         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_DVBUSPULSE_DVBUSP_Field is HAL.UInt12;

   --  OTG_HS device VBUS pulsing time register
   type OTG_HS_DVBUSPULSE_Register is record
      --  Device VBUS pulsing time
      DVBUSP         : OTG_HS_DVBUSPULSE_DVBUSP_Field := 16#5B8#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DVBUSPULSE_Register use record
      DVBUSP         at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype OTG_HS_DTHRCTL_TXTHRLEN_Field is HAL.UInt9;
   subtype OTG_HS_DTHRCTL_RXTHRLEN_Field is HAL.UInt9;

   --  OTG_HS Device threshold control register
   type OTG_HS_DTHRCTL_Register is record
      --  Nonisochronous IN endpoints threshold enable
      NONISOTHREN    : Boolean := False;
      --  ISO IN endpoint threshold enable
      ISOTHREN       : Boolean := False;
      --  Transmit threshold length
      TXTHRLEN       : OTG_HS_DTHRCTL_TXTHRLEN_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Receive threshold enable
      RXTHREN        : Boolean := False;
      --  Receive threshold length
      RXTHRLEN       : OTG_HS_DTHRCTL_RXTHRLEN_Field := 16#0#;
      --  unspecified
      Reserved_26_26 : HAL.Bit := 16#0#;
      --  Arbiter parking enable
      ARPEN          : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DTHRCTL_Register use record
      NONISOTHREN    at 0 range 0 .. 0;
      ISOTHREN       at 0 range 1 .. 1;
      TXTHRLEN       at 0 range 2 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      RXTHREN        at 0 range 16 .. 16;
      RXTHRLEN       at 0 range 17 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      ARPEN          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype OTG_HS_DIEPEMPMSK_INEPTXFEM_Field is HAL.UInt16;

   --  OTG_HS device IN endpoint FIFO empty interrupt mask register
   type OTG_HS_DIEPEMPMSK_Register is record
      --  IN EP Tx FIFO empty interrupt mask bits
      INEPTXFEM      : OTG_HS_DIEPEMPMSK_INEPTXFEM_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DIEPEMPMSK_Register use record
      INEPTXFEM      at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  OTG_HS device each endpoint interrupt register
   type OTG_HS_DEACHINT_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  IN endpoint 1interrupt bit
      IEP1INT        : Boolean := False;
      --  unspecified
      Reserved_2_16  : HAL.UInt15 := 16#0#;
      --  OUT endpoint 1 interrupt bit
      OEP1INT        : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DEACHINT_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      IEP1INT        at 0 range 1 .. 1;
      Reserved_2_16  at 0 range 2 .. 16;
      OEP1INT        at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  OTG_HS device each endpoint interrupt register mask
   type OTG_HS_DEACHINTMSK_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  IN Endpoint 1 interrupt mask bit
      IEP1INTM       : Boolean := False;
      --  unspecified
      Reserved_2_16  : HAL.UInt15 := 16#0#;
      --  OUT Endpoint 1 interrupt mask bit
      OEP1INTM       : Boolean := False;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DEACHINTMSK_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      IEP1INTM       at 0 range 1 .. 1;
      Reserved_2_16  at 0 range 2 .. 16;
      OEP1INTM       at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  OTG_HS device each in endpoint-1 interrupt register
   type OTG_HS_DIEPEACHMSK1_Register is record
      --  Transfer completed interrupt mask
      XFRCM          : Boolean := False;
      --  Endpoint disabled interrupt mask
      EPDM           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Timeout condition mask (nonisochronous endpoints)
      TOM            : Boolean := False;
      --  IN token received when TxFIFO empty mask
      ITTXFEMSK      : Boolean := False;
      --  IN token received with EP mismatch mask
      INEPNMM        : Boolean := False;
      --  IN endpoint NAK effective mask
      INEPNEM        : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  FIFO underrun mask
      TXFURM         : Boolean := False;
      --  BNA interrupt mask
      BIM            : Boolean := False;
      --  unspecified
      Reserved_10_12 : HAL.UInt3 := 16#0#;
      --  NAK interrupt mask
      NAKM           : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DIEPEACHMSK1_Register use record
      XFRCM          at 0 range 0 .. 0;
      EPDM           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      TOM            at 0 range 3 .. 3;
      ITTXFEMSK      at 0 range 4 .. 4;
      INEPNMM        at 0 range 5 .. 5;
      INEPNEM        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TXFURM         at 0 range 8 .. 8;
      BIM            at 0 range 9 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      NAKM           at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  OTG_HS device each OUT endpoint-1 interrupt register
   type OTG_HS_DOEPEACHMSK1_Register is record
      --  Transfer completed interrupt mask
      XFRCM          : Boolean := False;
      --  Endpoint disabled interrupt mask
      EPDM           : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Timeout condition mask
      TOM            : Boolean := False;
      --  IN token received when TxFIFO empty mask
      ITTXFEMSK      : Boolean := False;
      --  IN token received with EP mismatch mask
      INEPNMM        : Boolean := False;
      --  IN endpoint NAK effective mask
      INEPNEM        : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  OUT packet error mask
      TXFURM         : Boolean := False;
      --  BNA interrupt mask
      BIM            : Boolean := False;
      --  unspecified
      Reserved_10_11 : HAL.UInt2 := 16#0#;
      --  Bubble error interrupt mask
      BERRM          : Boolean := False;
      --  NAK interrupt mask
      NAKM           : Boolean := False;
      --  NYET interrupt mask
      NYETM          : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DOEPEACHMSK1_Register use record
      XFRCM          at 0 range 0 .. 0;
      EPDM           at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      TOM            at 0 range 3 .. 3;
      ITTXFEMSK      at 0 range 4 .. 4;
      INEPNMM        at 0 range 5 .. 5;
      INEPNEM        at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TXFURM         at 0 range 8 .. 8;
      BIM            at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      BERRM          at 0 range 12 .. 12;
      NAKM           at 0 range 13 .. 13;
      NYETM          at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype OTG_HS_DIEPCTL_MPSIZ_Field is HAL.UInt11;
   subtype OTG_HS_DIEPCTL_EPTYP_Field is HAL.UInt2;
   subtype OTG_HS_DIEPCTL_TXFNUM_Field is HAL.UInt4;

   --  OTG device endpoint-0 control register
   type OTG_HS_DIEPCTL_Register is record
      --  Maximum packet size
      MPSIZ          : OTG_HS_DIEPCTL_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  USB active endpoint
      USBAEP         : Boolean := False;
      --  Read-only. Even/odd frame
      EONUM_DPID     : Boolean := False;
      --  Read-only. NAK status
      NAKSTS         : Boolean := False;
      --  Endpoint type
      EPTYP          : OTG_HS_DIEPCTL_EPTYP_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : HAL.Bit := 16#0#;
      --  STALL handshake
      Stall          : Boolean := False;
      --  TxFIFO number
      TXFNUM         : OTG_HS_DIEPCTL_TXFNUM_Field := 16#0#;
      --  Write-only. Clear NAK
      CNAK           : Boolean := False;
      --  Write-only. Set NAK
      SNAK           : Boolean := False;
      --  Write-only. Set DATA0 PID
      SD0PID_SEVNFRM : Boolean := False;
      --  Write-only. Set odd frame
      SODDFRM        : Boolean := False;
      --  Endpoint disable
      EPDIS          : Boolean := False;
      --  Endpoint enable
      EPENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DIEPCTL_Register use record
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

   --  OTG device endpoint-0 interrupt register
   type OTG_HS_DIEPINT_Register is record
      --  Transfer completed interrupt
      XFRC           : Boolean := False;
      --  Endpoint disabled interrupt
      EPDISD         : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  Timeout condition
      TOC            : Boolean := False;
      --  IN token received when TxFIFO is empty
      ITTXFE         : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  IN endpoint NAK effective
      INEPNE         : Boolean := False;
      --  Read-only. Transmit FIFO empty
      TXFE           : Boolean := True;
      --  Transmit Fifo Underrun
      TXFIFOUDRN     : Boolean := False;
      --  Buffer not available interrupt
      BNA            : Boolean := False;
      --  unspecified
      Reserved_10_10 : HAL.Bit := 16#0#;
      --  Packet dropped status
      PKTDRPSTS      : Boolean := False;
      --  Babble error interrupt
      BERR           : Boolean := False;
      --  NAK interrupt
      NAK            : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DIEPINT_Register use record
      XFRC           at 0 range 0 .. 0;
      EPDISD         at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      TOC            at 0 range 3 .. 3;
      ITTXFE         at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      INEPNE         at 0 range 6 .. 6;
      TXFE           at 0 range 7 .. 7;
      TXFIFOUDRN     at 0 range 8 .. 8;
      BNA            at 0 range 9 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      PKTDRPSTS      at 0 range 11 .. 11;
      BERR           at 0 range 12 .. 12;
      NAK            at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype OTG_HS_DIEPTSIZ0_XFRSIZ_Field is HAL.UInt7;
   subtype OTG_HS_DIEPTSIZ0_PKTCNT_Field is HAL.UInt2;

   --  OTG_HS device IN endpoint 0 transfer size register
   type OTG_HS_DIEPTSIZ0_Register is record
      --  Transfer size
      XFRSIZ         : OTG_HS_DIEPTSIZ0_XFRSIZ_Field := 16#0#;
      --  unspecified
      Reserved_7_18  : HAL.UInt12 := 16#0#;
      --  Packet count
      PKTCNT         : OTG_HS_DIEPTSIZ0_PKTCNT_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DIEPTSIZ0_Register use record
      XFRSIZ         at 0 range 0 .. 6;
      Reserved_7_18  at 0 range 7 .. 18;
      PKTCNT         at 0 range 19 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype OTG_HS_DTXFSTS_INEPTFSAV_Field is HAL.UInt16;

   --  OTG_HS device IN endpoint transmit FIFO status register
   type OTG_HS_DTXFSTS_Register is record
      --  Read-only. IN endpoint TxFIFO space avail
      INEPTFSAV      : OTG_HS_DTXFSTS_INEPTFSAV_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DTXFSTS_Register use record
      INEPTFSAV      at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_DIEPTSIZ_XFRSIZ_Field is HAL.UInt19;
   subtype OTG_HS_DIEPTSIZ_PKTCNT_Field is HAL.UInt10;
   subtype OTG_HS_DIEPTSIZ_MCNT_Field is HAL.UInt2;

   --  OTG_HS device endpoint transfer size register
   type OTG_HS_DIEPTSIZ_Register is record
      --  Transfer size
      XFRSIZ         : OTG_HS_DIEPTSIZ_XFRSIZ_Field := 16#0#;
      --  Packet count
      PKTCNT         : OTG_HS_DIEPTSIZ_PKTCNT_Field := 16#0#;
      --  Multi count
      MCNT           : OTG_HS_DIEPTSIZ_MCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DIEPTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      MCNT           at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype OTG_HS_DOEPCTL0_MPSIZ_Field is HAL.UInt2;
   subtype OTG_HS_DOEPCTL0_EPTYP_Field is HAL.UInt2;

   --  OTG_HS device control OUT endpoint 0 control register
   type OTG_HS_DOEPCTL0_Register is record
      --  Read-only. Maximum packet size
      MPSIZ          : OTG_HS_DOEPCTL0_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_2_14  : HAL.UInt13 := 16#0#;
      --  Read-only. USB active endpoint
      USBAEP         : Boolean := True;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Read-only. NAK status
      NAKSTS         : Boolean := False;
      --  Read-only. Endpoint type
      EPTYP          : OTG_HS_DOEPCTL0_EPTYP_Field := 16#0#;
      --  Snoop mode
      SNPM           : Boolean := False;
      --  STALL handshake
      Stall          : Boolean := False;
      --  unspecified
      Reserved_22_25 : HAL.UInt4 := 16#0#;
      --  Write-only. Clear NAK
      CNAK           : Boolean := False;
      --  Write-only. Set NAK
      SNAK           : Boolean := False;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Read-only. Endpoint disable
      EPDIS          : Boolean := False;
      --  Write-only. Endpoint enable
      EPENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DOEPCTL0_Register use record
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

   --  OTG_HS device endpoint-0 interrupt register
   type OTG_HS_DOEPINT_Register is record
      --  Transfer completed interrupt
      XFRC           : Boolean := False;
      --  Endpoint disabled interrupt
      EPDISD         : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  SETUP phase done
      STUP           : Boolean := False;
      --  OUT token received when endpoint disabled
      OTEPDIS        : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Back-to-back SETUP packets received
      B2BSTUP        : Boolean := False;
      --  unspecified
      Reserved_7_13  : HAL.UInt7 := 16#1#;
      --  NYET interrupt
      NYET           : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DOEPINT_Register use record
      XFRC           at 0 range 0 .. 0;
      EPDISD         at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      STUP           at 0 range 3 .. 3;
      OTEPDIS        at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      B2BSTUP        at 0 range 6 .. 6;
      Reserved_7_13  at 0 range 7 .. 13;
      NYET           at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype OTG_HS_DOEPTSIZ0_XFRSIZ_Field is HAL.UInt7;
   subtype OTG_HS_DOEPTSIZ0_STUPCNT_Field is HAL.UInt2;

   --  OTG_HS device endpoint-1 transfer size register
   type OTG_HS_DOEPTSIZ0_Register is record
      --  Transfer size
      XFRSIZ         : OTG_HS_DOEPTSIZ0_XFRSIZ_Field := 16#0#;
      --  unspecified
      Reserved_7_18  : HAL.UInt12 := 16#0#;
      --  Packet count
      PKTCNT         : Boolean := False;
      --  unspecified
      Reserved_20_28 : HAL.UInt9 := 16#0#;
      --  SETUP packet count
      STUPCNT        : OTG_HS_DOEPTSIZ0_STUPCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DOEPTSIZ0_Register use record
      XFRSIZ         at 0 range 0 .. 6;
      Reserved_7_18  at 0 range 7 .. 18;
      PKTCNT         at 0 range 19 .. 19;
      Reserved_20_28 at 0 range 20 .. 28;
      STUPCNT        at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype OTG_HS_DOEPCTL_MPSIZ_Field is HAL.UInt11;
   subtype OTG_HS_DOEPCTL_EPTYP_Field is HAL.UInt2;

   --  OTG device endpoint-1 control register
   type OTG_HS_DOEPCTL_Register is record
      --  Maximum packet size
      MPSIZ          : OTG_HS_DOEPCTL_MPSIZ_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  USB active endpoint
      USBAEP         : Boolean := False;
      --  Read-only. Even odd frame/Endpoint data PID
      EONUM_DPID     : Boolean := False;
      --  Read-only. NAK status
      NAKSTS         : Boolean := False;
      --  Endpoint type
      EPTYP          : OTG_HS_DOEPCTL_EPTYP_Field := 16#0#;
      --  Snoop mode
      SNPM           : Boolean := False;
      --  STALL handshake
      Stall          : Boolean := False;
      --  unspecified
      Reserved_22_25 : HAL.UInt4 := 16#0#;
      --  Write-only. Clear NAK
      CNAK           : Boolean := False;
      --  Write-only. Set NAK
      SNAK           : Boolean := False;
      --  Write-only. Set DATA0 PID/Set even frame
      SD0PID_SEVNFRM : Boolean := False;
      --  Write-only. Set odd frame
      SODDFRM        : Boolean := False;
      --  Endpoint disable
      EPDIS          : Boolean := False;
      --  Endpoint enable
      EPENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DOEPCTL_Register use record
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

   subtype OTG_HS_DOEPTSIZ_XFRSIZ_Field is HAL.UInt19;
   subtype OTG_HS_DOEPTSIZ_PKTCNT_Field is HAL.UInt10;
   subtype OTG_HS_DOEPTSIZ_RXDPID_STUPCNT_Field is HAL.UInt2;

   --  OTG_HS device endpoint-2 transfer size register
   type OTG_HS_DOEPTSIZ_Register is record
      --  Transfer size
      XFRSIZ         : OTG_HS_DOEPTSIZ_XFRSIZ_Field := 16#0#;
      --  Packet count
      PKTCNT         : OTG_HS_DOEPTSIZ_PKTCNT_Field := 16#0#;
      --  Received data PID/SETUP packet count
      RXDPID_STUPCNT : OTG_HS_DOEPTSIZ_RXDPID_STUPCNT_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DOEPTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      RXDPID_STUPCNT at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  OTG_HS control and status register
   type OTG_HS_GOTGCTL_Register is record
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

   for OTG_HS_GOTGCTL_Register use record
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

   --  OTG_HS interrupt register
   type OTG_HS_GOTGINT_Register is record
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

   for OTG_HS_GOTGINT_Register use record
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

   subtype OTG_HS_GAHBCFG_HBSTLEN_Field is HAL.UInt4;

   --  OTG_HS AHB configuration register
   type OTG_HS_GAHBCFG_Register is record
      --  Global interrupt mask
      GINT          : Boolean := False;
      --  Burst length/type
      HBSTLEN       : OTG_HS_GAHBCFG_HBSTLEN_Field := 16#0#;
      --  DMA enable
      DMAEN         : Boolean := False;
      --  unspecified
      Reserved_6_6  : HAL.Bit := 16#0#;
      --  TxFIFO empty level
      TXFELVL       : Boolean := False;
      --  Periodic TxFIFO empty level
      PTXFELVL      : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GAHBCFG_Register use record
      GINT          at 0 range 0 .. 0;
      HBSTLEN       at 0 range 1 .. 4;
      DMAEN         at 0 range 5 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      TXFELVL       at 0 range 7 .. 7;
      PTXFELVL      at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype OTG_HS_GUSBCFG_TOCAL_Field is HAL.UInt3;
   subtype OTG_HS_GUSBCFG_TRDT_Field is HAL.UInt4;

   --  OTG_HS USB configuration register
   type OTG_HS_GUSBCFG_Register is record
      --  FS timeout calibration
      TOCAL          : OTG_HS_GUSBCFG_TOCAL_Field := 16#0#;
      --  unspecified
      Reserved_3_5   : HAL.UInt3 := 16#0#;
      --  Write-only. USB 2.0 high-speed ULPI PHY or USB 1.1 full-speed serial
      --  transceiver select
      PHYSEL         : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  SRP-capable
      SRPCAP         : Boolean := False;
      --  HNP-capable
      HNPCAP         : Boolean := True;
      --  USB turnaround time
      TRDT           : OTG_HS_GUSBCFG_TRDT_Field := 16#2#;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  PHY Low-power clock select
      PHYLPCS        : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  ULPI FS/LS select
      ULPIFSLS       : Boolean := False;
      --  ULPI Auto-resume
      ULPIAR         : Boolean := False;
      --  ULPI Clock SuspendM
      ULPICSM        : Boolean := False;
      --  ULPI External VBUS Drive
      ULPIEVBUSD     : Boolean := False;
      --  ULPI external VBUS indicator
      ULPIEVBUSI     : Boolean := False;
      --  TermSel DLine pulsing selection
      TSDPS          : Boolean := False;
      --  Indicator complement
      PCCI           : Boolean := False;
      --  Indicator pass through
      PTCI           : Boolean := False;
      --  ULPI interface protect disable
      ULPIIPD        : Boolean := False;
      --  unspecified
      Reserved_26_28 : HAL.UInt3 := 16#0#;
      --  Forced host mode
      FHMOD          : Boolean := False;
      --  Forced peripheral mode
      FDMOD          : Boolean := False;
      --  Corrupt Tx packet
      CTXPKT         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GUSBCFG_Register use record
      TOCAL          at 0 range 0 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      PHYSEL         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SRPCAP         at 0 range 8 .. 8;
      HNPCAP         at 0 range 9 .. 9;
      TRDT           at 0 range 10 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      PHYLPCS        at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      ULPIFSLS       at 0 range 17 .. 17;
      ULPIAR         at 0 range 18 .. 18;
      ULPICSM        at 0 range 19 .. 19;
      ULPIEVBUSD     at 0 range 20 .. 20;
      ULPIEVBUSI     at 0 range 21 .. 21;
      TSDPS          at 0 range 22 .. 22;
      PCCI           at 0 range 23 .. 23;
      PTCI           at 0 range 24 .. 24;
      ULPIIPD        at 0 range 25 .. 25;
      Reserved_26_28 at 0 range 26 .. 28;
      FHMOD          at 0 range 29 .. 29;
      FDMOD          at 0 range 30 .. 30;
      CTXPKT         at 0 range 31 .. 31;
   end record;

   subtype OTG_HS_GRSTCTL_TXFNUM_Field is HAL.UInt5;

   --  OTG_HS reset register
   type OTG_HS_GRSTCTL_Register is record
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
      TXFNUM         : OTG_HS_GRSTCTL_TXFNUM_Field := 16#0#;
      --  unspecified
      Reserved_11_29 : HAL.UInt19 := 16#40000#;
      --  Read-only. DMA request signal
      DMAREQ         : Boolean := False;
      --  Read-only. AHB master idle
      AHBIDL         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GRSTCTL_Register use record
      CSRST          at 0 range 0 .. 0;
      HSRST          at 0 range 1 .. 1;
      FCRST          at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      RXFFLSH        at 0 range 4 .. 4;
      TXFFLSH        at 0 range 5 .. 5;
      TXFNUM         at 0 range 6 .. 10;
      Reserved_11_29 at 0 range 11 .. 29;
      DMAREQ         at 0 range 30 .. 30;
      AHBIDL         at 0 range 31 .. 31;
   end record;

   --  OTG_HS core interrupt register
   type OTG_HS_GINTSTS_Register is record
      --  Read-only. Current mode of operation
      CMOD              : Boolean := False;
      --  Mode mismatch interrupt
      MMIS              : Boolean := False;
      --  Read-only. OTG interrupt
      OTGINT            : Boolean := False;
      --  Start of frame
      SOF               : Boolean := False;
      --  Read-only. RxFIFO nonempty
      RXFLVL            : Boolean := False;
      --  Read-only. Nonperiodic TxFIFO empty
      NPTXFE            : Boolean := True;
      --  Read-only. Global IN nonperiodic NAK effective
      GINAKEFF          : Boolean := False;
      --  Read-only. Global OUT NAK effective
      BOUTNAKEFF        : Boolean := False;
      --  unspecified
      Reserved_8_9      : HAL.UInt2 := 16#0#;
      --  Early suspend
      ESUSP             : Boolean := False;
      --  USB suspend
      USBSUSP           : Boolean := False;
      --  USB reset
      USBRST            : Boolean := False;
      --  Enumeration done
      ENUMDNE           : Boolean := False;
      --  Isochronous OUT packet dropped interrupt
      ISOODRP           : Boolean := False;
      --  End of periodic frame interrupt
      EOPF              : Boolean := False;
      --  unspecified
      Reserved_16_17    : HAL.UInt2 := 16#0#;
      --  Read-only. IN endpoint interrupt
      IEPINT            : Boolean := False;
      --  Read-only. OUT endpoint interrupt
      OEPINT            : Boolean := False;
      --  Incomplete isochronous IN transfer
      IISOIXFR          : Boolean := False;
      --  Incomplete periodic transfer
      PXFR_INCOMPISOOUT : Boolean := False;
      --  Data fetch suspended
      DATAFSUSP         : Boolean := False;
      --  unspecified
      Reserved_23_23    : HAL.Bit := 16#0#;
      --  Read-only. Host port interrupt
      HPRTINT           : Boolean := False;
      --  Read-only. Host channels interrupt
      HCINT             : Boolean := False;
      --  Read-only. Periodic TxFIFO empty
      PTXFE             : Boolean := True;
      --  unspecified
      Reserved_27_27    : HAL.Bit := 16#0#;
      --  Connector ID status change
      CIDSCHG           : Boolean := False;
      --  Disconnect detected interrupt
      DISCINT           : Boolean := False;
      --  Session request/new session detected interrupt
      SRQINT            : Boolean := False;
      --  Resume/remote wakeup detected interrupt
      WKUINT            : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GINTSTS_Register use record
      CMOD              at 0 range 0 .. 0;
      MMIS              at 0 range 1 .. 1;
      OTGINT            at 0 range 2 .. 2;
      SOF               at 0 range 3 .. 3;
      RXFLVL            at 0 range 4 .. 4;
      NPTXFE            at 0 range 5 .. 5;
      GINAKEFF          at 0 range 6 .. 6;
      BOUTNAKEFF        at 0 range 7 .. 7;
      Reserved_8_9      at 0 range 8 .. 9;
      ESUSP             at 0 range 10 .. 10;
      USBSUSP           at 0 range 11 .. 11;
      USBRST            at 0 range 12 .. 12;
      ENUMDNE           at 0 range 13 .. 13;
      ISOODRP           at 0 range 14 .. 14;
      EOPF              at 0 range 15 .. 15;
      Reserved_16_17    at 0 range 16 .. 17;
      IEPINT            at 0 range 18 .. 18;
      OEPINT            at 0 range 19 .. 19;
      IISOIXFR          at 0 range 20 .. 20;
      PXFR_INCOMPISOOUT at 0 range 21 .. 21;
      DATAFSUSP         at 0 range 22 .. 22;
      Reserved_23_23    at 0 range 23 .. 23;
      HPRTINT           at 0 range 24 .. 24;
      HCINT             at 0 range 25 .. 25;
      PTXFE             at 0 range 26 .. 26;
      Reserved_27_27    at 0 range 27 .. 27;
      CIDSCHG           at 0 range 28 .. 28;
      DISCINT           at 0 range 29 .. 29;
      SRQINT            at 0 range 30 .. 30;
      WKUINT            at 0 range 31 .. 31;
   end record;

   --  OTG_HS interrupt mask register
   type OTG_HS_GINTMSK_Register is record
      --  unspecified
      Reserved_0_0    : HAL.Bit := 16#0#;
      --  Mode mismatch interrupt mask
      MMISM           : Boolean := False;
      --  OTG interrupt mask
      OTGINT          : Boolean := False;
      --  Start of frame mask
      SOFM            : Boolean := False;
      --  Receive FIFO nonempty mask
      RXFLVLM         : Boolean := False;
      --  Nonperiodic TxFIFO empty mask
      NPTXFEM         : Boolean := False;
      --  Global nonperiodic IN NAK effective mask
      GINAKEFFM       : Boolean := False;
      --  Global OUT NAK effective mask
      GONAKEFFM       : Boolean := False;
      --  unspecified
      Reserved_8_9    : HAL.UInt2 := 16#0#;
      --  Early suspend mask
      ESUSPM          : Boolean := False;
      --  USB suspend mask
      USBSUSPM        : Boolean := False;
      --  USB reset mask
      USBRST          : Boolean := False;
      --  Enumeration done mask
      ENUMDNEM        : Boolean := False;
      --  Isochronous OUT packet dropped interrupt mask
      ISOODRPM        : Boolean := False;
      --  End of periodic frame interrupt mask
      EOPFM           : Boolean := False;
      --  unspecified
      Reserved_16_16  : HAL.Bit := 16#0#;
      --  Endpoint mismatch interrupt mask
      EPMISM          : Boolean := False;
      --  IN endpoints interrupt mask
      IEPINT          : Boolean := False;
      --  OUT endpoints interrupt mask
      OEPINT          : Boolean := False;
      --  Incomplete isochronous IN transfer mask
      IISOIXFRM       : Boolean := False;
      --  Incomplete periodic transfer mask
      PXFRM_IISOOXFRM : Boolean := False;
      --  Data fetch suspended mask
      FSUSPM          : Boolean := False;
      --  unspecified
      Reserved_23_23  : HAL.Bit := 16#0#;
      --  Read-only. Host port interrupt mask
      PRTIM           : Boolean := False;
      --  Host channels interrupt mask
      HCIM            : Boolean := False;
      --  Periodic TxFIFO empty mask
      PTXFEM          : Boolean := False;
      --  unspecified
      Reserved_27_27  : HAL.Bit := 16#0#;
      --  Connector ID status change mask
      CIDSCHGM        : Boolean := False;
      --  Disconnect detected interrupt mask
      DISCINT         : Boolean := False;
      --  Session request/new session detected interrupt mask
      SRQIM           : Boolean := False;
      --  Resume/remote wakeup detected interrupt mask
      WUIM            : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GINTMSK_Register use record
      Reserved_0_0    at 0 range 0 .. 0;
      MMISM           at 0 range 1 .. 1;
      OTGINT          at 0 range 2 .. 2;
      SOFM            at 0 range 3 .. 3;
      RXFLVLM         at 0 range 4 .. 4;
      NPTXFEM         at 0 range 5 .. 5;
      GINAKEFFM       at 0 range 6 .. 6;
      GONAKEFFM       at 0 range 7 .. 7;
      Reserved_8_9    at 0 range 8 .. 9;
      ESUSPM          at 0 range 10 .. 10;
      USBSUSPM        at 0 range 11 .. 11;
      USBRST          at 0 range 12 .. 12;
      ENUMDNEM        at 0 range 13 .. 13;
      ISOODRPM        at 0 range 14 .. 14;
      EOPFM           at 0 range 15 .. 15;
      Reserved_16_16  at 0 range 16 .. 16;
      EPMISM          at 0 range 17 .. 17;
      IEPINT          at 0 range 18 .. 18;
      OEPINT          at 0 range 19 .. 19;
      IISOIXFRM       at 0 range 20 .. 20;
      PXFRM_IISOOXFRM at 0 range 21 .. 21;
      FSUSPM          at 0 range 22 .. 22;
      Reserved_23_23  at 0 range 23 .. 23;
      PRTIM           at 0 range 24 .. 24;
      HCIM            at 0 range 25 .. 25;
      PTXFEM          at 0 range 26 .. 26;
      Reserved_27_27  at 0 range 27 .. 27;
      CIDSCHGM        at 0 range 28 .. 28;
      DISCINT         at 0 range 29 .. 29;
      SRQIM           at 0 range 30 .. 30;
      WUIM            at 0 range 31 .. 31;
   end record;

   subtype OTG_HS_GRXSTSR_Host_CHNUM_Field is HAL.UInt4;
   subtype OTG_HS_GRXSTSR_Host_BCNT_Field is HAL.UInt11;
   subtype OTG_HS_GRXSTSR_Host_DPID_Field is HAL.UInt2;
   subtype OTG_HS_GRXSTSR_Host_PKTSTS_Field is HAL.UInt4;

   --  OTG_HS Receive status debug read register (host mode)
   type OTG_HS_GRXSTSR_Host_Register is record
      --  Read-only. Channel number
      CHNUM          : OTG_HS_GRXSTSR_Host_CHNUM_Field;
      --  Read-only. Byte count
      BCNT           : OTG_HS_GRXSTSR_Host_BCNT_Field;
      --  Read-only. Data PID
      DPID           : OTG_HS_GRXSTSR_Host_DPID_Field;
      --  Read-only. Packet status
      PKTSTS         : OTG_HS_GRXSTSR_Host_PKTSTS_Field;
      --  unspecified
      Reserved_21_31 : HAL.UInt11;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GRXSTSR_Host_Register use record
      CHNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype OTG_HS_GRXSTSR_Peripheral_EPNUM_Field is HAL.UInt4;
   subtype OTG_HS_GRXSTSR_Peripheral_BCNT_Field is HAL.UInt11;
   subtype OTG_HS_GRXSTSR_Peripheral_DPID_Field is HAL.UInt2;
   subtype OTG_HS_GRXSTSR_Peripheral_PKTSTS_Field is HAL.UInt4;
   subtype OTG_HS_GRXSTSR_Peripheral_FRMNUM_Field is HAL.UInt4;

   --  OTG_HS Receive status debug read register (peripheral mode mode)
   type OTG_HS_GRXSTSR_Peripheral_Register is record
      --  Read-only. Endpoint number
      EPNUM          : OTG_HS_GRXSTSR_Peripheral_EPNUM_Field;
      --  Read-only. Byte count
      BCNT           : OTG_HS_GRXSTSR_Peripheral_BCNT_Field;
      --  Read-only. Data PID
      DPID           : OTG_HS_GRXSTSR_Peripheral_DPID_Field;
      --  Read-only. Packet status
      PKTSTS         : OTG_HS_GRXSTSR_Peripheral_PKTSTS_Field;
      --  Read-only. Frame number
      FRMNUM         : OTG_HS_GRXSTSR_Peripheral_FRMNUM_Field;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GRXSTSR_Peripheral_Register use record
      EPNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      FRMNUM         at 0 range 21 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype OTG_HS_GRXSTSP_Host_CHNUM_Field is HAL.UInt4;
   subtype OTG_HS_GRXSTSP_Host_BCNT_Field is HAL.UInt11;
   subtype OTG_HS_GRXSTSP_Host_DPID_Field is HAL.UInt2;
   subtype OTG_HS_GRXSTSP_Host_PKTSTS_Field is HAL.UInt4;

   --  OTG_HS status read and pop register (host mode)
   type OTG_HS_GRXSTSP_Host_Register is record
      --  Read-only. Channel number
      CHNUM          : OTG_HS_GRXSTSP_Host_CHNUM_Field;
      --  Read-only. Byte count
      BCNT           : OTG_HS_GRXSTSP_Host_BCNT_Field;
      --  Read-only. Data PID
      DPID           : OTG_HS_GRXSTSP_Host_DPID_Field;
      --  Read-only. Packet status
      PKTSTS         : OTG_HS_GRXSTSP_Host_PKTSTS_Field;
      --  unspecified
      Reserved_21_31 : HAL.UInt11;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GRXSTSP_Host_Register use record
      CHNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype OTG_HS_GRXSTSP_Peripheral_EPNUM_Field is HAL.UInt4;
   subtype OTG_HS_GRXSTSP_Peripheral_BCNT_Field is HAL.UInt11;
   subtype OTG_HS_GRXSTSP_Peripheral_DPID_Field is HAL.UInt2;
   subtype OTG_HS_GRXSTSP_Peripheral_PKTSTS_Field is HAL.UInt4;
   subtype OTG_HS_GRXSTSP_Peripheral_FRMNUM_Field is HAL.UInt4;

   --  OTG_HS status read and pop register (peripheral mode)
   type OTG_HS_GRXSTSP_Peripheral_Register is record
      --  Read-only. Endpoint number
      EPNUM          : OTG_HS_GRXSTSP_Peripheral_EPNUM_Field;
      --  Read-only. Byte count
      BCNT           : OTG_HS_GRXSTSP_Peripheral_BCNT_Field;
      --  Read-only. Data PID
      DPID           : OTG_HS_GRXSTSP_Peripheral_DPID_Field;
      --  Read-only. Packet status
      PKTSTS         : OTG_HS_GRXSTSP_Peripheral_PKTSTS_Field;
      --  Read-only. Frame number
      FRMNUM         : OTG_HS_GRXSTSP_Peripheral_FRMNUM_Field;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GRXSTSP_Peripheral_Register use record
      EPNUM          at 0 range 0 .. 3;
      BCNT           at 0 range 4 .. 14;
      DPID           at 0 range 15 .. 16;
      PKTSTS         at 0 range 17 .. 20;
      FRMNUM         at 0 range 21 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype OTG_HS_GRXFSIZ_RXFD_Field is HAL.UInt16;

   --  OTG_HS Receive FIFO size register
   type OTG_HS_GRXFSIZ_Register is record
      --  RxFIFO depth
      RXFD           : OTG_HS_GRXFSIZ_RXFD_Field := 16#200#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GRXFSIZ_Register use record
      RXFD           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_GNPTXFSIZ_Host_NPTXFSA_Field is HAL.UInt16;
   subtype OTG_HS_GNPTXFSIZ_Host_NPTXFD_Field is HAL.UInt16;

   --  OTG_HS nonperiodic transmit FIFO size register (host mode)
   type OTG_HS_GNPTXFSIZ_Host_Register is record
      --  Nonperiodic transmit RAM start address
      NPTXFSA : OTG_HS_GNPTXFSIZ_Host_NPTXFSA_Field := 16#200#;
      --  Nonperiodic TxFIFO depth
      NPTXFD  : OTG_HS_GNPTXFSIZ_Host_NPTXFD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GNPTXFSIZ_Host_Register use record
      NPTXFSA at 0 range 0 .. 15;
      NPTXFD  at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_TX0FSIZ_Peripheral_TX0FSA_Field is HAL.UInt16;
   subtype OTG_HS_TX0FSIZ_Peripheral_TX0FD_Field is HAL.UInt16;

   --  Endpoint 0 transmit FIFO size (peripheral mode)
   type OTG_HS_TX0FSIZ_Peripheral_Register is record
      --  Endpoint 0 transmit RAM start address
      TX0FSA : OTG_HS_TX0FSIZ_Peripheral_TX0FSA_Field := 16#200#;
      --  Endpoint 0 TxFIFO depth
      TX0FD  : OTG_HS_TX0FSIZ_Peripheral_TX0FD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_TX0FSIZ_Peripheral_Register use record
      TX0FSA at 0 range 0 .. 15;
      TX0FD  at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_GNPTXSTS_NPTXFSAV_Field is HAL.UInt16;
   subtype OTG_HS_GNPTXSTS_NPTQXSAV_Field is HAL.UInt8;
   subtype OTG_HS_GNPTXSTS_NPTXQTOP_Field is HAL.UInt7;

   --  OTG_HS nonperiodic transmit FIFO/queue status register
   type OTG_HS_GNPTXSTS_Register is record
      --  Read-only. Nonperiodic TxFIFO space available
      NPTXFSAV       : OTG_HS_GNPTXSTS_NPTXFSAV_Field;
      --  Read-only. Nonperiodic transmit request queue space available
      NPTQXSAV       : OTG_HS_GNPTXSTS_NPTQXSAV_Field;
      --  Read-only. Top of the nonperiodic transmit request queue
      NPTXQTOP       : OTG_HS_GNPTXSTS_NPTXQTOP_Field;
      --  unspecified
      Reserved_31_31 : HAL.Bit;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GNPTXSTS_Register use record
      NPTXFSAV       at 0 range 0 .. 15;
      NPTQXSAV       at 0 range 16 .. 23;
      NPTXQTOP       at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  OTG_HS general core configuration register
   type OTG_HS_GCCFG_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  Power down
      PWRDWN         : Boolean := False;
      --  Enable I2C bus connection for the external I2C PHY interface
      I2CPADEN       : Boolean := False;
      --  Enable the VBUS sensing device
      VBUSASEN       : Boolean := False;
      --  Enable the VBUS sensing device
      VBUSBSEN       : Boolean := False;
      --  SOF output enable
      SOFOUTEN       : Boolean := False;
      --  VBUS sensing disable option
      NOVBUSSENS     : Boolean := False;
      --  unspecified
      Reserved_22_31 : HAL.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_GCCFG_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      PWRDWN         at 0 range 16 .. 16;
      I2CPADEN       at 0 range 17 .. 17;
      VBUSASEN       at 0 range 18 .. 18;
      VBUSBSEN       at 0 range 19 .. 19;
      SOFOUTEN       at 0 range 20 .. 20;
      NOVBUSSENS     at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype OTG_HS_HPTXFSIZ_PTXSA_Field is HAL.UInt16;
   subtype OTG_HS_HPTXFSIZ_PTXFD_Field is HAL.UInt16;

   --  OTG_HS Host periodic transmit FIFO size register
   type OTG_HS_HPTXFSIZ_Register is record
      --  Host periodic TxFIFO start address
      PTXSA : OTG_HS_HPTXFSIZ_PTXSA_Field := 16#600#;
      --  Host periodic TxFIFO depth
      PTXFD : OTG_HS_HPTXFSIZ_PTXFD_Field := 16#200#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HPTXFSIZ_Register use record
      PTXSA at 0 range 0 .. 15;
      PTXFD at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_DIEPTXF_INEPTXSA_Field is HAL.UInt16;
   subtype OTG_HS_DIEPTXF_INEPTXFD_Field is HAL.UInt16;

   --  OTG_HS device IN endpoint transmit FIFO size register
   type OTG_HS_DIEPTXF_Register is record
      --  IN endpoint FIFOx transmit RAM start address
      INEPTXSA : OTG_HS_DIEPTXF_INEPTXSA_Field := 16#400#;
      --  IN endpoint TxFIFO depth
      INEPTXFD : OTG_HS_DIEPTXF_INEPTXFD_Field := 16#200#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_DIEPTXF_Register use record
      INEPTXSA at 0 range 0 .. 15;
      INEPTXFD at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_HCFG_FSLSPCS_Field is HAL.UInt2;

   --  OTG_HS host configuration register
   type OTG_HS_HCFG_Register is record
      --  FS/LS PHY clock select
      FSLSPCS       : OTG_HS_HCFG_FSLSPCS_Field := 16#0#;
      --  Read-only. FS- and LS-only support
      FSLSS         : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HCFG_Register use record
      FSLSPCS       at 0 range 0 .. 1;
      FSLSS         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype OTG_HS_HFIR_FRIVL_Field is HAL.UInt16;

   --  OTG_HS Host frame interval register
   type OTG_HS_HFIR_Register is record
      --  Frame interval
      FRIVL          : OTG_HS_HFIR_FRIVL_Field := 16#EA60#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HFIR_Register use record
      FRIVL          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_HFNUM_FRNUM_Field is HAL.UInt16;
   subtype OTG_HS_HFNUM_FTREM_Field is HAL.UInt16;

   --  OTG_HS host frame number/frame time remaining register
   type OTG_HS_HFNUM_Register is record
      --  Read-only. Frame number
      FRNUM : OTG_HS_HFNUM_FRNUM_Field;
      --  Read-only. Frame time remaining
      FTREM : OTG_HS_HFNUM_FTREM_Field;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HFNUM_Register use record
      FRNUM at 0 range 0 .. 15;
      FTREM at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_HPTXSTS_PTXFSAVL_Field is HAL.UInt16;
   subtype OTG_HS_HPTXSTS_PTXQSAV_Field is HAL.UInt8;
   subtype OTG_HS_HPTXSTS_PTXQTOP_Field is HAL.UInt8;

   --  OTG_HS_Host periodic transmit FIFO/queue status register
   type OTG_HS_HPTXSTS_Register is record
      --  Periodic transmit data FIFO space available
      PTXFSAVL : OTG_HS_HPTXSTS_PTXFSAVL_Field := 16#100#;
      --  Read-only. Periodic transmit request queue space available
      PTXQSAV  : OTG_HS_HPTXSTS_PTXQSAV_Field := 16#8#;
      --  Read-only. Top of the periodic transmit request queue
      PTXQTOP  : OTG_HS_HPTXSTS_PTXQTOP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HPTXSTS_Register use record
      PTXFSAVL at 0 range 0 .. 15;
      PTXQSAV  at 0 range 16 .. 23;
      PTXQTOP  at 0 range 24 .. 31;
   end record;

   subtype OTG_HS_HAINT_HAINT_Field is HAL.UInt16;

   --  OTG_HS Host all channels interrupt register
   type OTG_HS_HAINT_Register is record
      --  Read-only. Channel interrupts
      HAINT          : OTG_HS_HAINT_HAINT_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HAINT_Register use record
      HAINT          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_HAINTMSK_HAINTM_Field is HAL.UInt16;

   --  OTG_HS host all channels interrupt mask register
   type OTG_HS_HAINTMSK_Register is record
      --  Channel interrupt mask
      HAINTM         : OTG_HS_HAINTMSK_HAINTM_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HAINTMSK_Register use record
      HAINTM         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OTG_HS_HPRT_PLSTS_Field is HAL.UInt2;
   subtype OTG_HS_HPRT_PTCTL_Field is HAL.UInt4;
   subtype OTG_HS_HPRT_PSPD_Field is HAL.UInt2;

   --  OTG_HS host port control and status register
   type OTG_HS_HPRT_Register is record
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
      PLSTS          : OTG_HS_HPRT_PLSTS_Field := 16#0#;
      --  Port power
      PPWR           : Boolean := False;
      --  Port test control
      PTCTL          : OTG_HS_HPRT_PTCTL_Field := 16#0#;
      --  Read-only. Port speed
      PSPD           : OTG_HS_HPRT_PSPD_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HPRT_Register use record
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

   subtype OTG_HS_HCCHAR_MPSIZ_Field is HAL.UInt11;
   subtype OTG_HS_HCCHAR_EPNUM_Field is HAL.UInt4;
   subtype OTG_HS_HCCHAR_EPTYP_Field is HAL.UInt2;
   subtype OTG_HS_HCCHAR_MC_Field is HAL.UInt2;
   subtype OTG_HS_HCCHAR_DAD_Field is HAL.UInt7;

   --  OTG_HS host channel-0 characteristics register
   type OTG_HS_HCCHAR_Register is record
      --  Maximum packet size
      MPSIZ          : OTG_HS_HCCHAR_MPSIZ_Field := 16#0#;
      --  Endpoint number
      EPNUM          : OTG_HS_HCCHAR_EPNUM_Field := 16#0#;
      --  Endpoint direction
      EPDIR          : Boolean := False;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Low-speed device
      LSDEV          : Boolean := False;
      --  Endpoint type
      EPTYP          : OTG_HS_HCCHAR_EPTYP_Field := 16#0#;
      --  Multi Count (MC) / Error Count (EC)
      MC             : OTG_HS_HCCHAR_MC_Field := 16#0#;
      --  Device address
      DAD            : OTG_HS_HCCHAR_DAD_Field := 16#0#;
      --  Odd frame
      ODDFRM         : Boolean := False;
      --  Channel disable
      CHDIS          : Boolean := False;
      --  Channel enable
      CHENA          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HCCHAR_Register use record
      MPSIZ          at 0 range 0 .. 10;
      EPNUM          at 0 range 11 .. 14;
      EPDIR          at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      LSDEV          at 0 range 17 .. 17;
      EPTYP          at 0 range 18 .. 19;
      MC             at 0 range 20 .. 21;
      DAD            at 0 range 22 .. 28;
      ODDFRM         at 0 range 29 .. 29;
      CHDIS          at 0 range 30 .. 30;
      CHENA          at 0 range 31 .. 31;
   end record;

   subtype OTG_HS_HCSPLT_PRTADDR_Field is HAL.UInt7;
   subtype OTG_HS_HCSPLT_HUBADDR_Field is HAL.UInt7;
   subtype OTG_HS_HCSPLT_XACTPOS_Field is HAL.UInt2;

   --  OTG_HS host channel-0 split control register
   type OTG_HS_HCSPLT_Register is record
      --  Port address
      PRTADDR        : OTG_HS_HCSPLT_PRTADDR_Field := 16#0#;
      --  Hub address
      HUBADDR        : OTG_HS_HCSPLT_HUBADDR_Field := 16#0#;
      --  XACTPOS
      XACTPOS        : OTG_HS_HCSPLT_XACTPOS_Field := 16#0#;
      --  Do complete split
      COMPLSPLT      : Boolean := False;
      --  unspecified
      Reserved_17_30 : HAL.UInt14 := 16#0#;
      --  Split enable
      SPLITEN        : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HCSPLT_Register use record
      PRTADDR        at 0 range 0 .. 6;
      HUBADDR        at 0 range 7 .. 13;
      XACTPOS        at 0 range 14 .. 15;
      COMPLSPLT      at 0 range 16 .. 16;
      Reserved_17_30 at 0 range 17 .. 30;
      SPLITEN        at 0 range 31 .. 31;
   end record;

   --  OTG_HS host channel-11 interrupt register
   type OTG_HS_HCINT_Register is record
      --  Transfer completed
      XFRC           : Boolean := False;
      --  Channel halted
      CHH            : Boolean := False;
      --  AHB error
      AHBERR         : Boolean := False;
      --  STALL response received interrupt
      STALL          : Boolean := False;
      --  NAK response received interrupt
      NAK            : Boolean := False;
      --  ACK response received/transmitted interrupt
      ACK            : Boolean := False;
      --  Response received interrupt
      NYET           : Boolean := False;
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

   for OTG_HS_HCINT_Register use record
      XFRC           at 0 range 0 .. 0;
      CHH            at 0 range 1 .. 1;
      AHBERR         at 0 range 2 .. 2;
      STALL          at 0 range 3 .. 3;
      NAK            at 0 range 4 .. 4;
      ACK            at 0 range 5 .. 5;
      NYET           at 0 range 6 .. 6;
      TXERR          at 0 range 7 .. 7;
      BBERR          at 0 range 8 .. 8;
      FRMOR          at 0 range 9 .. 9;
      DTERR          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  OTG_HS host channel-11 interrupt mask register
   type OTG_HS_HCINTMSK_Register is record
      --  Transfer completed mask
      XFRCM          : Boolean := False;
      --  Channel halted mask
      CHHM           : Boolean := False;
      --  AHB error
      AHBERR         : Boolean := False;
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

   for OTG_HS_HCINTMSK_Register use record
      XFRCM          at 0 range 0 .. 0;
      CHHM           at 0 range 1 .. 1;
      AHBERR         at 0 range 2 .. 2;
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

   subtype OTG_HS_HCTSIZ_XFRSIZ_Field is HAL.UInt19;
   subtype OTG_HS_HCTSIZ_PKTCNT_Field is HAL.UInt10;
   subtype OTG_HS_HCTSIZ_DPID_Field is HAL.UInt2;

   --  OTG_HS host channel-11 transfer size register
   type OTG_HS_HCTSIZ_Register is record
      --  Transfer size
      XFRSIZ         : OTG_HS_HCTSIZ_XFRSIZ_Field := 16#0#;
      --  Packet count
      PKTCNT         : OTG_HS_HCTSIZ_PKTCNT_Field := 16#0#;
      --  Data PID
      DPID           : OTG_HS_HCTSIZ_DPID_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : HAL.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_HCTSIZ_Register use record
      XFRSIZ         at 0 range 0 .. 18;
      PKTCNT         at 0 range 19 .. 28;
      DPID           at 0 range 29 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  Power and clock gating control register
   type OTG_HS_PCGCR_Register is record
      --  Stop PHY clock
      STPPCLK       : Boolean := False;
      --  Gate HCLK
      GATEHCLK      : Boolean := False;
      --  unspecified
      Reserved_2_3  : HAL.UInt2 := 16#0#;
      --  PHY suspended
      PHYSUSP       : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OTG_HS_PCGCR_Register use record
      STPPCLK       at 0 range 0 .. 0;
      GATEHCLK      at 0 range 1 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      PHYSUSP       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  USB on the go high speed
   type OTG_HS_DEVICE_Peripheral is record
      --  OTG_HS device configuration register
      OTG_HS_DCFG         : aliased OTG_HS_DCFG_Register;
      --  OTG_HS device control register
      OTG_HS_DCTL         : aliased OTG_HS_DCTL_Register;
      --  OTG_HS device status register
      OTG_HS_DSTS         : aliased OTG_HS_DSTS_Register;
      --  OTG_HS device IN endpoint common interrupt mask register
      OTG_HS_DIEPMSK      : aliased OTG_HS_DIEPMSK_Register;
      --  OTG_HS device OUT endpoint common interrupt mask register
      OTG_HS_DOEPMSK      : aliased OTG_HS_DOEPMSK_Register;
      --  OTG_HS device all endpoints interrupt register
      OTG_HS_DAINT        : aliased OTG_HS_DAINT_Register;
      --  OTG_HS all endpoints interrupt mask register
      OTG_HS_DAINTMSK     : aliased OTG_HS_DAINTMSK_Register;
      --  OTG_HS device VBUS discharge time register
      OTG_HS_DVBUSDIS     : aliased OTG_HS_DVBUSDIS_Register;
      --  OTG_HS device VBUS pulsing time register
      OTG_HS_DVBUSPULSE   : aliased OTG_HS_DVBUSPULSE_Register;
      --  OTG_HS Device threshold control register
      OTG_HS_DTHRCTL      : aliased OTG_HS_DTHRCTL_Register;
      --  OTG_HS device IN endpoint FIFO empty interrupt mask register
      OTG_HS_DIEPEMPMSK   : aliased OTG_HS_DIEPEMPMSK_Register;
      --  OTG_HS device each endpoint interrupt register
      OTG_HS_DEACHINT     : aliased OTG_HS_DEACHINT_Register;
      --  OTG_HS device each endpoint interrupt register mask
      OTG_HS_DEACHINTMSK  : aliased OTG_HS_DEACHINTMSK_Register;
      --  OTG_HS device each in endpoint-1 interrupt register
      OTG_HS_DIEPEACHMSK1 : aliased OTG_HS_DIEPEACHMSK1_Register;
      --  OTG_HS device each OUT endpoint-1 interrupt register
      OTG_HS_DOEPEACHMSK1 : aliased OTG_HS_DOEPEACHMSK1_Register;
      --  OTG device endpoint-0 control register
      OTG_HS_DIEPCTL0     : aliased OTG_HS_DIEPCTL_Register;
      --  OTG device endpoint-0 interrupt register
      OTG_HS_DIEPINT0     : aliased OTG_HS_DIEPINT_Register;
      --  OTG_HS device IN endpoint 0 transfer size register
      OTG_HS_DIEPTSIZ0    : aliased OTG_HS_DIEPTSIZ0_Register;
      --  OTG_HS device endpoint-1 DMA address register
      OTG_HS_DIEPDMA1     : aliased HAL.UInt32;
      --  OTG_HS device IN endpoint transmit FIFO status register
      OTG_HS_DTXFSTS0     : aliased OTG_HS_DTXFSTS_Register;
      --  OTG device endpoint-1 control register
      OTG_HS_DIEPCTL1     : aliased OTG_HS_DIEPCTL_Register;
      --  OTG device endpoint-1 interrupt register
      OTG_HS_DIEPINT1     : aliased OTG_HS_DIEPINT_Register;
      --  OTG_HS device endpoint transfer size register
      OTG_HS_DIEPTSIZ1    : aliased OTG_HS_DIEPTSIZ_Register;
      --  OTG_HS device endpoint-2 DMA address register
      OTG_HS_DIEPDMA2     : aliased HAL.UInt32;
      --  OTG_HS device IN endpoint transmit FIFO status register
      OTG_HS_DTXFSTS1     : aliased OTG_HS_DTXFSTS_Register;
      --  OTG device endpoint-2 control register
      OTG_HS_DIEPCTL2     : aliased OTG_HS_DIEPCTL_Register;
      --  OTG device endpoint-2 interrupt register
      OTG_HS_DIEPINT2     : aliased OTG_HS_DIEPINT_Register;
      --  OTG_HS device endpoint transfer size register
      OTG_HS_DIEPTSIZ2    : aliased OTG_HS_DIEPTSIZ_Register;
      --  OTG_HS device endpoint-3 DMA address register
      OTG_HS_DIEPDMA3     : aliased HAL.UInt32;
      --  OTG_HS device IN endpoint transmit FIFO status register
      OTG_HS_DTXFSTS2     : aliased OTG_HS_DTXFSTS_Register;
      --  OTG device endpoint-3 control register
      OTG_HS_DIEPCTL3     : aliased OTG_HS_DIEPCTL_Register;
      --  OTG device endpoint-3 interrupt register
      OTG_HS_DIEPINT3     : aliased OTG_HS_DIEPINT_Register;
      --  OTG_HS device endpoint transfer size register
      OTG_HS_DIEPTSIZ3    : aliased OTG_HS_DIEPTSIZ_Register;
      --  OTG_HS device endpoint-4 DMA address register
      OTG_HS_DIEPDMA4     : aliased HAL.UInt32;
      --  OTG_HS device IN endpoint transmit FIFO status register
      OTG_HS_DTXFSTS3     : aliased OTG_HS_DTXFSTS_Register;
      --  OTG device endpoint-4 control register
      OTG_HS_DIEPCTL4     : aliased OTG_HS_DIEPCTL_Register;
      --  OTG device endpoint-4 interrupt register
      OTG_HS_DIEPINT4     : aliased OTG_HS_DIEPINT_Register;
      --  OTG_HS device endpoint transfer size register
      OTG_HS_DIEPTSIZ4    : aliased OTG_HS_DIEPTSIZ_Register;
      --  OTG_HS device endpoint-5 DMA address register
      OTG_HS_DIEPDMA5     : aliased HAL.UInt32;
      --  OTG_HS device IN endpoint transmit FIFO status register
      OTG_HS_DTXFSTS4     : aliased OTG_HS_DTXFSTS_Register;
      --  OTG device endpoint-5 control register
      OTG_HS_DIEPCTL5     : aliased OTG_HS_DIEPCTL_Register;
      --  OTG device endpoint-5 interrupt register
      OTG_HS_DIEPINT5     : aliased OTG_HS_DIEPINT_Register;
      --  OTG_HS device endpoint transfer size register
      OTG_HS_DIEPTSIZ5    : aliased OTG_HS_DIEPTSIZ_Register;
      --  OTG_HS device IN endpoint transmit FIFO status register
      OTG_HS_DTXFSTS5     : aliased OTG_HS_DTXFSTS_Register;
      --  OTG device endpoint-6 control register
      OTG_HS_DIEPCTL6     : aliased OTG_HS_DIEPCTL_Register;
      --  OTG device endpoint-6 interrupt register
      OTG_HS_DIEPINT6     : aliased OTG_HS_DIEPINT_Register;
      --  OTG device endpoint-7 control register
      OTG_HS_DIEPCTL7     : aliased OTG_HS_DIEPCTL_Register;
      --  OTG device endpoint-7 interrupt register
      OTG_HS_DIEPINT7     : aliased OTG_HS_DIEPINT_Register;
      --  OTG_HS device control OUT endpoint 0 control register
      OTG_HS_DOEPCTL0     : aliased OTG_HS_DOEPCTL0_Register;
      --  OTG_HS device endpoint-0 interrupt register
      OTG_HS_DOEPINT0     : aliased OTG_HS_DOEPINT_Register;
      --  OTG_HS device endpoint-1 transfer size register
      OTG_HS_DOEPTSIZ0    : aliased OTG_HS_DOEPTSIZ0_Register;
      --  OTG device endpoint-1 control register
      OTG_HS_DOEPCTL1     : aliased OTG_HS_DOEPCTL_Register;
      --  OTG_HS device endpoint-1 interrupt register
      OTG_HS_DOEPINT1     : aliased OTG_HS_DOEPINT_Register;
      --  OTG_HS device endpoint-2 transfer size register
      OTG_HS_DOEPTSIZ1    : aliased OTG_HS_DOEPTSIZ_Register;
      --  OTG device endpoint-2 control register
      OTG_HS_DOEPCTL2     : aliased OTG_HS_DOEPCTL_Register;
      --  OTG_HS device endpoint-2 interrupt register
      OTG_HS_DOEPINT2     : aliased OTG_HS_DOEPINT_Register;
      --  OTG_HS device endpoint-3 transfer size register
      OTG_HS_DOEPTSIZ2    : aliased OTG_HS_DOEPTSIZ_Register;
      --  OTG device endpoint-3 control register
      OTG_HS_DOEPCTL3     : aliased OTG_HS_DOEPCTL_Register;
      --  OTG_HS device endpoint-3 interrupt register
      OTG_HS_DOEPINT3     : aliased OTG_HS_DOEPINT_Register;
      --  OTG_HS device endpoint-4 transfer size register
      OTG_HS_DOEPTSIZ3    : aliased OTG_HS_DOEPTSIZ_Register;
      --  OTG_HS device endpoint-4 interrupt register
      OTG_HS_DOEPINT4     : aliased OTG_HS_DOEPINT_Register;
      --  OTG_HS device endpoint-5 transfer size register
      OTG_HS_DOEPTSIZ4    : aliased OTG_HS_DOEPTSIZ_Register;
      --  OTG_HS device endpoint-5 interrupt register
      OTG_HS_DOEPINT5     : aliased OTG_HS_DOEPINT_Register;
      --  OTG_HS device endpoint-6 interrupt register
      OTG_HS_DOEPINT6     : aliased OTG_HS_DOEPINT_Register;
      --  OTG_HS device endpoint-7 interrupt register
      OTG_HS_DOEPINT7     : aliased OTG_HS_DOEPINT_Register;
   end record
     with Volatile;

   for OTG_HS_DEVICE_Peripheral use record
      OTG_HS_DCFG         at 16#0# range 0 .. 31;
      OTG_HS_DCTL         at 16#4# range 0 .. 31;
      OTG_HS_DSTS         at 16#8# range 0 .. 31;
      OTG_HS_DIEPMSK      at 16#10# range 0 .. 31;
      OTG_HS_DOEPMSK      at 16#14# range 0 .. 31;
      OTG_HS_DAINT        at 16#18# range 0 .. 31;
      OTG_HS_DAINTMSK     at 16#1C# range 0 .. 31;
      OTG_HS_DVBUSDIS     at 16#28# range 0 .. 31;
      OTG_HS_DVBUSPULSE   at 16#2C# range 0 .. 31;
      OTG_HS_DTHRCTL      at 16#30# range 0 .. 31;
      OTG_HS_DIEPEMPMSK   at 16#34# range 0 .. 31;
      OTG_HS_DEACHINT     at 16#38# range 0 .. 31;
      OTG_HS_DEACHINTMSK  at 16#3C# range 0 .. 31;
      OTG_HS_DIEPEACHMSK1 at 16#40# range 0 .. 31;
      OTG_HS_DOEPEACHMSK1 at 16#80# range 0 .. 31;
      OTG_HS_DIEPCTL0     at 16#100# range 0 .. 31;
      OTG_HS_DIEPINT0     at 16#108# range 0 .. 31;
      OTG_HS_DIEPTSIZ0    at 16#110# range 0 .. 31;
      OTG_HS_DIEPDMA1     at 16#114# range 0 .. 31;
      OTG_HS_DTXFSTS0     at 16#118# range 0 .. 31;
      OTG_HS_DIEPCTL1     at 16#120# range 0 .. 31;
      OTG_HS_DIEPINT1     at 16#128# range 0 .. 31;
      OTG_HS_DIEPTSIZ1    at 16#130# range 0 .. 31;
      OTG_HS_DIEPDMA2     at 16#134# range 0 .. 31;
      OTG_HS_DTXFSTS1     at 16#138# range 0 .. 31;
      OTG_HS_DIEPCTL2     at 16#140# range 0 .. 31;
      OTG_HS_DIEPINT2     at 16#148# range 0 .. 31;
      OTG_HS_DIEPTSIZ2    at 16#150# range 0 .. 31;
      OTG_HS_DIEPDMA3     at 16#154# range 0 .. 31;
      OTG_HS_DTXFSTS2     at 16#158# range 0 .. 31;
      OTG_HS_DIEPCTL3     at 16#160# range 0 .. 31;
      OTG_HS_DIEPINT3     at 16#168# range 0 .. 31;
      OTG_HS_DIEPTSIZ3    at 16#170# range 0 .. 31;
      OTG_HS_DIEPDMA4     at 16#174# range 0 .. 31;
      OTG_HS_DTXFSTS3     at 16#178# range 0 .. 31;
      OTG_HS_DIEPCTL4     at 16#180# range 0 .. 31;
      OTG_HS_DIEPINT4     at 16#188# range 0 .. 31;
      OTG_HS_DIEPTSIZ4    at 16#190# range 0 .. 31;
      OTG_HS_DIEPDMA5     at 16#194# range 0 .. 31;
      OTG_HS_DTXFSTS4     at 16#198# range 0 .. 31;
      OTG_HS_DIEPCTL5     at 16#1A0# range 0 .. 31;
      OTG_HS_DIEPINT5     at 16#1A8# range 0 .. 31;
      OTG_HS_DIEPTSIZ5    at 16#1B0# range 0 .. 31;
      OTG_HS_DTXFSTS5     at 16#1B8# range 0 .. 31;
      OTG_HS_DIEPCTL6     at 16#1C0# range 0 .. 31;
      OTG_HS_DIEPINT6     at 16#1C8# range 0 .. 31;
      OTG_HS_DIEPCTL7     at 16#1E0# range 0 .. 31;
      OTG_HS_DIEPINT7     at 16#1E8# range 0 .. 31;
      OTG_HS_DOEPCTL0     at 16#300# range 0 .. 31;
      OTG_HS_DOEPINT0     at 16#308# range 0 .. 31;
      OTG_HS_DOEPTSIZ0    at 16#310# range 0 .. 31;
      OTG_HS_DOEPCTL1     at 16#320# range 0 .. 31;
      OTG_HS_DOEPINT1     at 16#328# range 0 .. 31;
      OTG_HS_DOEPTSIZ1    at 16#330# range 0 .. 31;
      OTG_HS_DOEPCTL2     at 16#340# range 0 .. 31;
      OTG_HS_DOEPINT2     at 16#348# range 0 .. 31;
      OTG_HS_DOEPTSIZ2    at 16#350# range 0 .. 31;
      OTG_HS_DOEPCTL3     at 16#360# range 0 .. 31;
      OTG_HS_DOEPINT3     at 16#368# range 0 .. 31;
      OTG_HS_DOEPTSIZ3    at 16#370# range 0 .. 31;
      OTG_HS_DOEPINT4     at 16#388# range 0 .. 31;
      OTG_HS_DOEPTSIZ4    at 16#390# range 0 .. 31;
      OTG_HS_DOEPINT5     at 16#3A8# range 0 .. 31;
      OTG_HS_DOEPINT6     at 16#3C8# range 0 .. 31;
      OTG_HS_DOEPINT7     at 16#3E8# range 0 .. 31;
   end record;

   --  USB on the go high speed
   OTG_HS_DEVICE_Periph : aliased OTG_HS_DEVICE_Peripheral
     with Import, Address => System'To_Address (16#40040800#);

   type OTG_HS_GLOBAL_Disc is
     (
      Host,
      Peripheral,
      Gnptxfsiz_Host,
      Tx0Fsiz_Peripheral);

   --  USB on the go high speed
   type OTG_HS_GLOBAL_Peripheral
     (Discriminent : OTG_HS_GLOBAL_Disc := Host)
   is record
      --  OTG_HS control and status register
      OTG_HS_GOTGCTL            : aliased OTG_HS_GOTGCTL_Register;
      --  OTG_HS interrupt register
      OTG_HS_GOTGINT            : aliased OTG_HS_GOTGINT_Register;
      --  OTG_HS AHB configuration register
      OTG_HS_GAHBCFG            : aliased OTG_HS_GAHBCFG_Register;
      --  OTG_HS USB configuration register
      OTG_HS_GUSBCFG            : aliased OTG_HS_GUSBCFG_Register;
      --  OTG_HS reset register
      OTG_HS_GRSTCTL            : aliased OTG_HS_GRSTCTL_Register;
      --  OTG_HS core interrupt register
      OTG_HS_GINTSTS            : aliased OTG_HS_GINTSTS_Register;
      --  OTG_HS interrupt mask register
      OTG_HS_GINTMSK            : aliased OTG_HS_GINTMSK_Register;
      --  OTG_HS Receive FIFO size register
      OTG_HS_GRXFSIZ            : aliased OTG_HS_GRXFSIZ_Register;
      --  OTG_HS nonperiodic transmit FIFO/queue status register
      OTG_HS_GNPTXSTS           : aliased OTG_HS_GNPTXSTS_Register;
      --  OTG_HS general core configuration register
      OTG_HS_GCCFG              : aliased OTG_HS_GCCFG_Register;
      --  OTG_HS core ID register
      OTG_HS_CID                : aliased HAL.UInt32;
      --  OTG_HS Host periodic transmit FIFO size register
      OTG_HS_HPTXFSIZ           : aliased OTG_HS_HPTXFSIZ_Register;
      --  OTG_HS device IN endpoint transmit FIFO size register
      OTG_HS_DIEPTXF1           : aliased OTG_HS_DIEPTXF_Register;
      --  OTG_HS device IN endpoint transmit FIFO size register
      OTG_HS_DIEPTXF2           : aliased OTG_HS_DIEPTXF_Register;
      --  OTG_HS device IN endpoint transmit FIFO size register
      OTG_HS_DIEPTXF3           : aliased OTG_HS_DIEPTXF_Register;
      --  OTG_HS device IN endpoint transmit FIFO size register
      OTG_HS_DIEPTXF4           : aliased OTG_HS_DIEPTXF_Register;
      --  OTG_HS device IN endpoint transmit FIFO size register
      OTG_HS_DIEPTXF5           : aliased OTG_HS_DIEPTXF_Register;
      --  OTG_HS device IN endpoint transmit FIFO size register
      OTG_HS_DIEPTXF6           : aliased OTG_HS_DIEPTXF_Register;
      --  OTG_HS device IN endpoint transmit FIFO size register
      OTG_HS_DIEPTXF7           : aliased OTG_HS_DIEPTXF_Register;
      case Discriminent is
         when Host =>
            --  OTG_HS Receive status debug read register (host mode)
            OTG_HS_GRXSTSR_Host : aliased OTG_HS_GRXSTSR_Host_Register;
            --  OTG_HS status read and pop register (host mode)
            OTG_HS_GRXSTSP_Host : aliased OTG_HS_GRXSTSP_Host_Register;
         when Peripheral =>
            --  OTG_HS Receive status debug read register (peripheral mode
            --  mode)
            OTG_HS_GRXSTSR_Peripheral : aliased OTG_HS_GRXSTSR_Peripheral_Register;
            --  OTG_HS status read and pop register (peripheral mode)
            OTG_HS_GRXSTSP_Peripheral : aliased OTG_HS_GRXSTSP_Peripheral_Register;
         when Gnptxfsiz_Host =>
            --  OTG_HS nonperiodic transmit FIFO size register (host mode)
            OTG_HS_GNPTXFSIZ_Host : aliased OTG_HS_GNPTXFSIZ_Host_Register;
         when Tx0Fsiz_Peripheral =>
            --  Endpoint 0 transmit FIFO size (peripheral mode)
            OTG_HS_TX0FSIZ_Peripheral : aliased OTG_HS_TX0FSIZ_Peripheral_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for OTG_HS_GLOBAL_Peripheral use record
      OTG_HS_GOTGCTL            at 16#0# range 0 .. 31;
      OTG_HS_GOTGINT            at 16#4# range 0 .. 31;
      OTG_HS_GAHBCFG            at 16#8# range 0 .. 31;
      OTG_HS_GUSBCFG            at 16#C# range 0 .. 31;
      OTG_HS_GRSTCTL            at 16#10# range 0 .. 31;
      OTG_HS_GINTSTS            at 16#14# range 0 .. 31;
      OTG_HS_GINTMSK            at 16#18# range 0 .. 31;
      OTG_HS_GRXFSIZ            at 16#24# range 0 .. 31;
      OTG_HS_GNPTXSTS           at 16#2C# range 0 .. 31;
      OTG_HS_GCCFG              at 16#38# range 0 .. 31;
      OTG_HS_CID                at 16#3C# range 0 .. 31;
      OTG_HS_HPTXFSIZ           at 16#100# range 0 .. 31;
      OTG_HS_DIEPTXF1           at 16#104# range 0 .. 31;
      OTG_HS_DIEPTXF2           at 16#108# range 0 .. 31;
      OTG_HS_DIEPTXF3           at 16#11C# range 0 .. 31;
      OTG_HS_DIEPTXF4           at 16#120# range 0 .. 31;
      OTG_HS_DIEPTXF5           at 16#124# range 0 .. 31;
      OTG_HS_DIEPTXF6           at 16#128# range 0 .. 31;
      OTG_HS_DIEPTXF7           at 16#12C# range 0 .. 31;
      OTG_HS_GRXSTSR_Host       at 16#1C# range 0 .. 31;
      OTG_HS_GRXSTSP_Host       at 16#20# range 0 .. 31;
      OTG_HS_GRXSTSR_Peripheral at 16#1C# range 0 .. 31;
      OTG_HS_GRXSTSP_Peripheral at 16#20# range 0 .. 31;
      OTG_HS_GNPTXFSIZ_Host     at 16#28# range 0 .. 31;
      OTG_HS_TX0FSIZ_Peripheral at 16#28# range 0 .. 31;
   end record;

   --  USB on the go high speed
   OTG_HS_GLOBAL_Periph : aliased OTG_HS_GLOBAL_Peripheral
     with Import, Address => System'To_Address (16#40040000#);

   --  USB on the go high speed
   type OTG_HS_HOST_Peripheral is record
      --  OTG_HS host configuration register
      OTG_HS_HCFG       : aliased OTG_HS_HCFG_Register;
      --  OTG_HS Host frame interval register
      OTG_HS_HFIR       : aliased OTG_HS_HFIR_Register;
      --  OTG_HS host frame number/frame time remaining register
      OTG_HS_HFNUM      : aliased OTG_HS_HFNUM_Register;
      --  OTG_HS_Host periodic transmit FIFO/queue status register
      OTG_HS_HPTXSTS    : aliased OTG_HS_HPTXSTS_Register;
      --  OTG_HS Host all channels interrupt register
      OTG_HS_HAINT      : aliased OTG_HS_HAINT_Register;
      --  OTG_HS host all channels interrupt mask register
      OTG_HS_HAINTMSK   : aliased OTG_HS_HAINTMSK_Register;
      --  OTG_HS host port control and status register
      OTG_HS_HPRT       : aliased OTG_HS_HPRT_Register;
      --  OTG_HS host channel-0 characteristics register
      OTG_HS_HCCHAR0    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-0 split control register
      OTG_HS_HCSPLT0    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-11 interrupt register
      OTG_HS_HCINT0     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-11 interrupt mask register
      OTG_HS_HCINTMSK0  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-11 transfer size register
      OTG_HS_HCTSIZ0    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-0 DMA address register
      OTG_HS_HCDMA0     : aliased HAL.UInt32;
      --  OTG_HS host channel-1 characteristics register
      OTG_HS_HCCHAR1    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-1 split control register
      OTG_HS_HCSPLT1    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-1 interrupt register
      OTG_HS_HCINT1     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-1 interrupt mask register
      OTG_HS_HCINTMSK1  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-1 transfer size register
      OTG_HS_HCTSIZ1    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-1 DMA address register
      OTG_HS_HCDMA1     : aliased HAL.UInt32;
      --  OTG_HS host channel-2 characteristics register
      OTG_HS_HCCHAR2    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-2 split control register
      OTG_HS_HCSPLT2    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-2 interrupt register
      OTG_HS_HCINT2     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-2 interrupt mask register
      OTG_HS_HCINTMSK2  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-2 transfer size register
      OTG_HS_HCTSIZ2    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-2 DMA address register
      OTG_HS_HCDMA2     : aliased HAL.UInt32;
      --  OTG_HS host channel-3 characteristics register
      OTG_HS_HCCHAR3    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-3 split control register
      OTG_HS_HCSPLT3    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-3 interrupt register
      OTG_HS_HCINT3     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-3 interrupt mask register
      OTG_HS_HCINTMSK3  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-3 transfer size register
      OTG_HS_HCTSIZ3    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-3 DMA address register
      OTG_HS_HCDMA3     : aliased HAL.UInt32;
      --  OTG_HS host channel-4 characteristics register
      OTG_HS_HCCHAR4    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-4 split control register
      OTG_HS_HCSPLT4    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-4 interrupt register
      OTG_HS_HCINT4     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-4 interrupt mask register
      OTG_HS_HCINTMSK4  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-4 transfer size register
      OTG_HS_HCTSIZ4    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-4 DMA address register
      OTG_HS_HCDMA4     : aliased HAL.UInt32;
      --  OTG_HS host channel-5 characteristics register
      OTG_HS_HCCHAR5    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-5 split control register
      OTG_HS_HCSPLT5    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-5 interrupt register
      OTG_HS_HCINT5     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-5 interrupt mask register
      OTG_HS_HCINTMSK5  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-5 transfer size register
      OTG_HS_HCTSIZ5    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-5 DMA address register
      OTG_HS_HCDMA5     : aliased HAL.UInt32;
      --  OTG_HS host channel-6 characteristics register
      OTG_HS_HCCHAR6    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-6 split control register
      OTG_HS_HCSPLT6    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-6 interrupt register
      OTG_HS_HCINT6     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-6 interrupt mask register
      OTG_HS_HCINTMSK6  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-6 transfer size register
      OTG_HS_HCTSIZ6    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-6 DMA address register
      OTG_HS_HCDMA6     : aliased HAL.UInt32;
      --  OTG_HS host channel-7 characteristics register
      OTG_HS_HCCHAR7    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-7 split control register
      OTG_HS_HCSPLT7    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-7 interrupt register
      OTG_HS_HCINT7     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-7 interrupt mask register
      OTG_HS_HCINTMSK7  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-7 transfer size register
      OTG_HS_HCTSIZ7    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-7 DMA address register
      OTG_HS_HCDMA7     : aliased HAL.UInt32;
      --  OTG_HS host channel-8 characteristics register
      OTG_HS_HCCHAR8    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-8 split control register
      OTG_HS_HCSPLT8    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-8 interrupt register
      OTG_HS_HCINT8     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-8 interrupt mask register
      OTG_HS_HCINTMSK8  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-8 transfer size register
      OTG_HS_HCTSIZ8    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-8 DMA address register
      OTG_HS_HCDMA8     : aliased HAL.UInt32;
      --  OTG_HS host channel-9 characteristics register
      OTG_HS_HCCHAR9    : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-9 split control register
      OTG_HS_HCSPLT9    : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-9 interrupt register
      OTG_HS_HCINT9     : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-9 interrupt mask register
      OTG_HS_HCINTMSK9  : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-9 transfer size register
      OTG_HS_HCTSIZ9    : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-9 DMA address register
      OTG_HS_HCDMA9     : aliased HAL.UInt32;
      --  OTG_HS host channel-10 characteristics register
      OTG_HS_HCCHAR10   : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-10 split control register
      OTG_HS_HCSPLT10   : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-10 interrupt register
      OTG_HS_HCINT10    : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-10 interrupt mask register
      OTG_HS_HCINTMSK10 : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-10 transfer size register
      OTG_HS_HCTSIZ10   : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-10 DMA address register
      OTG_HS_HCDMA10    : aliased HAL.UInt32;
      --  OTG_HS host channel-11 characteristics register
      OTG_HS_HCCHAR11   : aliased OTG_HS_HCCHAR_Register;
      --  OTG_HS host channel-11 split control register
      OTG_HS_HCSPLT11   : aliased OTG_HS_HCSPLT_Register;
      --  OTG_HS host channel-11 interrupt register
      OTG_HS_HCINT11    : aliased OTG_HS_HCINT_Register;
      --  OTG_HS host channel-11 interrupt mask register
      OTG_HS_HCINTMSK11 : aliased OTG_HS_HCINTMSK_Register;
      --  OTG_HS host channel-11 transfer size register
      OTG_HS_HCTSIZ11   : aliased OTG_HS_HCTSIZ_Register;
      --  OTG_HS host channel-11 DMA address register
      OTG_HS_HCDMA11    : aliased HAL.UInt32;
   end record
     with Volatile;

   for OTG_HS_HOST_Peripheral use record
      OTG_HS_HCFG       at 16#0# range 0 .. 31;
      OTG_HS_HFIR       at 16#4# range 0 .. 31;
      OTG_HS_HFNUM      at 16#8# range 0 .. 31;
      OTG_HS_HPTXSTS    at 16#10# range 0 .. 31;
      OTG_HS_HAINT      at 16#14# range 0 .. 31;
      OTG_HS_HAINTMSK   at 16#18# range 0 .. 31;
      OTG_HS_HPRT       at 16#40# range 0 .. 31;
      OTG_HS_HCCHAR0    at 16#100# range 0 .. 31;
      OTG_HS_HCSPLT0    at 16#104# range 0 .. 31;
      OTG_HS_HCINT0     at 16#108# range 0 .. 31;
      OTG_HS_HCINTMSK0  at 16#10C# range 0 .. 31;
      OTG_HS_HCTSIZ0    at 16#110# range 0 .. 31;
      OTG_HS_HCDMA0     at 16#114# range 0 .. 31;
      OTG_HS_HCCHAR1    at 16#120# range 0 .. 31;
      OTG_HS_HCSPLT1    at 16#124# range 0 .. 31;
      OTG_HS_HCINT1     at 16#128# range 0 .. 31;
      OTG_HS_HCINTMSK1  at 16#12C# range 0 .. 31;
      OTG_HS_HCTSIZ1    at 16#130# range 0 .. 31;
      OTG_HS_HCDMA1     at 16#134# range 0 .. 31;
      OTG_HS_HCCHAR2    at 16#140# range 0 .. 31;
      OTG_HS_HCSPLT2    at 16#144# range 0 .. 31;
      OTG_HS_HCINT2     at 16#148# range 0 .. 31;
      OTG_HS_HCINTMSK2  at 16#14C# range 0 .. 31;
      OTG_HS_HCTSIZ2    at 16#150# range 0 .. 31;
      OTG_HS_HCDMA2     at 16#154# range 0 .. 31;
      OTG_HS_HCCHAR3    at 16#160# range 0 .. 31;
      OTG_HS_HCSPLT3    at 16#164# range 0 .. 31;
      OTG_HS_HCINT3     at 16#168# range 0 .. 31;
      OTG_HS_HCINTMSK3  at 16#16C# range 0 .. 31;
      OTG_HS_HCTSIZ3    at 16#170# range 0 .. 31;
      OTG_HS_HCDMA3     at 16#174# range 0 .. 31;
      OTG_HS_HCCHAR4    at 16#180# range 0 .. 31;
      OTG_HS_HCSPLT4    at 16#184# range 0 .. 31;
      OTG_HS_HCINT4     at 16#188# range 0 .. 31;
      OTG_HS_HCINTMSK4  at 16#18C# range 0 .. 31;
      OTG_HS_HCTSIZ4    at 16#190# range 0 .. 31;
      OTG_HS_HCDMA4     at 16#194# range 0 .. 31;
      OTG_HS_HCCHAR5    at 16#1A0# range 0 .. 31;
      OTG_HS_HCSPLT5    at 16#1A4# range 0 .. 31;
      OTG_HS_HCINT5     at 16#1A8# range 0 .. 31;
      OTG_HS_HCINTMSK5  at 16#1AC# range 0 .. 31;
      OTG_HS_HCTSIZ5    at 16#1B0# range 0 .. 31;
      OTG_HS_HCDMA5     at 16#1B4# range 0 .. 31;
      OTG_HS_HCCHAR6    at 16#1C0# range 0 .. 31;
      OTG_HS_HCSPLT6    at 16#1C4# range 0 .. 31;
      OTG_HS_HCINT6     at 16#1C8# range 0 .. 31;
      OTG_HS_HCINTMSK6  at 16#1CC# range 0 .. 31;
      OTG_HS_HCTSIZ6    at 16#1D0# range 0 .. 31;
      OTG_HS_HCDMA6     at 16#1D4# range 0 .. 31;
      OTG_HS_HCCHAR7    at 16#1E0# range 0 .. 31;
      OTG_HS_HCSPLT7    at 16#1E4# range 0 .. 31;
      OTG_HS_HCINT7     at 16#1E8# range 0 .. 31;
      OTG_HS_HCINTMSK7  at 16#1EC# range 0 .. 31;
      OTG_HS_HCTSIZ7    at 16#1F0# range 0 .. 31;
      OTG_HS_HCDMA7     at 16#1F4# range 0 .. 31;
      OTG_HS_HCCHAR8    at 16#200# range 0 .. 31;
      OTG_HS_HCSPLT8    at 16#204# range 0 .. 31;
      OTG_HS_HCINT8     at 16#208# range 0 .. 31;
      OTG_HS_HCINTMSK8  at 16#20C# range 0 .. 31;
      OTG_HS_HCTSIZ8    at 16#210# range 0 .. 31;
      OTG_HS_HCDMA8     at 16#214# range 0 .. 31;
      OTG_HS_HCCHAR9    at 16#220# range 0 .. 31;
      OTG_HS_HCSPLT9    at 16#224# range 0 .. 31;
      OTG_HS_HCINT9     at 16#228# range 0 .. 31;
      OTG_HS_HCINTMSK9  at 16#22C# range 0 .. 31;
      OTG_HS_HCTSIZ9    at 16#230# range 0 .. 31;
      OTG_HS_HCDMA9     at 16#234# range 0 .. 31;
      OTG_HS_HCCHAR10   at 16#240# range 0 .. 31;
      OTG_HS_HCSPLT10   at 16#244# range 0 .. 31;
      OTG_HS_HCINT10    at 16#248# range 0 .. 31;
      OTG_HS_HCINTMSK10 at 16#24C# range 0 .. 31;
      OTG_HS_HCTSIZ10   at 16#250# range 0 .. 31;
      OTG_HS_HCDMA10    at 16#254# range 0 .. 31;
      OTG_HS_HCCHAR11   at 16#260# range 0 .. 31;
      OTG_HS_HCSPLT11   at 16#264# range 0 .. 31;
      OTG_HS_HCINT11    at 16#268# range 0 .. 31;
      OTG_HS_HCINTMSK11 at 16#26C# range 0 .. 31;
      OTG_HS_HCTSIZ11   at 16#270# range 0 .. 31;
      OTG_HS_HCDMA11    at 16#274# range 0 .. 31;
   end record;

   --  USB on the go high speed
   OTG_HS_HOST_Periph : aliased OTG_HS_HOST_Peripheral
     with Import, Address => System'To_Address (16#40040400#);

   --  USB on the go high speed
   type OTG_HS_PWRCLK_Peripheral is record
      --  Power and clock gating control register
      OTG_HS_PCGCR : aliased OTG_HS_PCGCR_Register;
   end record
     with Volatile;

   for OTG_HS_PWRCLK_Peripheral use record
      OTG_HS_PCGCR at 0 range 0 .. 31;
   end record;

   --  USB on the go high speed
   OTG_HS_PWRCLK_Periph : aliased OTG_HS_PWRCLK_Peripheral
     with Import, Address => System'To_Address (16#40040E00#);

end STM32_SVD.USB_OTG_HS;
