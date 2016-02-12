--  Automatically generated from STM32F40x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.SDIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --------------------
   -- POWER_Register --
   --------------------

   subtype POWER_PWRCTRL_Field is STM32_SVD.UInt2;

   --  power control register
   type POWER_Register is record
      --  PWRCTRL
      PWRCTRL       : POWER_PWRCTRL_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for POWER_Register use record
      PWRCTRL       at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --------------------
   -- CLKCR_Register --
   --------------------

   subtype CLKCR_CLKDIV_Field is STM32_SVD.Byte;
   subtype CLKCR_CLKEN_Field is STM32_SVD.Bit;
   subtype CLKCR_PWRSAV_Field is STM32_SVD.Bit;
   subtype CLKCR_BYPASS_Field is STM32_SVD.Bit;
   subtype CLKCR_WIDBUS_Field is STM32_SVD.UInt2;
   subtype CLKCR_NEGEDGE_Field is STM32_SVD.Bit;
   subtype CLKCR_HWFC_EN_Field is STM32_SVD.Bit;

   --  SDI clock control register
   type CLKCR_Register is record
      --  Clock divide factor
      CLKDIV         : CLKCR_CLKDIV_Field := 16#0#;
      --  Clock enable bit
      CLKEN          : CLKCR_CLKEN_Field := 16#0#;
      --  Power saving configuration bit
      PWRSAV         : CLKCR_PWRSAV_Field := 16#0#;
      --  Clock divider bypass enable bit
      BYPASS         : CLKCR_BYPASS_Field := 16#0#;
      --  Wide bus mode enable bit
      WIDBUS         : CLKCR_WIDBUS_Field := 16#0#;
      --  SDIO_CK dephasing selection bit
      NEGEDGE        : CLKCR_NEGEDGE_Field := 16#0#;
      --  HW Flow Control enable
      HWFC_EN        : CLKCR_HWFC_EN_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : STM32_SVD.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLKCR_Register use record
      CLKDIV         at 0 range 0 .. 7;
      CLKEN          at 0 range 8 .. 8;
      PWRSAV         at 0 range 9 .. 9;
      BYPASS         at 0 range 10 .. 10;
      WIDBUS         at 0 range 11 .. 12;
      NEGEDGE        at 0 range 13 .. 13;
      HWFC_EN        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   ------------------
   -- CMD_Register --
   ------------------

   subtype CMD_CMDINDEX_Field is STM32_SVD.UInt6;
   subtype CMD_WAITRESP_Field is STM32_SVD.UInt2;
   subtype CMD_WAITINT_Field is STM32_SVD.Bit;
   subtype CMD_WAITPEND_Field is STM32_SVD.Bit;
   subtype CMD_CPSMEN_Field is STM32_SVD.Bit;
   subtype CMD_SDIOSuspend_Field is STM32_SVD.Bit;
   subtype CMD_ENCMDcompl_Field is STM32_SVD.Bit;
   subtype CMD_nIEN_Field is STM32_SVD.Bit;
   subtype CMD_CE_ATACMD_Field is STM32_SVD.Bit;

   --  command register
   type CMD_Register is record
      --  Command index
      CMDINDEX       : CMD_CMDINDEX_Field := 16#0#;
      --  Wait for response bits
      WAITRESP       : CMD_WAITRESP_Field := 16#0#;
      --  CPSM waits for interrupt request
      WAITINT        : CMD_WAITINT_Field := 16#0#;
      --  CPSM Waits for ends of data transfer (CmdPend internal signal).
      WAITPEND       : CMD_WAITPEND_Field := 16#0#;
      --  Command path state machine (CPSM) Enable bit
      CPSMEN         : CMD_CPSMEN_Field := 16#0#;
      --  SD I/O suspend command
      SDIOSuspend    : CMD_SDIOSuspend_Field := 16#0#;
      --  Enable CMD completion
      ENCMDcompl     : CMD_ENCMDcompl_Field := 16#0#;
      --  not Interrupt Enable
      nIEN           : CMD_nIEN_Field := 16#0#;
      --  CE-ATA command
      CE_ATACMD      : CMD_CE_ATACMD_Field := 16#0#;
      --  unspecified
      Reserved_15_31 : STM32_SVD.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMD_Register use record
      CMDINDEX       at 0 range 0 .. 5;
      WAITRESP       at 0 range 6 .. 7;
      WAITINT        at 0 range 8 .. 8;
      WAITPEND       at 0 range 9 .. 9;
      CPSMEN         at 0 range 10 .. 10;
      SDIOSuspend    at 0 range 11 .. 11;
      ENCMDcompl     at 0 range 12 .. 12;
      nIEN           at 0 range 13 .. 13;
      CE_ATACMD      at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   ----------------------
   -- RESPCMD_Register --
   ----------------------

   subtype RESPCMD_RESPCMD_Field is STM32_SVD.UInt6;

   --  command response register
   type RESPCMD_Register is record
      --  Response command index
      RESPCMD       : RESPCMD_RESPCMD_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32_SVD.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESPCMD_Register use record
      RESPCMD       at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   -------------------
   -- DLEN_Register --
   -------------------

   subtype DLEN_DATALENGTH_Field is STM32_SVD.UInt25;

   --  data length register
   type DLEN_Register is record
      --  Data length value
      DATALENGTH     : DLEN_DATALENGTH_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DLEN_Register use record
      DATALENGTH     at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --------------------
   -- DCTRL_Register --
   --------------------

   subtype DCTRL_DTEN_Field is STM32_SVD.Bit;
   subtype DCTRL_DTDIR_Field is STM32_SVD.Bit;
   subtype DCTRL_DTMODE_Field is STM32_SVD.Bit;
   subtype DCTRL_DMAEN_Field is STM32_SVD.Bit;
   subtype DCTRL_DBLOCKSIZE_Field is STM32_SVD.UInt4;
   subtype DCTRL_RWSTART_Field is STM32_SVD.Bit;
   subtype DCTRL_RWSTOP_Field is STM32_SVD.Bit;
   subtype DCTRL_RWMOD_Field is STM32_SVD.Bit;
   subtype DCTRL_SDIOEN_Field is STM32_SVD.Bit;

   --  data control register
   type DCTRL_Register is record
      --  DTEN
      DTEN           : DCTRL_DTEN_Field := 16#0#;
      --  Data transfer direction selection
      DTDIR          : DCTRL_DTDIR_Field := 16#0#;
      --  Data transfer mode selection 1: Stream or SDIO multibyte data
      --  transfer.
      DTMODE         : DCTRL_DTMODE_Field := 16#0#;
      --  DMA enable bit
      DMAEN          : DCTRL_DMAEN_Field := 16#0#;
      --  Data block size
      DBLOCKSIZE     : DCTRL_DBLOCKSIZE_Field := 16#0#;
      --  Read wait start
      RWSTART        : DCTRL_RWSTART_Field := 16#0#;
      --  Read wait stop
      RWSTOP         : DCTRL_RWSTOP_Field := 16#0#;
      --  Read wait mode
      RWMOD          : DCTRL_RWMOD_Field := 16#0#;
      --  SD I/O enable functions
      SDIOEN         : DCTRL_SDIOEN_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : STM32_SVD.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCTRL_Register use record
      DTEN           at 0 range 0 .. 0;
      DTDIR          at 0 range 1 .. 1;
      DTMODE         at 0 range 2 .. 2;
      DMAEN          at 0 range 3 .. 3;
      DBLOCKSIZE     at 0 range 4 .. 7;
      RWSTART        at 0 range 8 .. 8;
      RWSTOP         at 0 range 9 .. 9;
      RWMOD          at 0 range 10 .. 10;
      SDIOEN         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   ---------------------
   -- DCOUNT_Register --
   ---------------------

   subtype DCOUNT_DATACOUNT_Field is STM32_SVD.UInt25;

   --  data counter register
   type DCOUNT_Register is record
      --  Data count value
      DATACOUNT      : DCOUNT_DATACOUNT_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCOUNT_Register use record
      DATACOUNT      at 0 range 0 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   ------------------
   -- STA_Register --
   ------------------

   subtype STA_CCRCFAIL_Field is STM32_SVD.Bit;
   subtype STA_DCRCFAIL_Field is STM32_SVD.Bit;
   subtype STA_CTIMEOUT_Field is STM32_SVD.Bit;
   subtype STA_DTIMEOUT_Field is STM32_SVD.Bit;
   subtype STA_TXUNDERR_Field is STM32_SVD.Bit;
   subtype STA_RXOVERR_Field is STM32_SVD.Bit;
   subtype STA_CMDREND_Field is STM32_SVD.Bit;
   subtype STA_CMDSENT_Field is STM32_SVD.Bit;
   subtype STA_DATAEND_Field is STM32_SVD.Bit;
   subtype STA_STBITERR_Field is STM32_SVD.Bit;
   subtype STA_DBCKEND_Field is STM32_SVD.Bit;
   subtype STA_CMDACT_Field is STM32_SVD.Bit;
   subtype STA_TXACT_Field is STM32_SVD.Bit;
   subtype STA_RXACT_Field is STM32_SVD.Bit;
   subtype STA_TXFIFOHE_Field is STM32_SVD.Bit;
   subtype STA_RXFIFOHF_Field is STM32_SVD.Bit;
   subtype STA_TXFIFOF_Field is STM32_SVD.Bit;
   subtype STA_RXFIFOF_Field is STM32_SVD.Bit;
   subtype STA_TXFIFOE_Field is STM32_SVD.Bit;
   subtype STA_RXFIFOE_Field is STM32_SVD.Bit;
   subtype STA_TXDAVL_Field is STM32_SVD.Bit;
   subtype STA_RXDAVL_Field is STM32_SVD.Bit;
   subtype STA_SDIOIT_Field is STM32_SVD.Bit;
   subtype STA_CEATAEND_Field is STM32_SVD.Bit;

   --  status register
   type STA_Register is record
      --  Command response received (CRC check failed)
      CCRCFAIL       : STA_CCRCFAIL_Field := 16#0#;
      --  Data block sent/received (CRC check failed)
      DCRCFAIL       : STA_DCRCFAIL_Field := 16#0#;
      --  Command response timeout
      CTIMEOUT       : STA_CTIMEOUT_Field := 16#0#;
      --  Data timeout
      DTIMEOUT       : STA_DTIMEOUT_Field := 16#0#;
      --  Transmit FIFO underrun error
      TXUNDERR       : STA_TXUNDERR_Field := 16#0#;
      --  Received FIFO overrun error
      RXOVERR        : STA_RXOVERR_Field := 16#0#;
      --  Command response received (CRC check passed)
      CMDREND        : STA_CMDREND_Field := 16#0#;
      --  Command sent (no response required)
      CMDSENT        : STA_CMDSENT_Field := 16#0#;
      --  Data end (data counter, SDIDCOUNT, is zero)
      DATAEND        : STA_DATAEND_Field := 16#0#;
      --  Start bit not detected on all data signals in wide bus mode
      STBITERR       : STA_STBITERR_Field := 16#0#;
      --  Data block sent/received (CRC check passed)
      DBCKEND        : STA_DBCKEND_Field := 16#0#;
      --  Command transfer in progress
      CMDACT         : STA_CMDACT_Field := 16#0#;
      --  Data transmit in progress
      TXACT          : STA_TXACT_Field := 16#0#;
      --  Data receive in progress
      RXACT          : STA_RXACT_Field := 16#0#;
      --  Transmit FIFO half empty: at least 8 words can be written into the
      --  FIFO
      TXFIFOHE       : STA_TXFIFOHE_Field := 16#0#;
      --  Receive FIFO half full: there are at least 8 words in the FIFO
      RXFIFOHF       : STA_RXFIFOHF_Field := 16#0#;
      --  Transmit FIFO full
      TXFIFOF        : STA_TXFIFOF_Field := 16#0#;
      --  Receive FIFO full
      RXFIFOF        : STA_RXFIFOF_Field := 16#0#;
      --  Transmit FIFO empty
      TXFIFOE        : STA_TXFIFOE_Field := 16#0#;
      --  Receive FIFO empty
      RXFIFOE        : STA_RXFIFOE_Field := 16#0#;
      --  Data available in transmit FIFO
      TXDAVL         : STA_TXDAVL_Field := 16#0#;
      --  Data available in receive FIFO
      RXDAVL         : STA_RXDAVL_Field := 16#0#;
      --  SDIO interrupt received
      SDIOIT         : STA_SDIOIT_Field := 16#0#;
      --  CE-ATA command completion signal received for CMD61
      CEATAEND       : STA_CEATAEND_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STA_Register use record
      CCRCFAIL       at 0 range 0 .. 0;
      DCRCFAIL       at 0 range 1 .. 1;
      CTIMEOUT       at 0 range 2 .. 2;
      DTIMEOUT       at 0 range 3 .. 3;
      TXUNDERR       at 0 range 4 .. 4;
      RXOVERR        at 0 range 5 .. 5;
      CMDREND        at 0 range 6 .. 6;
      CMDSENT        at 0 range 7 .. 7;
      DATAEND        at 0 range 8 .. 8;
      STBITERR       at 0 range 9 .. 9;
      DBCKEND        at 0 range 10 .. 10;
      CMDACT         at 0 range 11 .. 11;
      TXACT          at 0 range 12 .. 12;
      RXACT          at 0 range 13 .. 13;
      TXFIFOHE       at 0 range 14 .. 14;
      RXFIFOHF       at 0 range 15 .. 15;
      TXFIFOF        at 0 range 16 .. 16;
      RXFIFOF        at 0 range 17 .. 17;
      TXFIFOE        at 0 range 18 .. 18;
      RXFIFOE        at 0 range 19 .. 19;
      TXDAVL         at 0 range 20 .. 20;
      RXDAVL         at 0 range 21 .. 21;
      SDIOIT         at 0 range 22 .. 22;
      CEATAEND       at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ------------------
   -- ICR_Register --
   ------------------

   subtype ICR_CCRCFAILC_Field is STM32_SVD.Bit;
   subtype ICR_DCRCFAILC_Field is STM32_SVD.Bit;
   subtype ICR_CTIMEOUTC_Field is STM32_SVD.Bit;
   subtype ICR_DTIMEOUTC_Field is STM32_SVD.Bit;
   subtype ICR_TXUNDERRC_Field is STM32_SVD.Bit;
   subtype ICR_RXOVERRC_Field is STM32_SVD.Bit;
   subtype ICR_CMDRENDC_Field is STM32_SVD.Bit;
   subtype ICR_CMDSENTC_Field is STM32_SVD.Bit;
   subtype ICR_DATAENDC_Field is STM32_SVD.Bit;
   subtype ICR_STBITERRC_Field is STM32_SVD.Bit;
   subtype ICR_DBCKENDC_Field is STM32_SVD.Bit;
   subtype ICR_SDIOITC_Field is STM32_SVD.Bit;
   subtype ICR_CEATAENDC_Field is STM32_SVD.Bit;

   --  interrupt clear register
   type ICR_Register is record
      --  CCRCFAIL flag clear bit
      CCRCFAILC      : ICR_CCRCFAILC_Field := 16#0#;
      --  DCRCFAIL flag clear bit
      DCRCFAILC      : ICR_DCRCFAILC_Field := 16#0#;
      --  CTIMEOUT flag clear bit
      CTIMEOUTC      : ICR_CTIMEOUTC_Field := 16#0#;
      --  DTIMEOUT flag clear bit
      DTIMEOUTC      : ICR_DTIMEOUTC_Field := 16#0#;
      --  TXUNDERR flag clear bit
      TXUNDERRC      : ICR_TXUNDERRC_Field := 16#0#;
      --  RXOVERR flag clear bit
      RXOVERRC       : ICR_RXOVERRC_Field := 16#0#;
      --  CMDREND flag clear bit
      CMDRENDC       : ICR_CMDRENDC_Field := 16#0#;
      --  CMDSENT flag clear bit
      CMDSENTC       : ICR_CMDSENTC_Field := 16#0#;
      --  DATAEND flag clear bit
      DATAENDC       : ICR_DATAENDC_Field := 16#0#;
      --  STBITERR flag clear bit
      STBITERRC      : ICR_STBITERRC_Field := 16#0#;
      --  DBCKEND flag clear bit
      DBCKENDC       : ICR_DBCKENDC_Field := 16#0#;
      --  unspecified
      Reserved_11_21 : STM32_SVD.UInt11 := 16#0#;
      --  SDIOIT flag clear bit
      SDIOITC        : ICR_SDIOITC_Field := 16#0#;
      --  CEATAEND flag clear bit
      CEATAENDC      : ICR_CEATAENDC_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      CCRCFAILC      at 0 range 0 .. 0;
      DCRCFAILC      at 0 range 1 .. 1;
      CTIMEOUTC      at 0 range 2 .. 2;
      DTIMEOUTC      at 0 range 3 .. 3;
      TXUNDERRC      at 0 range 4 .. 4;
      RXOVERRC       at 0 range 5 .. 5;
      CMDRENDC       at 0 range 6 .. 6;
      CMDSENTC       at 0 range 7 .. 7;
      DATAENDC       at 0 range 8 .. 8;
      STBITERRC      at 0 range 9 .. 9;
      DBCKENDC       at 0 range 10 .. 10;
      Reserved_11_21 at 0 range 11 .. 21;
      SDIOITC        at 0 range 22 .. 22;
      CEATAENDC      at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -------------------
   -- MASK_Register --
   -------------------

   subtype MASK_CCRCFAILIE_Field is STM32_SVD.Bit;
   subtype MASK_DCRCFAILIE_Field is STM32_SVD.Bit;
   subtype MASK_CTIMEOUTIE_Field is STM32_SVD.Bit;
   subtype MASK_DTIMEOUTIE_Field is STM32_SVD.Bit;
   subtype MASK_TXUNDERRIE_Field is STM32_SVD.Bit;
   subtype MASK_RXOVERRIE_Field is STM32_SVD.Bit;
   subtype MASK_CMDRENDIE_Field is STM32_SVD.Bit;
   subtype MASK_CMDSENTIE_Field is STM32_SVD.Bit;
   subtype MASK_DATAENDIE_Field is STM32_SVD.Bit;
   subtype MASK_STBITERRIE_Field is STM32_SVD.Bit;
   subtype MASK_DBCKENDIE_Field is STM32_SVD.Bit;
   subtype MASK_CMDACTIE_Field is STM32_SVD.Bit;
   subtype MASK_TXACTIE_Field is STM32_SVD.Bit;
   subtype MASK_RXACTIE_Field is STM32_SVD.Bit;
   subtype MASK_TXFIFOHEIE_Field is STM32_SVD.Bit;
   subtype MASK_RXFIFOHFIE_Field is STM32_SVD.Bit;
   subtype MASK_TXFIFOFIE_Field is STM32_SVD.Bit;
   subtype MASK_RXFIFOFIE_Field is STM32_SVD.Bit;
   subtype MASK_TXFIFOEIE_Field is STM32_SVD.Bit;
   subtype MASK_RXFIFOEIE_Field is STM32_SVD.Bit;
   subtype MASK_TXDAVLIE_Field is STM32_SVD.Bit;
   subtype MASK_RXDAVLIE_Field is STM32_SVD.Bit;
   subtype MASK_SDIOITIE_Field is STM32_SVD.Bit;
   subtype MASK_CEATAENDIE_Field is STM32_SVD.Bit;

   --  mask register
   type MASK_Register is record
      --  Command CRC fail interrupt enable
      CCRCFAILIE     : MASK_CCRCFAILIE_Field := 16#0#;
      --  Data CRC fail interrupt enable
      DCRCFAILIE     : MASK_DCRCFAILIE_Field := 16#0#;
      --  Command timeout interrupt enable
      CTIMEOUTIE     : MASK_CTIMEOUTIE_Field := 16#0#;
      --  Data timeout interrupt enable
      DTIMEOUTIE     : MASK_DTIMEOUTIE_Field := 16#0#;
      --  Tx FIFO underrun error interrupt enable
      TXUNDERRIE     : MASK_TXUNDERRIE_Field := 16#0#;
      --  Rx FIFO overrun error interrupt enable
      RXOVERRIE      : MASK_RXOVERRIE_Field := 16#0#;
      --  Command response received interrupt enable
      CMDRENDIE      : MASK_CMDRENDIE_Field := 16#0#;
      --  Command sent interrupt enable
      CMDSENTIE      : MASK_CMDSENTIE_Field := 16#0#;
      --  Data end interrupt enable
      DATAENDIE      : MASK_DATAENDIE_Field := 16#0#;
      --  Start bit error interrupt enable
      STBITERRIE     : MASK_STBITERRIE_Field := 16#0#;
      --  Data block end interrupt enable
      DBCKENDIE      : MASK_DBCKENDIE_Field := 16#0#;
      --  Command acting interrupt enable
      CMDACTIE       : MASK_CMDACTIE_Field := 16#0#;
      --  Data transmit acting interrupt enable
      TXACTIE        : MASK_TXACTIE_Field := 16#0#;
      --  Data receive acting interrupt enable
      RXACTIE        : MASK_RXACTIE_Field := 16#0#;
      --  Tx FIFO half empty interrupt enable
      TXFIFOHEIE     : MASK_TXFIFOHEIE_Field := 16#0#;
      --  Rx FIFO half full interrupt enable
      RXFIFOHFIE     : MASK_RXFIFOHFIE_Field := 16#0#;
      --  Tx FIFO full interrupt enable
      TXFIFOFIE      : MASK_TXFIFOFIE_Field := 16#0#;
      --  Rx FIFO full interrupt enable
      RXFIFOFIE      : MASK_RXFIFOFIE_Field := 16#0#;
      --  Tx FIFO empty interrupt enable
      TXFIFOEIE      : MASK_TXFIFOEIE_Field := 16#0#;
      --  Rx FIFO empty interrupt enable
      RXFIFOEIE      : MASK_RXFIFOEIE_Field := 16#0#;
      --  Data available in Tx FIFO interrupt enable
      TXDAVLIE       : MASK_TXDAVLIE_Field := 16#0#;
      --  Data available in Rx FIFO interrupt enable
      RXDAVLIE       : MASK_RXDAVLIE_Field := 16#0#;
      --  SDIO mode interrupt received interrupt enable
      SDIOITIE       : MASK_SDIOITIE_Field := 16#0#;
      --  CE-ATA command completion signal received interrupt enable
      CEATAENDIE     : MASK_CEATAENDIE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MASK_Register use record
      CCRCFAILIE     at 0 range 0 .. 0;
      DCRCFAILIE     at 0 range 1 .. 1;
      CTIMEOUTIE     at 0 range 2 .. 2;
      DTIMEOUTIE     at 0 range 3 .. 3;
      TXUNDERRIE     at 0 range 4 .. 4;
      RXOVERRIE      at 0 range 5 .. 5;
      CMDRENDIE      at 0 range 6 .. 6;
      CMDSENTIE      at 0 range 7 .. 7;
      DATAENDIE      at 0 range 8 .. 8;
      STBITERRIE     at 0 range 9 .. 9;
      DBCKENDIE      at 0 range 10 .. 10;
      CMDACTIE       at 0 range 11 .. 11;
      TXACTIE        at 0 range 12 .. 12;
      RXACTIE        at 0 range 13 .. 13;
      TXFIFOHEIE     at 0 range 14 .. 14;
      RXFIFOHFIE     at 0 range 15 .. 15;
      TXFIFOFIE      at 0 range 16 .. 16;
      RXFIFOFIE      at 0 range 17 .. 17;
      TXFIFOEIE      at 0 range 18 .. 18;
      RXFIFOEIE      at 0 range 19 .. 19;
      TXDAVLIE       at 0 range 20 .. 20;
      RXDAVLIE       at 0 range 21 .. 21;
      SDIOITIE       at 0 range 22 .. 22;
      CEATAENDIE     at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ----------------------
   -- FIFOCNT_Register --
   ----------------------

   subtype FIFOCNT_FIFOCOUNT_Field is STM32_SVD.UInt24;

   --  FIFO counter register
   type FIFOCNT_Register is record
      --  Remaining number of words to be written to or read from the FIFO.
      FIFOCOUNT      : FIFOCNT_FIFOCOUNT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FIFOCNT_Register use record
      FIFOCOUNT      at 0 range 0 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Secure digital input/output interface
   type SDIO_Peripheral is record
      --  power control register
      POWER   : POWER_Register;
      --  SDI clock control register
      CLKCR   : CLKCR_Register;
      --  argument register
      ARG     : STM32_SVD.Word;
      --  command register
      CMD     : CMD_Register;
      --  command response register
      RESPCMD : RESPCMD_Register;
      --  response 1..4 register
      RESP1   : STM32_SVD.Word;
      --  response 1..4 register
      RESP2   : STM32_SVD.Word;
      --  response 1..4 register
      RESP3   : STM32_SVD.Word;
      --  response 1..4 register
      RESP4   : STM32_SVD.Word;
      --  data timer register
      DTIMER  : STM32_SVD.Word;
      --  data length register
      DLEN    : DLEN_Register;
      --  data control register
      DCTRL   : DCTRL_Register;
      --  data counter register
      DCOUNT  : DCOUNT_Register;
      --  status register
      STA     : STA_Register;
      --  interrupt clear register
      ICR     : ICR_Register;
      --  mask register
      MASK    : MASK_Register;
      --  FIFO counter register
      FIFOCNT : FIFOCNT_Register;
      --  data FIFO register
      FIFO    : STM32_SVD.Word;
   end record
     with Volatile;

   for SDIO_Peripheral use record
      POWER   at 0 range 0 .. 31;
      CLKCR   at 4 range 0 .. 31;
      ARG     at 8 range 0 .. 31;
      CMD     at 12 range 0 .. 31;
      RESPCMD at 16 range 0 .. 31;
      RESP1   at 20 range 0 .. 31;
      RESP2   at 24 range 0 .. 31;
      RESP3   at 28 range 0 .. 31;
      RESP4   at 32 range 0 .. 31;
      DTIMER  at 36 range 0 .. 31;
      DLEN    at 40 range 0 .. 31;
      DCTRL   at 44 range 0 .. 31;
      DCOUNT  at 48 range 0 .. 31;
      STA     at 52 range 0 .. 31;
      ICR     at 56 range 0 .. 31;
      MASK    at 60 range 0 .. 31;
      FIFOCNT at 72 range 0 .. 31;
      FIFO    at 128 range 0 .. 31;
   end record;

   --  Secure digital input/output interface
   SDIO_Periph : aliased SDIO_Peripheral
     with Import, Address => System'To_Address (16#40012C00#);

end STM32_SVD.SDIO;
