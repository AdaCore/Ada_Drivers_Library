--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.SDIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --------------------
   -- POWER_Register --
   --------------------

   subtype POWER_PWRCTRL_Field is HAL.UInt2;

   --  power control register
   type POWER_Register is record
      --  PWRCTRL
      PWRCTRL       : POWER_PWRCTRL_Field := 16#0#;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
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

   subtype CLKCR_CLKDIV_Field is HAL.Byte;
   subtype CLKCR_WIDBUS_Field is HAL.UInt2;

   --  SDI clock control register
   type CLKCR_Register is record
      --  Clock divide factor
      CLKDIV         : CLKCR_CLKDIV_Field := 16#0#;
      --  Clock enable bit
      CLKEN          : Boolean := False;
      --  Power saving configuration bit
      PWRSAV         : Boolean := False;
      --  Clock divider bypass enable bit
      BYPASS         : Boolean := False;
      --  Wide bus mode enable bit
      WIDBUS         : CLKCR_WIDBUS_Field := 16#0#;
      --  SDIO_CK dephasing selection bit
      NEGEDGE        : Boolean := False;
      --  HW Flow Control enable
      HWFC_EN        : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
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

   subtype CMD_CMDINDEX_Field is HAL.UInt6;
   subtype CMD_WAITRESP_Field is HAL.UInt2;

   --  command register
   type CMD_Register is record
      --  Command index
      CMDINDEX       : CMD_CMDINDEX_Field := 16#0#;
      --  Wait for response bits
      WAITRESP       : CMD_WAITRESP_Field := 16#0#;
      --  CPSM waits for interrupt request
      WAITINT        : Boolean := False;
      --  CPSM Waits for ends of data transfer (CmdPend internal signal).
      WAITPEND       : Boolean := False;
      --  Command path state machine (CPSM) Enable bit
      CPSMEN         : Boolean := False;
      --  SD I/O suspend command
      SDIOSuspend    : Boolean := False;
      --  Enable CMD completion
      ENCMDcompl     : Boolean := False;
      --  not Interrupt Enable
      nIEN           : Boolean := False;
      --  CE-ATA command
      CE_ATACMD      : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
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

   subtype RESPCMD_RESPCMD_Field is HAL.UInt6;

   --  command response register
   type RESPCMD_Register is record
      --  Read-only. Response command index
      RESPCMD       : RESPCMD_RESPCMD_Field;
      --  unspecified
      Reserved_6_31 : HAL.UInt26;
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

   subtype DLEN_DATALENGTH_Field is HAL.UInt25;

   --  data length register
   type DLEN_Register is record
      --  Data length value
      DATALENGTH     : DLEN_DATALENGTH_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
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

   subtype DCTRL_DBLOCKSIZE_Field is HAL.UInt4;

   --  data control register
   type DCTRL_Register is record
      --  DTEN
      DTEN           : Boolean := False;
      --  Data transfer direction selection
      DTDIR          : Boolean := False;
      --  Data transfer mode selection 1: Stream or SDIO multibyte data
      --  transfer.
      DTMODE         : Boolean := False;
      --  DMA enable bit
      DMAEN          : Boolean := False;
      --  Data block size
      DBLOCKSIZE     : DCTRL_DBLOCKSIZE_Field := 16#0#;
      --  Read wait start
      RWSTART        : Boolean := False;
      --  Read wait stop
      RWSTOP         : Boolean := False;
      --  Read wait mode
      RWMOD          : Boolean := False;
      --  SD I/O enable functions
      SDIOEN         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
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

   subtype DCOUNT_DATACOUNT_Field is HAL.UInt25;

   --  data counter register
   type DCOUNT_Register is record
      --  Read-only. Data count value
      DATACOUNT      : DCOUNT_DATACOUNT_Field;
      --  unspecified
      Reserved_25_31 : HAL.UInt7;
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

   --  status register
   type STA_Register is record
      --  Read-only. Command response received (CRC check failed)
      CCRCFAIL       : Boolean;
      --  Read-only. Data block sent/received (CRC check failed)
      DCRCFAIL       : Boolean;
      --  Read-only. Command response timeout
      CTIMEOUT       : Boolean;
      --  Read-only. Data timeout
      DTIMEOUT       : Boolean;
      --  Read-only. Transmit FIFO underrun error
      TXUNDERR       : Boolean;
      --  Read-only. Received FIFO overrun error
      RXOVERR        : Boolean;
      --  Read-only. Command response received (CRC check passed)
      CMDREND        : Boolean;
      --  Read-only. Command sent (no response required)
      CMDSENT        : Boolean;
      --  Read-only. Data end (data counter, SDIDCOUNT, is zero)
      DATAEND        : Boolean;
      --  Read-only. Start bit not detected on all data signals in wide bus
      --  mode
      STBITERR       : Boolean;
      --  Read-only. Data block sent/received (CRC check passed)
      DBCKEND        : Boolean;
      --  Read-only. Command transfer in progress
      CMDACT         : Boolean;
      --  Read-only. Data transmit in progress
      TXACT          : Boolean;
      --  Read-only. Data receive in progress
      RXACT          : Boolean;
      --  Read-only. Transmit FIFO half empty: at least 8 words can be written
      --  into the FIFO
      TXFIFOHE       : Boolean;
      --  Read-only. Receive FIFO half full: there are at least 8 words in the
      --  FIFO
      RXFIFOHF       : Boolean;
      --  Read-only. Transmit FIFO full
      TXFIFOF        : Boolean;
      --  Read-only. Receive FIFO full
      RXFIFOF        : Boolean;
      --  Read-only. Transmit FIFO empty
      TXFIFOE        : Boolean;
      --  Read-only. Receive FIFO empty
      RXFIFOE        : Boolean;
      --  Read-only. Data available in transmit FIFO
      TXDAVL         : Boolean;
      --  Read-only. Data available in receive FIFO
      RXDAVL         : Boolean;
      --  Read-only. SDIO interrupt received
      SDIOIT         : Boolean;
      --  Read-only. CE-ATA command completion signal received for CMD61
      CEATAEND       : Boolean;
      --  unspecified
      Reserved_24_31 : HAL.Byte;
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

   --  interrupt clear register
   type ICR_Register is record
      --  CCRCFAIL flag clear bit
      CCRCFAILC      : Boolean := False;
      --  DCRCFAIL flag clear bit
      DCRCFAILC      : Boolean := False;
      --  CTIMEOUT flag clear bit
      CTIMEOUTC      : Boolean := False;
      --  DTIMEOUT flag clear bit
      DTIMEOUTC      : Boolean := False;
      --  TXUNDERR flag clear bit
      TXUNDERRC      : Boolean := False;
      --  RXOVERR flag clear bit
      RXOVERRC       : Boolean := False;
      --  CMDREND flag clear bit
      CMDRENDC       : Boolean := False;
      --  CMDSENT flag clear bit
      CMDSENTC       : Boolean := False;
      --  DATAEND flag clear bit
      DATAENDC       : Boolean := False;
      --  STBITERR flag clear bit
      STBITERRC      : Boolean := False;
      --  DBCKEND flag clear bit
      DBCKENDC       : Boolean := False;
      --  unspecified
      Reserved_11_21 : HAL.UInt11 := 16#0#;
      --  SDIOIT flag clear bit
      SDIOITC        : Boolean := False;
      --  CEATAEND flag clear bit
      CEATAENDC      : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.Byte := 16#0#;
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

   --  mask register
   type MASK_Register is record
      --  Command CRC fail interrupt enable
      CCRCFAILIE     : Boolean := False;
      --  Data CRC fail interrupt enable
      DCRCFAILIE     : Boolean := False;
      --  Command timeout interrupt enable
      CTIMEOUTIE     : Boolean := False;
      --  Data timeout interrupt enable
      DTIMEOUTIE     : Boolean := False;
      --  Tx FIFO underrun error interrupt enable
      TXUNDERRIE     : Boolean := False;
      --  Rx FIFO overrun error interrupt enable
      RXOVERRIE      : Boolean := False;
      --  Command response received interrupt enable
      CMDRENDIE      : Boolean := False;
      --  Command sent interrupt enable
      CMDSENTIE      : Boolean := False;
      --  Data end interrupt enable
      DATAENDIE      : Boolean := False;
      --  Start bit error interrupt enable
      STBITERRIE     : Boolean := False;
      --  Data block end interrupt enable
      DBCKENDIE      : Boolean := False;
      --  Command acting interrupt enable
      CMDACTIE       : Boolean := False;
      --  Data transmit acting interrupt enable
      TXACTIE        : Boolean := False;
      --  Data receive acting interrupt enable
      RXACTIE        : Boolean := False;
      --  Tx FIFO half empty interrupt enable
      TXFIFOHEIE     : Boolean := False;
      --  Rx FIFO half full interrupt enable
      RXFIFOHFIE     : Boolean := False;
      --  Tx FIFO full interrupt enable
      TXFIFOFIE      : Boolean := False;
      --  Rx FIFO full interrupt enable
      RXFIFOFIE      : Boolean := False;
      --  Tx FIFO empty interrupt enable
      TXFIFOEIE      : Boolean := False;
      --  Rx FIFO empty interrupt enable
      RXFIFOEIE      : Boolean := False;
      --  Data available in Tx FIFO interrupt enable
      TXDAVLIE       : Boolean := False;
      --  Data available in Rx FIFO interrupt enable
      RXDAVLIE       : Boolean := False;
      --  SDIO mode interrupt received interrupt enable
      SDIOITIE       : Boolean := False;
      --  CE-ATA command completion signal received interrupt enable
      CEATAENDIE     : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.Byte := 16#0#;
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

   subtype FIFOCNT_FIFOCOUNT_Field is HAL.UInt24;

   --  FIFO counter register
   type FIFOCNT_Register is record
      --  Read-only. Remaining number of words to be written to or read from
      --  the FIFO.
      FIFOCOUNT      : FIFOCNT_FIFOCOUNT_Field;
      --  unspecified
      Reserved_24_31 : HAL.Byte;
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
      ARG     : HAL.Word;
      --  command register
      CMD     : CMD_Register;
      --  command response register
      RESPCMD : RESPCMD_Register;
      --  response 1..4 register
      RESP1   : HAL.Word;
      --  response 1..4 register
      RESP2   : HAL.Word;
      --  response 1..4 register
      RESP3   : HAL.Word;
      --  response 1..4 register
      RESP4   : HAL.Word;
      --  data timer register
      DTIMER  : HAL.Word;
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
      FIFO    : HAL.Word;
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
     with Import, Address => SDIO_Base;

end STM32_SVD.SDIO;
