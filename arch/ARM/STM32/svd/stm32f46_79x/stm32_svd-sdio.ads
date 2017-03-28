--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.SDIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  PWRCTRL
   type POWER_PWRCTRL_Field is
     (
      --  The clock to card is stopped.
      Power_Off,
      --  The card is clocked.
      Power_On)
     with Size => 2;
   for POWER_PWRCTRL_Field use
     (Power_Off => 0,
      Power_On => 3);

   --  power control register
   type POWER_Register is record
      --  PWRCTRL
      PWRCTRL       : POWER_PWRCTRL_Field := STM32_SVD.SDIO.Power_Off;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for POWER_Register use record
      PWRCTRL       at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype CLKCR_CLKDIV_Field is HAL.UInt8;

   --  Wide bus mode enable bit
   type CLKCR_WIDBUS_Field is
     (
      --  Default bus mode: SDMMC_D0 is used.
      Bus_Wide_1B,
      --  4-wide bus mode: SDMMC_D[3:0] used.
      Bus_Wide_4B,
      --  8-wide bus mode: SDMMC_D[7:0] used.
      Bus_Wide_8B)
     with Size => 2;
   for CLKCR_WIDBUS_Field use
     (Bus_Wide_1B => 0,
      Bus_Wide_4B => 1,
      Bus_Wide_8B => 2);

   --  SDIO_CK dephasing selection bit
   type CLKCR_NEGEDGE_Field is
     (
      --  Cmd and Data changed on the SDMMCCLK falling edge succeeding the
      --  rising edge of SDMMC_CK.
      Edge_Rising,
      --  Cmd and Data changed on the SDMMC_CK falling edge.
      Edge_Falling)
     with Size => 1;
   for CLKCR_NEGEDGE_Field use
     (Edge_Rising => 0,
      Edge_Falling => 1);

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
      WIDBUS         : CLKCR_WIDBUS_Field := STM32_SVD.SDIO.Bus_Wide_1B;
      --  SDIO_CK dephasing selection bit
      NEGEDGE        : CLKCR_NEGEDGE_Field := STM32_SVD.SDIO.Edge_Rising;
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

   subtype CMD_CMDINDEX_Field is HAL.UInt6;

   --  Wait for response bits
   type CMD_WAITRESP_Field is
     (
      --  No response, expect CMDSENT flag.
      No_Response,
      --  Short response, expect CMDREND or CCRCFAIL flag.
      Short_Response,
      --  Long response, expect CMDREND or CCRCFAIL flag.
      Long_Response)
     with Size => 2;
   for CMD_WAITRESP_Field use
     (No_Response => 0,
      Short_Response => 1,
      Long_Response => 3);

   --  command register
   type CMD_Register is record
      --  Command index
      CMDINDEX       : CMD_CMDINDEX_Field := 16#0#;
      --  Wait for response bits
      WAITRESP       : CMD_WAITRESP_Field := STM32_SVD.SDIO.No_Response;
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

   --  Data transfer direction selection
   type DCTRL_DTDIR_Field is
     (
      --  Data is sent to the card
      Controller_To_Card,
      --  Data is read from the card
      Card_To_Controller)
     with Size => 1;
   for DCTRL_DTDIR_Field use
     (Controller_To_Card => 0,
      Card_To_Controller => 1);

   --  Data transfer mode selection 1: Stream or SDIO multibyte data transfer.
   type DCTRL_DTMODE_Field is
     (
      --  Block data transfer
      Block,
      --  Stream or SDIO multibyte data transfer
      Stream)
     with Size => 1;
   for DCTRL_DTMODE_Field use
     (Block => 0,
      Stream => 1);

   --  Data block size
   type DCTRL_DBLOCKSIZE_Field is
     (
      --  Block length = 2**0 = 1 byte
      Block_1B,
      --  Block length = 2**1 = 2 byte
      Block_2B,
      --  Block length = 2**2 = 4 byte
      Block_4B,
      --  Block length = 2**3 = 8 byte
      Block_8B,
      --  Block length = 2**4 = 16 byte
      Block_16B,
      --  Block length = 2**5 = 32 byte
      Block_32B,
      --  Block length = 2**6 = 64 byte
      Block_64B,
      --  Block length = 2**7 = 128 byte
      Block_128B,
      --  Block length = 2**8 = 256 byte
      Block_256B,
      --  Block length = 2**9 = 512 byte
      Block_512B,
      --  Block length = 2**10 = 1024 byte
      Block_1024B,
      --  Block length = 2**11 = 2048 byte
      Block_2048B,
      --  Block length = 2**12 = 4096 byte
      Block_4096B,
      --  Block length = 2**13 = 8192 byte
      Block_8192B,
      --  Block length = 2**14 = 16384 byte
      Block_16384B)
     with Size => 4;
   for DCTRL_DBLOCKSIZE_Field use
     (Block_1B => 0,
      Block_2B => 1,
      Block_4B => 2,
      Block_8B => 3,
      Block_16B => 4,
      Block_32B => 5,
      Block_64B => 6,
      Block_128B => 7,
      Block_256B => 8,
      Block_512B => 9,
      Block_1024B => 10,
      Block_2048B => 11,
      Block_4096B => 12,
      Block_8192B => 13,
      Block_16384B => 14);

   --  data control register
   type DCTRL_Register is record
      --  DTEN
      DTEN           : Boolean := False;
      --  Data transfer direction selection
      DTDIR          : DCTRL_DTDIR_Field := STM32_SVD.SDIO.Controller_To_Card;
      --  Data transfer mode selection 1: Stream or SDIO multibyte data
      --  transfer.
      DTMODE         : DCTRL_DTMODE_Field := STM32_SVD.SDIO.Block;
      --  DMA enable bit
      DMAEN          : Boolean := False;
      --  Data block size
      DBLOCKSIZE     : DCTRL_DBLOCKSIZE_Field := STM32_SVD.SDIO.Block_1B;
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
      Reserved_24_31 : HAL.UInt8;
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
      Reserved_24_31 : HAL.UInt8 := 16#0#;
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
      Reserved_24_31 : HAL.UInt8 := 16#0#;
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

   subtype FIFOCNT_FIFOCOUNT_Field is HAL.UInt24;

   --  FIFO counter register
   type FIFOCNT_Register is record
      --  Read-only. Remaining number of words to be written to or read from
      --  the FIFO.
      FIFOCOUNT      : FIFOCNT_FIFOCOUNT_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
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
      POWER   : aliased POWER_Register;
      --  SDI clock control register
      CLKCR   : aliased CLKCR_Register;
      --  argument register
      ARG     : aliased HAL.UInt32;
      --  command register
      CMD     : aliased CMD_Register;
      --  command response register
      RESPCMD : aliased RESPCMD_Register;
      --  response 1..4 register
      RESP1   : aliased HAL.UInt32;
      --  response 1..4 register
      RESP2   : aliased HAL.UInt32;
      --  response 1..4 register
      RESP3   : aliased HAL.UInt32;
      --  response 1..4 register
      RESP4   : aliased HAL.UInt32;
      --  data timer register
      DTIMER  : aliased HAL.UInt32;
      --  data length register
      DLEN    : aliased DLEN_Register;
      --  data control register
      DCTRL   : aliased DCTRL_Register;
      --  data counter register
      DCOUNT  : aliased DCOUNT_Register;
      --  status register
      STA     : aliased STA_Register;
      --  interrupt clear register
      ICR     : aliased ICR_Register;
      --  mask register
      MASK    : aliased MASK_Register;
      --  FIFO counter register
      FIFOCNT : aliased FIFOCNT_Register;
      --  data FIFO register
      FIFO    : aliased HAL.UInt32;
   end record
     with Volatile;

   for SDIO_Peripheral use record
      POWER   at 16#0# range 0 .. 31;
      CLKCR   at 16#4# range 0 .. 31;
      ARG     at 16#8# range 0 .. 31;
      CMD     at 16#C# range 0 .. 31;
      RESPCMD at 16#10# range 0 .. 31;
      RESP1   at 16#14# range 0 .. 31;
      RESP2   at 16#18# range 0 .. 31;
      RESP3   at 16#1C# range 0 .. 31;
      RESP4   at 16#20# range 0 .. 31;
      DTIMER  at 16#24# range 0 .. 31;
      DLEN    at 16#28# range 0 .. 31;
      DCTRL   at 16#2C# range 0 .. 31;
      DCOUNT  at 16#30# range 0 .. 31;
      STA     at 16#34# range 0 .. 31;
      ICR     at 16#38# range 0 .. 31;
      MASK    at 16#3C# range 0 .. 31;
      FIFOCNT at 16#48# range 0 .. 31;
      FIFO    at 16#80# range 0 .. 31;
   end record;

   --  Secure digital input/output interface
   SDIO_Periph : aliased SDIO_Peripheral
     with Import, Address => System'To_Address (16#40012C00#);

end STM32_SVD.SDIO;
