--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.HSMCI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Control Register
   type HSMCI_HSMCI_CR_Register is record
      --  Write-only. Multi-Media Interface Enable
      MCIEN         : Boolean := False;
      --  Write-only. Multi-Media Interface Disable
      MCIDIS        : Boolean := False;
      --  Write-only. Power Save Mode Enable
      PWSEN         : Boolean := False;
      --  Write-only. Power Save Mode Disable
      PWSDIS        : Boolean := False;
      --  unspecified
      Reserved_4_6  : HAL.UInt3 := 16#0#;
      --  Write-only. Software Reset
      SWRST         : Boolean := False;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_CR_Register use record
      MCIEN         at 0 range 0 .. 0;
      MCIDIS        at 0 range 1 .. 1;
      PWSEN         at 0 range 2 .. 2;
      PWSDIS        at 0 range 3 .. 3;
      Reserved_4_6  at 0 range 4 .. 6;
      SWRST         at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype HSMCI_HSMCI_MR_CLKDIV_Field is HAL.UInt8;
   subtype HSMCI_HSMCI_MR_PWSDIV_Field is HAL.UInt3;

   --  Mode Register
   type HSMCI_HSMCI_MR_Register is record
      --  Clock Divider
      CLKDIV         : HSMCI_HSMCI_MR_CLKDIV_Field := 16#0#;
      --  Power Saving Divider
      PWSDIV         : HSMCI_HSMCI_MR_PWSDIV_Field := 16#0#;
      --  Read Proof Enable
      RDPROOF        : Boolean := False;
      --  Write Proof Enable
      WRPROOF        : Boolean := False;
      --  Force Byte Transfer
      FBYTE          : Boolean := False;
      --  Padding Value
      PADV           : Boolean := False;
      --  unspecified
      Reserved_15_15 : HAL.Bit := 16#0#;
      --  Clock divider is odd
      CLKODD         : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_MR_Register use record
      CLKDIV         at 0 range 0 .. 7;
      PWSDIV         at 0 range 8 .. 10;
      RDPROOF        at 0 range 11 .. 11;
      WRPROOF        at 0 range 12 .. 12;
      FBYTE          at 0 range 13 .. 13;
      PADV           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CLKODD         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype HSMCI_HSMCI_DTOR_DTOCYC_Field is HAL.UInt4;

   --  Data Timeout Multiplier
   type HSMCI_DTOR_DTOMUL_Field is
     (
      --  DTOCYC
      Val_1,
      --  DTOCYC x 16
      Val_16,
      --  DTOCYC x 128
      Val_128,
      --  DTOCYC x 256
      Val_256,
      --  DTOCYC x 1024
      Val_1024,
      --  DTOCYC x 4096
      Val_4096,
      --  DTOCYC x 65536
      Val_65536,
      --  DTOCYC x 1048576
      Val_1048576)
     with Size => 3;
   for HSMCI_DTOR_DTOMUL_Field use
     (Val_1 => 0,
      Val_16 => 1,
      Val_128 => 2,
      Val_256 => 3,
      Val_1024 => 4,
      Val_4096 => 5,
      Val_65536 => 6,
      Val_1048576 => 7);

   --  Data Timeout Register
   type HSMCI_HSMCI_DTOR_Register is record
      --  Data Timeout Cycle Number
      DTOCYC        : HSMCI_HSMCI_DTOR_DTOCYC_Field := 16#0#;
      --  Data Timeout Multiplier
      DTOMUL        : HSMCI_DTOR_DTOMUL_Field := SAM_SVD.HSMCI.Val_1;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_DTOR_Register use record
      DTOCYC        at 0 range 0 .. 3;
      DTOMUL        at 0 range 4 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  SDCard/SDIO Slot
   type HSMCI_SDCR_SDCSEL_Field is
     (
      --  Slot A is selected.
      Slota)
     with Size => 2;
   for HSMCI_SDCR_SDCSEL_Field use
     (Slota => 0);

   --  SDCard/SDIO Bus Width
   type HSMCI_SDCR_SDCBUS_Field is
     (
      --  1 bit
      Val_1,
      --  4 bits
      Val_4,
      --  8 bits
      Val_8)
     with Size => 2;
   for HSMCI_SDCR_SDCBUS_Field use
     (Val_1 => 0,
      Val_4 => 2,
      Val_8 => 3);

   --  SD/SDIO Card Register
   type HSMCI_HSMCI_SDCR_Register is record
      --  SDCard/SDIO Slot
      SDCSEL        : HSMCI_SDCR_SDCSEL_Field := SAM_SVD.HSMCI.Slota;
      --  unspecified
      Reserved_2_5  : HAL.UInt4 := 16#0#;
      --  SDCard/SDIO Bus Width
      SDCBUS        : HSMCI_SDCR_SDCBUS_Field := SAM_SVD.HSMCI.Val_1;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_SDCR_Register use record
      SDCSEL        at 0 range 0 .. 1;
      Reserved_2_5  at 0 range 2 .. 5;
      SDCBUS        at 0 range 6 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype HSMCI_HSMCI_CMDR_CMDNB_Field is HAL.UInt6;

   --  Response Type
   type HSMCI_CMDR_RSPTYP_Field is
     (
      --  No response
      Noresp,
      --  48-bit response
      Val_48_Bit,
      --  136-bit response
      Val_136_Bit,
      --  R1b response type
      R1B)
     with Size => 2;
   for HSMCI_CMDR_RSPTYP_Field use
     (Noresp => 0,
      Val_48_Bit => 1,
      Val_136_Bit => 2,
      R1B => 3);

   --  Special Command
   type HSMCI_CMDR_SPCMD_Field is
     (
      --  Not a special CMD.
      Std,
      --  Initialization CMD: 74 clock cycles for initialization sequence.
      Init,
      --  Synchronized CMD: Wait for the end of the current data block transfer
      --  before sending the pending command.
      Sync,
      --  CE-ATA Completion Signal disable Command. The host cancels the
      --  ability for the device to return a command completion signal on the
      --  command line.
      Ce_Ata,
      --  Interrupt command: Corresponds to the Interrupt Mode (CMD40).
      It_Cmd,
      --  Interrupt response: Corresponds to the Interrupt Mode (CMD40).
      It_Resp,
      --  Boot Operation Request. Start a boot operation mode, the host
      --  processor can read boot data from the MMC device directly.
      Bor,
      --  End Boot Operation. This command allows the host processor to
      --  terminate the boot operation mode.
      Ebo)
     with Size => 3;
   for HSMCI_CMDR_SPCMD_Field use
     (Std => 0,
      Init => 1,
      Sync => 2,
      Ce_Ata => 3,
      It_Cmd => 4,
      It_Resp => 5,
      Bor => 6,
      Ebo => 7);

   --  Open Drain Command
   type HSMCI_CMDR_OPDCMD_Field is
     (
      --  Push pull command.
      Pushpull,
      --  Open drain command.
      Opendrain)
     with Size => 1;
   for HSMCI_CMDR_OPDCMD_Field use
     (Pushpull => 0,
      Opendrain => 1);

   --  Max Latency for Command to Response
   type HSMCI_CMDR_MAXLAT_Field is
     (
      --  5-cycle max latency.
      Val_5,
      --  64-cycle max latency.
      Val_64)
     with Size => 1;
   for HSMCI_CMDR_MAXLAT_Field use
     (Val_5 => 0,
      Val_64 => 1);

   --  Transfer Command
   type HSMCI_CMDR_TRCMD_Field is
     (
      --  No data transfer
      No_Data,
      --  Start data transfer
      Start_Data,
      --  Stop data transfer
      Stop_Data)
     with Size => 2;
   for HSMCI_CMDR_TRCMD_Field use
     (No_Data => 0,
      Start_Data => 1,
      Stop_Data => 2);

   --  Transfer Direction
   type HSMCI_CMDR_TRDIR_Field is
     (
      --  Write.
      Write,
      --  Read.
      Read)
     with Size => 1;
   for HSMCI_CMDR_TRDIR_Field use
     (Write => 0,
      Read => 1);

   --  Transfer Type
   type HSMCI_CMDR_TRTYP_Field is
     (
      --  MMC/SD Card Single Block
      Single,
      --  MMC/SD Card Multiple Block
      Multiple,
      --  MMC Stream
      Stream,
      --  SDIO Byte
      Byte,
      --  SDIO Block
      Block)
     with Size => 3;
   for HSMCI_CMDR_TRTYP_Field use
     (Single => 0,
      Multiple => 1,
      Stream => 2,
      Byte => 4,
      Block => 5);

   --  SDIO Special Command
   type HSMCI_CMDR_IOSPCMD_Field is
     (
      --  Not an SDIO Special Command
      Std,
      --  SDIO Suspend Command
      Suspend,
      --  SDIO Resume Command
      Resume)
     with Size => 2;
   for HSMCI_CMDR_IOSPCMD_Field use
     (Std => 0,
      Suspend => 1,
      Resume => 2);

   --  ATA with Command Completion Signal
   type HSMCI_CMDR_ATACS_Field is
     (
      --  Normal operation mode.
      Normal,
      --  This bit indicates that a completion signal is expected within a
      --  programmed amount of time (HSMCI_CSTOR).
      Completion)
     with Size => 1;
   for HSMCI_CMDR_ATACS_Field use
     (Normal => 0,
      Completion => 1);

   --  Command Register
   type HSMCI_HSMCI_CMDR_Register is record
      --  Write-only. Command Number
      CMDNB          : HSMCI_HSMCI_CMDR_CMDNB_Field := 16#0#;
      --  Write-only. Response Type
      RSPTYP         : HSMCI_CMDR_RSPTYP_Field := SAM_SVD.HSMCI.Noresp;
      --  Write-only. Special Command
      SPCMD          : HSMCI_CMDR_SPCMD_Field := SAM_SVD.HSMCI.Std;
      --  Write-only. Open Drain Command
      OPDCMD         : HSMCI_CMDR_OPDCMD_Field := SAM_SVD.HSMCI.Pushpull;
      --  Write-only. Max Latency for Command to Response
      MAXLAT         : HSMCI_CMDR_MAXLAT_Field := SAM_SVD.HSMCI.Val_5;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Write-only. Transfer Command
      TRCMD          : HSMCI_CMDR_TRCMD_Field := SAM_SVD.HSMCI.No_Data;
      --  Write-only. Transfer Direction
      TRDIR          : HSMCI_CMDR_TRDIR_Field := SAM_SVD.HSMCI.Write;
      --  Write-only. Transfer Type
      TRTYP          : HSMCI_CMDR_TRTYP_Field := SAM_SVD.HSMCI.Single;
      --  unspecified
      Reserved_22_23 : HAL.UInt2 := 16#0#;
      --  Write-only. SDIO Special Command
      IOSPCMD        : HSMCI_CMDR_IOSPCMD_Field := SAM_SVD.HSMCI.Std;
      --  Write-only. ATA with Command Completion Signal
      ATACS          : HSMCI_CMDR_ATACS_Field := SAM_SVD.HSMCI.Normal;
      --  Write-only. Boot Operation Acknowledge
      BOOT_ACK       : Boolean := False;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_CMDR_Register use record
      CMDNB          at 0 range 0 .. 5;
      RSPTYP         at 0 range 6 .. 7;
      SPCMD          at 0 range 8 .. 10;
      OPDCMD         at 0 range 11 .. 11;
      MAXLAT         at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TRCMD          at 0 range 16 .. 17;
      TRDIR          at 0 range 18 .. 18;
      TRTYP          at 0 range 19 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      IOSPCMD        at 0 range 24 .. 25;
      ATACS          at 0 range 26 .. 26;
      BOOT_ACK       at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype HSMCI_HSMCI_BLKR_BCNT_Field is HAL.UInt16;
   subtype HSMCI_HSMCI_BLKR_BLKLEN_Field is HAL.UInt16;

   --  Block Register
   type HSMCI_HSMCI_BLKR_Register is record
      --  MMC/SDIO Block Count - SDIO Byte Count
      BCNT   : HSMCI_HSMCI_BLKR_BCNT_Field := 16#0#;
      --  Data Block Length
      BLKLEN : HSMCI_HSMCI_BLKR_BLKLEN_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_BLKR_Register use record
      BCNT   at 0 range 0 .. 15;
      BLKLEN at 0 range 16 .. 31;
   end record;

   subtype HSMCI_HSMCI_CSTOR_CSTOCYC_Field is HAL.UInt4;

   --  Completion Signal Timeout Multiplier
   type HSMCI_CSTOR_CSTOMUL_Field is
     (
      --  CSTOCYC x 1
      Val_1,
      --  CSTOCYC x 16
      Val_16,
      --  CSTOCYC x 128
      Val_128,
      --  CSTOCYC x 256
      Val_256,
      --  CSTOCYC x 1024
      Val_1024,
      --  CSTOCYC x 4096
      Val_4096,
      --  CSTOCYC x 65536
      Val_65536,
      --  CSTOCYC x 1048576
      Val_1048576)
     with Size => 3;
   for HSMCI_CSTOR_CSTOMUL_Field use
     (Val_1 => 0,
      Val_16 => 1,
      Val_128 => 2,
      Val_256 => 3,
      Val_1024 => 4,
      Val_4096 => 5,
      Val_65536 => 6,
      Val_1048576 => 7);

   --  Completion Signal Timeout Register
   type HSMCI_HSMCI_CSTOR_Register is record
      --  Completion Signal Timeout Cycle Number
      CSTOCYC       : HSMCI_HSMCI_CSTOR_CSTOCYC_Field := 16#0#;
      --  Completion Signal Timeout Multiplier
      CSTOMUL       : HSMCI_CSTOR_CSTOMUL_Field := SAM_SVD.HSMCI.Val_1;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_CSTOR_Register use record
      CSTOCYC       at 0 range 0 .. 3;
      CSTOMUL       at 0 range 4 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Response Register 0

   --  Response Register 0
   type HSMCI_HSMCI_RSPR_Registers is array (0 .. 3) of HAL.UInt32
     with Volatile;

   --  Status Register
   type HSMCI_HSMCI_SR_Register is record
      --  Read-only. Command Ready (cleared by writing in HSMCI_CMDR)
      CMDRDY         : Boolean;
      --  Read-only. Receiver Ready (cleared by reading HSMCI_RDR)
      RXRDY          : Boolean;
      --  Read-only. Transmit Ready (cleared by writing in HSMCI_TDR)
      TXRDY          : Boolean;
      --  Read-only. Data Block Ended (cleared on read)
      BLKE           : Boolean;
      --  Read-only. Data Transfer in Progress (cleared at the end of CRC16
      --  calculation)
      DTIP           : Boolean;
      --  Read-only. HSMCI Not Busy
      NOTBUSY        : Boolean;
      --  unspecified
      Reserved_6_7   : HAL.UInt2;
      --  Read-only. SDIO Interrupt for Slot A (cleared on read)
      SDIOIRQA       : Boolean;
      --  unspecified
      Reserved_9_11  : HAL.UInt3;
      --  Read-only. SDIO Read Wait Operation Status
      SDIOWAIT       : Boolean;
      --  Read-only. CE-ATA Completion Signal Received (cleared on read)
      CSRCV          : Boolean;
      --  unspecified
      Reserved_14_15 : HAL.UInt2;
      --  Read-only. Response Index Error (cleared by writing in HSMCI_CMDR)
      RINDE          : Boolean;
      --  Read-only. Response Direction Error (cleared by writing in
      --  HSMCI_CMDR)
      RDIRE          : Boolean;
      --  Read-only. Response CRC Error (cleared by writing in HSMCI_CMDR)
      RCRCE          : Boolean;
      --  Read-only. Response End Bit Error (cleared by writing in HSMCI_CMDR)
      RENDE          : Boolean;
      --  Read-only. Response Time-out Error (cleared by writing in HSMCI_CMDR)
      RTOE           : Boolean;
      --  Read-only. Data CRC Error (cleared on read)
      DCRCE          : Boolean;
      --  Read-only. Data Time-out Error (cleared on read)
      DTOE           : Boolean;
      --  Read-only. Completion Signal Time-out Error (cleared on read)
      CSTOE          : Boolean;
      --  Read-only. DMA Block Overrun Error (cleared on read)
      BLKOVRE        : Boolean;
      --  unspecified
      Reserved_25_25 : HAL.Bit;
      --  Read-only. FIFO empty flag
      FIFOEMPTY      : Boolean;
      --  Read-only. Transfer Done flag
      XFRDONE        : Boolean;
      --  Read-only. Boot Operation Acknowledge Received (cleared on read)
      ACKRCV         : Boolean;
      --  Read-only. Boot Operation Acknowledge Error (cleared on read)
      ACKRCVE        : Boolean;
      --  Read-only. Overrun (if FERRCTRL = 1, cleared by writing in HSMCI_CMDR
      --  or cleared on read if FERRCTRL = 0)
      OVRE           : Boolean;
      --  Read-only. Underrun (if FERRCTRL = 1, cleared by writing in
      --  HSMCI_CMDR or cleared on read if FERRCTRL = 0)
      UNRE           : Boolean;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_SR_Register use record
      CMDRDY         at 0 range 0 .. 0;
      RXRDY          at 0 range 1 .. 1;
      TXRDY          at 0 range 2 .. 2;
      BLKE           at 0 range 3 .. 3;
      DTIP           at 0 range 4 .. 4;
      NOTBUSY        at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      SDIOIRQA       at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      SDIOWAIT       at 0 range 12 .. 12;
      CSRCV          at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RINDE          at 0 range 16 .. 16;
      RDIRE          at 0 range 17 .. 17;
      RCRCE          at 0 range 18 .. 18;
      RENDE          at 0 range 19 .. 19;
      RTOE           at 0 range 20 .. 20;
      DCRCE          at 0 range 21 .. 21;
      DTOE           at 0 range 22 .. 22;
      CSTOE          at 0 range 23 .. 23;
      BLKOVRE        at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      FIFOEMPTY      at 0 range 26 .. 26;
      XFRDONE        at 0 range 27 .. 27;
      ACKRCV         at 0 range 28 .. 28;
      ACKRCVE        at 0 range 29 .. 29;
      OVRE           at 0 range 30 .. 30;
      UNRE           at 0 range 31 .. 31;
   end record;

   --  Interrupt Enable Register
   type HSMCI_HSMCI_IER_Register is record
      --  Write-only. Command Ready Interrupt Enable
      CMDRDY         : Boolean := False;
      --  Write-only. Receiver Ready Interrupt Enable
      RXRDY          : Boolean := False;
      --  Write-only. Transmit Ready Interrupt Enable
      TXRDY          : Boolean := False;
      --  Write-only. Data Block Ended Interrupt Enable
      BLKE           : Boolean := False;
      --  Write-only. Data Transfer in Progress Interrupt Enable
      DTIP           : Boolean := False;
      --  Write-only. Data Not Busy Interrupt Enable
      NOTBUSY        : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Write-only. SDIO Interrupt for Slot A Interrupt Enable
      SDIOIRQA       : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Write-only. SDIO Read Wait Operation Status Interrupt Enable
      SDIOWAIT       : Boolean := False;
      --  Write-only. Completion Signal Received Interrupt Enable
      CSRCV          : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Write-only. Response Index Error Interrupt Enable
      RINDE          : Boolean := False;
      --  Write-only. Response Direction Error Interrupt Enable
      RDIRE          : Boolean := False;
      --  Write-only. Response CRC Error Interrupt Enable
      RCRCE          : Boolean := False;
      --  Write-only. Response End Bit Error Interrupt Enable
      RENDE          : Boolean := False;
      --  Write-only. Response Time-out Error Interrupt Enable
      RTOE           : Boolean := False;
      --  Write-only. Data CRC Error Interrupt Enable
      DCRCE          : Boolean := False;
      --  Write-only. Data Time-out Error Interrupt Enable
      DTOE           : Boolean := False;
      --  Write-only. Completion Signal Timeout Error Interrupt Enable
      CSTOE          : Boolean := False;
      --  Write-only. DMA Block Overrun Error Interrupt Enable
      BLKOVRE        : Boolean := False;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  Write-only. FIFO empty Interrupt enable
      FIFOEMPTY      : Boolean := False;
      --  Write-only. Transfer Done Interrupt enable
      XFRDONE        : Boolean := False;
      --  Write-only. Boot Acknowledge Interrupt Enable
      ACKRCV         : Boolean := False;
      --  Write-only. Boot Acknowledge Error Interrupt Enable
      ACKRCVE        : Boolean := False;
      --  Write-only. Overrun Interrupt Enable
      OVRE           : Boolean := False;
      --  Write-only. Underrun Interrupt Enable
      UNRE           : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_IER_Register use record
      CMDRDY         at 0 range 0 .. 0;
      RXRDY          at 0 range 1 .. 1;
      TXRDY          at 0 range 2 .. 2;
      BLKE           at 0 range 3 .. 3;
      DTIP           at 0 range 4 .. 4;
      NOTBUSY        at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      SDIOIRQA       at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      SDIOWAIT       at 0 range 12 .. 12;
      CSRCV          at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RINDE          at 0 range 16 .. 16;
      RDIRE          at 0 range 17 .. 17;
      RCRCE          at 0 range 18 .. 18;
      RENDE          at 0 range 19 .. 19;
      RTOE           at 0 range 20 .. 20;
      DCRCE          at 0 range 21 .. 21;
      DTOE           at 0 range 22 .. 22;
      CSTOE          at 0 range 23 .. 23;
      BLKOVRE        at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      FIFOEMPTY      at 0 range 26 .. 26;
      XFRDONE        at 0 range 27 .. 27;
      ACKRCV         at 0 range 28 .. 28;
      ACKRCVE        at 0 range 29 .. 29;
      OVRE           at 0 range 30 .. 30;
      UNRE           at 0 range 31 .. 31;
   end record;

   --  Interrupt Disable Register
   type HSMCI_HSMCI_IDR_Register is record
      --  Write-only. Command Ready Interrupt Disable
      CMDRDY         : Boolean := False;
      --  Write-only. Receiver Ready Interrupt Disable
      RXRDY          : Boolean := False;
      --  Write-only. Transmit Ready Interrupt Disable
      TXRDY          : Boolean := False;
      --  Write-only. Data Block Ended Interrupt Disable
      BLKE           : Boolean := False;
      --  Write-only. Data Transfer in Progress Interrupt Disable
      DTIP           : Boolean := False;
      --  Write-only. Data Not Busy Interrupt Disable
      NOTBUSY        : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Write-only. SDIO Interrupt for Slot A Interrupt Disable
      SDIOIRQA       : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Write-only. SDIO Read Wait Operation Status Interrupt Disable
      SDIOWAIT       : Boolean := False;
      --  Write-only. Completion Signal received interrupt Disable
      CSRCV          : Boolean := False;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      --  Write-only. Response Index Error Interrupt Disable
      RINDE          : Boolean := False;
      --  Write-only. Response Direction Error Interrupt Disable
      RDIRE          : Boolean := False;
      --  Write-only. Response CRC Error Interrupt Disable
      RCRCE          : Boolean := False;
      --  Write-only. Response End Bit Error Interrupt Disable
      RENDE          : Boolean := False;
      --  Write-only. Response Time-out Error Interrupt Disable
      RTOE           : Boolean := False;
      --  Write-only. Data CRC Error Interrupt Disable
      DCRCE          : Boolean := False;
      --  Write-only. Data Time-out Error Interrupt Disable
      DTOE           : Boolean := False;
      --  Write-only. Completion Signal Time out Error Interrupt Disable
      CSTOE          : Boolean := False;
      --  Write-only. DMA Block Overrun Error Interrupt Disable
      BLKOVRE        : Boolean := False;
      --  unspecified
      Reserved_25_25 : HAL.Bit := 16#0#;
      --  Write-only. FIFO empty Interrupt Disable
      FIFOEMPTY      : Boolean := False;
      --  Write-only. Transfer Done Interrupt Disable
      XFRDONE        : Boolean := False;
      --  Write-only. Boot Acknowledge Interrupt Disable
      ACKRCV         : Boolean := False;
      --  Write-only. Boot Acknowledge Error Interrupt Disable
      ACKRCVE        : Boolean := False;
      --  Write-only. Overrun Interrupt Disable
      OVRE           : Boolean := False;
      --  Write-only. Underrun Interrupt Disable
      UNRE           : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_IDR_Register use record
      CMDRDY         at 0 range 0 .. 0;
      RXRDY          at 0 range 1 .. 1;
      TXRDY          at 0 range 2 .. 2;
      BLKE           at 0 range 3 .. 3;
      DTIP           at 0 range 4 .. 4;
      NOTBUSY        at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      SDIOIRQA       at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      SDIOWAIT       at 0 range 12 .. 12;
      CSRCV          at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RINDE          at 0 range 16 .. 16;
      RDIRE          at 0 range 17 .. 17;
      RCRCE          at 0 range 18 .. 18;
      RENDE          at 0 range 19 .. 19;
      RTOE           at 0 range 20 .. 20;
      DCRCE          at 0 range 21 .. 21;
      DTOE           at 0 range 22 .. 22;
      CSTOE          at 0 range 23 .. 23;
      BLKOVRE        at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      FIFOEMPTY      at 0 range 26 .. 26;
      XFRDONE        at 0 range 27 .. 27;
      ACKRCV         at 0 range 28 .. 28;
      ACKRCVE        at 0 range 29 .. 29;
      OVRE           at 0 range 30 .. 30;
      UNRE           at 0 range 31 .. 31;
   end record;

   --  Interrupt Mask Register
   type HSMCI_HSMCI_IMR_Register is record
      --  Read-only. Command Ready Interrupt Mask
      CMDRDY         : Boolean;
      --  Read-only. Receiver Ready Interrupt Mask
      RXRDY          : Boolean;
      --  Read-only. Transmit Ready Interrupt Mask
      TXRDY          : Boolean;
      --  Read-only. Data Block Ended Interrupt Mask
      BLKE           : Boolean;
      --  Read-only. Data Transfer in Progress Interrupt Mask
      DTIP           : Boolean;
      --  Read-only. Data Not Busy Interrupt Mask
      NOTBUSY        : Boolean;
      --  unspecified
      Reserved_6_7   : HAL.UInt2;
      --  Read-only. SDIO Interrupt for Slot A Interrupt Mask
      SDIOIRQA       : Boolean;
      --  unspecified
      Reserved_9_11  : HAL.UInt3;
      --  Read-only. SDIO Read Wait Operation Status Interrupt Mask
      SDIOWAIT       : Boolean;
      --  Read-only. Completion Signal Received Interrupt Mask
      CSRCV          : Boolean;
      --  unspecified
      Reserved_14_15 : HAL.UInt2;
      --  Read-only. Response Index Error Interrupt Mask
      RINDE          : Boolean;
      --  Read-only. Response Direction Error Interrupt Mask
      RDIRE          : Boolean;
      --  Read-only. Response CRC Error Interrupt Mask
      RCRCE          : Boolean;
      --  Read-only. Response End Bit Error Interrupt Mask
      RENDE          : Boolean;
      --  Read-only. Response Time-out Error Interrupt Mask
      RTOE           : Boolean;
      --  Read-only. Data CRC Error Interrupt Mask
      DCRCE          : Boolean;
      --  Read-only. Data Time-out Error Interrupt Mask
      DTOE           : Boolean;
      --  Read-only. Completion Signal Time-out Error Interrupt Mask
      CSTOE          : Boolean;
      --  Read-only. DMA Block Overrun Error Interrupt Mask
      BLKOVRE        : Boolean;
      --  unspecified
      Reserved_25_25 : HAL.Bit;
      --  Read-only. FIFO Empty Interrupt Mask
      FIFOEMPTY      : Boolean;
      --  Read-only. Transfer Done Interrupt Mask
      XFRDONE        : Boolean;
      --  Read-only. Boot Operation Acknowledge Received Interrupt Mask
      ACKRCV         : Boolean;
      --  Read-only. Boot Operation Acknowledge Error Interrupt Mask
      ACKRCVE        : Boolean;
      --  Read-only. Overrun Interrupt Mask
      OVRE           : Boolean;
      --  Read-only. Underrun Interrupt Mask
      UNRE           : Boolean;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_IMR_Register use record
      CMDRDY         at 0 range 0 .. 0;
      RXRDY          at 0 range 1 .. 1;
      TXRDY          at 0 range 2 .. 2;
      BLKE           at 0 range 3 .. 3;
      DTIP           at 0 range 4 .. 4;
      NOTBUSY        at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      SDIOIRQA       at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      SDIOWAIT       at 0 range 12 .. 12;
      CSRCV          at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      RINDE          at 0 range 16 .. 16;
      RDIRE          at 0 range 17 .. 17;
      RCRCE          at 0 range 18 .. 18;
      RENDE          at 0 range 19 .. 19;
      RTOE           at 0 range 20 .. 20;
      DCRCE          at 0 range 21 .. 21;
      DTOE           at 0 range 22 .. 22;
      CSTOE          at 0 range 23 .. 23;
      BLKOVRE        at 0 range 24 .. 24;
      Reserved_25_25 at 0 range 25 .. 25;
      FIFOEMPTY      at 0 range 26 .. 26;
      XFRDONE        at 0 range 27 .. 27;
      ACKRCV         at 0 range 28 .. 28;
      ACKRCVE        at 0 range 29 .. 29;
      OVRE           at 0 range 30 .. 30;
      UNRE           at 0 range 31 .. 31;
   end record;

   --  DMA Channel Read and Write Chunk Size
   type HSMCI_DMA_CHKSIZE_Field is
     (
      --  1 data available
      Val_1,
      --  2 data available
      Val_2,
      --  4 data available
      Val_4,
      --  8 data available
      Val_8,
      --  16 data available
      Val_16)
     with Size => 3;
   for HSMCI_DMA_CHKSIZE_Field use
     (Val_1 => 0,
      Val_2 => 1,
      Val_4 => 2,
      Val_8 => 3,
      Val_16 => 4);

   --  DMA Configuration Register
   type HSMCI_HSMCI_DMA_Register is record
      --  unspecified
      Reserved_0_3  : HAL.UInt4 := 16#0#;
      --  DMA Channel Read and Write Chunk Size
      CHKSIZE       : HSMCI_DMA_CHKSIZE_Field := SAM_SVD.HSMCI.Val_1;
      --  unspecified
      Reserved_7_7  : HAL.Bit := 16#0#;
      --  DMA Hardware Handshaking Enable
      DMAEN         : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_DMA_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      CHKSIZE       at 0 range 4 .. 6;
      Reserved_7_7  at 0 range 7 .. 7;
      DMAEN         at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  Configuration Register
   type HSMCI_HSMCI_CFG_Register is record
      --  HSMCI Internal FIFO control mode
      FIFOMODE       : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  Flow Error flag reset control mode
      FERRCTRL       : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  High Speed Mode
      HSMODE         : Boolean := False;
      --  unspecified
      Reserved_9_11  : HAL.UInt3 := 16#0#;
      --  Synchronize on the last block
      LSYNC          : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_CFG_Register use record
      FIFOMODE       at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      FERRCTRL       at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      HSMODE         at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      LSYNC          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  Write Protect Key
   type HSMCI_WPMR_WPKEY_Field is
     (
      --  Reset value for the field
      Hsmci_Wpmr_Wpkey_Field_Reset,
      --  Writing any other value in this field aborts the write operation of
      --  the WPEN bit.Always reads as 0.
      Passwd)
     with Size => 24;
   for HSMCI_WPMR_WPKEY_Field use
     (Hsmci_Wpmr_Wpkey_Field_Reset => 0,
      Passwd => 5063497);

   --  Write Protection Mode Register
   type HSMCI_HSMCI_WPMR_Register is record
      --  Write Protect Enable
      WPEN         : Boolean := False;
      --  unspecified
      Reserved_1_7 : HAL.UInt7 := 16#0#;
      --  Write Protect Key
      WPKEY        : HSMCI_WPMR_WPKEY_Field := Hsmci_Wpmr_Wpkey_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_WPMR_Register use record
      WPEN         at 0 range 0 .. 0;
      Reserved_1_7 at 0 range 1 .. 7;
      WPKEY        at 0 range 8 .. 31;
   end record;

   subtype HSMCI_HSMCI_WPSR_WPVSRC_Field is HAL.UInt16;

   --  Write Protection Status Register
   type HSMCI_HSMCI_WPSR_Register is record
      --  Read-only. Write Protection Violation Status
      WPVS           : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. Write Protection Violation Source
      WPVSRC         : HSMCI_HSMCI_WPSR_WPVSRC_Field;
      --  unspecified
      Reserved_24_31 : HAL.UInt8;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_WPSR_Register use record
      WPVS           at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      WPVSRC         at 0 range 8 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype HSMCI_HSMCI_VERSION_VERSION_Field is HAL.UInt12;
   subtype HSMCI_HSMCI_VERSION_MFN_Field is HAL.UInt3;

   --  Version Register
   type HSMCI_HSMCI_VERSION_Register is record
      --  Read-only. Hardware Module Version
      VERSION        : HSMCI_HSMCI_VERSION_VERSION_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. Metal Fix Number
      MFN            : HSMCI_HSMCI_VERSION_MFN_Field;
      --  unspecified
      Reserved_19_31 : HAL.UInt13;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for HSMCI_HSMCI_VERSION_Register use record
      VERSION        at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MFN            at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  FIFO Memory Aperture0 0

   --  FIFO Memory Aperture0 0
   type HSMCI_HSMCI_FIFO_Registers is array (0 .. 255) of HAL.UInt32
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  High Speed MultiMedia Card Interface
   type HSMCI_Peripheral is record
      --  Control Register
      HSMCI_CR      : aliased HSMCI_HSMCI_CR_Register;
      --  Mode Register
      HSMCI_MR      : aliased HSMCI_HSMCI_MR_Register;
      --  Data Timeout Register
      HSMCI_DTOR    : aliased HSMCI_HSMCI_DTOR_Register;
      --  SD/SDIO Card Register
      HSMCI_SDCR    : aliased HSMCI_HSMCI_SDCR_Register;
      --  Argument Register
      HSMCI_ARGR    : aliased HAL.UInt32;
      --  Command Register
      HSMCI_CMDR    : aliased HSMCI_HSMCI_CMDR_Register;
      --  Block Register
      HSMCI_BLKR    : aliased HSMCI_HSMCI_BLKR_Register;
      --  Completion Signal Timeout Register
      HSMCI_CSTOR   : aliased HSMCI_HSMCI_CSTOR_Register;
      --  Response Register 0
      HSMCI_RSPR    : aliased HSMCI_HSMCI_RSPR_Registers;
      --  Receive Data Register
      HSMCI_RDR     : aliased HAL.UInt32;
      --  Transmit Data Register
      HSMCI_TDR     : aliased HAL.UInt32;
      --  Status Register
      HSMCI_SR      : aliased HSMCI_HSMCI_SR_Register;
      --  Interrupt Enable Register
      HSMCI_IER     : aliased HSMCI_HSMCI_IER_Register;
      --  Interrupt Disable Register
      HSMCI_IDR     : aliased HSMCI_HSMCI_IDR_Register;
      --  Interrupt Mask Register
      HSMCI_IMR     : aliased HSMCI_HSMCI_IMR_Register;
      --  DMA Configuration Register
      HSMCI_DMA     : aliased HSMCI_HSMCI_DMA_Register;
      --  Configuration Register
      HSMCI_CFG     : aliased HSMCI_HSMCI_CFG_Register;
      --  Write Protection Mode Register
      HSMCI_WPMR    : aliased HSMCI_HSMCI_WPMR_Register;
      --  Write Protection Status Register
      HSMCI_WPSR    : aliased HSMCI_HSMCI_WPSR_Register;
      --  Version Register
      HSMCI_VERSION : aliased HSMCI_HSMCI_VERSION_Register;
      --  FIFO Memory Aperture0 0
      HSMCI_FIFO    : aliased HSMCI_HSMCI_FIFO_Registers;
   end record
     with Volatile;

   for HSMCI_Peripheral use record
      HSMCI_CR      at 16#0# range 0 .. 31;
      HSMCI_MR      at 16#4# range 0 .. 31;
      HSMCI_DTOR    at 16#8# range 0 .. 31;
      HSMCI_SDCR    at 16#C# range 0 .. 31;
      HSMCI_ARGR    at 16#10# range 0 .. 31;
      HSMCI_CMDR    at 16#14# range 0 .. 31;
      HSMCI_BLKR    at 16#18# range 0 .. 31;
      HSMCI_CSTOR   at 16#1C# range 0 .. 31;
      HSMCI_RSPR    at 16#20# range 0 .. 127;
      HSMCI_RDR     at 16#30# range 0 .. 31;
      HSMCI_TDR     at 16#34# range 0 .. 31;
      HSMCI_SR      at 16#40# range 0 .. 31;
      HSMCI_IER     at 16#44# range 0 .. 31;
      HSMCI_IDR     at 16#48# range 0 .. 31;
      HSMCI_IMR     at 16#4C# range 0 .. 31;
      HSMCI_DMA     at 16#50# range 0 .. 31;
      HSMCI_CFG     at 16#54# range 0 .. 31;
      HSMCI_WPMR    at 16#E4# range 0 .. 31;
      HSMCI_WPSR    at 16#E8# range 0 .. 31;
      HSMCI_VERSION at 16#FC# range 0 .. 31;
      HSMCI_FIFO    at 16#200# range 0 .. 8191;
   end record;

   --  High Speed MultiMedia Card Interface
   HSMCI_Periph : aliased HSMCI_Peripheral
     with Import, Address => System'To_Address (16#40000000#);

end SAM_SVD.HSMCI;
