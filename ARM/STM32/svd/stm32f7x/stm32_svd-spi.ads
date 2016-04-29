--  This spec has been automatically generated from STM32F7x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.SPI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ------------------
   -- CR1_Register --
   ------------------

   subtype CR1_BR_Field is HAL.UInt3;

   --  control register 1
   type CR1_Register is record
      --  Clock phase
      CPHA           : Boolean := False;
      --  Clock polarity
      CPOL           : Boolean := False;
      --  Master selection
      MSTR           : Boolean := False;
      --  Baud rate control
      BR             : CR1_BR_Field := 16#0#;
      --  SPI enable
      SPE            : Boolean := False;
      --  Frame format
      LSBFIRST       : Boolean := False;
      --  Internal slave select
      SSI            : Boolean := False;
      --  Software slave management
      SSM            : Boolean := False;
      --  Receive only
      RXONLY         : Boolean := False;
      --  CRC length
      DFF            : Boolean := False;
      --  CRC transfer next
      CRCNEXT        : Boolean := False;
      --  Hardware CRC calculation enable
      CRCEN          : Boolean := False;
      --  Output enable in bidirectional mode
      BIDIOE         : Boolean := False;
      --  Bidirectional data mode enable
      BIDIMODE       : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      CPHA           at 0 range 0 .. 0;
      CPOL           at 0 range 1 .. 1;
      MSTR           at 0 range 2 .. 2;
      BR             at 0 range 3 .. 5;
      SPE            at 0 range 6 .. 6;
      LSBFIRST       at 0 range 7 .. 7;
      SSI            at 0 range 8 .. 8;
      SSM            at 0 range 9 .. 9;
      RXONLY         at 0 range 10 .. 10;
      DFF            at 0 range 11 .. 11;
      CRCNEXT        at 0 range 12 .. 12;
      CRCEN          at 0 range 13 .. 13;
      BIDIOE         at 0 range 14 .. 14;
      BIDIMODE       at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------
   -- CR2_Register --
   ------------------

   --  Data size
   type DS_Field is
     (
      --  Reset value for the field
      Ds_Field_Reset,
      Size_4Bit,
      Size_5Bit,
      Size_6Bit,
      Size_7Bit,
      Size_8Bit,
      Size_9Bit,
      Size_10Bit,
      Size_11Bit,
      Size_12Bit,
      Size_13Bit,
      Size_14Bit,
      Size_15Bit,
      Size_16Bit)
     with Size => 4;
   for DS_Field use
     (Ds_Field_Reset => 0,
      Size_4Bit => 3,
      Size_5Bit => 4,
      Size_6Bit => 5,
      Size_7Bit => 6,
      Size_8Bit => 7,
      Size_9Bit => 8,
      Size_10Bit => 9,
      Size_11Bit => 10,
      Size_12Bit => 11,
      Size_13Bit => 12,
      Size_14Bit => 13,
      Size_15Bit => 14,
      Size_16Bit => 15);

   --  FIFO reception threshold
   type FRXTH_Field is
     (
      --  RXNE event is generated if the FIFO level is greater than or equal to
      --  1/2 (16-bit).
      Half,
      --  RXNE event is generated if the FIFO level is greater than or equal to
      --  1/4 (8-bit).
      Quarter)
     with Size => 1;
   for FRXTH_Field use
     (Half => 0,
      Quarter => 1);

   --  Last DMA transfer for reception
   type LDMA_RX_Field is
     (
      --  Number of data to transfer is even.
      Even,
      --  Number of data is odd.
      Odd)
     with Size => 1;
   for LDMA_RX_Field use
     (Even => 0,
      Odd => 1);

   --  Last DMA transfer for transmission
   type LDMA_TX_Field is
     (
      --  Number of data to transfer is even.
      Even,
      --  Number of data is odd.
      Odd)
     with Size => 1;
   for LDMA_TX_Field use
     (Even => 0,
      Odd => 1);

   --  control register 2
   type CR2_Register is record
      --  Rx buffer DMA enable
      RXDMAEN        : Boolean := False;
      --  Tx buffer DMA enable
      TXDMAEN        : Boolean := False;
      --  SS output enable
      SSOE           : Boolean := False;
      --  NSS pulse management
      NSSP           : Boolean := False;
      --  Frame format
      FRF            : Boolean := False;
      --  Error interrupt enable
      ERRIE          : Boolean := False;
      --  RX buffer not empty interrupt enable
      RXNEIE         : Boolean := False;
      --  Tx buffer empty interrupt enable
      TXEIE          : Boolean := False;
      --  Data size
      DS             : DS_Field := Ds_Field_Reset;
      --  FIFO reception threshold
      FRXTH          : FRXTH_Field := Half;
      --  Last DMA transfer for reception
      LDMA_RX        : LDMA_RX_Field := Even;
      --  Last DMA transfer for transmission
      LDMA_TX        : LDMA_TX_Field := Even;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      RXDMAEN        at 0 range 0 .. 0;
      TXDMAEN        at 0 range 1 .. 1;
      SSOE           at 0 range 2 .. 2;
      NSSP           at 0 range 3 .. 3;
      FRF            at 0 range 4 .. 4;
      ERRIE          at 0 range 5 .. 5;
      RXNEIE         at 0 range 6 .. 6;
      TXEIE          at 0 range 7 .. 7;
      DS             at 0 range 8 .. 11;
      FRXTH          at 0 range 12 .. 12;
      LDMA_RX        at 0 range 13 .. 13;
      LDMA_TX        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   --  FIFO reception level
   type FRLVL_Field is
     (
      --  FIFO is empty.
      Fifo_Empty,
      --  1/4 FIFO.
      Fifo_Quarter,
      --  1/2 FIFO.
      Fifo_Half,
      --  FIFO full.
      Fifo_Full)
     with Size => 2;
   for FRLVL_Field use
     (Fifo_Empty => 0,
      Fifo_Quarter => 1,
      Fifo_Half => 2,
      Fifo_Full => 3);

   --  FIFO transmission level
   type FTLVL_Field is
     (
      --  FIFO is empty.
      Fifo_Empty,
      --  1/4 FIFO.
      Fifo_Quarter,
      --  1/2 FIFO.
      Fifo_Half,
      --  FIFO full.
      Fifo_Full)
     with Size => 2;
   for FTLVL_Field use
     (Fifo_Empty => 0,
      Fifo_Quarter => 1,
      Fifo_Half => 2,
      Fifo_Full => 3);

   --  status register
   type SR_Register is record
      --  Read-only. Receive buffer not empty
      RXNE           : Boolean := False;
      --  Read-only. Transmit buffer empty
      TXE            : Boolean := True;
      --  Read-only. Channel side
      CHSIDE         : Boolean := False;
      --  Read-only. Underrun flag
      UDR            : Boolean := False;
      --  CRC error flag
      CRCERR         : Boolean := False;
      --  Read-only. Mode fault
      MODF           : Boolean := False;
      --  Read-only. Overrun flag
      OVR            : Boolean := False;
      --  Read-only. Busy flag
      BSY            : Boolean := False;
      --  Read-only. TI frame format error
      TIFRFE         : Boolean := False;
      --  Read-only. FIFO reception level
      FRLVL          : FRLVL_Field := Fifo_Empty;
      --  Read-only. FIFO transmission level
      FTLVL          : FTLVL_Field := Fifo_Empty;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      RXNE           at 0 range 0 .. 0;
      TXE            at 0 range 1 .. 1;
      CHSIDE         at 0 range 2 .. 2;
      UDR            at 0 range 3 .. 3;
      CRCERR         at 0 range 4 .. 4;
      MODF           at 0 range 5 .. 5;
      OVR            at 0 range 6 .. 6;
      BSY            at 0 range 7 .. 7;
      TIFRFE         at 0 range 8 .. 8;
      FRLVL          at 0 range 9 .. 10;
      FTLVL          at 0 range 11 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------
   -- DR_Register --
   -----------------

   subtype DR_DR_Field is HAL.Short;

   --  data register
   type DR_Register is record
      --  Data register
      DR             : DR_DR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      DR             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --------------------
   -- CRCPR_Register --
   --------------------

   subtype CRCPR_CRCPOLY_Field is HAL.Short;

   --  CRC polynomial register
   type CRCPR_Register is record
      --  CRC polynomial register
      CRCPOLY        : CRCPR_CRCPOLY_Field := 16#7#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCPR_Register use record
      CRCPOLY        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ---------------------
   -- RXCRCR_Register --
   ---------------------

   subtype RXCRCR_RxCRC_Field is HAL.Short;

   --  RX CRC register
   type RXCRCR_Register is record
      --  Read-only. Rx CRC register
      RxCRC          : RXCRCR_RxCRC_Field;
      --  unspecified
      Reserved_16_31 : HAL.Short;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXCRCR_Register use record
      RxCRC          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ---------------------
   -- TXCRCR_Register --
   ---------------------

   subtype TXCRCR_TxCRC_Field is HAL.Short;

   --  TX CRC register
   type TXCRCR_Register is record
      --  Read-only. Tx CRC register
      TxCRC          : TXCRCR_TxCRC_Field;
      --  unspecified
      Reserved_16_31 : HAL.Short;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXCRCR_Register use record
      TxCRC          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ----------------------
   -- I2SCFGR_Register --
   ----------------------

   subtype I2SCFGR_DATLEN_Field is HAL.UInt2;
   subtype I2SCFGR_I2SSTD_Field is HAL.UInt2;
   subtype I2SCFGR_I2SCFG_Field is HAL.UInt2;

   --  I2S configuration register
   type I2SCFGR_Register is record
      --  Channel length (number of bits per audio channel)
      CHLEN          : Boolean := False;
      --  Data length to be transferred
      DATLEN         : I2SCFGR_DATLEN_Field := 16#0#;
      --  Steady state clock polarity
      CKPOL          : Boolean := False;
      --  I2S standard selection
      I2SSTD         : I2SCFGR_I2SSTD_Field := 16#0#;
      --  unspecified
      Reserved_6_6   : HAL.Bit := 16#0#;
      --  PCM frame synchronization
      PCMSYNC        : Boolean := False;
      --  I2S configuration mode
      I2SCFG         : I2SCFGR_I2SCFG_Field := 16#0#;
      --  I2S Enable
      I2SE           : Boolean := False;
      --  I2S mode selection
      I2SMOD         : Boolean := False;
      --  Asynchronous start enable
      ASTRTEN        : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for I2SCFGR_Register use record
      CHLEN          at 0 range 0 .. 0;
      DATLEN         at 0 range 1 .. 2;
      CKPOL          at 0 range 3 .. 3;
      I2SSTD         at 0 range 4 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      PCMSYNC        at 0 range 7 .. 7;
      I2SCFG         at 0 range 8 .. 9;
      I2SE           at 0 range 10 .. 10;
      I2SMOD         at 0 range 11 .. 11;
      ASTRTEN        at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --------------------
   -- I2SPR_Register --
   --------------------

   subtype I2SPR_I2SDIV_Field is HAL.Byte;

   --  I2S prescaler register
   type I2SPR_Register is record
      --  I2S Linear prescaler
      I2SDIV         : I2SPR_I2SDIV_Field := 16#A#;
      --  Odd factor for the prescaler
      ODD            : Boolean := False;
      --  Master clock output enable
      MCKOE          : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for I2SPR_Register use record
      I2SDIV         at 0 range 0 .. 7;
      ODD            at 0 range 8 .. 8;
      MCKOE          at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Serial peripheral interface
   type SPI_Peripheral is record
      --  control register 1
      CR1     : CR1_Register;
      --  control register 2
      CR2     : CR2_Register;
      --  status register
      SR      : SR_Register;
      --  data register
      DR      : DR_Register;
      --  CRC polynomial register
      CRCPR   : CRCPR_Register;
      --  RX CRC register
      RXCRCR  : RXCRCR_Register;
      --  TX CRC register
      TXCRCR  : TXCRCR_Register;
      --  I2S configuration register
      I2SCFGR : I2SCFGR_Register;
      --  I2S prescaler register
      I2SPR   : I2SPR_Register;
   end record
     with Volatile;

   for SPI_Peripheral use record
      CR1     at 0 range 0 .. 31;
      CR2     at 4 range 0 .. 31;
      SR      at 8 range 0 .. 31;
      DR      at 12 range 0 .. 31;
      CRCPR   at 16 range 0 .. 31;
      RXCRCR  at 20 range 0 .. 31;
      TXCRCR  at 24 range 0 .. 31;
      I2SCFGR at 28 range 0 .. 31;
      I2SPR   at 32 range 0 .. 31;
   end record;

   --  Serial peripheral interface
   SPI2_Periph : aliased SPI_Peripheral
     with Import, Address => SPI2_Base;

   --  Serial peripheral interface
   SPI3_Periph : aliased SPI_Peripheral
     with Import, Address => SPI3_Base;

   --  Serial peripheral interface
   SPI1_Periph : aliased SPI_Peripheral
     with Import, Address => SPI1_Base;

   --  Serial peripheral interface
   SPI4_Periph : aliased SPI_Peripheral
     with Import, Address => SPI4_Base;

   --  Serial peripheral interface
   SPI5_Periph : aliased SPI_Peripheral
     with Import, Address => SPI5_Base;

   --  Serial peripheral interface
   SPI6_Periph : aliased SPI_Peripheral
     with Import, Address => SPI6_Base;

end STM32_SVD.SPI;
