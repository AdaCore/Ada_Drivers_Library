--  This spec has been automatically generated from FE310.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package FE310_SVD.SPI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype SCKDIV_SCALE_Field is HAL.UInt12;

   --  Serial Clock Divisor Register.
   type SCKDIV_Register is record
      SCALE          : SCKDIV_SCALE_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCKDIV_Register use record
      SCALE          at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Serial Clock Phase
   type SCKMODE_CPHA is
     (
      --  Data is sampled on the leading edge of SCK and shifted on the
      --  trailing edge of SCK.
      Val_0,
      --  Data is shifted on the leading edge of SCK and sampled on the
      --  trailing edge of SCK.
      Val_1)
     with Size => 1;
   for SCKMODE_CPHA use
     (Val_0 => 0,
      Val_1 => 1);

   --  Serial Clock Polarity
   type SCKMODE_CPOL is
     (
      --  Inactive state of SCK is logical 0.
      Val_0,
      --  Inactive state of SCK is logical 1.
      Val_1)
     with Size => 1;
   for SCKMODE_CPOL use
     (Val_0 => 0,
      Val_1 => 1);

   --  Serial Clock Mode Register.
   type SCKMODE_Register is record
      --  Serial Clock Phase
      PHA           : SCKMODE_CPHA := FE310_SVD.SPI.Val_0;
      --  Serial Clock Polarity
      POL           : SCKMODE_CPOL := FE310_SVD.SPI.Val_0;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCKMODE_Register use record
      PHA           at 0 range 0 .. 0;
      POL           at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   type CSMODE_Chip_Select_Modes is
     (
      --  Assert/de-assert CS at the beginning/end of each frame.
      Auto,
      --  Keep CS continuously asserted after the initial frame.
      Hold,
      --  Disable hardware control of the CS pin.
      Off)
     with Size => 2;
   for CSMODE_Chip_Select_Modes use
     (Auto => 0,
      Hold => 2,
      Off => 3);

   --  Chip Select Mode Register.
   type CSMODE_Register is record
      MODE          : CSMODE_Chip_Select_Modes := FE310_SVD.SPI.Auto;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSMODE_Register use record
      MODE          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype DELAY0_CSSCK_Field is HAL.UInt8;
   subtype DELAY0_SCKCS_Field is HAL.UInt8;

   --  Delay Control Register 0.
   type DELAY0_Register is record
      CSSCK          : DELAY0_CSSCK_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      SCKCS          : DELAY0_SCKCS_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DELAY0_Register use record
      CSSCK          at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      SCKCS          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DELAY1_INTERCS_Field is HAL.UInt8;
   subtype DELAY1_INTERXFR_Field is HAL.UInt8;

   --  Delay Control Register 1.
   type DELAY1_Register is record
      INTERCS        : DELAY1_INTERCS_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : HAL.UInt8 := 16#0#;
      INTERXFR       : DELAY1_INTERXFR_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DELAY1_Register use record
      INTERCS        at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      INTERXFR       at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   type FMT_SPI_Protocol is
     (
      --  Data Pins: DQ0 (MOSI), DQ1 (MISO).
      Single,
      --  Data Pins: DQ0, DQ1.
      Dual,
      --  Data Pins: DQ0, DQ1, DQ2, DQ3.
      Quad)
     with Size => 2;
   for FMT_SPI_Protocol use
     (Single => 0,
      Dual => 1,
      Quad => 2);

   type FMT_SPI_Endianness is
     (
      --  Tansmit most-significant bit first.
      Msb_First,
      --  Transmit least-significant bit first.
      Lsb_First)
     with Size => 1;
   for FMT_SPI_Endianness use
     (Msb_First => 0,
      Lsb_First => 1);

   type FMT_SPI_IO_Direction is
     (
      --  For dual and quad protocols, the DQ pins are tri-stated. For the
      --  single protocol, the DQ0 pin is driven with the transmit data as
      --  normal.
      Rx,
      --  The receive FIFO is not populated.
      Tx)
     with Size => 1;
   for FMT_SPI_IO_Direction use
     (Rx => 0,
      Tx => 1);

   subtype FMT_LEN_Field is HAL.UInt4;

   --  Frame Format Register.
   type FMT_Register is record
      PROTO          : FMT_SPI_Protocol := FE310_SVD.SPI.Single;
      ENDIAN         : FMT_SPI_Endianness := FE310_SVD.SPI.Msb_First;
      DIR            : FMT_SPI_IO_Direction := FE310_SVD.SPI.Rx;
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      LEN            : FMT_LEN_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FMT_Register use record
      PROTO          at 0 range 0 .. 1;
      ENDIAN         at 0 range 2 .. 2;
      DIR            at 0 range 3 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      LEN            at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype TXDATA_DATA_Field is HAL.UInt8;

   --  Transmit Data Register.
   type TXDATA_Register is record
      DATA          : TXDATA_DATA_Field := 16#0#;
      --  unspecified
      Reserved_8_30 : HAL.UInt23 := 16#0#;
      FULL          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXDATA_Register use record
      DATA          at 0 range 0 .. 7;
      Reserved_8_30 at 0 range 8 .. 30;
      FULL          at 0 range 31 .. 31;
   end record;

   subtype RXDATA_DATA_Field is HAL.UInt8;

   --  Receive Data Register.
   type RXDATA_Register is record
      DATA          : RXDATA_DATA_Field := 16#0#;
      --  unspecified
      Reserved_8_30 : HAL.UInt23 := 16#0#;
      EMPTY         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXDATA_Register use record
      DATA          at 0 range 0 .. 7;
      Reserved_8_30 at 0 range 8 .. 30;
      EMPTY         at 0 range 31 .. 31;
   end record;

   subtype TXMARK_TXMARK_Field is HAL.UInt3;

   --  Transmit Watermark Register.
   type TXMARK_Register is record
      TXMARK        : TXMARK_TXMARK_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXMARK_Register use record
      TXMARK        at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype RXMARK_RXMARK_Field is HAL.UInt3;

   --  Receive Watermark Register.
   type RXMARK_Register is record
      RXMARK        : RXMARK_RXMARK_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXMARK_Register use record
      RXMARK        at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  SPI Flash Interface Control Register.
   type FCTRL_Register is record
      ENABLE        : Boolean := False;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCTRL_Register use record
      ENABLE        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype FFMT_ADDR_LEN_Field is HAL.UInt3;
   subtype FFMT_PAD_CNT_Field is HAL.UInt4;
   subtype FFMT_CMD_PROTO_Field is HAL.UInt2;
   subtype FFMT_ADDR_PROTO_Field is HAL.UInt2;
   subtype FFMT_DATA_PROTO_Field is HAL.UInt2;
   subtype FFMT_CMD_CODE_Field is HAL.UInt8;
   subtype FFMT_PAD_CODE_Field is HAL.UInt8;

   --  SPI Flash Instruction Format Register.
   type FFMT_Register is record
      CMD_EN         : Boolean := False;
      ADDR_LEN       : FFMT_ADDR_LEN_Field := 16#0#;
      PAD_CNT        : FFMT_PAD_CNT_Field := 16#0#;
      CMD_PROTO      : FFMT_CMD_PROTO_Field := 16#0#;
      ADDR_PROTO     : FFMT_ADDR_PROTO_Field := 16#0#;
      DATA_PROTO     : FFMT_DATA_PROTO_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : HAL.UInt2 := 16#0#;
      CMD_CODE       : FFMT_CMD_CODE_Field := 16#0#;
      PAD_CODE       : FFMT_PAD_CODE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FFMT_Register use record
      CMD_EN         at 0 range 0 .. 0;
      ADDR_LEN       at 0 range 1 .. 3;
      PAD_CNT        at 0 range 4 .. 7;
      CMD_PROTO      at 0 range 8 .. 9;
      ADDR_PROTO     at 0 range 10 .. 11;
      DATA_PROTO     at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      CMD_CODE       at 0 range 16 .. 23;
      PAD_CODE       at 0 range 24 .. 31;
   end record;

   --  SPI Interrupt Enable Register.
   type IE_Register is record
      TXWM          : Boolean := False;
      RXWM          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IE_Register use record
      TXWM          at 0 range 0 .. 0;
      RXWM          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  SPI Interrupt Pending Register.
   type IP_Register is record
      TXWM          : Boolean := False;
      RXWM          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IP_Register use record
      TXWM          at 0 range 0 .. 0;
      RXWM          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Serial Peripheral Interface.
   type SPI_Peripheral is record
      --  Serial Clock Divisor Register.
      SCKDIV  : aliased SCKDIV_Register;
      --  Serial Clock Mode Register.
      SCKMODE : aliased SCKMODE_Register;
      --  Chip Select ID Register.
      CSID    : aliased HAL.UInt32;
      --  Chip Select Default Register.
      CSDEF   : aliased HAL.UInt32;
      --  Chip Select Mode Register.
      CSMODE  : aliased CSMODE_Register;
      --  Delay Control Register 0.
      DELAY0  : aliased DELAY0_Register;
      --  Delay Control Register 1.
      DELAY1  : aliased DELAY1_Register;
      --  Frame Format Register.
      FMT     : aliased FMT_Register;
      --  Transmit Data Register.
      TXDATA  : aliased TXDATA_Register;
      --  Receive Data Register.
      RXDATA  : aliased RXDATA_Register;
      --  Transmit Watermark Register.
      TXMARK  : aliased TXMARK_Register;
      --  Receive Watermark Register.
      RXMARK  : aliased RXMARK_Register;
      --  SPI Flash Interface Control Register.
      FCTRL   : aliased FCTRL_Register;
      --  SPI Flash Instruction Format Register.
      FFMT    : aliased FFMT_Register;
      --  SPI Interrupt Enable Register.
      IE      : aliased IE_Register;
      --  SPI Interrupt Pending Register.
      IP      : aliased IP_Register;
   end record
     with Volatile;

   for SPI_Peripheral use record
      SCKDIV  at 16#0# range 0 .. 31;
      SCKMODE at 16#4# range 0 .. 31;
      CSID    at 16#10# range 0 .. 31;
      CSDEF   at 16#14# range 0 .. 31;
      CSMODE  at 16#18# range 0 .. 31;
      DELAY0  at 16#28# range 0 .. 31;
      DELAY1  at 16#2C# range 0 .. 31;
      FMT     at 16#40# range 0 .. 31;
      TXDATA  at 16#48# range 0 .. 31;
      RXDATA  at 16#4C# range 0 .. 31;
      TXMARK  at 16#50# range 0 .. 31;
      RXMARK  at 16#54# range 0 .. 31;
      FCTRL   at 16#60# range 0 .. 31;
      FFMT    at 16#64# range 0 .. 31;
      IE      at 16#70# range 0 .. 31;
      IP      at 16#74# range 0 .. 31;
   end record;

   --  Serial Peripheral Interface.
   QSPI0_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#10014000#);

   --  Serial Peripheral Interface.
   QSPI1_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#10024000#);

   --  Serial Peripheral Interface.
   QSPI2_Periph : aliased SPI_Peripheral
     with Import, Address => System'To_Address (16#10034000#);

end FE310_SVD.SPI;
