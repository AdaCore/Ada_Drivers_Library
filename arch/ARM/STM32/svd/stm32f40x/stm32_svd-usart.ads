--  This spec has been automatically generated from STM32F40x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.USART is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Status register
   type SR_Register is record
      --  Read-only. Parity error
      PE            : Boolean := False;
      --  Read-only. Framing error
      FE            : Boolean := False;
      --  Read-only. Noise detected flag
      NF            : Boolean := False;
      --  Read-only. Overrun error
      ORE           : Boolean := False;
      --  Read-only. IDLE line detected
      IDLE          : Boolean := False;
      --  Read data register not empty
      RXNE          : Boolean := False;
      --  Transmission complete
      TC            : Boolean := False;
      --  Read-only. Transmit data register empty
      TXE           : Boolean := False;
      --  LIN break detection flag
      LBD           : Boolean := False;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#6000#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      PE            at 0 range 0 .. 0;
      FE            at 0 range 1 .. 1;
      NF            at 0 range 2 .. 2;
      ORE           at 0 range 3 .. 3;
      IDLE          at 0 range 4 .. 4;
      RXNE          at 0 range 5 .. 5;
      TC            at 0 range 6 .. 6;
      TXE           at 0 range 7 .. 7;
      LBD           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype DR_DR_Field is HAL.UInt9;

   --  Data register
   type DR_Register is record
      --  Data value
      DR            : DR_DR_Field := 16#0#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      DR            at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype BRR_DIV_Fraction_Field is HAL.UInt4;
   subtype BRR_DIV_Mantissa_Field is HAL.UInt12;

   --  Baud rate register
   type BRR_Register is record
      --  fraction of USARTDIV
      DIV_Fraction   : BRR_DIV_Fraction_Field := 16#0#;
      --  mantissa of USARTDIV
      DIV_Mantissa   : BRR_DIV_Mantissa_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for BRR_Register use record
      DIV_Fraction   at 0 range 0 .. 3;
      DIV_Mantissa   at 0 range 4 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Control register 1
   type CR1_Register is record
      --  Send break
      SBK            : Boolean := False;
      --  Receiver wakeup
      RWU            : Boolean := False;
      --  Receiver enable
      RE             : Boolean := False;
      --  Transmitter enable
      TE             : Boolean := False;
      --  IDLE interrupt enable
      IDLEIE         : Boolean := False;
      --  RXNE interrupt enable
      RXNEIE         : Boolean := False;
      --  Transmission complete interrupt enable
      TCIE           : Boolean := False;
      --  TXE interrupt enable
      TXEIE          : Boolean := False;
      --  PE interrupt enable
      PEIE           : Boolean := False;
      --  Parity selection
      PS             : Boolean := False;
      --  Parity control enable
      PCE            : Boolean := False;
      --  Wakeup method
      WAKE           : Boolean := False;
      --  Word length
      M              : Boolean := False;
      --  USART enable
      UE             : Boolean := False;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  Oversampling mode
      OVER8          : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      SBK            at 0 range 0 .. 0;
      RWU            at 0 range 1 .. 1;
      RE             at 0 range 2 .. 2;
      TE             at 0 range 3 .. 3;
      IDLEIE         at 0 range 4 .. 4;
      RXNEIE         at 0 range 5 .. 5;
      TCIE           at 0 range 6 .. 6;
      TXEIE          at 0 range 7 .. 7;
      PEIE           at 0 range 8 .. 8;
      PS             at 0 range 9 .. 9;
      PCE            at 0 range 10 .. 10;
      WAKE           at 0 range 11 .. 11;
      M              at 0 range 12 .. 12;
      UE             at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      OVER8          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CR2_ADD_Field is HAL.UInt4;
   subtype CR2_STOP_Field is HAL.UInt2;

   --  Control register 2
   type CR2_Register is record
      --  Address of the USART node
      ADD            : CR2_ADD_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  lin break detection length
      LBDL           : Boolean := False;
      --  LIN break detection interrupt enable
      LBDIE          : Boolean := False;
      --  unspecified
      Reserved_7_11  : HAL.UInt5 := 16#0#;
      --  STOP bits
      STOP           : CR2_STOP_Field := 16#0#;
      --  LIN mode enable
      LINEN          : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      ADD            at 0 range 0 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      LBDL           at 0 range 5 .. 5;
      LBDIE          at 0 range 6 .. 6;
      Reserved_7_11  at 0 range 7 .. 11;
      STOP           at 0 range 12 .. 13;
      LINEN          at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  Control register 3
   type CR3_Register is record
      --  Error interrupt enable
      EIE            : Boolean := False;
      --  IrDA mode enable
      IREN           : Boolean := False;
      --  IrDA low-power
      IRLP           : Boolean := False;
      --  Half-duplex selection
      HDSEL          : Boolean := False;
      --  unspecified
      Reserved_4_5   : HAL.UInt2 := 16#0#;
      --  DMA enable receiver
      DMAR           : Boolean := False;
      --  DMA enable transmitter
      DMAT           : Boolean := False;
      --  unspecified
      Reserved_8_10  : HAL.UInt3 := 16#0#;
      --  One sample bit method enable
      ONEBIT         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR3_Register use record
      EIE            at 0 range 0 .. 0;
      IREN           at 0 range 1 .. 1;
      IRLP           at 0 range 2 .. 2;
      HDSEL          at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      DMAR           at 0 range 6 .. 6;
      DMAT           at 0 range 7 .. 7;
      Reserved_8_10  at 0 range 8 .. 10;
      ONEBIT         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  Status register
   type SR_Register_1 is record
      --  Read-only. Parity error
      PE             : Boolean := False;
      --  Read-only. Framing error
      FE             : Boolean := False;
      --  Read-only. Noise detected flag
      NF             : Boolean := False;
      --  Read-only. Overrun error
      ORE            : Boolean := False;
      --  Read-only. IDLE line detected
      IDLE           : Boolean := False;
      --  Read data register not empty
      RXNE           : Boolean := False;
      --  Transmission complete
      TC             : Boolean := False;
      --  Read-only. Transmit data register empty
      TXE            : Boolean := False;
      --  LIN break detection flag
      LBD            : Boolean := False;
      --  CTS flag
      CTS            : Boolean := False;
      --  unspecified
      Reserved_10_31 : HAL.UInt22 := 16#3000#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register_1 use record
      PE             at 0 range 0 .. 0;
      FE             at 0 range 1 .. 1;
      NF             at 0 range 2 .. 2;
      ORE            at 0 range 3 .. 3;
      IDLE           at 0 range 4 .. 4;
      RXNE           at 0 range 5 .. 5;
      TC             at 0 range 6 .. 6;
      TXE            at 0 range 7 .. 7;
      LBD            at 0 range 8 .. 8;
      CTS            at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  Control register 2
   type CR2_Register_1 is record
      --  Address of the USART node
      ADD            : CR2_ADD_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : HAL.Bit := 16#0#;
      --  lin break detection length
      LBDL           : Boolean := False;
      --  LIN break detection interrupt enable
      LBDIE          : Boolean := False;
      --  unspecified
      Reserved_7_7   : HAL.Bit := 16#0#;
      --  Last bit clock pulse
      LBCL           : Boolean := False;
      --  Clock phase
      CPHA           : Boolean := False;
      --  Clock polarity
      CPOL           : Boolean := False;
      --  Clock enable
      CLKEN          : Boolean := False;
      --  STOP bits
      STOP           : CR2_STOP_Field := 16#0#;
      --  LIN mode enable
      LINEN          : Boolean := False;
      --  unspecified
      Reserved_15_31 : HAL.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register_1 use record
      ADD            at 0 range 0 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      LBDL           at 0 range 5 .. 5;
      LBDIE          at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      LBCL           at 0 range 8 .. 8;
      CPHA           at 0 range 9 .. 9;
      CPOL           at 0 range 10 .. 10;
      CLKEN          at 0 range 11 .. 11;
      STOP           at 0 range 12 .. 13;
      LINEN          at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  Control register 3
   type CR3_Register_1 is record
      --  Error interrupt enable
      EIE            : Boolean := False;
      --  IrDA mode enable
      IREN           : Boolean := False;
      --  IrDA low-power
      IRLP           : Boolean := False;
      --  Half-duplex selection
      HDSEL          : Boolean := False;
      --  Smartcard NACK enable
      NACK           : Boolean := False;
      --  Smartcard mode enable
      SCEN           : Boolean := False;
      --  DMA enable receiver
      DMAR           : Boolean := False;
      --  DMA enable transmitter
      DMAT           : Boolean := False;
      --  RTS enable
      RTSE           : Boolean := False;
      --  CTS enable
      CTSE           : Boolean := False;
      --  CTS interrupt enable
      CTSIE          : Boolean := False;
      --  One sample bit method enable
      ONEBIT         : Boolean := False;
      --  unspecified
      Reserved_12_31 : HAL.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR3_Register_1 use record
      EIE            at 0 range 0 .. 0;
      IREN           at 0 range 1 .. 1;
      IRLP           at 0 range 2 .. 2;
      HDSEL          at 0 range 3 .. 3;
      NACK           at 0 range 4 .. 4;
      SCEN           at 0 range 5 .. 5;
      DMAR           at 0 range 6 .. 6;
      DMAT           at 0 range 7 .. 7;
      RTSE           at 0 range 8 .. 8;
      CTSE           at 0 range 9 .. 9;
      CTSIE          at 0 range 10 .. 10;
      ONEBIT         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype GTPR_PSC_Field is HAL.UInt8;
   subtype GTPR_GT_Field is HAL.UInt8;

   --  Guard time and prescaler register
   type GTPR_Register is record
      --  Prescaler value
      PSC            : GTPR_PSC_Field := 16#0#;
      --  Guard time value
      GT             : GTPR_GT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GTPR_Register use record
      PSC            at 0 range 0 .. 7;
      GT             at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Universal synchronous asynchronous receiver transmitter
   type UART4_Peripheral is record
      --  Status register
      SR  : aliased SR_Register;
      --  Data register
      DR  : aliased DR_Register;
      --  Baud rate register
      BRR : aliased BRR_Register;
      --  Control register 1
      CR1 : aliased CR1_Register;
      --  Control register 2
      CR2 : aliased CR2_Register;
      --  Control register 3
      CR3 : aliased CR3_Register;
   end record
     with Volatile;

   for UART4_Peripheral use record
      SR  at 16#0# range 0 .. 31;
      DR  at 16#4# range 0 .. 31;
      BRR at 16#8# range 0 .. 31;
      CR1 at 16#C# range 0 .. 31;
      CR2 at 16#10# range 0 .. 31;
      CR3 at 16#14# range 0 .. 31;
   end record;

   --  Universal synchronous asynchronous receiver transmitter
   UART4_Periph : aliased UART4_Peripheral
     with Import, Address => System'To_Address (16#40004C00#);

   --  Universal synchronous asynchronous receiver transmitter
   UART5_Periph : aliased UART4_Peripheral
     with Import, Address => System'To_Address (16#40005000#);

   --  Universal synchronous asynchronous receiver transmitter
   type USART1_Peripheral is record
      --  Status register
      SR   : aliased SR_Register_1;
      --  Data register
      DR   : aliased DR_Register;
      --  Baud rate register
      BRR  : aliased BRR_Register;
      --  Control register 1
      CR1  : aliased CR1_Register;
      --  Control register 2
      CR2  : aliased CR2_Register_1;
      --  Control register 3
      CR3  : aliased CR3_Register_1;
      --  Guard time and prescaler register
      GTPR : aliased GTPR_Register;
   end record
     with Volatile;

   for USART1_Peripheral use record
      SR   at 16#0# range 0 .. 31;
      DR   at 16#4# range 0 .. 31;
      BRR  at 16#8# range 0 .. 31;
      CR1  at 16#C# range 0 .. 31;
      CR2  at 16#10# range 0 .. 31;
      CR3  at 16#14# range 0 .. 31;
      GTPR at 16#18# range 0 .. 31;
   end record;

   --  Universal synchronous asynchronous receiver transmitter
   USART1_Periph : aliased USART1_Peripheral
     with Import, Address => System'To_Address (16#40011000#);

   --  Universal synchronous asynchronous receiver transmitter
   USART2_Periph : aliased USART1_Peripheral
     with Import, Address => System'To_Address (16#40004400#);

   --  Universal synchronous asynchronous receiver transmitter
   USART3_Periph : aliased USART1_Peripheral
     with Import, Address => System'To_Address (16#40004800#);

   --  Universal synchronous asynchronous receiver transmitter
   USART6_Periph : aliased USART1_Peripheral
     with Import, Address => System'To_Address (16#40011400#);

end STM32_SVD.USART;
