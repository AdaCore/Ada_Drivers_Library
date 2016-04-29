--  This spec has been automatically generated from STM32F7x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.I2C is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ------------------
   -- CR1_Register --
   ------------------

   subtype CR1_DNF_Field is HAL.UInt4;

   --  Control register 1
   type CR1_Register is record
      --  Peripheral enable
      PE             : Boolean := False;
      --  TX Interrupt enable
      TXIE           : Boolean := False;
      --  RX Interrupt enable
      RXIE           : Boolean := False;
      --  Address match interrupt enable (slave only)
      ADDRIE         : Boolean := False;
      --  Not acknowledge received interrupt enable
      NACKIE         : Boolean := False;
      --  STOP detection Interrupt enable
      STOPIE         : Boolean := False;
      --  Transfer Complete interrupt enable
      TCIE           : Boolean := False;
      --  Error interrupts enable
      ERRIE          : Boolean := False;
      --  Digital noise filter
      DNF            : CR1_DNF_Field := 16#0#;
      --  Analog noise filter OFF
      ANFOFF         : Boolean := False;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  DMA transmission requests enable
      TXDMAEN        : Boolean := False;
      --  DMA reception requests enable
      RXDMAEN        : Boolean := False;
      --  Slave byte control
      SBC            : Boolean := False;
      --  Clock stretching disable
      NOSTRETCH      : Boolean := False;
      --  Wakeup from STOP enable
      WUPEN          : Boolean := False;
      --  General call enable
      GCEN           : Boolean := False;
      --  SMBus Host address enable
      SMBHEN         : Boolean := False;
      --  SMBus Device Default address enable
      SMBDEN         : Boolean := False;
      --  SMBUS alert enable
      ALERTEN        : Boolean := False;
      --  PEC enable
      PECEN          : Boolean := False;
      --  unspecified
      Reserved_24_31 : HAL.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      PE             at 0 range 0 .. 0;
      TXIE           at 0 range 1 .. 1;
      RXIE           at 0 range 2 .. 2;
      ADDRIE         at 0 range 3 .. 3;
      NACKIE         at 0 range 4 .. 4;
      STOPIE         at 0 range 5 .. 5;
      TCIE           at 0 range 6 .. 6;
      ERRIE          at 0 range 7 .. 7;
      DNF            at 0 range 8 .. 11;
      ANFOFF         at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      TXDMAEN        at 0 range 14 .. 14;
      RXDMAEN        at 0 range 15 .. 15;
      SBC            at 0 range 16 .. 16;
      NOSTRETCH      at 0 range 17 .. 17;
      WUPEN          at 0 range 18 .. 18;
      GCEN           at 0 range 19 .. 19;
      SMBHEN         at 0 range 20 .. 20;
      SMBDEN         at 0 range 21 .. 21;
      ALERTEN        at 0 range 22 .. 22;
      PECEN          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ------------------
   -- CR2_Register --
   ------------------

   subtype CR2_SADD_Field is HAL.UInt10;
   subtype CR2_NBYTES_Field is HAL.Byte;

   --  Control register 2
   type CR2_Register is record
      --  Slave address bit (master mode)
      SADD           : CR2_SADD_Field := 16#0#;
      --  Transfer direction (master mode)
      RD_WRN         : Boolean := False;
      --  10-bit addressing mode (master mode)
      ADD10          : Boolean := False;
      --  10-bit address header only read direction (master receiver mode)
      HEAD10R        : Boolean := False;
      --  Start generation
      START          : Boolean := False;
      --  Stop generation (master mode)
      STOP           : Boolean := False;
      --  NACK generation (slave mode)
      NACK           : Boolean := False;
      --  Number of bytes
      NBYTES         : CR2_NBYTES_Field := 16#0#;
      --  NBYTES reload mode
      RELOAD         : Boolean := False;
      --  Automatic end mode (master mode)
      AUTOEND        : Boolean := False;
      --  Packet error checking byte
      PECBYTE        : Boolean := False;
      --  unspecified
      Reserved_27_31 : HAL.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      SADD           at 0 range 0 .. 9;
      RD_WRN         at 0 range 10 .. 10;
      ADD10          at 0 range 11 .. 11;
      HEAD10R        at 0 range 12 .. 12;
      START          at 0 range 13 .. 13;
      STOP           at 0 range 14 .. 14;
      NACK           at 0 range 15 .. 15;
      NBYTES         at 0 range 16 .. 23;
      RELOAD         at 0 range 24 .. 24;
      AUTOEND        at 0 range 25 .. 25;
      PECBYTE        at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   -------------------
   -- OAR1_Register --
   -------------------

   subtype OAR1_OA1_Field is HAL.UInt10;

   --  Own address register 1
   type OAR1_Register is record
      --  Interface address
      OA1            : OAR1_OA1_Field := 16#0#;
      --  Own Address 1 10-bit mode
      OA1MODE        : Boolean := False;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  Own Address 1 enable
      OA1EN          : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OAR1_Register use record
      OA1            at 0 range 0 .. 9;
      OA1MODE        at 0 range 10 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      OA1EN          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -------------------
   -- OAR2_Register --
   -------------------

   subtype OAR2_OA2_Field is HAL.UInt7;
   subtype OAR2_OA2MSK_Field is HAL.UInt3;

   --  Own address register 2
   type OAR2_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Interface address
      OA2            : OAR2_OA2_Field := 16#0#;
      --  Own Address 2 masks
      OA2MSK         : OAR2_OA2MSK_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : HAL.UInt4 := 16#0#;
      --  Own Address 2 enable
      OA2EN          : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OAR2_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      OA2            at 0 range 1 .. 7;
      OA2MSK         at 0 range 8 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      OA2EN          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ----------------------
   -- TIMINGR_Register --
   ----------------------

   subtype TIMINGR_SCLL_Field is HAL.Byte;
   subtype TIMINGR_SCLH_Field is HAL.Byte;
   subtype TIMINGR_SDADEL_Field is HAL.UInt4;
   subtype TIMINGR_SCLDEL_Field is HAL.UInt4;
   subtype TIMINGR_PRESC_Field is HAL.UInt4;

   --  Timing register
   type TIMINGR_Register is record
      --  SCL low period (master mode)
      SCLL           : TIMINGR_SCLL_Field := 16#0#;
      --  SCL high period (master mode)
      SCLH           : TIMINGR_SCLH_Field := 16#0#;
      --  Data hold time
      SDADEL         : TIMINGR_SDADEL_Field := 16#0#;
      --  Data setup time
      SCLDEL         : TIMINGR_SCLDEL_Field := 16#0#;
      --  unspecified
      Reserved_24_27 : HAL.UInt4 := 16#0#;
      --  Timing prescaler
      PRESC          : TIMINGR_PRESC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMINGR_Register use record
      SCLL           at 0 range 0 .. 7;
      SCLH           at 0 range 8 .. 15;
      SDADEL         at 0 range 16 .. 19;
      SCLDEL         at 0 range 20 .. 23;
      Reserved_24_27 at 0 range 24 .. 27;
      PRESC          at 0 range 28 .. 31;
   end record;

   -----------------------
   -- TIMEOUTR_Register --
   -----------------------

   subtype TIMEOUTR_TIMEOUTA_Field is HAL.UInt12;
   subtype TIMEOUTR_TIMEOUTB_Field is HAL.UInt12;

   --  Status register 1
   type TIMEOUTR_Register is record
      --  Bus timeout A
      TIMEOUTA       : TIMEOUTR_TIMEOUTA_Field := 16#0#;
      --  Idle clock timeout detection
      TIDLE          : Boolean := False;
      --  unspecified
      Reserved_13_14 : HAL.UInt2 := 16#0#;
      --  Clock timeout enable
      TIMOUTEN       : Boolean := False;
      --  Bus timeout B
      TIMEOUTB       : TIMEOUTR_TIMEOUTB_Field := 16#0#;
      --  unspecified
      Reserved_28_30 : HAL.UInt3 := 16#0#;
      --  Extended clock timeout enable
      TEXTEN         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIMEOUTR_Register use record
      TIMEOUTA       at 0 range 0 .. 11;
      TIDLE          at 0 range 12 .. 12;
      Reserved_13_14 at 0 range 13 .. 14;
      TIMOUTEN       at 0 range 15 .. 15;
      TIMEOUTB       at 0 range 16 .. 27;
      Reserved_28_30 at 0 range 28 .. 30;
      TEXTEN         at 0 range 31 .. 31;
   end record;

   ------------------
   -- ISR_Register --
   ------------------

   subtype ISR_ADDCODE_Field is HAL.UInt7;

   --  Interrupt and Status register
   type ISR_Register is record
      --  Transmit data register empty (transmitters)
      TXE            : Boolean := True;
      --  Transmit interrupt status (transmitters)
      TXIS           : Boolean := False;
      --  Read-only. Receive data register not empty (receivers)
      RXNE           : Boolean := False;
      --  Read-only. Address matched (slave mode)
      ADDR           : Boolean := False;
      --  Read-only. Not acknowledge received flag
      NACKF          : Boolean := False;
      --  Read-only. Stop detection flag
      STOPF          : Boolean := False;
      --  Read-only. Transfer Complete (master mode)
      TC             : Boolean := False;
      --  Read-only. Transfer Complete Reload
      TCR            : Boolean := False;
      --  Read-only. Bus error
      BERR           : Boolean := False;
      --  Read-only. Arbitration lost
      ARLO           : Boolean := False;
      --  Read-only. Overrun/Underrun (slave mode)
      OVR            : Boolean := False;
      --  Read-only. PEC Error in reception
      PECERR         : Boolean := False;
      --  Read-only. Timeout or t_low detection flag
      TIMEOUT        : Boolean := False;
      --  Read-only. SMBus alert
      ALERT          : Boolean := False;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  Read-only. Bus busy
      BUSY           : Boolean := False;
      --  Read-only. Transfer direction (Slave mode)
      DIR            : Boolean := False;
      --  Read-only. Address match code (Slave mode)
      ADDCODE        : ISR_ADDCODE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : HAL.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      TXE            at 0 range 0 .. 0;
      TXIS           at 0 range 1 .. 1;
      RXNE           at 0 range 2 .. 2;
      ADDR           at 0 range 3 .. 3;
      NACKF          at 0 range 4 .. 4;
      STOPF          at 0 range 5 .. 5;
      TC             at 0 range 6 .. 6;
      TCR            at 0 range 7 .. 7;
      BERR           at 0 range 8 .. 8;
      ARLO           at 0 range 9 .. 9;
      OVR            at 0 range 10 .. 10;
      PECERR         at 0 range 11 .. 11;
      TIMEOUT        at 0 range 12 .. 12;
      ALERT          at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      BUSY           at 0 range 15 .. 15;
      DIR            at 0 range 16 .. 16;
      ADDCODE        at 0 range 17 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   ------------------
   -- ICR_Register --
   ------------------

   --  Interrupt clear register
   type ICR_Register is record
      --  unspecified
      Reserved_0_2   : HAL.UInt3 := 16#0#;
      --  Write-only. Address Matched flag clear
      ADDRCF         : Boolean := False;
      --  Write-only. Not Acknowledge flag clear
      NACKCF         : Boolean := False;
      --  Write-only. Stop detection flag clear
      STOPCF         : Boolean := False;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Write-only. Bus error flag clear
      BERRCF         : Boolean := False;
      --  Write-only. Arbitration lost flag clear
      ARLOCF         : Boolean := False;
      --  Write-only. Overrun/Underrun flag clear
      OVRCF          : Boolean := False;
      --  Write-only. PEC Error flag clear
      PECCF          : Boolean := False;
      --  Write-only. Timeout detection flag clear
      TIMOUTCF       : Boolean := False;
      --  Write-only. Alert flag clear
      ALERTCF        : Boolean := False;
      --  unspecified
      Reserved_14_31 : HAL.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      ADDRCF         at 0 range 3 .. 3;
      NACKCF         at 0 range 4 .. 4;
      STOPCF         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      BERRCF         at 0 range 8 .. 8;
      ARLOCF         at 0 range 9 .. 9;
      OVRCF          at 0 range 10 .. 10;
      PECCF          at 0 range 11 .. 11;
      TIMOUTCF       at 0 range 12 .. 12;
      ALERTCF        at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   -------------------
   -- PECR_Register --
   -------------------

   subtype PECR_PEC_Field is HAL.Byte;

   --  PEC register
   type PECR_Register is record
      --  Read-only. Packet error checking register
      PEC           : PECR_PEC_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PECR_Register use record
      PEC           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -------------------
   -- RXDR_Register --
   -------------------

   subtype RXDR_RXDATA_Field is HAL.Byte;

   --  Receive data register
   type RXDR_Register is record
      --  Read-only. 8-bit receive data
      RXDATA        : RXDR_RXDATA_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXDR_Register use record
      RXDATA        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -------------------
   -- TXDR_Register --
   -------------------

   subtype TXDR_TXDATA_Field is HAL.Byte;

   --  Transmit data register
   type TXDR_Register is record
      --  8-bit transmit data
      TXDATA        : TXDR_TXDATA_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXDR_Register use record
      TXDATA        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Inter-integrated circuit
   type I2C_Peripheral is record
      --  Control register 1
      CR1      : CR1_Register;
      --  Control register 2
      CR2      : CR2_Register;
      --  Own address register 1
      OAR1     : OAR1_Register;
      --  Own address register 2
      OAR2     : OAR2_Register;
      --  Timing register
      TIMINGR  : TIMINGR_Register;
      --  Status register 1
      TIMEOUTR : TIMEOUTR_Register;
      --  Interrupt and Status register
      ISR      : ISR_Register;
      --  Interrupt clear register
      ICR      : ICR_Register;
      --  PEC register
      PECR     : PECR_Register;
      --  Receive data register
      RXDR     : RXDR_Register;
      --  Transmit data register
      TXDR     : TXDR_Register;
   end record
     with Volatile;

   for I2C_Peripheral use record
      CR1      at 0 range 0 .. 31;
      CR2      at 4 range 0 .. 31;
      OAR1     at 8 range 0 .. 31;
      OAR2     at 12 range 0 .. 31;
      TIMINGR  at 16 range 0 .. 31;
      TIMEOUTR at 20 range 0 .. 31;
      ISR      at 24 range 0 .. 31;
      ICR      at 28 range 0 .. 31;
      PECR     at 32 range 0 .. 31;
      RXDR     at 36 range 0 .. 31;
      TXDR     at 40 range 0 .. 31;
   end record;

   --  Inter-integrated circuit
   I2C1_Periph : aliased I2C_Peripheral
     with Import, Address => I2C1_Base;

   --  Inter-integrated circuit
   I2C2_Periph : aliased I2C_Peripheral
     with Import, Address => I2C2_Base;

   --  Inter-integrated circuit
   I2C3_Periph : aliased I2C_Peripheral
     with Import, Address => I2C3_Base;

   --  Inter-integrated circuit
   I2C4_Periph : aliased I2C_Peripheral
     with Import, Address => I2C4_Base;

end STM32_SVD.I2C;
