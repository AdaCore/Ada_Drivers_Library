--  Automatically generated from STM32F7x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.I2C is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ------------------
   -- CR1_Register --
   ------------------

   subtype CR1_PE_Field is STM32_SVD.Bit;
   subtype CR1_TXIE_Field is STM32_SVD.Bit;
   subtype CR1_RXIE_Field is STM32_SVD.Bit;
   subtype CR1_ADDRIE_Field is STM32_SVD.Bit;
   subtype CR1_NACKIE_Field is STM32_SVD.Bit;
   subtype CR1_STOPIE_Field is STM32_SVD.Bit;
   subtype CR1_TCIE_Field is STM32_SVD.Bit;
   subtype CR1_ERRIE_Field is STM32_SVD.Bit;
   subtype CR1_DNF_Field is STM32_SVD.UInt4;
   subtype CR1_ANFOFF_Field is STM32_SVD.Bit;
   subtype CR1_TXDMAEN_Field is STM32_SVD.Bit;
   subtype CR1_RXDMAEN_Field is STM32_SVD.Bit;
   subtype CR1_SBC_Field is STM32_SVD.Bit;
   subtype CR1_NOSTRETCH_Field is STM32_SVD.Bit;
   subtype CR1_WUPEN_Field is STM32_SVD.Bit;
   subtype CR1_GCEN_Field is STM32_SVD.Bit;
   subtype CR1_SMBHEN_Field is STM32_SVD.Bit;
   subtype CR1_SMBDEN_Field is STM32_SVD.Bit;
   subtype CR1_ALERTEN_Field is STM32_SVD.Bit;
   subtype CR1_PECEN_Field is STM32_SVD.Bit;

   --  Control register 1
   type CR1_Register is record
      --  Peripheral enable
      PE             : CR1_PE_Field := 16#0#;
      --  TX Interrupt enable
      TXIE           : CR1_TXIE_Field := 16#0#;
      --  RX Interrupt enable
      RXIE           : CR1_RXIE_Field := 16#0#;
      --  Address match interrupt enable (slave only)
      ADDRIE         : CR1_ADDRIE_Field := 16#0#;
      --  Not acknowledge received interrupt enable
      NACKIE         : CR1_NACKIE_Field := 16#0#;
      --  STOP detection Interrupt enable
      STOPIE         : CR1_STOPIE_Field := 16#0#;
      --  Transfer Complete interrupt enable
      TCIE           : CR1_TCIE_Field := 16#0#;
      --  Error interrupts enable
      ERRIE          : CR1_ERRIE_Field := 16#0#;
      --  Digital noise filter
      DNF            : CR1_DNF_Field := 16#0#;
      --  Analog noise filter OFF
      ANFOFF         : CR1_ANFOFF_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : STM32_SVD.Bit := 16#0#;
      --  DMA transmission requests enable
      TXDMAEN        : CR1_TXDMAEN_Field := 16#0#;
      --  DMA reception requests enable
      RXDMAEN        : CR1_RXDMAEN_Field := 16#0#;
      --  Slave byte control
      SBC            : CR1_SBC_Field := 16#0#;
      --  Clock stretching disable
      NOSTRETCH      : CR1_NOSTRETCH_Field := 16#0#;
      --  Wakeup from STOP enable
      WUPEN          : CR1_WUPEN_Field := 16#0#;
      --  General call enable
      GCEN           : CR1_GCEN_Field := 16#0#;
      --  SMBus Host address enable
      SMBHEN         : CR1_SMBHEN_Field := 16#0#;
      --  SMBus Device Default address enable
      SMBDEN         : CR1_SMBDEN_Field := 16#0#;
      --  SMBUS alert enable
      ALERTEN        : CR1_ALERTEN_Field := 16#0#;
      --  PEC enable
      PECEN          : CR1_PECEN_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
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

   subtype CR2_SADD_Field is STM32_SVD.UInt10;
   subtype CR2_RD_WRN_Field is STM32_SVD.Bit;
   subtype CR2_ADD10_Field is STM32_SVD.Bit;
   subtype CR2_HEAD10R_Field is STM32_SVD.Bit;
   subtype CR2_START_Field is STM32_SVD.Bit;
   subtype CR2_STOP_Field is STM32_SVD.Bit;
   subtype CR2_NACK_Field is STM32_SVD.Bit;
   subtype CR2_NBYTES_Field is STM32_SVD.Byte;
   subtype CR2_RELOAD_Field is STM32_SVD.Bit;
   subtype CR2_AUTOEND_Field is STM32_SVD.Bit;
   subtype CR2_PECBYTE_Field is STM32_SVD.Bit;

   --  Control register 2
   type CR2_Register is record
      --  Slave address bit (master mode)
      SADD           : CR2_SADD_Field := 16#0#;
      --  Transfer direction (master mode)
      RD_WRN         : CR2_RD_WRN_Field := 16#0#;
      --  10-bit addressing mode (master mode)
      ADD10          : CR2_ADD10_Field := 16#0#;
      --  10-bit address header only read direction (master receiver mode)
      HEAD10R        : CR2_HEAD10R_Field := 16#0#;
      --  Start generation
      START          : CR2_START_Field := 16#0#;
      --  Stop generation (master mode)
      STOP           : CR2_STOP_Field := 16#0#;
      --  NACK generation (slave mode)
      NACK           : CR2_NACK_Field := 16#0#;
      --  Number of bytes
      NBYTES         : CR2_NBYTES_Field := 16#0#;
      --  NBYTES reload mode
      RELOAD         : CR2_RELOAD_Field := 16#0#;
      --  Automatic end mode (master mode)
      AUTOEND        : CR2_AUTOEND_Field := 16#0#;
      --  Packet error checking byte
      PECBYTE        : CR2_PECBYTE_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32_SVD.UInt5 := 16#0#;
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

   subtype OAR1_OA1_Field is STM32_SVD.UInt10;
   subtype OAR1_OA1MODE_Field is STM32_SVD.Bit;
   subtype OAR1_OA1EN_Field is STM32_SVD.Bit;

   --  Own address register 1
   type OAR1_Register is record
      --  Interface address
      OA1            : OAR1_OA1_Field := 16#0#;
      --  Own Address 1 10-bit mode
      OA1MODE        : OAR1_OA1MODE_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : STM32_SVD.UInt4 := 16#0#;
      --  Own Address 1 enable
      OA1EN          : OAR1_OA1EN_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
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

   subtype OAR2_OA2_Field is STM32_SVD.UInt7;
   subtype OAR2_OA2MSK_Field is STM32_SVD.UInt3;
   subtype OAR2_OA2EN_Field is STM32_SVD.Bit;

   --  Own address register 2
   type OAR2_Register is record
      --  unspecified
      Reserved_0_0   : STM32_SVD.Bit := 16#0#;
      --  Interface address
      OA2            : OAR2_OA2_Field := 16#0#;
      --  Own Address 2 masks
      OA2MSK         : OAR2_OA2MSK_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : STM32_SVD.UInt4 := 16#0#;
      --  Own Address 2 enable
      OA2EN          : OAR2_OA2EN_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
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

   subtype TIMINGR_SCLL_Field is STM32_SVD.Byte;
   subtype TIMINGR_SCLH_Field is STM32_SVD.Byte;
   subtype TIMINGR_SDADEL_Field is STM32_SVD.UInt4;
   subtype TIMINGR_SCLDEL_Field is STM32_SVD.UInt4;
   subtype TIMINGR_PRESC_Field is STM32_SVD.UInt4;

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
      Reserved_24_27 : STM32_SVD.UInt4 := 16#0#;
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

   subtype TIMEOUTR_TIMEOUTA_Field is STM32_SVD.UInt12;
   subtype TIMEOUTR_TIDLE_Field is STM32_SVD.Bit;
   subtype TIMEOUTR_TIMOUTEN_Field is STM32_SVD.Bit;
   subtype TIMEOUTR_TIMEOUTB_Field is STM32_SVD.UInt12;
   subtype TIMEOUTR_TEXTEN_Field is STM32_SVD.Bit;

   --  Status register 1
   type TIMEOUTR_Register is record
      --  Bus timeout A
      TIMEOUTA       : TIMEOUTR_TIMEOUTA_Field := 16#0#;
      --  Idle clock timeout detection
      TIDLE          : TIMEOUTR_TIDLE_Field := 16#0#;
      --  unspecified
      Reserved_13_14 : STM32_SVD.UInt2 := 16#0#;
      --  Clock timeout enable
      TIMOUTEN       : TIMEOUTR_TIMOUTEN_Field := 16#0#;
      --  Bus timeout B
      TIMEOUTB       : TIMEOUTR_TIMEOUTB_Field := 16#0#;
      --  unspecified
      Reserved_28_30 : STM32_SVD.UInt3 := 16#0#;
      --  Extended clock timeout enable
      TEXTEN         : TIMEOUTR_TEXTEN_Field := 16#0#;
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

   subtype ISR_TXE_Field is STM32_SVD.Bit;
   subtype ISR_TXIS_Field is STM32_SVD.Bit;
   subtype ISR_RXNE_Field is STM32_SVD.Bit;
   subtype ISR_ADDR_Field is STM32_SVD.Bit;
   subtype ISR_NACKF_Field is STM32_SVD.Bit;
   subtype ISR_STOPF_Field is STM32_SVD.Bit;
   subtype ISR_TC_Field is STM32_SVD.Bit;
   subtype ISR_TCR_Field is STM32_SVD.Bit;
   subtype ISR_BERR_Field is STM32_SVD.Bit;
   subtype ISR_ARLO_Field is STM32_SVD.Bit;
   subtype ISR_OVR_Field is STM32_SVD.Bit;
   subtype ISR_PECERR_Field is STM32_SVD.Bit;
   subtype ISR_TIMEOUT_Field is STM32_SVD.Bit;
   subtype ISR_ALERT_Field is STM32_SVD.Bit;
   subtype ISR_BUSY_Field is STM32_SVD.Bit;
   subtype ISR_DIR_Field is STM32_SVD.Bit;
   subtype ISR_ADDCODE_Field is STM32_SVD.UInt7;

   --  Interrupt and Status register
   type ISR_Register is record
      --  Transmit data register empty (transmitters)
      TXE            : ISR_TXE_Field := 16#1#;
      --  Transmit interrupt status (transmitters)
      TXIS           : ISR_TXIS_Field := 16#0#;
      --  Receive data register not empty (receivers)
      RXNE           : ISR_RXNE_Field := 16#0#;
      --  Address matched (slave mode)
      ADDR           : ISR_ADDR_Field := 16#0#;
      --  Not acknowledge received flag
      NACKF          : ISR_NACKF_Field := 16#0#;
      --  Stop detection flag
      STOPF          : ISR_STOPF_Field := 16#0#;
      --  Transfer Complete (master mode)
      TC             : ISR_TC_Field := 16#0#;
      --  Transfer Complete Reload
      TCR            : ISR_TCR_Field := 16#0#;
      --  Bus error
      BERR           : ISR_BERR_Field := 16#0#;
      --  Arbitration lost
      ARLO           : ISR_ARLO_Field := 16#0#;
      --  Overrun/Underrun (slave mode)
      OVR            : ISR_OVR_Field := 16#0#;
      --  PEC Error in reception
      PECERR         : ISR_PECERR_Field := 16#0#;
      --  Timeout or t_low detection flag
      TIMEOUT        : ISR_TIMEOUT_Field := 16#0#;
      --  SMBus alert
      ALERT          : ISR_ALERT_Field := 16#0#;
      --  unspecified
      Reserved_14_14 : STM32_SVD.Bit := 16#0#;
      --  Bus busy
      BUSY           : ISR_BUSY_Field := 16#0#;
      --  Transfer direction (Slave mode)
      DIR            : ISR_DIR_Field := 16#0#;
      --  Address match code (Slave mode)
      ADDCODE        : ISR_ADDCODE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32_SVD.Byte := 16#0#;
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

   subtype ICR_ADDRCF_Field is STM32_SVD.Bit;
   subtype ICR_NACKCF_Field is STM32_SVD.Bit;
   subtype ICR_STOPCF_Field is STM32_SVD.Bit;
   subtype ICR_BERRCF_Field is STM32_SVD.Bit;
   subtype ICR_ARLOCF_Field is STM32_SVD.Bit;
   subtype ICR_OVRCF_Field is STM32_SVD.Bit;
   subtype ICR_PECCF_Field is STM32_SVD.Bit;
   subtype ICR_TIMOUTCF_Field is STM32_SVD.Bit;
   subtype ICR_ALERTCF_Field is STM32_SVD.Bit;

   --  Interrupt clear register
   type ICR_Register is record
      --  unspecified
      Reserved_0_2   : STM32_SVD.UInt3 := 16#0#;
      --  Address Matched flag clear
      ADDRCF         : ICR_ADDRCF_Field := 16#0#;
      --  Not Acknowledge flag clear
      NACKCF         : ICR_NACKCF_Field := 16#0#;
      --  Stop detection flag clear
      STOPCF         : ICR_STOPCF_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : STM32_SVD.UInt2 := 16#0#;
      --  Bus error flag clear
      BERRCF         : ICR_BERRCF_Field := 16#0#;
      --  Arbitration lost flag clear
      ARLOCF         : ICR_ARLOCF_Field := 16#0#;
      --  Overrun/Underrun flag clear
      OVRCF          : ICR_OVRCF_Field := 16#0#;
      --  PEC Error flag clear
      PECCF          : ICR_PECCF_Field := 16#0#;
      --  Timeout detection flag clear
      TIMOUTCF       : ICR_TIMOUTCF_Field := 16#0#;
      --  Alert flag clear
      ALERTCF        : ICR_ALERTCF_Field := 16#0#;
      --  unspecified
      Reserved_14_31 : STM32_SVD.UInt18 := 16#0#;
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

   subtype PECR_PEC_Field is STM32_SVD.Byte;

   --  PEC register
   type PECR_Register is record
      --  Packet error checking register
      PEC           : PECR_PEC_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   subtype RXDR_RXDATA_Field is STM32_SVD.Byte;

   --  Receive data register
   type RXDR_Register is record
      --  8-bit receive data
      RXDATA        : RXDR_RXDATA_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   subtype TXDR_TXDATA_Field is STM32_SVD.Byte;

   --  Transmit data register
   type TXDR_Register is record
      --  8-bit transmit data
      TXDATA        : TXDR_TXDATA_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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
     with Import, Address => System'To_Address (16#40005400#);

   --  Inter-integrated circuit
   I2C2_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40005800#);

   --  Inter-integrated circuit
   I2C3_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40005C00#);

   --  Inter-integrated circuit
   I2C4_Periph : aliased I2C_Peripheral
     with Import, Address => System'To_Address (16#40006000#);

end STM32_SVD.I2C;
