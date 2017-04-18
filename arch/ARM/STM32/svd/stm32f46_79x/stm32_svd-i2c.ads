--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.I2C is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Control register 1
   type CR1_Register is record
      --  Peripheral enable
      PE             : Boolean := False;
      --  SMBus mode
      SMBUS          : Boolean := False;
      --  unspecified
      Reserved_2_2   : HAL.Bit := 16#0#;
      --  SMBus type
      SMBTYPE        : Boolean := False;
      --  ARP enable
      ENARP          : Boolean := False;
      --  PEC enable
      ENPEC          : Boolean := False;
      --  General call enable
      ENGC           : Boolean := False;
      --  Clock stretching disable (Slave mode)
      NOSTRETCH      : Boolean := False;
      --  Start generation
      START          : Boolean := False;
      --  Stop generation
      STOP           : Boolean := False;
      --  Acknowledge enable
      ACK            : Boolean := False;
      --  Acknowledge/PEC Position (for data reception)
      POS            : Boolean := False;
      --  Packet error checking
      PEC            : Boolean := False;
      --  SMBus alert
      ALERT          : Boolean := False;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  Software reset
      SWRST          : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      PE             at 0 range 0 .. 0;
      SMBUS          at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      SMBTYPE        at 0 range 3 .. 3;
      ENARP          at 0 range 4 .. 4;
      ENPEC          at 0 range 5 .. 5;
      ENGC           at 0 range 6 .. 6;
      NOSTRETCH      at 0 range 7 .. 7;
      START          at 0 range 8 .. 8;
      STOP           at 0 range 9 .. 9;
      ACK            at 0 range 10 .. 10;
      POS            at 0 range 11 .. 11;
      PEC            at 0 range 12 .. 12;
      ALERT          at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      SWRST          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CR2_FREQ_Field is HAL.UInt6;

   --  Control register 2
   type CR2_Register is record
      --  Peripheral clock frequency
      FREQ           : CR2_FREQ_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : HAL.UInt2 := 16#0#;
      --  Error interrupt enable
      ITERREN        : Boolean := False;
      --  Event interrupt enable
      ITEVTEN        : Boolean := False;
      --  Buffer interrupt enable
      ITBUFEN        : Boolean := False;
      --  DMA requests enable
      DMAEN          : Boolean := False;
      --  DMA last transfer
      LAST           : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      FREQ           at 0 range 0 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      ITERREN        at 0 range 8 .. 8;
      ITEVTEN        at 0 range 9 .. 9;
      ITBUFEN        at 0 range 10 .. 10;
      DMAEN          at 0 range 11 .. 11;
      LAST           at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype OAR1_ADD7_Field is HAL.UInt7;
   subtype OAR1_ADD10_Field is HAL.UInt2;

   --  Own address register 1
   type OAR1_Register is record
      --  Interface address
      ADD0           : Boolean := False;
      --  Interface address
      ADD7           : OAR1_ADD7_Field := 16#0#;
      --  Interface address
      ADD10          : OAR1_ADD10_Field := 16#0#;
      --  unspecified
      Reserved_10_14 : HAL.UInt5 := 16#0#;
      --  Addressing mode (slave mode)
      ADDMODE        : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OAR1_Register use record
      ADD0           at 0 range 0 .. 0;
      ADD7           at 0 range 1 .. 7;
      ADD10          at 0 range 8 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      ADDMODE        at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OAR2_ADD2_Field is HAL.UInt7;

   --  Own address register 2
   type OAR2_Register is record
      --  Dual addressing mode enable
      ENDUAL        : Boolean := False;
      --  Interface address
      ADD2          : OAR2_ADD2_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OAR2_Register use record
      ENDUAL        at 0 range 0 .. 0;
      ADD2          at 0 range 1 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DR_DR_Field is HAL.UInt8;

   --  Data register
   type DR_Register is record
      --  8-bit data register
      DR            : DR_DR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      DR            at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Status register 1
   type SR1_Register is record
      --  Read-only. Start bit (Master mode)
      SB             : Boolean := False;
      --  Read-only. Address sent (master mode)/matched (slave mode)
      ADDR           : Boolean := False;
      --  Read-only. Byte transfer finished
      BTF            : Boolean := False;
      --  Read-only. 10-bit header sent (Master mode)
      ADD10          : Boolean := False;
      --  Read-only. Stop detection (slave mode)
      STOPF          : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Read-only. Data register not empty (receivers)
      RxNE           : Boolean := False;
      --  Read-only. Data register empty (transmitters)
      TxE            : Boolean := False;
      --  Bus error
      BERR           : Boolean := False;
      --  Arbitration lost (master mode)
      ARLO           : Boolean := False;
      --  Acknowledge failure
      AF             : Boolean := False;
      --  Overrun/Underrun
      OVR            : Boolean := False;
      --  PEC Error in reception
      PECERR         : Boolean := False;
      --  unspecified
      Reserved_13_13 : HAL.Bit := 16#0#;
      --  Timeout or Tlow error
      TIMEOUT        : Boolean := False;
      --  SMBus alert
      SMBALERT       : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR1_Register use record
      SB             at 0 range 0 .. 0;
      ADDR           at 0 range 1 .. 1;
      BTF            at 0 range 2 .. 2;
      ADD10          at 0 range 3 .. 3;
      STOPF          at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      RxNE           at 0 range 6 .. 6;
      TxE            at 0 range 7 .. 7;
      BERR           at 0 range 8 .. 8;
      ARLO           at 0 range 9 .. 9;
      AF             at 0 range 10 .. 10;
      OVR            at 0 range 11 .. 11;
      PECERR         at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      TIMEOUT        at 0 range 14 .. 14;
      SMBALERT       at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype SR2_PEC_Field is HAL.UInt8;

   --  Status register 2
   type SR2_Register is record
      --  Read-only. Master/slave
      MSL            : Boolean;
      --  Read-only. Bus busy
      BUSY           : Boolean;
      --  Read-only. Transmitter/receiver
      TRA            : Boolean;
      --  unspecified
      Reserved_3_3   : HAL.Bit;
      --  Read-only. General call address (Slave mode)
      GENCALL        : Boolean;
      --  Read-only. SMBus device default address (Slave mode)
      SMBDEFAULT     : Boolean;
      --  Read-only. SMBus host header (Slave mode)
      SMBHOST        : Boolean;
      --  Read-only. Dual flag (Slave mode)
      DUALF          : Boolean;
      --  Read-only. acket error checking register
      PEC            : SR2_PEC_Field;
      --  unspecified
      Reserved_16_31 : HAL.UInt16;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR2_Register use record
      MSL            at 0 range 0 .. 0;
      BUSY           at 0 range 1 .. 1;
      TRA            at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      GENCALL        at 0 range 4 .. 4;
      SMBDEFAULT     at 0 range 5 .. 5;
      SMBHOST        at 0 range 6 .. 6;
      DUALF          at 0 range 7 .. 7;
      PEC            at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CCR_CCR_Field is HAL.UInt12;

   --  Clock control register
   type CCR_Register is record
      --  Clock control register in Fast/Standard mode (Master mode)
      CCR            : CCR_CCR_Field := 16#0#;
      --  unspecified
      Reserved_12_13 : HAL.UInt2 := 16#0#;
      --  Fast mode duty cycle
      DUTY           : Boolean := False;
      --  I2C master mode selection
      F_S            : Boolean := False;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      CCR            at 0 range 0 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      DUTY           at 0 range 14 .. 14;
      F_S            at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype TRISE_TRISE_Field is HAL.UInt6;

   --  TRISE register
   type TRISE_Register is record
      --  Maximum rise time in Fast/Standard mode (Master mode)
      TRISE         : TRISE_TRISE_Field := 16#2#;
      --  unspecified
      Reserved_6_31 : HAL.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TRISE_Register use record
      TRISE         at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype FLTR_DNF_Field is HAL.UInt4;

   --  I2C FLTR register
   type FLTR_Register is record
      --  Digital noise filter
      DNF           : FLTR_DNF_Field := 16#0#;
      --  Analog noise filter OFF
      ANOFF         : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FLTR_Register use record
      DNF           at 0 range 0 .. 3;
      ANOFF         at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Inter-integrated circuit
   type I2C_Peripheral is record
      --  Control register 1
      CR1   : aliased CR1_Register;
      --  Control register 2
      CR2   : aliased CR2_Register;
      --  Own address register 1
      OAR1  : aliased OAR1_Register;
      --  Own address register 2
      OAR2  : aliased OAR2_Register;
      --  Data register
      DR    : aliased DR_Register;
      --  Status register 1
      SR1   : aliased SR1_Register;
      --  Status register 2
      SR2   : aliased SR2_Register;
      --  Clock control register
      CCR   : aliased CCR_Register;
      --  TRISE register
      TRISE : aliased TRISE_Register;
      --  I2C FLTR register
      FLTR  : aliased FLTR_Register;
   end record
     with Volatile;

   for I2C_Peripheral use record
      CR1   at 16#0# range 0 .. 31;
      CR2   at 16#4# range 0 .. 31;
      OAR1  at 16#8# range 0 .. 31;
      OAR2  at 16#C# range 0 .. 31;
      DR    at 16#10# range 0 .. 31;
      SR1   at 16#14# range 0 .. 31;
      SR2   at 16#18# range 0 .. 31;
      CCR   at 16#1C# range 0 .. 31;
      TRISE at 16#20# range 0 .. 31;
      FLTR  at 16#24# range 0 .. 31;
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

end STM32_SVD.I2C;
