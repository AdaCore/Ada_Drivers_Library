--  This spec has been automatically generated from STM32F7x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.CEC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- CR_Register --
   -----------------

   --  control register
   type CR_Register is record
      --  CEC Enable
      CECEN         : Boolean := False;
      --  Tx start of message
      TXSOM         : Boolean := False;
      --  Tx End Of Message
      TXEOM         : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      CECEN         at 0 range 0 .. 0;
      TXSOM         at 0 range 1 .. 1;
      TXEOM         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   -------------------
   -- CFGR_Register --
   -------------------

   subtype CFGR_SFT_Field is HAL.UInt3;
   subtype CFGR_OAR_Field is HAL.UInt15;

   --  configuration register
   type CFGR_Register is record
      --  Signal Free Time
      SFT           : CFGR_SFT_Field := 16#0#;
      --  Rx-Tolerance
      RXTOL         : Boolean := False;
      --  Rx-stop on bit rising error
      BRESTP        : Boolean := False;
      --  Generate error-bit on bit rising error
      BREGEN        : Boolean := False;
      --  Generate Error-Bit on Long Bit Period Error
      LBPEGEN       : Boolean := False;
      --  Avoid Error-Bit Generation in Broadcast
      BRDNOGEN      : Boolean := False;
      --  SFT Option Bit
      SFTOP         : Boolean := False;
      --  unspecified
      Reserved_9_15 : HAL.UInt7 := 16#0#;
      --  Own addresses configuration
      OAR           : CFGR_OAR_Field := 16#0#;
      --  Listen mode
      LSTN          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_Register use record
      SFT           at 0 range 0 .. 2;
      RXTOL         at 0 range 3 .. 3;
      BRESTP        at 0 range 4 .. 4;
      BREGEN        at 0 range 5 .. 5;
      LBPEGEN       at 0 range 6 .. 6;
      BRDNOGEN      at 0 range 7 .. 7;
      SFTOP         at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      OAR           at 0 range 16 .. 30;
      LSTN          at 0 range 31 .. 31;
   end record;

   -------------------
   -- TXDR_Register --
   -------------------

   subtype TXDR_TXD_Field is HAL.Byte;

   --  Tx data register
   type TXDR_Register is record
      --  Write-only. Tx Data register
      TXD           : TXDR_TXD_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXDR_Register use record
      TXD           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -------------------
   -- RXDR_Register --
   -------------------

   subtype RXDR_RXDR_Field is HAL.Byte;

   --  Rx Data Register
   type RXDR_Register is record
      --  Read-only. CEC Rx Data Register
      RXDR          : RXDR_RXDR_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXDR_Register use record
      RXDR          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   ------------------
   -- ISR_Register --
   ------------------

   --  Interrupt and Status Register
   type ISR_Register is record
      --  Rx-Byte Received
      RXBR           : Boolean := False;
      --  End Of Reception
      RXEND          : Boolean := False;
      --  Rx-Overrun
      RXOVR          : Boolean := False;
      --  Rx-Bit rising error
      BRE            : Boolean := False;
      --  Rx-Short Bit period error
      SBPE           : Boolean := False;
      --  Rx-Long Bit Period Error
      LBPE           : Boolean := False;
      --  Rx-Missing Acknowledge
      RXACKE         : Boolean := False;
      --  Arbitration Lost
      ARBLST         : Boolean := False;
      --  Tx-Byte Request
      TXBR           : Boolean := False;
      --  End of Transmission
      TXEND          : Boolean := False;
      --  Tx-Buffer Underrun
      TXUDR          : Boolean := False;
      --  Tx-Error
      TXERR          : Boolean := False;
      --  Tx-Missing acknowledge error
      TXACKE         : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      RXBR           at 0 range 0 .. 0;
      RXEND          at 0 range 1 .. 1;
      RXOVR          at 0 range 2 .. 2;
      BRE            at 0 range 3 .. 3;
      SBPE           at 0 range 4 .. 4;
      LBPE           at 0 range 5 .. 5;
      RXACKE         at 0 range 6 .. 6;
      ARBLST         at 0 range 7 .. 7;
      TXBR           at 0 range 8 .. 8;
      TXEND          at 0 range 9 .. 9;
      TXUDR          at 0 range 10 .. 10;
      TXERR          at 0 range 11 .. 11;
      TXACKE         at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   ------------------
   -- IER_Register --
   ------------------

   --  interrupt enable register
   type IER_Register is record
      --  Rx-Byte Received Interrupt Enable
      RXBRIE         : Boolean := False;
      --  End Of Reception Interrupt Enable
      RXENDIE        : Boolean := False;
      --  Rx-Buffer Overrun Interrupt Enable
      RXOVRIE        : Boolean := False;
      --  Bit Rising Error Interrupt Enable
      BREIE          : Boolean := False;
      --  Short Bit Period Error Interrupt Enable
      SBPEIE         : Boolean := False;
      --  Long Bit Period Error Interrupt Enable
      LBPEIE         : Boolean := False;
      --  Rx-Missing Acknowledge Error Interrupt Enable
      RXACKIE        : Boolean := False;
      --  Arbitration Lost Interrupt Enable
      ARBLSTIE       : Boolean := False;
      --  Tx-Byte Request Interrupt Enable
      TXBRIE         : Boolean := False;
      --  Tx-End of message interrupt enable
      TXENDIE        : Boolean := False;
      --  Tx-Underrun interrupt enable
      TXUDRIE        : Boolean := False;
      --  Tx-Error Interrupt Enable
      TXERRIE        : Boolean := False;
      --  Tx-Missing Acknowledge Error Interrupt Enable
      TXACKIE        : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      RXBRIE         at 0 range 0 .. 0;
      RXENDIE        at 0 range 1 .. 1;
      RXOVRIE        at 0 range 2 .. 2;
      BREIE          at 0 range 3 .. 3;
      SBPEIE         at 0 range 4 .. 4;
      LBPEIE         at 0 range 5 .. 5;
      RXACKIE        at 0 range 6 .. 6;
      ARBLSTIE       at 0 range 7 .. 7;
      TXBRIE         at 0 range 8 .. 8;
      TXENDIE        at 0 range 9 .. 9;
      TXUDRIE        at 0 range 10 .. 10;
      TXERRIE        at 0 range 11 .. 11;
      TXACKIE        at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  HDMI-CEC controller
   type CEC_Peripheral is record
      --  control register
      CR   : CR_Register;
      --  configuration register
      CFGR : CFGR_Register;
      --  Tx data register
      TXDR : TXDR_Register;
      --  Rx Data Register
      RXDR : RXDR_Register;
      --  Interrupt and Status Register
      ISR  : ISR_Register;
      --  interrupt enable register
      IER  : IER_Register;
   end record
     with Volatile;

   for CEC_Peripheral use record
      CR   at 0 range 0 .. 31;
      CFGR at 4 range 0 .. 31;
      TXDR at 8 range 0 .. 31;
      RXDR at 12 range 0 .. 31;
      ISR  at 16 range 0 .. 31;
      IER  at 20 range 0 .. 31;
   end record;

   --  HDMI-CEC controller
   CEC_Periph : aliased CEC_Peripheral
     with Import, Address => CEC_Base;

end STM32_SVD.CEC;
