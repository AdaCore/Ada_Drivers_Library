--  Automatically generated from STM32F7x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.CEC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- CR_Register --
   -----------------

   subtype CR_CECEN_Field is STM32_SVD.Bit;
   subtype CR_TXSOM_Field is STM32_SVD.Bit;
   subtype CR_TXEOM_Field is STM32_SVD.Bit;

   --  control register
   type CR_Register is record
      --  CEC Enable
      CECEN         : CR_CECEN_Field := 16#0#;
      --  Tx start of message
      TXSOM         : CR_TXSOM_Field := 16#0#;
      --  Tx End Of Message
      TXEOM         : CR_TXEOM_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : STM32_SVD.UInt29 := 16#0#;
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

   subtype CFGR_SFT_Field is STM32_SVD.UInt3;
   subtype CFGR_RXTOL_Field is STM32_SVD.Bit;
   subtype CFGR_BRESTP_Field is STM32_SVD.Bit;
   subtype CFGR_BREGEN_Field is STM32_SVD.Bit;
   subtype CFGR_LBPEGEN_Field is STM32_SVD.Bit;
   subtype CFGR_BRDNOGEN_Field is STM32_SVD.Bit;
   subtype CFGR_SFTOP_Field is STM32_SVD.Bit;
   subtype CFGR_OAR_Field is STM32_SVD.UInt15;
   subtype CFGR_LSTN_Field is STM32_SVD.Bit;

   --  configuration register
   type CFGR_Register is record
      --  Signal Free Time
      SFT           : CFGR_SFT_Field := 16#0#;
      --  Rx-Tolerance
      RXTOL         : CFGR_RXTOL_Field := 16#0#;
      --  Rx-stop on bit rising error
      BRESTP        : CFGR_BRESTP_Field := 16#0#;
      --  Generate error-bit on bit rising error
      BREGEN        : CFGR_BREGEN_Field := 16#0#;
      --  Generate Error-Bit on Long Bit Period Error
      LBPEGEN       : CFGR_LBPEGEN_Field := 16#0#;
      --  Avoid Error-Bit Generation in Broadcast
      BRDNOGEN      : CFGR_BRDNOGEN_Field := 16#0#;
      --  SFT Option Bit
      SFTOP         : CFGR_SFTOP_Field := 16#0#;
      --  unspecified
      Reserved_9_15 : STM32_SVD.UInt7 := 16#0#;
      --  Own addresses configuration
      OAR           : CFGR_OAR_Field := 16#0#;
      --  Listen mode
      LSTN          : CFGR_LSTN_Field := 16#0#;
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

   subtype TXDR_TXD_Field is STM32_SVD.Byte;

   --  Tx data register
   type TXDR_Register is record
      --  Tx Data register
      TXD           : TXDR_TXD_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   subtype RXDR_RXDR_Field is STM32_SVD.Byte;

   --  Rx Data Register
   type RXDR_Register is record
      --  CEC Rx Data Register
      RXDR          : RXDR_RXDR_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32_SVD.UInt24 := 16#0#;
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

   subtype ISR_RXBR_Field is STM32_SVD.Bit;
   subtype ISR_RXEND_Field is STM32_SVD.Bit;
   subtype ISR_RXOVR_Field is STM32_SVD.Bit;
   subtype ISR_BRE_Field is STM32_SVD.Bit;
   subtype ISR_SBPE_Field is STM32_SVD.Bit;
   subtype ISR_LBPE_Field is STM32_SVD.Bit;
   subtype ISR_RXACKE_Field is STM32_SVD.Bit;
   subtype ISR_ARBLST_Field is STM32_SVD.Bit;
   subtype ISR_TXBR_Field is STM32_SVD.Bit;
   subtype ISR_TXEND_Field is STM32_SVD.Bit;
   subtype ISR_TXUDR_Field is STM32_SVD.Bit;
   subtype ISR_TXERR_Field is STM32_SVD.Bit;
   subtype ISR_TXACKE_Field is STM32_SVD.Bit;

   --  Interrupt and Status Register
   type ISR_Register is record
      --  Rx-Byte Received
      RXBR           : ISR_RXBR_Field := 16#0#;
      --  End Of Reception
      RXEND          : ISR_RXEND_Field := 16#0#;
      --  Rx-Overrun
      RXOVR          : ISR_RXOVR_Field := 16#0#;
      --  Rx-Bit rising error
      BRE            : ISR_BRE_Field := 16#0#;
      --  Rx-Short Bit period error
      SBPE           : ISR_SBPE_Field := 16#0#;
      --  Rx-Long Bit Period Error
      LBPE           : ISR_LBPE_Field := 16#0#;
      --  Rx-Missing Acknowledge
      RXACKE         : ISR_RXACKE_Field := 16#0#;
      --  Arbitration Lost
      ARBLST         : ISR_ARBLST_Field := 16#0#;
      --  Tx-Byte Request
      TXBR           : ISR_TXBR_Field := 16#0#;
      --  End of Transmission
      TXEND          : ISR_TXEND_Field := 16#0#;
      --  Tx-Buffer Underrun
      TXUDR          : ISR_TXUDR_Field := 16#0#;
      --  Tx-Error
      TXERR          : ISR_TXERR_Field := 16#0#;
      --  Tx-Missing acknowledge error
      TXACKE         : ISR_TXACKE_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19 := 16#0#;
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

   subtype IER_RXBRIE_Field is STM32_SVD.Bit;
   subtype IER_RXENDIE_Field is STM32_SVD.Bit;
   subtype IER_RXOVRIE_Field is STM32_SVD.Bit;
   subtype IER_BREIE_Field is STM32_SVD.Bit;
   subtype IER_SBPEIE_Field is STM32_SVD.Bit;
   subtype IER_LBPEIE_Field is STM32_SVD.Bit;
   subtype IER_RXACKIE_Field is STM32_SVD.Bit;
   subtype IER_ARBLSTIE_Field is STM32_SVD.Bit;
   subtype IER_TXBRIE_Field is STM32_SVD.Bit;
   subtype IER_TXENDIE_Field is STM32_SVD.Bit;
   subtype IER_TXUDRIE_Field is STM32_SVD.Bit;
   subtype IER_TXERRIE_Field is STM32_SVD.Bit;
   subtype IER_TXACKIE_Field is STM32_SVD.Bit;

   --  interrupt enable register
   type IER_Register is record
      --  Rx-Byte Received Interrupt Enable
      RXBRIE         : IER_RXBRIE_Field := 16#0#;
      --  End Of Reception Interrupt Enable
      RXENDIE        : IER_RXENDIE_Field := 16#0#;
      --  Rx-Buffer Overrun Interrupt Enable
      RXOVRIE        : IER_RXOVRIE_Field := 16#0#;
      --  Bit Rising Error Interrupt Enable
      BREIE          : IER_BREIE_Field := 16#0#;
      --  Short Bit Period Error Interrupt Enable
      SBPEIE         : IER_SBPEIE_Field := 16#0#;
      --  Long Bit Period Error Interrupt Enable
      LBPEIE         : IER_LBPEIE_Field := 16#0#;
      --  Rx-Missing Acknowledge Error Interrupt Enable
      RXACKIE        : IER_RXACKIE_Field := 16#0#;
      --  Arbitration Lost Interrupt Enable
      ARBLSTIE       : IER_ARBLSTIE_Field := 16#0#;
      --  Tx-Byte Request Interrupt Enable
      TXBRIE         : IER_TXBRIE_Field := 16#0#;
      --  Tx-End of message interrupt enable
      TXENDIE        : IER_TXENDIE_Field := 16#0#;
      --  Tx-Underrun interrupt enable
      TXUDRIE        : IER_TXUDRIE_Field := 16#0#;
      --  Tx-Error Interrupt Enable
      TXERRIE        : IER_TXERRIE_Field := 16#0#;
      --  Tx-Missing Acknowledge Error Interrupt Enable
      TXACKIE        : IER_TXACKIE_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32_SVD.UInt19 := 16#0#;
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
     with Import, Address => System'To_Address (16#40006C00#);

end STM32_SVD.CEC;
