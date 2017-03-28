--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.FLASH is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype ACR_LATENCY_Field is HAL.UInt3;

   --  Flash access control register
   type ACR_Register is record
      --  Latency
      LATENCY        : ACR_LATENCY_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : HAL.UInt5 := 16#0#;
      --  Prefetch enable
      PRFTEN         : Boolean := False;
      --  Instruction cache enable
      ICEN           : Boolean := False;
      --  Data cache enable
      DCEN           : Boolean := False;
      --  Write-only. Instruction cache reset
      ICRST          : Boolean := False;
      --  Data cache reset
      DCRST          : Boolean := False;
      --  unspecified
      Reserved_13_31 : HAL.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACR_Register use record
      LATENCY        at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      PRFTEN         at 0 range 8 .. 8;
      ICEN           at 0 range 9 .. 9;
      DCEN           at 0 range 10 .. 10;
      ICRST          at 0 range 11 .. 11;
      DCRST          at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  Status register
   type SR_Register is record
      --  End of operation
      EOP            : Boolean := False;
      --  Operation error
      OPERR          : Boolean := False;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Write protection error
      WRPERR         : Boolean := False;
      --  Programming alignment error
      PGAERR         : Boolean := False;
      --  Programming parallelism error
      PGPERR         : Boolean := False;
      --  Programming sequence error
      PGSERR         : Boolean := False;
      --  Proprietary readout protection (PCROP) error
      RDERR          : Boolean := False;
      --  unspecified
      Reserved_9_15  : HAL.UInt7 := 16#0#;
      --  Read-only. Busy
      BSY            : Boolean := False;
      --  unspecified
      Reserved_17_31 : HAL.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      EOP            at 0 range 0 .. 0;
      OPERR          at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      WRPERR         at 0 range 4 .. 4;
      PGAERR         at 0 range 5 .. 5;
      PGPERR         at 0 range 6 .. 6;
      PGSERR         at 0 range 7 .. 7;
      RDERR          at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      BSY            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CR_SNB_Field is HAL.UInt5;
   subtype CR_PSIZE_Field is HAL.UInt2;

   --  Control register
   type CR_Register is record
      --  Programming
      PG             : Boolean := False;
      --  Sector Erase
      SER            : Boolean := False;
      --  Mass Erase of sectors 0 to 11
      MER            : Boolean := False;
      --  Sector number
      SNB            : CR_SNB_Field := 16#0#;
      --  Program size
      PSIZE          : CR_PSIZE_Field := 16#0#;
      --  unspecified
      Reserved_10_14 : HAL.UInt5 := 16#0#;
      --  Mass Erase of sectors 12 to 23
      MER1           : Boolean := False;
      --  Start
      STRT           : Boolean := False;
      --  unspecified
      Reserved_17_23 : HAL.UInt7 := 16#0#;
      --  End of operation interrupt enable
      EOPIE          : Boolean := False;
      --  Error interrupt enable
      ERRIE          : Boolean := False;
      --  unspecified
      Reserved_26_30 : HAL.UInt5 := 16#0#;
      --  Lock
      LOCK           : Boolean := True;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      PG             at 0 range 0 .. 0;
      SER            at 0 range 1 .. 1;
      MER            at 0 range 2 .. 2;
      SNB            at 0 range 3 .. 7;
      PSIZE          at 0 range 8 .. 9;
      Reserved_10_14 at 0 range 10 .. 14;
      MER1           at 0 range 15 .. 15;
      STRT           at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      EOPIE          at 0 range 24 .. 24;
      ERRIE          at 0 range 25 .. 25;
      Reserved_26_30 at 0 range 26 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype OPTCR_BOR_LEV_Field is HAL.UInt2;
   subtype OPTCR_RDP_Field is HAL.UInt8;
   subtype OPTCR_nWRP_Field is HAL.UInt12;

   --  Flash option control register
   type OPTCR_Register is record
      --  Option lock
      OPTLOCK        : Boolean := True;
      --  Option start
      OPTSTRT        : Boolean := False;
      --  BOR reset Level
      BOR_LEV        : OPTCR_BOR_LEV_Field := 16#3#;
      --  Dual-bank Boot option byte
      BFB2           : Boolean := False;
      --  WDG_SW User option bytes
      WDG_SW         : Boolean := True;
      --  nRST_STOP User option bytes
      nRST_STOP      : Boolean := True;
      --  nRST_STDBY User option bytes
      nRST_STDBY     : Boolean := True;
      --  Read protect
      RDP            : OPTCR_RDP_Field := 16#AA#;
      --  Not write protect
      nWRP           : OPTCR_nWRP_Field := 16#FFF#;
      --  unspecified
      Reserved_28_29 : HAL.UInt2 := 16#0#;
      --  Dual-bank on 1 Mbyte Flash memory devices
      DB1M           : Boolean := False;
      --  Selection of protection mode for nWPRi bits
      SPRMOD         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPTCR_Register use record
      OPTLOCK        at 0 range 0 .. 0;
      OPTSTRT        at 0 range 1 .. 1;
      BOR_LEV        at 0 range 2 .. 3;
      BFB2           at 0 range 4 .. 4;
      WDG_SW         at 0 range 5 .. 5;
      nRST_STOP      at 0 range 6 .. 6;
      nRST_STDBY     at 0 range 7 .. 7;
      RDP            at 0 range 8 .. 15;
      nWRP           at 0 range 16 .. 27;
      Reserved_28_29 at 0 range 28 .. 29;
      DB1M           at 0 range 30 .. 30;
      SPRMOD         at 0 range 31 .. 31;
   end record;

   subtype OPTCR1_nWRP_Field is HAL.UInt12;

   --  Flash option control register 1
   type OPTCR1_Register is record
      --  unspecified
      Reserved_0_15  : HAL.UInt16 := 16#0#;
      --  Not write protect
      nWRP           : OPTCR1_nWRP_Field := 16#FFF#;
      --  unspecified
      Reserved_28_31 : HAL.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPTCR1_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      nWRP           at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  FLASH
   type FLASH_Peripheral is record
      --  Flash access control register
      ACR     : aliased ACR_Register;
      --  Flash key register
      KEYR    : aliased HAL.UInt32;
      --  Flash option key register
      OPTKEYR : aliased HAL.UInt32;
      --  Status register
      SR      : aliased SR_Register;
      --  Control register
      CR      : aliased CR_Register;
      --  Flash option control register
      OPTCR   : aliased OPTCR_Register;
      --  Flash option control register 1
      OPTCR1  : aliased OPTCR1_Register;
   end record
     with Volatile;

   for FLASH_Peripheral use record
      ACR     at 16#0# range 0 .. 31;
      KEYR    at 16#4# range 0 .. 31;
      OPTKEYR at 16#8# range 0 .. 31;
      SR      at 16#C# range 0 .. 31;
      CR      at 16#10# range 0 .. 31;
      OPTCR   at 16#14# range 0 .. 31;
      OPTCR1  at 16#18# range 0 .. 31;
   end record;

   --  FLASH
   FLASH_Periph : aliased FLASH_Peripheral
     with Import, Address => System'To_Address (16#40023C00#);

end STM32_SVD.FLASH;
