--  This spec has been automatically generated from STM32F46_79x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.QUADSPI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype CR_FTHRES_Field is HAL.UInt5;
   subtype CR_PRESCALER_Field is HAL.UInt8;

   --  control register
   type CR_Register is record
      --  Enable
      EN             : Boolean := False;
      --  Abort request
      ABORT_k        : Boolean := False;
      --  DMA enable
      DMAEN          : Boolean := False;
      --  Timeout counter enable
      TCEN           : Boolean := False;
      --  Sample shift
      SSHIFT         : Boolean := False;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Dual-flash mode
      DFM            : Boolean := False;
      --  FLASH memory selection
      FSEL           : Boolean := False;
      --  IFO threshold level
      FTHRES         : CR_FTHRES_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : HAL.UInt3 := 16#0#;
      --  Transfer error interrupt enable
      TEIE           : Boolean := False;
      --  Transfer complete interrupt enable
      TCIE           : Boolean := False;
      --  FIFO threshold interrupt enable
      FTIE           : Boolean := False;
      --  Status match interrupt enable
      SMIE           : Boolean := False;
      --  TimeOut interrupt enable
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_21_21 : HAL.Bit := 16#0#;
      --  Automatic poll mode stop
      APMS           : Boolean := False;
      --  Polling match mode
      PMM            : Boolean := False;
      --  Clock prescaler
      PRESCALER      : CR_PRESCALER_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      EN             at 0 range 0 .. 0;
      ABORT_k        at 0 range 1 .. 1;
      DMAEN          at 0 range 2 .. 2;
      TCEN           at 0 range 3 .. 3;
      SSHIFT         at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      DFM            at 0 range 6 .. 6;
      FSEL           at 0 range 7 .. 7;
      FTHRES         at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TEIE           at 0 range 16 .. 16;
      TCIE           at 0 range 17 .. 17;
      FTIE           at 0 range 18 .. 18;
      SMIE           at 0 range 19 .. 19;
      TOIE           at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      APMS           at 0 range 22 .. 22;
      PMM            at 0 range 23 .. 23;
      PRESCALER      at 0 range 24 .. 31;
   end record;

   subtype DCR_CSHT_Field is HAL.UInt3;
   subtype DCR_FSIZE_Field is HAL.UInt5;

   --  device configuration register
   type DCR_Register is record
      --  Mode 0 / mode 3
      CKMODE         : Boolean := False;
      --  unspecified
      Reserved_1_7   : HAL.UInt7 := 16#0#;
      --  Chip select high time
      CSHT           : DCR_CSHT_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  FLASH memory size
      FSIZE          : DCR_FSIZE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : HAL.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR_Register use record
      CKMODE         at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      CSHT           at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      FSIZE          at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype SR_FLEVEL_Field is HAL.UInt7;

   --  status register
   type SR_Register is record
      --  Read-only. Transfer error flag
      TEF            : Boolean;
      --  Read-only. Transfer complete flag
      TCF            : Boolean;
      --  Read-only. FIFO threshold flag
      FTF            : Boolean;
      --  Read-only. Status match flag
      SMF            : Boolean;
      --  Read-only. Timeout flag
      TOF            : Boolean;
      --  Read-only. Busy
      BUSY           : Boolean;
      --  unspecified
      Reserved_6_7   : HAL.UInt2;
      --  Read-only. FIFO level
      FLEVEL         : SR_FLEVEL_Field;
      --  unspecified
      Reserved_15_31 : HAL.UInt17;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      TEF            at 0 range 0 .. 0;
      TCF            at 0 range 1 .. 1;
      FTF            at 0 range 2 .. 2;
      SMF            at 0 range 3 .. 3;
      TOF            at 0 range 4 .. 4;
      BUSY           at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      FLEVEL         at 0 range 8 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  flag clear register
   type FCR_Register is record
      --  Clear transfer error flag
      CTEF          : Boolean := False;
      --  Clear transfer complete flag
      CTCF          : Boolean := False;
      --  unspecified
      Reserved_2_2  : HAL.Bit := 16#0#;
      --  Clear status match flag
      CSMF          : Boolean := False;
      --  Clear timeout flag
      CTOF          : Boolean := False;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR_Register use record
      CTEF          at 0 range 0 .. 0;
      CTCF          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      CSMF          at 0 range 3 .. 3;
      CTOF          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype CCR_INSTRUCTION_Field is HAL.UInt8;
   subtype CCR_IMODE_Field is HAL.UInt2;
   subtype CCR_ADMODE_Field is HAL.UInt2;
   subtype CCR_ADSIZE_Field is HAL.UInt2;
   subtype CCR_ABMODE_Field is HAL.UInt2;
   subtype CCR_ABSIZE_Field is HAL.UInt2;
   subtype CCR_DCYC_Field is HAL.UInt5;
   subtype CCR_DMODE_Field is HAL.UInt2;
   subtype CCR_FMODE_Field is HAL.UInt2;

   --  communication configuration register
   type CCR_Register is record
      --  Instruction
      INSTRUCTION    : CCR_INSTRUCTION_Field := 16#0#;
      --  Instruction mode
      IMODE          : CCR_IMODE_Field := 16#0#;
      --  Address mode
      ADMODE         : CCR_ADMODE_Field := 16#0#;
      --  Address size
      ADSIZE         : CCR_ADSIZE_Field := 16#0#;
      --  Alternate bytes mode
      ABMODE         : CCR_ABMODE_Field := 16#0#;
      --  Alternate bytes size
      ABSIZE         : CCR_ABSIZE_Field := 16#0#;
      --  Number of dummy cycles
      DCYC           : CCR_DCYC_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : HAL.Bit := 16#0#;
      --  Data mode
      DMODE          : CCR_DMODE_Field := 16#0#;
      --  Functional mode
      FMODE          : CCR_FMODE_Field := 16#0#;
      --  Send instruction only once mode
      SIOO           : Boolean := False;
      --  unspecified
      Reserved_29_29 : HAL.Bit := 16#0#;
      --  DDR hold half cycle
      DHHC           : Boolean := False;
      --  Double data rate mode
      DDRM           : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      INSTRUCTION    at 0 range 0 .. 7;
      IMODE          at 0 range 8 .. 9;
      ADMODE         at 0 range 10 .. 11;
      ADSIZE         at 0 range 12 .. 13;
      ABMODE         at 0 range 14 .. 15;
      ABSIZE         at 0 range 16 .. 17;
      DCYC           at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      DMODE          at 0 range 24 .. 25;
      FMODE          at 0 range 26 .. 27;
      SIOO           at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      DHHC           at 0 range 30 .. 30;
      DDRM           at 0 range 31 .. 31;
   end record;

   subtype PIR_INTERVAL_Field is HAL.UInt16;

   --  polling interval register
   type PIR_Register is record
      --  Polling interval
      INTERVAL       : PIR_INTERVAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIR_Register use record
      INTERVAL       at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTR_TIMEOUT_Field is HAL.UInt16;

   --  low-power timeout register
   type LPTR_Register is record
      --  Timeout period
      TIMEOUT        : LPTR_TIMEOUT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTR_Register use record
      TIMEOUT        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  QuadSPI interface
   type QUADSPI_Peripheral is record
      --  control register
      CR    : aliased CR_Register;
      --  device configuration register
      DCR   : aliased DCR_Register;
      --  status register
      SR    : aliased SR_Register;
      --  flag clear register
      FCR   : aliased FCR_Register;
      --  data length register
      DLR   : aliased HAL.UInt32;
      --  communication configuration register
      CCR   : aliased CCR_Register;
      --  address register
      AR    : aliased HAL.UInt32;
      --  ABR
      ABR   : aliased HAL.UInt32;
      --  data register
      DR    : aliased HAL.UInt32;
      --  polling status mask register
      PSMKR : aliased HAL.UInt32;
      --  polling status match register
      PSMAR : aliased HAL.UInt32;
      --  polling interval register
      PIR   : aliased PIR_Register;
      --  low-power timeout register
      LPTR  : aliased LPTR_Register;
   end record
     with Volatile;

   for QUADSPI_Peripheral use record
      CR    at 16#0# range 0 .. 31;
      DCR   at 16#4# range 0 .. 31;
      SR    at 16#8# range 0 .. 31;
      FCR   at 16#C# range 0 .. 31;
      DLR   at 16#10# range 0 .. 31;
      CCR   at 16#14# range 0 .. 31;
      AR    at 16#18# range 0 .. 31;
      ABR   at 16#1C# range 0 .. 31;
      DR    at 16#20# range 0 .. 31;
      PSMKR at 16#24# range 0 .. 31;
      PSMAR at 16#28# range 0 .. 31;
      PIR   at 16#2C# range 0 .. 31;
      LPTR  at 16#30# range 0 .. 31;
   end record;

   --  QuadSPI interface
   QUADSPI_Periph : aliased QUADSPI_Peripheral
     with Import, Address => System'To_Address (16#A0001000#);

end STM32_SVD.QUADSPI;
