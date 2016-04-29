--  This spec has been automatically generated from STM32F7x.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;

with HAL;
with System;

package STM32_SVD.LPTIM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ------------------
   -- ISR_Register --
   ------------------

   --  Interrupt and Status Register
   type ISR_Register is record
      --  Read-only. Compare match
      CMPM          : Boolean;
      --  Read-only. Autoreload match
      ARRM          : Boolean;
      --  Read-only. External trigger edge event
      EXTTRIG       : Boolean;
      --  Read-only. Compare register update OK
      CMPOK         : Boolean;
      --  Read-only. Autoreload register update OK
      ARROK         : Boolean;
      --  Read-only. Counter direction change down to up
      UP            : Boolean;
      --  Read-only. Counter direction change up to down
      DOWN          : Boolean;
      --  unspecified
      Reserved_7_31 : HAL.UInt25;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      CMPM          at 0 range 0 .. 0;
      ARRM          at 0 range 1 .. 1;
      EXTTRIG       at 0 range 2 .. 2;
      CMPOK         at 0 range 3 .. 3;
      ARROK         at 0 range 4 .. 4;
      UP            at 0 range 5 .. 5;
      DOWN          at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   ------------------
   -- ICR_Register --
   ------------------

   --  Interrupt Clear Register
   type ICR_Register is record
      --  Write-only. compare match Clear Flag
      CMPMCF        : Boolean := False;
      --  Write-only. Autoreload match Clear Flag
      ARRMCF        : Boolean := False;
      --  Write-only. External trigger valid edge Clear Flag
      EXTTRIGCF     : Boolean := False;
      --  Write-only. Compare register update OK Clear Flag
      CMPOKCF       : Boolean := False;
      --  Write-only. Autoreload register update OK Clear Flag
      ARROKCF       : Boolean := False;
      --  Write-only. Direction change to UP Clear Flag
      UPCF          : Boolean := False;
      --  Write-only. Direction change to down Clear Flag
      DOWNCF        : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      CMPMCF        at 0 range 0 .. 0;
      ARRMCF        at 0 range 1 .. 1;
      EXTTRIGCF     at 0 range 2 .. 2;
      CMPOKCF       at 0 range 3 .. 3;
      ARROKCF       at 0 range 4 .. 4;
      UPCF          at 0 range 5 .. 5;
      DOWNCF        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   ------------------
   -- IER_Register --
   ------------------

   --  Interrupt Enable Register
   type IER_Register is record
      --  Compare match Interrupt Enable
      CMPMIE        : Boolean := False;
      --  Autoreload match Interrupt Enable
      ARRMIE        : Boolean := False;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE     : Boolean := False;
      --  Compare register update OK Interrupt Enable
      CMPOKIE       : Boolean := False;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE       : Boolean := False;
      --  Direction change to UP Interrupt Enable
      UPIE          : Boolean := False;
      --  Direction change to down Interrupt Enable
      DOWNIE        : Boolean := False;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      CMPMIE        at 0 range 0 .. 0;
      ARRMIE        at 0 range 1 .. 1;
      EXTTRIGIE     at 0 range 2 .. 2;
      CMPOKIE       at 0 range 3 .. 3;
      ARROKIE       at 0 range 4 .. 4;
      UPIE          at 0 range 5 .. 5;
      DOWNIE        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -------------------
   -- CFGR_Register --
   -------------------

   subtype CFGR_CKPOL_Field is HAL.UInt2;
   subtype CFGR_CKFLT_Field is HAL.UInt2;
   subtype CFGR_TRGFLT_Field is HAL.UInt2;
   subtype CFGR_PRESC_Field is HAL.UInt3;
   subtype CFGR_TRIGSEL_Field is HAL.UInt3;
   subtype CFGR_TRIGEN_Field is HAL.UInt2;

   --  Configuration Register
   type CFGR_Register is record
      --  Clock selector
      CKSEL          : Boolean := False;
      --  Clock Polarity
      CKPOL          : CFGR_CKPOL_Field := 16#0#;
      --  Configurable digital filter for external clock
      CKFLT          : CFGR_CKFLT_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : HAL.Bit := 16#0#;
      --  Configurable digital filter for trigger
      TRGFLT         : CFGR_TRGFLT_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Clock prescaler
      PRESC          : CFGR_PRESC_Field := 16#0#;
      --  unspecified
      Reserved_12_12 : HAL.Bit := 16#0#;
      --  Trigger selector
      TRIGSEL        : CFGR_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : HAL.Bit := 16#0#;
      --  Trigger enable and polarity
      TRIGEN         : CFGR_TRIGEN_Field := 16#0#;
      --  Timeout enable
      TIMOUT         : Boolean := False;
      --  Waveform shape
      WAVE           : Boolean := False;
      --  Waveform shape polarity
      WAVPOL         : Boolean := False;
      --  Registers update mode
      PRELOAD        : Boolean := False;
      --  counter mode enabled
      COUNTMODE      : Boolean := False;
      --  Encoder mode enable
      ENC            : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_Register use record
      CKSEL          at 0 range 0 .. 0;
      CKPOL          at 0 range 1 .. 2;
      CKFLT          at 0 range 3 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      TRGFLT         at 0 range 6 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      PRESC          at 0 range 9 .. 11;
      Reserved_12_12 at 0 range 12 .. 12;
      TRIGSEL        at 0 range 13 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      TRIGEN         at 0 range 17 .. 18;
      TIMOUT         at 0 range 19 .. 19;
      WAVE           at 0 range 20 .. 20;
      WAVPOL         at 0 range 21 .. 21;
      PRELOAD        at 0 range 22 .. 22;
      COUNTMODE      at 0 range 23 .. 23;
      ENC            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   -----------------
   -- CR_Register --
   -----------------

   --  Control Register
   type CR_Register is record
      --  LPTIM Enable
      ENABLE        : Boolean := False;
      --  LPTIM start in single mode
      SNGSTRT       : Boolean := False;
      --  Timer start in continuous mode
      CNTSTRT       : Boolean := False;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      ENABLE        at 0 range 0 .. 0;
      SNGSTRT       at 0 range 1 .. 1;
      CNTSTRT       at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   ------------------
   -- CMP_Register --
   ------------------

   subtype CMP_CMP_Field is HAL.Short;

   --  Compare Register
   type CMP_Register is record
      --  Compare value
      CMP            : CMP_CMP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CMP_Register use record
      CMP            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------
   -- ARR_Register --
   ------------------

   subtype ARR_ARR_Field is HAL.Short;

   --  Autoreload Register
   type ARR_Register is record
      --  Auto reload value
      ARR            : ARR_ARR_Field := 16#1#;
      --  unspecified
      Reserved_16_31 : HAL.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ARR_Register use record
      ARR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   ------------------
   -- CNT_Register --
   ------------------

   subtype CNT_CNT_Field is HAL.Short;

   --  Counter Register
   type CNT_Register is record
      --  Read-only. Counter value
      CNT            : CNT_CNT_Field;
      --  unspecified
      Reserved_16_31 : HAL.Short;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Low power timer
   type LPTIM1_Peripheral is record
      --  Interrupt and Status Register
      ISR  : ISR_Register;
      --  Interrupt Clear Register
      ICR  : ICR_Register;
      --  Interrupt Enable Register
      IER  : IER_Register;
      --  Configuration Register
      CFGR : CFGR_Register;
      --  Control Register
      CR   : CR_Register;
      --  Compare Register
      CMP  : CMP_Register;
      --  Autoreload Register
      ARR  : ARR_Register;
      --  Counter Register
      CNT  : CNT_Register;
   end record
     with Volatile;

   for LPTIM1_Peripheral use record
      ISR  at 0 range 0 .. 31;
      ICR  at 4 range 0 .. 31;
      IER  at 8 range 0 .. 31;
      CFGR at 12 range 0 .. 31;
      CR   at 16 range 0 .. 31;
      CMP  at 20 range 0 .. 31;
      ARR  at 24 range 0 .. 31;
      CNT  at 28 range 0 .. 31;
   end record;

   --  Low power timer
   LPTIM1_Periph : aliased LPTIM1_Peripheral
     with Import, Address => LPTIM1_Base;

end STM32_SVD.LPTIM;
