--  Automatically generated from STM32F7x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.LPTIM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   ------------------
   -- ISR_Register --
   ------------------

   subtype ISR_CMPM_Field is STM32_SVD.Bit;
   subtype ISR_ARRM_Field is STM32_SVD.Bit;
   subtype ISR_EXTTRIG_Field is STM32_SVD.Bit;
   subtype ISR_CMPOK_Field is STM32_SVD.Bit;
   subtype ISR_ARROK_Field is STM32_SVD.Bit;
   subtype ISR_UP_Field is STM32_SVD.Bit;
   subtype ISR_DOWN_Field is STM32_SVD.Bit;

   --  Interrupt and Status Register
   type ISR_Register is record
      --  Compare match
      CMPM          : ISR_CMPM_Field := 16#0#;
      --  Autoreload match
      ARRM          : ISR_ARRM_Field := 16#0#;
      --  External trigger edge event
      EXTTRIG       : ISR_EXTTRIG_Field := 16#0#;
      --  Compare register update OK
      CMPOK         : ISR_CMPOK_Field := 16#0#;
      --  Autoreload register update OK
      ARROK         : ISR_ARROK_Field := 16#0#;
      --  Counter direction change down to up
      UP            : ISR_UP_Field := 16#0#;
      --  Counter direction change up to down
      DOWN          : ISR_DOWN_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : STM32_SVD.UInt25 := 16#0#;
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

   subtype ICR_CMPMCF_Field is STM32_SVD.Bit;
   subtype ICR_ARRMCF_Field is STM32_SVD.Bit;
   subtype ICR_EXTTRIGCF_Field is STM32_SVD.Bit;
   subtype ICR_CMPOKCF_Field is STM32_SVD.Bit;
   subtype ICR_ARROKCF_Field is STM32_SVD.Bit;
   subtype ICR_UPCF_Field is STM32_SVD.Bit;
   subtype ICR_DOWNCF_Field is STM32_SVD.Bit;

   --  Interrupt Clear Register
   type ICR_Register is record
      --  compare match Clear Flag
      CMPMCF        : ICR_CMPMCF_Field := 16#0#;
      --  Autoreload match Clear Flag
      ARRMCF        : ICR_ARRMCF_Field := 16#0#;
      --  External trigger valid edge Clear Flag
      EXTTRIGCF     : ICR_EXTTRIGCF_Field := 16#0#;
      --  Compare register update OK Clear Flag
      CMPOKCF       : ICR_CMPOKCF_Field := 16#0#;
      --  Autoreload register update OK Clear Flag
      ARROKCF       : ICR_ARROKCF_Field := 16#0#;
      --  Direction change to UP Clear Flag
      UPCF          : ICR_UPCF_Field := 16#0#;
      --  Direction change to down Clear Flag
      DOWNCF        : ICR_DOWNCF_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : STM32_SVD.UInt25 := 16#0#;
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

   subtype IER_CMPMIE_Field is STM32_SVD.Bit;
   subtype IER_ARRMIE_Field is STM32_SVD.Bit;
   subtype IER_EXTTRIGIE_Field is STM32_SVD.Bit;
   subtype IER_CMPOKIE_Field is STM32_SVD.Bit;
   subtype IER_ARROKIE_Field is STM32_SVD.Bit;
   subtype IER_UPIE_Field is STM32_SVD.Bit;
   subtype IER_DOWNIE_Field is STM32_SVD.Bit;

   --  Interrupt Enable Register
   type IER_Register is record
      --  Compare match Interrupt Enable
      CMPMIE        : IER_CMPMIE_Field := 16#0#;
      --  Autoreload match Interrupt Enable
      ARRMIE        : IER_ARRMIE_Field := 16#0#;
      --  External trigger valid edge Interrupt Enable
      EXTTRIGIE     : IER_EXTTRIGIE_Field := 16#0#;
      --  Compare register update OK Interrupt Enable
      CMPOKIE       : IER_CMPOKIE_Field := 16#0#;
      --  Autoreload register update OK Interrupt Enable
      ARROKIE       : IER_ARROKIE_Field := 16#0#;
      --  Direction change to UP Interrupt Enable
      UPIE          : IER_UPIE_Field := 16#0#;
      --  Direction change to down Interrupt Enable
      DOWNIE        : IER_DOWNIE_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : STM32_SVD.UInt25 := 16#0#;
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

   subtype CFGR_CKSEL_Field is STM32_SVD.Bit;
   subtype CFGR_CKPOL_Field is STM32_SVD.UInt2;
   subtype CFGR_CKFLT_Field is STM32_SVD.UInt2;
   subtype CFGR_TRGFLT_Field is STM32_SVD.UInt2;
   subtype CFGR_PRESC_Field is STM32_SVD.UInt3;
   subtype CFGR_TRIGSEL_Field is STM32_SVD.UInt3;
   subtype CFGR_TRIGEN_Field is STM32_SVD.UInt2;
   subtype CFGR_TIMOUT_Field is STM32_SVD.Bit;
   subtype CFGR_WAVE_Field is STM32_SVD.Bit;
   subtype CFGR_WAVPOL_Field is STM32_SVD.Bit;
   subtype CFGR_PRELOAD_Field is STM32_SVD.Bit;
   subtype CFGR_COUNTMODE_Field is STM32_SVD.Bit;
   subtype CFGR_ENC_Field is STM32_SVD.Bit;

   --  Configuration Register
   type CFGR_Register is record
      --  Clock selector
      CKSEL          : CFGR_CKSEL_Field := 16#0#;
      --  Clock Polarity
      CKPOL          : CFGR_CKPOL_Field := 16#0#;
      --  Configurable digital filter for external clock
      CKFLT          : CFGR_CKFLT_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : STM32_SVD.Bit := 16#0#;
      --  Configurable digital filter for trigger
      TRGFLT         : CFGR_TRGFLT_Field := 16#0#;
      --  unspecified
      Reserved_8_8   : STM32_SVD.Bit := 16#0#;
      --  Clock prescaler
      PRESC          : CFGR_PRESC_Field := 16#0#;
      --  unspecified
      Reserved_12_12 : STM32_SVD.Bit := 16#0#;
      --  Trigger selector
      TRIGSEL        : CFGR_TRIGSEL_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : STM32_SVD.Bit := 16#0#;
      --  Trigger enable and polarity
      TRIGEN         : CFGR_TRIGEN_Field := 16#0#;
      --  Timeout enable
      TIMOUT         : CFGR_TIMOUT_Field := 16#0#;
      --  Waveform shape
      WAVE           : CFGR_WAVE_Field := 16#0#;
      --  Waveform shape polarity
      WAVPOL         : CFGR_WAVPOL_Field := 16#0#;
      --  Registers update mode
      PRELOAD        : CFGR_PRELOAD_Field := 16#0#;
      --  counter mode enabled
      COUNTMODE      : CFGR_COUNTMODE_Field := 16#0#;
      --  Encoder mode enable
      ENC            : CFGR_ENC_Field := 16#0#;
      --  unspecified
      Reserved_25_31 : STM32_SVD.UInt7 := 16#0#;
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

   subtype CR_ENABLE_Field is STM32_SVD.Bit;
   subtype CR_SNGSTRT_Field is STM32_SVD.Bit;
   subtype CR_CNTSTRT_Field is STM32_SVD.Bit;

   --  Control Register
   type CR_Register is record
      --  LPTIM Enable
      ENABLE        : CR_ENABLE_Field := 16#0#;
      --  LPTIM start in single mode
      SNGSTRT       : CR_SNGSTRT_Field := 16#0#;
      --  Timer start in continuous mode
      CNTSTRT       : CR_CNTSTRT_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : STM32_SVD.UInt29 := 16#0#;
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

   subtype CMP_CMP_Field is STM32_SVD.Short;

   --  Compare Register
   type CMP_Register is record
      --  Compare value
      CMP            : CMP_CMP_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
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

   subtype ARR_ARR_Field is STM32_SVD.Short;

   --  Autoreload Register
   type ARR_Register is record
      --  Auto reload value
      ARR            : ARR_ARR_Field := 16#1#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
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

   subtype CNT_CNT_Field is STM32_SVD.Short;

   --  Counter Register
   type CNT_Register is record
      --  Counter value
      CNT            : CNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
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
     with Import, Address => System'To_Address (16#40002400#);

end STM32_SVD.LPTIM;
