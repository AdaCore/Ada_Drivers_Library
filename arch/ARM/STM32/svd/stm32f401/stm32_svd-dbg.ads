--  This spec has been automatically generated from STM32F401.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package STM32_SVD.DBG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype DBGMCU_IDCODE_DEV_ID_Field is HAL.UInt12;
   subtype DBGMCU_IDCODE_REV_ID_Field is HAL.UInt16;

   --  IDCODE
   type DBGMCU_IDCODE_Register is record
      --  Read-only. DEV_ID
      DEV_ID         : DBGMCU_IDCODE_DEV_ID_Field;
      --  unspecified
      Reserved_12_15 : HAL.UInt4;
      --  Read-only. REV_ID
      REV_ID         : DBGMCU_IDCODE_REV_ID_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_IDCODE_Register use record
      DEV_ID         at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      REV_ID         at 0 range 16 .. 31;
   end record;

   subtype DBGMCU_CR_TRACE_MODE_Field is HAL.UInt2;

   --  Control Register
   type DBGMCU_CR_Register is record
      --  DBG_SLEEP
      DBG_SLEEP     : Boolean := False;
      --  DBG_STOP
      DBG_STOP      : Boolean := False;
      --  DBG_STANDBY
      DBG_STANDBY   : Boolean := False;
      --  unspecified
      Reserved_3_4  : HAL.UInt2 := 16#0#;
      --  TRACE_IOEN
      TRACE_IOEN    : Boolean := False;
      --  TRACE_MODE
      TRACE_MODE    : DBGMCU_CR_TRACE_MODE_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_CR_Register use record
      DBG_SLEEP     at 0 range 0 .. 0;
      DBG_STOP      at 0 range 1 .. 1;
      DBG_STANDBY   at 0 range 2 .. 2;
      Reserved_3_4  at 0 range 3 .. 4;
      TRACE_IOEN    at 0 range 5 .. 5;
      TRACE_MODE    at 0 range 6 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Debug MCU APB1 Freeze registe
   type DBGMCU_APB1_FZ_Register is record
      --  DBG_TIM2_STOP
      DBG_TIM2_STOP          : Boolean := False;
      --  DBG_TIM3 _STOP
      DBG_TIM3_STOP          : Boolean := False;
      --  DBG_TIM4_STOP
      DBG_TIM4_STOP          : Boolean := False;
      --  DBG_TIM5_STOP
      DBG_TIM5_STOP          : Boolean := False;
      --  unspecified
      Reserved_4_9           : HAL.UInt6 := 16#0#;
      --  RTC stopped when Core is halted
      DBG_RTC_Stop           : Boolean := False;
      --  DBG_WWDG_STOP
      DBG_WWDG_STOP          : Boolean := False;
      --  DBG_IWDEG_STOP
      DBG_IWDEG_STOP         : Boolean := False;
      --  unspecified
      Reserved_13_20         : HAL.UInt8 := 16#0#;
      --  DBG_J2C1_SMBUS_TIMEOUT
      DBG_I2C1_SMBUS_TIMEOUT : Boolean := False;
      --  DBG_J2C2_SMBUS_TIMEOUT
      DBG_I2C2_SMBUS_TIMEOUT : Boolean := False;
      --  DBG_J2C3SMBUS_TIMEOUT
      DBG_I2C3SMBUS_TIMEOUT  : Boolean := False;
      --  unspecified
      Reserved_24_31         : HAL.UInt8 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_APB1_FZ_Register use record
      DBG_TIM2_STOP          at 0 range 0 .. 0;
      DBG_TIM3_STOP          at 0 range 1 .. 1;
      DBG_TIM4_STOP          at 0 range 2 .. 2;
      DBG_TIM5_STOP          at 0 range 3 .. 3;
      Reserved_4_9           at 0 range 4 .. 9;
      DBG_RTC_Stop           at 0 range 10 .. 10;
      DBG_WWDG_STOP          at 0 range 11 .. 11;
      DBG_IWDEG_STOP         at 0 range 12 .. 12;
      Reserved_13_20         at 0 range 13 .. 20;
      DBG_I2C1_SMBUS_TIMEOUT at 0 range 21 .. 21;
      DBG_I2C2_SMBUS_TIMEOUT at 0 range 22 .. 22;
      DBG_I2C3SMBUS_TIMEOUT  at 0 range 23 .. 23;
      Reserved_24_31         at 0 range 24 .. 31;
   end record;

   --  Debug MCU APB2 Freeze registe
   type DBGMCU_APB2_FZ_Register is record
      --  TIM1 counter stopped when core is halted
      DBG_TIM1_STOP  : Boolean := False;
      --  unspecified
      Reserved_1_15  : HAL.UInt15 := 16#0#;
      --  TIM9 counter stopped when core is halted
      DBG_TIM9_STOP  : Boolean := False;
      --  TIM10 counter stopped when core is halted
      DBG_TIM10_STOP : Boolean := False;
      --  TIM11 counter stopped when core is halted
      DBG_TIM11_STOP : Boolean := False;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBGMCU_APB2_FZ_Register use record
      DBG_TIM1_STOP  at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      DBG_TIM9_STOP  at 0 range 16 .. 16;
      DBG_TIM10_STOP at 0 range 17 .. 17;
      DBG_TIM11_STOP at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Debug support
   type DBG_Peripheral is record
      --  IDCODE
      DBGMCU_IDCODE  : aliased DBGMCU_IDCODE_Register;
      --  Control Register
      DBGMCU_CR      : aliased DBGMCU_CR_Register;
      --  Debug MCU APB1 Freeze registe
      DBGMCU_APB1_FZ : aliased DBGMCU_APB1_FZ_Register;
      --  Debug MCU APB2 Freeze registe
      DBGMCU_APB2_FZ : aliased DBGMCU_APB2_FZ_Register;
   end record
     with Volatile;

   for DBG_Peripheral use record
      DBGMCU_IDCODE  at 16#0# range 0 .. 31;
      DBGMCU_CR      at 16#4# range 0 .. 31;
      DBGMCU_APB1_FZ at 16#8# range 0 .. 31;
      DBGMCU_APB2_FZ at 16#C# range 0 .. 31;
   end record;

   --  Debug support
   DBG_Periph : aliased DBG_Peripheral
     with Import, Address => DBG_Base;

end STM32_SVD.DBG;
