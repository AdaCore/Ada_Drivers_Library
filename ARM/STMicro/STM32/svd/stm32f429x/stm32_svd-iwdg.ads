--  Automatically generated from STM32F429x.svd2ada by SVD2Ada
--  see https://github.com/AdaCore/svd2ada

pragma Restrictions (No_Elaboration_Code);

with STM32_SVD;
with System;

package STM32_SVD.IWDG is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   -----------------
   -- KR_Register --
   -----------------

   subtype KR_KEY_Field is STM32_SVD.Short;

   --  Key register
   type KR_Register is record
      --  Key value (write only, read 0000h)
      KEY            : KR_KEY_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32_SVD.Short := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for KR_Register use record
      KEY            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- PR_Register --
   -----------------

   subtype PR_PR_Field is STM32_SVD.UInt3;

   --  Prescaler register
   type PR_Register is record
      --  Prescaler divider
      PR            : PR_PR_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : STM32_SVD.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PR_Register use record
      PR            at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   ------------------
   -- RLR_Register --
   ------------------

   subtype RLR_RL_Field is STM32_SVD.UInt12;

   --  Reload register
   type RLR_Register is record
      --  Watchdog counter reload value
      RL             : RLR_RL_Field := 16#FFF#;
      --  unspecified
      Reserved_12_31 : STM32_SVD.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RLR_Register use record
      RL             at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   -----------------
   -- SR_Register --
   -----------------

   subtype SR_PVU_Field is STM32_SVD.Bit;
   subtype SR_RVU_Field is STM32_SVD.Bit;

   --  Status register
   type SR_Register is record
      --  Watchdog prescaler value update
      PVU           : SR_PVU_Field;
      --  Watchdog counter reload value update
      RVU           : SR_RVU_Field;
      --  unspecified
      Reserved_2_31 : STM32_SVD.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      PVU           at 0 range 0 .. 0;
      RVU           at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Independent watchdog
   type IWDG_Peripheral is record
      --  Key register
      KR  : KR_Register;
      --  Prescaler register
      PR  : PR_Register;
      --  Reload register
      RLR : RLR_Register;
      --  Status register
      SR  : SR_Register;
   end record
     with Volatile;

   for IWDG_Peripheral use record
      KR  at 0 range 0 .. 31;
      PR  at 4 range 0 .. 31;
      RLR at 8 range 0 .. 31;
      SR  at 12 range 0 .. 31;
   end record;

   --  Independent watchdog
   IWDG_Periph : aliased IWDG_Peripheral
     with Import, Address => System'To_Address (16#40003000#);

end STM32_SVD.IWDG;
