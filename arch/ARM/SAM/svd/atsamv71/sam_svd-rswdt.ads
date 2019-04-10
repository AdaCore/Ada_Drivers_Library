--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.RSWDT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Password
   type RSWDT_CR_KEY_Field is
     (
      --  Reset value for the field
      Rswdt_Cr_Key_Field_Reset,
      --  Writing any other value in this field aborts the write operation.
      Passwd)
     with Size => 8;
   for RSWDT_CR_KEY_Field use
     (Rswdt_Cr_Key_Field_Reset => 0,
      Passwd => 196);

   --  Control Register
   type RSWDT_RSWDT_CR_Register is record
      --  Write-only. Watchdog Restart
      WDRSTT        : Boolean := False;
      --  unspecified
      Reserved_1_23 : HAL.UInt23 := 16#0#;
      --  Write-only. Password
      KEY           : RSWDT_CR_KEY_Field := Rswdt_Cr_Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSWDT_RSWDT_CR_Register use record
      WDRSTT        at 0 range 0 .. 0;
      Reserved_1_23 at 0 range 1 .. 23;
      KEY           at 0 range 24 .. 31;
   end record;

   subtype RSWDT_RSWDT_MR_WDV_Field is HAL.UInt12;
   subtype RSWDT_RSWDT_MR_ALLONES_Field is HAL.UInt12;

   --  Mode Register
   type RSWDT_RSWDT_MR_Register is record
      --  Watchdog Counter Value
      WDV            : RSWDT_RSWDT_MR_WDV_Field := 16#0#;
      --  Watchdog Fault Interrupt Enable
      WDFIEN         : Boolean := False;
      --  Watchdog Reset Enable
      WDRSTEN        : Boolean := False;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  Watchdog Disable
      WDDIS          : Boolean := False;
      --  Must Always Be Written with 0xFFF
      ALLONES        : RSWDT_RSWDT_MR_ALLONES_Field := 16#0#;
      --  Watchdog Debug Halt
      WDDBGHLT       : Boolean := False;
      --  Watchdog Idle Halt
      WDIDLEHLT      : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSWDT_RSWDT_MR_Register use record
      WDV            at 0 range 0 .. 11;
      WDFIEN         at 0 range 12 .. 12;
      WDRSTEN        at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      WDDIS          at 0 range 15 .. 15;
      ALLONES        at 0 range 16 .. 27;
      WDDBGHLT       at 0 range 28 .. 28;
      WDIDLEHLT      at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Status Register
   type RSWDT_RSWDT_SR_Register is record
      --  Read-only. Watchdog Underflow
      WDUNF         : Boolean;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSWDT_RSWDT_SR_Register use record
      WDUNF         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Reinforced Safety Watchdog Timer
   type RSWDT_Peripheral is record
      --  Control Register
      RSWDT_CR : aliased RSWDT_RSWDT_CR_Register;
      --  Mode Register
      RSWDT_MR : aliased RSWDT_RSWDT_MR_Register;
      --  Status Register
      RSWDT_SR : aliased RSWDT_RSWDT_SR_Register;
   end record
     with Volatile;

   for RSWDT_Peripheral use record
      RSWDT_CR at 16#0# range 0 .. 31;
      RSWDT_MR at 16#4# range 0 .. 31;
      RSWDT_SR at 16#8# range 0 .. 31;
   end record;

   --  Reinforced Safety Watchdog Timer
   RSWDT_Periph : aliased RSWDT_Peripheral
     with Import, Address => System'To_Address (16#400E1900#);

end SAM_SVD.RSWDT;
