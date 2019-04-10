--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.WDT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Password
   type WDT_CR_KEY_Field is
     (
      --  Reset value for the field
      Wdt_Cr_Key_Field_Reset,
      --  Writing any other value in this field aborts the write operation.
      Passwd)
     with Size => 8;
   for WDT_CR_KEY_Field use
     (Wdt_Cr_Key_Field_Reset => 0,
      Passwd => 165);

   --  Control Register
   type WDT_WDT_CR_Register is record
      --  Write-only. Watchdog Restart
      WDRSTT        : Boolean := False;
      --  unspecified
      Reserved_1_23 : HAL.UInt23 := 16#0#;
      --  Write-only. Password
      KEY           : WDT_CR_KEY_Field := Wdt_Cr_Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDT_WDT_CR_Register use record
      WDRSTT        at 0 range 0 .. 0;
      Reserved_1_23 at 0 range 1 .. 23;
      KEY           at 0 range 24 .. 31;
   end record;

   subtype WDT_WDT_MR_WDV_Field is HAL.UInt12;
   subtype WDT_WDT_MR_WDD_Field is HAL.UInt12;

   --  Mode Register
   type WDT_WDT_MR_Register is record
      --  Watchdog Counter Value
      WDV            : WDT_WDT_MR_WDV_Field := 16#0#;
      --  Watchdog Fault Interrupt Enable
      WDFIEN         : Boolean := False;
      --  Watchdog Reset Enable
      WDRSTEN        : Boolean := False;
      --  unspecified
      Reserved_14_14 : HAL.Bit := 16#0#;
      --  Watchdog Disable
      WDDIS          : Boolean := False;
      --  Watchdog Delta Value
      WDD            : WDT_WDT_MR_WDD_Field := 16#0#;
      --  Watchdog Debug Halt
      WDDBGHLT       : Boolean := False;
      --  Watchdog Idle Halt
      WDIDLEHLT      : Boolean := False;
      --  unspecified
      Reserved_30_31 : HAL.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDT_WDT_MR_Register use record
      WDV            at 0 range 0 .. 11;
      WDFIEN         at 0 range 12 .. 12;
      WDRSTEN        at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      WDDIS          at 0 range 15 .. 15;
      WDD            at 0 range 16 .. 27;
      WDDBGHLT       at 0 range 28 .. 28;
      WDIDLEHLT      at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  Status Register
   type WDT_WDT_SR_Register is record
      --  Read-only. Watchdog Underflow (cleared on read)
      WDUNF         : Boolean;
      --  Read-only. Watchdog Error (cleared on read)
      WDERR         : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for WDT_WDT_SR_Register use record
      WDUNF         at 0 range 0 .. 0;
      WDERR         at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Watchdog Timer
   type WDT_Peripheral is record
      --  Control Register
      WDT_CR : aliased WDT_WDT_CR_Register;
      --  Mode Register
      WDT_MR : aliased WDT_WDT_MR_Register;
      --  Status Register
      WDT_SR : aliased WDT_WDT_SR_Register;
   end record
     with Volatile;

   for WDT_Peripheral use record
      WDT_CR at 16#0# range 0 .. 31;
      WDT_MR at 16#4# range 0 .. 31;
      WDT_SR at 16#8# range 0 .. 31;
   end record;

   --  Watchdog Timer
   WDT_Periph : aliased WDT_Peripheral
     with Import, Address => System'To_Address (16#400E1850#);

end SAM_SVD.WDT;
