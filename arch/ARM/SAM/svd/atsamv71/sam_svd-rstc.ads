--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.RSTC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  System Reset Key
   type RSTC_CR_KEY_Field is
     (
      --  Reset value for the field
      Rstc_Cr_Key_Field_Reset,
      --  Writing any other value in this field aborts the write operation.
      Passwd)
     with Size => 8;
   for RSTC_CR_KEY_Field use
     (Rstc_Cr_Key_Field_Reset => 0,
      Passwd => 165);

   --  Control Register
   type RSTC_RSTC_CR_Register is record
      --  Write-only. Processor Reset
      PROCRST       : Boolean := False;
      --  unspecified
      Reserved_1_2  : HAL.UInt2 := 16#0#;
      --  Write-only. External Reset
      EXTRST        : Boolean := False;
      --  unspecified
      Reserved_4_23 : HAL.UInt20 := 16#0#;
      --  Write-only. System Reset Key
      KEY           : RSTC_CR_KEY_Field := Rstc_Cr_Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTC_RSTC_CR_Register use record
      PROCRST       at 0 range 0 .. 0;
      Reserved_1_2  at 0 range 1 .. 2;
      EXTRST        at 0 range 3 .. 3;
      Reserved_4_23 at 0 range 4 .. 23;
      KEY           at 0 range 24 .. 31;
   end record;

   --  Reset Type
   type RSTC_SR_RSTTYP_Field is
     (
      --  First powerup reset
      General_Rst,
      --  Return from Backup mode
      Backup_Rst,
      --  Watchdog fault occurred
      Wdt_Rst,
      --  Processor reset required by the software
      Soft_Rst,
      --  NRST pin detected low
      User_Rst)
     with Size => 3;
   for RSTC_SR_RSTTYP_Field use
     (General_Rst => 0,
      Backup_Rst => 1,
      Wdt_Rst => 2,
      Soft_Rst => 3,
      User_Rst => 4);

   --  Status Register
   type RSTC_RSTC_SR_Register is record
      --  Read-only. User Reset Status
      URSTS          : Boolean;
      --  unspecified
      Reserved_1_7   : HAL.UInt7;
      --  Read-only. Reset Type
      RSTTYP         : RSTC_SR_RSTTYP_Field;
      --  unspecified
      Reserved_11_15 : HAL.UInt5;
      --  Read-only. NRST Pin Level
      NRSTL          : Boolean;
      --  Read-only. Software Reset Command in Progress
      SRCMP          : Boolean;
      --  unspecified
      Reserved_18_31 : HAL.UInt14;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTC_RSTC_SR_Register use record
      URSTS          at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      RSTTYP         at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      NRSTL          at 0 range 16 .. 16;
      SRCMP          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype RSTC_RSTC_MR_ERSTL_Field is HAL.UInt4;

   --  Write Access Password
   type RSTC_MR_KEY_Field is
     (
      --  Reset value for the field
      Rstc_Mr_Key_Field_Reset,
      --  Writing any other value in this field aborts the write
      --  operation.Always reads as 0.
      Passwd)
     with Size => 8;
   for RSTC_MR_KEY_Field use
     (Rstc_Mr_Key_Field_Reset => 0,
      Passwd => 165);

   --  Mode Register
   type RSTC_RSTC_MR_Register is record
      --  User Reset Enable
      URSTEN         : Boolean := False;
      --  unspecified
      Reserved_1_3   : HAL.UInt3 := 16#0#;
      --  User Reset Interrupt Enable
      URSTIEN        : Boolean := False;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  External Reset Length
      ERSTL          : RSTC_RSTC_MR_ERSTL_Field := 16#0#;
      --  unspecified
      Reserved_12_23 : HAL.UInt12 := 16#0#;
      --  Write Access Password
      KEY            : RSTC_MR_KEY_Field := Rstc_Mr_Key_Field_Reset;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RSTC_RSTC_MR_Register use record
      URSTEN         at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      URSTIEN        at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      ERSTL          at 0 range 8 .. 11;
      Reserved_12_23 at 0 range 12 .. 23;
      KEY            at 0 range 24 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Reset Controller
   type RSTC_Peripheral is record
      --  Control Register
      RSTC_CR : aliased RSTC_RSTC_CR_Register;
      --  Status Register
      RSTC_SR : aliased RSTC_RSTC_SR_Register;
      --  Mode Register
      RSTC_MR : aliased RSTC_RSTC_MR_Register;
   end record
     with Volatile;

   for RSTC_Peripheral use record
      RSTC_CR at 16#0# range 0 .. 31;
      RSTC_SR at 16#4# range 0 .. 31;
      RSTC_MR at 16#8# range 0 .. 31;
   end record;

   --  Reset Controller
   RSTC_Periph : aliased RSTC_Peripheral
     with Import, Address => System'To_Address (16#400E1800#);

end SAM_SVD.RSTC;
