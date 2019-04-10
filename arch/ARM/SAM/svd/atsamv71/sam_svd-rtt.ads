--  This spec has been automatically generated from ATSAMV71Q21.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package SAM_SVD.RTT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   subtype RTT_RTT_MR_RTPRES_Field is HAL.UInt16;

   --  Mode Register
   type RTT_RTT_MR_Register is record
      --  Real-time Timer Prescaler Value
      RTPRES         : RTT_RTT_MR_RTPRES_Field := 16#0#;
      --  Alarm Interrupt Enable
      ALMIEN         : Boolean := False;
      --  Real-time Timer Increment Interrupt Enable
      RTTINCIEN      : Boolean := False;
      --  Real-time Timer Restart
      RTTRST         : Boolean := False;
      --  unspecified
      Reserved_19_19 : HAL.Bit := 16#0#;
      --  Real-time Timer Disable
      RTTDIS         : Boolean := False;
      --  unspecified
      Reserved_21_23 : HAL.UInt3 := 16#0#;
      --  Real-Time Clock 1Hz Clock Selection
      RTC1HZ         : Boolean := False;
      --  unspecified
      Reserved_25_31 : HAL.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTT_RTT_MR_Register use record
      RTPRES         at 0 range 0 .. 15;
      ALMIEN         at 0 range 16 .. 16;
      RTTINCIEN      at 0 range 17 .. 17;
      RTTRST         at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      RTTDIS         at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      RTC1HZ         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  Status Register
   type RTT_RTT_SR_Register is record
      --  Read-only. Real-time Alarm Status (cleared on read)
      ALMS          : Boolean;
      --  Read-only. Prescaler Roll-over Status (cleared on read)
      RTTINC        : Boolean;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTT_RTT_SR_Register use record
      ALMS          at 0 range 0 .. 0;
      RTTINC        at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Real-time Timer
   type RTT_Peripheral is record
      --  Mode Register
      RTT_MR : aliased RTT_RTT_MR_Register;
      --  Alarm Register
      RTT_AR : aliased HAL.UInt32;
      --  Value Register
      RTT_VR : aliased HAL.UInt32;
      --  Status Register
      RTT_SR : aliased RTT_RTT_SR_Register;
   end record
     with Volatile;

   for RTT_Peripheral use record
      RTT_MR at 16#0# range 0 .. 31;
      RTT_AR at 16#4# range 0 .. 31;
      RTT_VR at 16#8# range 0 .. 31;
      RTT_SR at 16#C# range 0 .. 31;
   end record;

   --  Real-time Timer
   RTT_Periph : aliased RTT_Peripheral
     with Import, Address => System'To_Address (16#400E1830#);

end SAM_SVD.RTT;
