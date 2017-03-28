--  Copyright (c) 2013, Nordic Semiconductor ASA
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright notice,
--  this list of conditions and the following disclaimer in the documentation
--  and/or other materials provided with the distribution.
--
--  * Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
--  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
--  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf51.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF51_SVD.WDT is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Enable interrupt on TIMEOUT event.
   type INTENSET_TIMEOUT_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_TIMEOUT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on TIMEOUT event.
   type INTENSET_TIMEOUT_Field_1 is
     (
      --  Reset value for the field
      Intenset_Timeout_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_TIMEOUT_Field_1 use
     (Intenset_Timeout_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on TIMEOUT event.
      TIMEOUT       : INTENSET_TIMEOUT_Field_1 :=
                       Intenset_Timeout_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      TIMEOUT       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Disable interrupt on TIMEOUT event.
   type INTENCLR_TIMEOUT_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_TIMEOUT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on TIMEOUT event.
   type INTENCLR_TIMEOUT_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Timeout_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_TIMEOUT_Field_1 use
     (Intenclr_Timeout_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on TIMEOUT event.
      TIMEOUT       : INTENCLR_TIMEOUT_Field_1 :=
                       Intenclr_Timeout_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      TIMEOUT       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Watchdog running status.
   type RUNSTATUS_RUNSTATUS_Field is
     (
      --  Watchdog timer is not running.
      Notrunning,
      --  Watchdog timer is running.
      Running)
     with Size => 1;
   for RUNSTATUS_RUNSTATUS_Field use
     (Notrunning => 0,
      Running => 1);

   --  Watchdog running status.
   type RUNSTATUS_Register is record
      --  Read-only. Watchdog running status.
      RUNSTATUS     : RUNSTATUS_RUNSTATUS_Field;
      --  unspecified
      Reserved_1_31 : HAL.UInt31;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RUNSTATUS_Register use record
      RUNSTATUS     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Request status for RR[0].
   type REQSTATUS_RR0_Field is
     (
      --  RR[0] register is not enabled or has already requested reload.
      Disabledorrequested,
      --  RR[0] register is enabled and has not jet requested.
      Enabledandunrequested)
     with Size => 1;
   for REQSTATUS_RR0_Field use
     (Disabledorrequested => 0,
      Enabledandunrequested => 1);

   --  Request status for RR[1].
   type REQSTATUS_RR1_Field is
     (
      --  RR[1] register is not enabled or has already requested reload.
      Disabledorrequested,
      --  RR[1] register is enabled and has not jet requested.
      Enabledandunrequested)
     with Size => 1;
   for REQSTATUS_RR1_Field use
     (Disabledorrequested => 0,
      Enabledandunrequested => 1);

   --  Request status for RR[2].
   type REQSTATUS_RR2_Field is
     (
      --  RR[2] register is not enabled or has already requested reload.
      Disabledorrequested,
      --  RR[2] register is enabled and has not jet requested.
      Enabledandunrequested)
     with Size => 1;
   for REQSTATUS_RR2_Field use
     (Disabledorrequested => 0,
      Enabledandunrequested => 1);

   --  Request status for RR[3].
   type REQSTATUS_RR3_Field is
     (
      --  RR[3] register is not enabled or has already requested reload.
      Disabledorrequested,
      --  RR[3] register is enabled and has not jet requested.
      Enabledandunrequested)
     with Size => 1;
   for REQSTATUS_RR3_Field use
     (Disabledorrequested => 0,
      Enabledandunrequested => 1);

   --  Request status for RR[4].
   type REQSTATUS_RR4_Field is
     (
      --  RR[4] register is not enabled or has already requested reload.
      Disabledorrequested,
      --  RR[4] register is enabled and has not jet requested.
      Enabledandunrequested)
     with Size => 1;
   for REQSTATUS_RR4_Field use
     (Disabledorrequested => 0,
      Enabledandunrequested => 1);

   --  Request status for RR[5].
   type REQSTATUS_RR5_Field is
     (
      --  RR[5] register is not enabled or has already requested reload.
      Disabledorrequested,
      --  RR[5] register is enabled and has not jet requested.
      Enabledandunrequested)
     with Size => 1;
   for REQSTATUS_RR5_Field use
     (Disabledorrequested => 0,
      Enabledandunrequested => 1);

   --  Request status for RR[6].
   type REQSTATUS_RR6_Field is
     (
      --  RR[6] register is not enabled or has already requested reload.
      Disabledorrequested,
      --  RR[6] register is enabled and has not jet requested.
      Enabledandunrequested)
     with Size => 1;
   for REQSTATUS_RR6_Field use
     (Disabledorrequested => 0,
      Enabledandunrequested => 1);

   --  Request status for RR[7].
   type REQSTATUS_RR7_Field is
     (
      --  RR[7] register is not enabled or has already requested reload.
      Disabledorrequested,
      --  RR[7] register is enabled and has not jet requested.
      Enabledandunrequested)
     with Size => 1;
   for REQSTATUS_RR7_Field use
     (Disabledorrequested => 0,
      Enabledandunrequested => 1);

   --  Request status.
   type REQSTATUS_Register is record
      --  Read-only. Request status for RR[0].
      RR0           : REQSTATUS_RR0_Field;
      --  Read-only. Request status for RR[1].
      RR1           : REQSTATUS_RR1_Field;
      --  Read-only. Request status for RR[2].
      RR2           : REQSTATUS_RR2_Field;
      --  Read-only. Request status for RR[3].
      RR3           : REQSTATUS_RR3_Field;
      --  Read-only. Request status for RR[4].
      RR4           : REQSTATUS_RR4_Field;
      --  Read-only. Request status for RR[5].
      RR5           : REQSTATUS_RR5_Field;
      --  Read-only. Request status for RR[6].
      RR6           : REQSTATUS_RR6_Field;
      --  Read-only. Request status for RR[7].
      RR7           : REQSTATUS_RR7_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for REQSTATUS_Register use record
      RR0           at 0 range 0 .. 0;
      RR1           at 0 range 1 .. 1;
      RR2           at 0 range 2 .. 2;
      RR3           at 0 range 3 .. 3;
      RR4           at 0 range 4 .. 4;
      RR5           at 0 range 5 .. 5;
      RR6           at 0 range 6 .. 6;
      RR7           at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Enable or disable RR[0] register.
   type RREN_RR0_Field is
     (
      --  RR[0] register is disabled.
      Disabled,
      --  RR[0] register is enabled.
      Enabled)
     with Size => 1;
   for RREN_RR0_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable RR[1] register.
   type RREN_RR1_Field is
     (
      --  RR[1] register is disabled.
      Disabled,
      --  RR[1] register is enabled.
      Enabled)
     with Size => 1;
   for RREN_RR1_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable RR[2] register.
   type RREN_RR2_Field is
     (
      --  RR[2] register is disabled.
      Disabled,
      --  RR[2] register is enabled.
      Enabled)
     with Size => 1;
   for RREN_RR2_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable RR[3] register.
   type RREN_RR3_Field is
     (
      --  RR[3] register is disabled.
      Disabled,
      --  RR[3] register is enabled.
      Enabled)
     with Size => 1;
   for RREN_RR3_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable RR[4] register.
   type RREN_RR4_Field is
     (
      --  RR[4] register is disabled.
      Disabled,
      --  RR[4] register is enabled.
      Enabled)
     with Size => 1;
   for RREN_RR4_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable RR[5] register.
   type RREN_RR5_Field is
     (
      --  RR[5] register is disabled.
      Disabled,
      --  RR[5] register is enabled.
      Enabled)
     with Size => 1;
   for RREN_RR5_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable RR[6] register.
   type RREN_RR6_Field is
     (
      --  RR[6] register is disabled.
      Disabled,
      --  RR[6] register is enabled.
      Enabled)
     with Size => 1;
   for RREN_RR6_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable or disable RR[7] register.
   type RREN_RR7_Field is
     (
      --  RR[7] register is disabled.
      Disabled,
      --  RR[7] register is enabled.
      Enabled)
     with Size => 1;
   for RREN_RR7_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Reload request enable.
   type RREN_Register is record
      --  Enable or disable RR[0] register.
      RR0           : RREN_RR0_Field := NRF51_SVD.WDT.Enabled;
      --  Enable or disable RR[1] register.
      RR1           : RREN_RR1_Field := NRF51_SVD.WDT.Disabled;
      --  Enable or disable RR[2] register.
      RR2           : RREN_RR2_Field := NRF51_SVD.WDT.Disabled;
      --  Enable or disable RR[3] register.
      RR3           : RREN_RR3_Field := NRF51_SVD.WDT.Disabled;
      --  Enable or disable RR[4] register.
      RR4           : RREN_RR4_Field := NRF51_SVD.WDT.Disabled;
      --  Enable or disable RR[5] register.
      RR5           : RREN_RR5_Field := NRF51_SVD.WDT.Disabled;
      --  Enable or disable RR[6] register.
      RR6           : RREN_RR6_Field := NRF51_SVD.WDT.Disabled;
      --  Enable or disable RR[7] register.
      RR7           : RREN_RR7_Field := NRF51_SVD.WDT.Disabled;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RREN_Register use record
      RR0           at 0 range 0 .. 0;
      RR1           at 0 range 1 .. 1;
      RR2           at 0 range 2 .. 2;
      RR3           at 0 range 3 .. 3;
      RR4           at 0 range 4 .. 4;
      RR5           at 0 range 5 .. 5;
      RR6           at 0 range 6 .. 6;
      RR7           at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Configure the watchdog to pause or not while the CPU is sleeping.
   type CONFIG_SLEEP_Field is
     (
      --  Pause watchdog while the CPU is asleep.
      Pause,
      --  Do not pause watchdog while the CPU is asleep.
      Run)
     with Size => 1;
   for CONFIG_SLEEP_Field use
     (Pause => 0,
      Run => 1);

   --  Configure the watchdog to pause or not while the CPU is halted by the
   --  debugger.
   type CONFIG_HALT_Field is
     (
      --  Pause watchdog while the CPU is halted by the debugger.
      Pause,
      --  Do not pause watchdog while the CPU is halted by the debugger.
      Run)
     with Size => 1;
   for CONFIG_HALT_Field use
     (Pause => 0,
      Run => 1);

   --  Configuration register.
   type CONFIG_Register is record
      --  Configure the watchdog to pause or not while the CPU is sleeping.
      SLEEP         : CONFIG_SLEEP_Field := NRF51_SVD.WDT.Run;
      --  unspecified
      Reserved_1_2  : HAL.UInt2 := 16#0#;
      --  Configure the watchdog to pause or not while the CPU is halted by the
      --  debugger.
      HALT          : CONFIG_HALT_Field := NRF51_SVD.WDT.Pause;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      SLEEP         at 0 range 0 .. 0;
      Reserved_1_2  at 0 range 1 .. 2;
      HALT          at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Reload requests registers.

   --  Reload requests registers.
   type RR_Registers is array (0 .. 7) of HAL.UInt32
     with Volatile;

   --  Peripheral power control.
   type POWER_POWER_Field is
     (
      --  Module power disabled.
      Disabled,
      --  Module power enabled.
      Enabled)
     with Size => 1;
   for POWER_POWER_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Peripheral power control.
   type POWER_Register is record
      --  Peripheral power control.
      POWER         : POWER_POWER_Field := NRF51_SVD.WDT.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for POWER_Register use record
      POWER         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Watchdog Timer.
   type WDT_Peripheral is record
      --  Start the watchdog.
      TASKS_START    : aliased HAL.UInt32;
      --  Watchdog timeout.
      EVENTS_TIMEOUT : aliased HAL.UInt32;
      --  Interrupt enable set register.
      INTENSET       : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR       : aliased INTENCLR_Register;
      --  Watchdog running status.
      RUNSTATUS      : aliased RUNSTATUS_Register;
      --  Request status.
      REQSTATUS      : aliased REQSTATUS_Register;
      --  Counter reload value in number of 32kiHz clock cycles.
      CRV            : aliased HAL.UInt32;
      --  Reload request enable.
      RREN           : aliased RREN_Register;
      --  Configuration register.
      CONFIG         : aliased CONFIG_Register;
      --  Reload requests registers.
      RR             : aliased RR_Registers;
      --  Peripheral power control.
      POWER          : aliased POWER_Register;
   end record
     with Volatile;

   for WDT_Peripheral use record
      TASKS_START    at 16#0# range 0 .. 31;
      EVENTS_TIMEOUT at 16#100# range 0 .. 31;
      INTENSET       at 16#304# range 0 .. 31;
      INTENCLR       at 16#308# range 0 .. 31;
      RUNSTATUS      at 16#400# range 0 .. 31;
      REQSTATUS      at 16#404# range 0 .. 31;
      CRV            at 16#504# range 0 .. 31;
      RREN           at 16#508# range 0 .. 31;
      CONFIG         at 16#50C# range 0 .. 31;
      RR             at 16#600# range 0 .. 255;
      POWER          at 16#FFC# range 0 .. 31;
   end record;

   --  Watchdog Timer.
   WDT_Periph : aliased WDT_Peripheral
     with Import, Address => System'To_Address (16#40010000#);

end NRF51_SVD.WDT;
