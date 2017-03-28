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

package NRF51_SVD.POWER is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Enable interrupt on POFWARN event.
   type INTENSET_POFWARN_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_POFWARN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on POFWARN event.
   type INTENSET_POFWARN_Field_1 is
     (
      --  Reset value for the field
      Intenset_Pofwarn_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_POFWARN_Field_1 use
     (Intenset_Pofwarn_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Enable interrupt on POFWARN event.
      POFWARN       : INTENSET_POFWARN_Field_1 :=
                       Intenset_Pofwarn_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      POFWARN       at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Disable interrupt on POFWARN event.
   type INTENCLR_POFWARN_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_POFWARN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on POFWARN event.
   type INTENCLR_POFWARN_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Pofwarn_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_POFWARN_Field_1 use
     (Intenclr_Pofwarn_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Disable interrupt on POFWARN event.
      POFWARN       : INTENCLR_POFWARN_Field_1 :=
                       Intenclr_Pofwarn_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      POFWARN       at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Reset from pin-reset detected.
   type RESETREAS_RESETPIN_Field is
     (
      --  Reset not detected.
      Notdetected,
      --  Reset detected.
      Detected)
     with Size => 1;
   for RESETREAS_RESETPIN_Field use
     (Notdetected => 0,
      Detected => 1);

   --  Reset from watchdog detected.
   type RESETREAS_DOG_Field is
     (
      --  Reset not detected.
      Notdetected,
      --  Reset detected.
      Detected)
     with Size => 1;
   for RESETREAS_DOG_Field use
     (Notdetected => 0,
      Detected => 1);

   --  Reset from AIRCR.SYSRESETREQ detected.
   type RESETREAS_SREQ_Field is
     (
      --  Reset not detected.
      Notdetected,
      --  Reset detected.
      Detected)
     with Size => 1;
   for RESETREAS_SREQ_Field use
     (Notdetected => 0,
      Detected => 1);

   --  Reset from CPU lock-up detected.
   type RESETREAS_LOCKUP_Field is
     (
      --  Reset not detected.
      Notdetected,
      --  Reset detected.
      Detected)
     with Size => 1;
   for RESETREAS_LOCKUP_Field use
     (Notdetected => 0,
      Detected => 1);

   --  Reset from wake-up from OFF mode detected by the use of DETECT signal
   --  from GPIO.
   type RESETREAS_OFF_Field is
     (
      --  Reset not detected.
      Notdetected,
      --  Reset detected.
      Detected)
     with Size => 1;
   for RESETREAS_OFF_Field use
     (Notdetected => 0,
      Detected => 1);

   --  Reset from wake-up from OFF mode detected by the use of ANADETECT signal
   --  from LPCOMP.
   type RESETREAS_LPCOMP_Field is
     (
      --  Reset not detected.
      Notdetected,
      --  Reset detected.
      Detected)
     with Size => 1;
   for RESETREAS_LPCOMP_Field use
     (Notdetected => 0,
      Detected => 1);

   --  Reset from wake-up from OFF mode detected by entering into debug
   --  interface mode.
   type RESETREAS_DIF_Field is
     (
      --  Reset not detected.
      Notdetected,
      --  Reset detected.
      Detected)
     with Size => 1;
   for RESETREAS_DIF_Field use
     (Notdetected => 0,
      Detected => 1);

   --  Reset reason.
   type RESETREAS_Register is record
      --  Reset from pin-reset detected.
      RESETPIN       : RESETREAS_RESETPIN_Field :=
                        NRF51_SVD.POWER.Notdetected;
      --  Reset from watchdog detected.
      DOG            : RESETREAS_DOG_Field := NRF51_SVD.POWER.Notdetected;
      --  Reset from AIRCR.SYSRESETREQ detected.
      SREQ           : RESETREAS_SREQ_Field := NRF51_SVD.POWER.Notdetected;
      --  Reset from CPU lock-up detected.
      LOCKUP         : RESETREAS_LOCKUP_Field := NRF51_SVD.POWER.Notdetected;
      --  unspecified
      Reserved_4_15  : HAL.UInt12 := 16#0#;
      --  Reset from wake-up from OFF mode detected by the use of DETECT signal
      --  from GPIO.
      OFF            : RESETREAS_OFF_Field := NRF51_SVD.POWER.Notdetected;
      --  Reset from wake-up from OFF mode detected by the use of ANADETECT
      --  signal from LPCOMP.
      LPCOMP         : RESETREAS_LPCOMP_Field := NRF51_SVD.POWER.Notdetected;
      --  Reset from wake-up from OFF mode detected by entering into debug
      --  interface mode.
      DIF            : RESETREAS_DIF_Field := NRF51_SVD.POWER.Notdetected;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESETREAS_Register use record
      RESETPIN       at 0 range 0 .. 0;
      DOG            at 0 range 1 .. 1;
      SREQ           at 0 range 2 .. 2;
      LOCKUP         at 0 range 3 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      OFF            at 0 range 16 .. 16;
      LPCOMP         at 0 range 17 .. 17;
      DIF            at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  RAM block 0 status.
   type RAMSTATUS_RAMBLOCK0_Field is
     (
      --  RAM block 0 is off or powering up.
      Off,
      --  RAM block 0 is on.
      On)
     with Size => 1;
   for RAMSTATUS_RAMBLOCK0_Field use
     (Off => 0,
      On => 1);

   --  RAM block 1 status.
   type RAMSTATUS_RAMBLOCK1_Field is
     (
      --  RAM block 1 is off or powering up.
      Off,
      --  RAM block 1 is on.
      On)
     with Size => 1;
   for RAMSTATUS_RAMBLOCK1_Field use
     (Off => 0,
      On => 1);

   --  RAM block 2 status.
   type RAMSTATUS_RAMBLOCK2_Field is
     (
      --  RAM block 2 is off or powering up.
      Off,
      --  RAM block 2 is on.
      On)
     with Size => 1;
   for RAMSTATUS_RAMBLOCK2_Field use
     (Off => 0,
      On => 1);

   --  RAM block 3 status.
   type RAMSTATUS_RAMBLOCK3_Field is
     (
      --  RAM block 3 is off or powering up.
      Off,
      --  RAM block 3 is on.
      On)
     with Size => 1;
   for RAMSTATUS_RAMBLOCK3_Field use
     (Off => 0,
      On => 1);

   --  Ram status register.
   type RAMSTATUS_Register is record
      --  Read-only. RAM block 0 status.
      RAMBLOCK0     : RAMSTATUS_RAMBLOCK0_Field;
      --  Read-only. RAM block 1 status.
      RAMBLOCK1     : RAMSTATUS_RAMBLOCK1_Field;
      --  Read-only. RAM block 2 status.
      RAMBLOCK2     : RAMSTATUS_RAMBLOCK2_Field;
      --  Read-only. RAM block 3 status.
      RAMBLOCK3     : RAMSTATUS_RAMBLOCK3_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAMSTATUS_Register use record
      RAMBLOCK0     at 0 range 0 .. 0;
      RAMBLOCK1     at 0 range 1 .. 1;
      RAMBLOCK2     at 0 range 2 .. 2;
      RAMBLOCK3     at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Enter system off mode.
   type SYSTEMOFF_SYSTEMOFF_Field is
     (
      --  Reset value for the field
      Systemoff_Systemoff_Field_Reset,
      --  Enter system off mode.
      Enter)
     with Size => 1;
   for SYSTEMOFF_SYSTEMOFF_Field use
     (Systemoff_Systemoff_Field_Reset => 0,
      Enter => 1);

   --  System off register.
   type SYSTEMOFF_Register is record
      --  Write-only. Enter system off mode.
      SYSTEMOFF     : SYSTEMOFF_SYSTEMOFF_Field :=
                       Systemoff_Systemoff_Field_Reset;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SYSTEMOFF_Register use record
      SYSTEMOFF     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Power failure comparator enable.
   type POFCON_POF_Field is
     (
      --  Disabled.
      Disabled,
      --  Enabled.
      Enabled)
     with Size => 1;
   for POFCON_POF_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Set threshold level.
   type POFCON_THRESHOLD_Field is
     (
      --  Set threshold to 2.1Volts.
      V21,
      --  Set threshold to 2.3Volts.
      V23,
      --  Set threshold to 2.5Volts.
      V25,
      --  Set threshold to 2.7Volts.
      V27)
     with Size => 2;
   for POFCON_THRESHOLD_Field use
     (V21 => 0,
      V23 => 1,
      V25 => 2,
      V27 => 3);

   --  Power failure configuration.
   type POFCON_Register is record
      --  Power failure comparator enable.
      POF           : POFCON_POF_Field := NRF51_SVD.POWER.Disabled;
      --  Set threshold level.
      THRESHOLD     : POFCON_THRESHOLD_Field := NRF51_SVD.POWER.V21;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for POFCON_Register use record
      POF           at 0 range 0 .. 0;
      THRESHOLD     at 0 range 1 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype GPREGRET_GPREGRET_Field is HAL.UInt8;

   --  General purpose retention register. This register is a retained
   --  register.
   type GPREGRET_Register is record
      --  General purpose retention register.
      GPREGRET      : GPREGRET_GPREGRET_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for GPREGRET_Register use record
      GPREGRET      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  RAM block 0 behaviour in ON mode.
   type RAMON_ONRAM0_Field is
     (
      --  RAM block 0 OFF in ON mode.
      Ram0Off,
      --  RAM block 0 ON in ON mode.
      Ram0On)
     with Size => 1;
   for RAMON_ONRAM0_Field use
     (Ram0Off => 0,
      Ram0On => 1);

   --  RAM block 1 behaviour in ON mode.
   type RAMON_ONRAM1_Field is
     (
      --  RAM block 1 OFF in ON mode.
      Ram1Off,
      --  RAM block 1 ON in ON mode.
      Ram1On)
     with Size => 1;
   for RAMON_ONRAM1_Field use
     (Ram1Off => 0,
      Ram1On => 1);

   --  RAM block 0 behaviour in OFF mode.
   type RAMON_OFFRAM0_Field is
     (
      --  RAM block 0 OFF in OFF mode.
      Ram0Off,
      --  RAM block 0 ON in OFF mode.
      Ram0On)
     with Size => 1;
   for RAMON_OFFRAM0_Field use
     (Ram0Off => 0,
      Ram0On => 1);

   --  RAM block 1 behaviour in OFF mode.
   type RAMON_OFFRAM1_Field is
     (
      --  RAM block 1 OFF in OFF mode.
      Ram1Off,
      --  RAM block 1 ON in OFF mode.
      Ram1On)
     with Size => 1;
   for RAMON_OFFRAM1_Field use
     (Ram1Off => 0,
      Ram1On => 1);

   --  Ram on/off.
   type RAMON_Register is record
      --  RAM block 0 behaviour in ON mode.
      ONRAM0         : RAMON_ONRAM0_Field := NRF51_SVD.POWER.Ram0On;
      --  RAM block 1 behaviour in ON mode.
      ONRAM1         : RAMON_ONRAM1_Field := NRF51_SVD.POWER.Ram1On;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  RAM block 0 behaviour in OFF mode.
      OFFRAM0        : RAMON_OFFRAM0_Field := NRF51_SVD.POWER.Ram0Off;
      --  RAM block 1 behaviour in OFF mode.
      OFFRAM1        : RAMON_OFFRAM1_Field := NRF51_SVD.POWER.Ram1Off;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAMON_Register use record
      ONRAM0         at 0 range 0 .. 0;
      ONRAM1         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      OFFRAM0        at 0 range 16 .. 16;
      OFFRAM1        at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Enable or disable pin reset in debug interface mode.
   type RESET_RESET_Field is
     (
      --  Pin reset in debug interface mode disabled.
      Disabled,
      --  Pin reset in debug interface mode enabled.
      Enabled)
     with Size => 1;
   for RESET_RESET_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Pin reset functionality configuration register. This register is a
   --  retained register.
   type RESET_Register is record
      --  Enable or disable pin reset in debug interface mode.
      RESET         : RESET_RESET_Field := NRF51_SVD.POWER.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RESET_Register use record
      RESET         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  RAM block 2 behaviour in ON mode.
   type RAMONB_ONRAM2_Field is
     (
      --  RAM block 2 OFF in ON mode.
      Ram2Off,
      --  RAM block 2 ON in ON mode.
      Ram2On)
     with Size => 1;
   for RAMONB_ONRAM2_Field use
     (Ram2Off => 0,
      Ram2On => 1);

   --  RAM block 3 behaviour in ON mode.
   type RAMONB_ONRAM3_Field is
     (
      --  RAM block 33 OFF in ON mode.
      Ram3Off,
      --  RAM block 3 ON in ON mode.
      Ram3On)
     with Size => 1;
   for RAMONB_ONRAM3_Field use
     (Ram3Off => 0,
      Ram3On => 1);

   --  RAM block 2 behaviour in OFF mode.
   type RAMONB_OFFRAM2_Field is
     (
      --  RAM block 2 OFF in OFF mode.
      Ram2Off,
      --  RAM block 2 ON in OFF mode.
      Ram2On)
     with Size => 1;
   for RAMONB_OFFRAM2_Field use
     (Ram2Off => 0,
      Ram2On => 1);

   --  RAM block 3 behaviour in OFF mode.
   type RAMONB_OFFRAM3_Field is
     (
      --  RAM block 3 OFF in OFF mode.
      Ram3Off,
      --  RAM block 3 ON in OFF mode.
      Ram3On)
     with Size => 1;
   for RAMONB_OFFRAM3_Field use
     (Ram3Off => 0,
      Ram3On => 1);

   --  Ram on/off.
   type RAMONB_Register is record
      --  RAM block 2 behaviour in ON mode.
      ONRAM2         : RAMONB_ONRAM2_Field := NRF51_SVD.POWER.Ram2On;
      --  RAM block 3 behaviour in ON mode.
      ONRAM3         : RAMONB_ONRAM3_Field := NRF51_SVD.POWER.Ram3On;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      --  RAM block 2 behaviour in OFF mode.
      OFFRAM2        : RAMONB_OFFRAM2_Field := NRF51_SVD.POWER.Ram2Off;
      --  RAM block 3 behaviour in OFF mode.
      OFFRAM3        : RAMONB_OFFRAM3_Field := NRF51_SVD.POWER.Ram3Off;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RAMONB_Register use record
      ONRAM2         at 0 range 0 .. 0;
      ONRAM3         at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      OFFRAM2        at 0 range 16 .. 16;
      OFFRAM3        at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Enable DCDC converter.
   type DCDCEN_DCDCEN_Field is
     (
      --  DCDC converter disabled.
      Disabled,
      --  DCDC converter enabled.
      Enabled)
     with Size => 1;
   for DCDCEN_DCDCEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  DCDC converter enable configuration register.
   type DCDCEN_Register is record
      --  Enable DCDC converter.
      DCDCEN        : DCDCEN_DCDCEN_Field := NRF51_SVD.POWER.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCDCEN_Register use record
      DCDCEN        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  DCDC power-up force off.
   type DCDCFORCE_FORCEOFF_Field is
     (
      --  No force.
      Noforce,
      --  Force.
      Force)
     with Size => 1;
   for DCDCFORCE_FORCEOFF_Field use
     (Noforce => 0,
      Force => 1);

   --  DCDC power-up force on.
   type DCDCFORCE_FORCEON_Field is
     (
      --  No force.
      Noforce,
      --  Force.
      Force)
     with Size => 1;
   for DCDCFORCE_FORCEON_Field use
     (Noforce => 0,
      Force => 1);

   --  DCDC power-up force register.
   type DCDCFORCE_Register is record
      --  DCDC power-up force off.
      FORCEOFF      : DCDCFORCE_FORCEOFF_Field := NRF51_SVD.POWER.Noforce;
      --  DCDC power-up force on.
      FORCEON       : DCDCFORCE_FORCEON_Field := NRF51_SVD.POWER.Noforce;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCDCFORCE_Register use record
      FORCEOFF      at 0 range 0 .. 0;
      FORCEON       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Power Control.
   type POWER_Peripheral is record
      --  Enable constant latency mode.
      TASKS_CONSTLAT : aliased HAL.UInt32;
      --  Enable low power mode (variable latency).
      TASKS_LOWPWR   : aliased HAL.UInt32;
      --  Power failure warning.
      EVENTS_POFWARN : aliased HAL.UInt32;
      --  Interrupt enable set register.
      INTENSET       : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR       : aliased INTENCLR_Register;
      --  Reset reason.
      RESETREAS      : aliased RESETREAS_Register;
      --  Ram status register.
      RAMSTATUS      : aliased RAMSTATUS_Register;
      --  System off register.
      SYSTEMOFF      : aliased SYSTEMOFF_Register;
      --  Power failure configuration.
      POFCON         : aliased POFCON_Register;
      --  General purpose retention register. This register is a retained
      --  register.
      GPREGRET       : aliased GPREGRET_Register;
      --  Ram on/off.
      RAMON          : aliased RAMON_Register;
      --  Pin reset functionality configuration register. This register is a
      --  retained register.
      RESET          : aliased RESET_Register;
      --  Ram on/off.
      RAMONB         : aliased RAMONB_Register;
      --  DCDC converter enable configuration register.
      DCDCEN         : aliased DCDCEN_Register;
      --  DCDC power-up force register.
      DCDCFORCE      : aliased DCDCFORCE_Register;
   end record
     with Volatile;

   for POWER_Peripheral use record
      TASKS_CONSTLAT at 16#78# range 0 .. 31;
      TASKS_LOWPWR   at 16#7C# range 0 .. 31;
      EVENTS_POFWARN at 16#108# range 0 .. 31;
      INTENSET       at 16#304# range 0 .. 31;
      INTENCLR       at 16#308# range 0 .. 31;
      RESETREAS      at 16#400# range 0 .. 31;
      RAMSTATUS      at 16#428# range 0 .. 31;
      SYSTEMOFF      at 16#500# range 0 .. 31;
      POFCON         at 16#510# range 0 .. 31;
      GPREGRET       at 16#51C# range 0 .. 31;
      RAMON          at 16#524# range 0 .. 31;
      RESET          at 16#544# range 0 .. 31;
      RAMONB         at 16#554# range 0 .. 31;
      DCDCEN         at 16#578# range 0 .. 31;
      DCDCFORCE      at 16#A08# range 0 .. 31;
   end record;

   --  Power Control.
   POWER_Periph : aliased POWER_Peripheral
     with Import, Address => System'To_Address (16#40000000#);

end NRF51_SVD.POWER;
