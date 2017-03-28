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

package NRF51_SVD.QDEC is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between REPORTRDY event and READCLRACC task.
   type SHORTS_REPORTRDY_READCLRACC_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_REPORTRDY_READCLRACC_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between SAMPLERDY event and STOP task.
   type SHORTS_SAMPLERDY_STOP_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_SAMPLERDY_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcuts for the QDEC.
   type SHORTS_Register is record
      --  Shortcut between REPORTRDY event and READCLRACC task.
      REPORTRDY_READCLRACC : SHORTS_REPORTRDY_READCLRACC_Field :=
                              NRF51_SVD.QDEC.Disabled;
      --  Shortcut between SAMPLERDY event and STOP task.
      SAMPLERDY_STOP       : SHORTS_SAMPLERDY_STOP_Field :=
                              NRF51_SVD.QDEC.Disabled;
      --  unspecified
      Reserved_2_31        : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      REPORTRDY_READCLRACC at 0 range 0 .. 0;
      SAMPLERDY_STOP       at 0 range 1 .. 1;
      Reserved_2_31        at 0 range 2 .. 31;
   end record;

   --  Enable interrupt on SAMPLERDY event.
   type INTENSET_SAMPLERDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_SAMPLERDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on SAMPLERDY event.
   type INTENSET_SAMPLERDY_Field_1 is
     (
      --  Reset value for the field
      Intenset_Samplerdy_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_SAMPLERDY_Field_1 use
     (Intenset_Samplerdy_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on REPORTRDY event.
   type INTENSET_REPORTRDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_REPORTRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on REPORTRDY event.
   type INTENSET_REPORTRDY_Field_1 is
     (
      --  Reset value for the field
      Intenset_Reportrdy_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_REPORTRDY_Field_1 use
     (Intenset_Reportrdy_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on ACCOF event.
   type INTENSET_ACCOF_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_ACCOF_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on ACCOF event.
   type INTENSET_ACCOF_Field_1 is
     (
      --  Reset value for the field
      Intenset_Accof_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_ACCOF_Field_1 use
     (Intenset_Accof_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on SAMPLERDY event.
      SAMPLERDY     : INTENSET_SAMPLERDY_Field_1 :=
                       Intenset_Samplerdy_Field_Reset;
      --  Enable interrupt on REPORTRDY event.
      REPORTRDY     : INTENSET_REPORTRDY_Field_1 :=
                       Intenset_Reportrdy_Field_Reset;
      --  Enable interrupt on ACCOF event.
      ACCOF         : INTENSET_ACCOF_Field_1 := Intenset_Accof_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      SAMPLERDY     at 0 range 0 .. 0;
      REPORTRDY     at 0 range 1 .. 1;
      ACCOF         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Disable interrupt on SAMPLERDY event.
   type INTENCLR_SAMPLERDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_SAMPLERDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on SAMPLERDY event.
   type INTENCLR_SAMPLERDY_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Samplerdy_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_SAMPLERDY_Field_1 use
     (Intenclr_Samplerdy_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on REPORTRDY event.
   type INTENCLR_REPORTRDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_REPORTRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on REPORTRDY event.
   type INTENCLR_REPORTRDY_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Reportrdy_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_REPORTRDY_Field_1 use
     (Intenclr_Reportrdy_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on ACCOF event.
   type INTENCLR_ACCOF_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_ACCOF_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on ACCOF event.
   type INTENCLR_ACCOF_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Accof_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_ACCOF_Field_1 use
     (Intenclr_Accof_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on SAMPLERDY event.
      SAMPLERDY     : INTENCLR_SAMPLERDY_Field_1 :=
                       Intenclr_Samplerdy_Field_Reset;
      --  Disable interrupt on REPORTRDY event.
      REPORTRDY     : INTENCLR_REPORTRDY_Field_1 :=
                       Intenclr_Reportrdy_Field_Reset;
      --  Disable interrupt on ACCOF event.
      ACCOF         : INTENCLR_ACCOF_Field_1 := Intenclr_Accof_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      SAMPLERDY     at 0 range 0 .. 0;
      REPORTRDY     at 0 range 1 .. 1;
      ACCOF         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Enable or disable QDEC.
   type ENABLE_ENABLE_Field is
     (
      --  Disabled QDEC.
      Disabled,
      --  Enable QDEC.
      Enabled)
     with Size => 1;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable the QDEC.
   type ENABLE_Register is record
      --  Enable or disable QDEC.
      ENABLE        : ENABLE_ENABLE_Field := NRF51_SVD.QDEC.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  LED output pin polarity.
   type LEDPOL_LEDPOL_Field is
     (
      --  LED output is active low.
      Activelow,
      --  LED output is active high.
      Activehigh)
     with Size => 1;
   for LEDPOL_LEDPOL_Field use
     (Activelow => 0,
      Activehigh => 1);

   --  LED output pin polarity.
   type LEDPOL_Register is record
      --  LED output pin polarity.
      LEDPOL        : LEDPOL_LEDPOL_Field := NRF51_SVD.QDEC.Activelow;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LEDPOL_Register use record
      LEDPOL        at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  Sample period.
   type SAMPLEPER_SAMPLEPER_Field is
     (
      --  128us sample period.
      SAMPLEPER_SAMPLEPER_Field_128US,
      --  256us sample period.
      SAMPLEPER_SAMPLEPER_Field_256US,
      --  512us sample period.
      SAMPLEPER_SAMPLEPER_Field_512US,
      --  1024us sample period.
      SAMPLEPER_SAMPLEPER_Field_1024US,
      --  2048us sample period.
      SAMPLEPER_SAMPLEPER_Field_2048US,
      --  4096us sample period.
      SAMPLEPER_SAMPLEPER_Field_4096US,
      --  8192us sample period.
      SAMPLEPER_SAMPLEPER_Field_8192US,
      --  16384us sample period.
      SAMPLEPER_SAMPLEPER_Field_16384US)
     with Size => 3;
   for SAMPLEPER_SAMPLEPER_Field use
     (SAMPLEPER_SAMPLEPER_Field_128US => 0,
      SAMPLEPER_SAMPLEPER_Field_256US => 1,
      SAMPLEPER_SAMPLEPER_Field_512US => 2,
      SAMPLEPER_SAMPLEPER_Field_1024US => 3,
      SAMPLEPER_SAMPLEPER_Field_2048US => 4,
      SAMPLEPER_SAMPLEPER_Field_4096US => 5,
      SAMPLEPER_SAMPLEPER_Field_8192US => 6,
      SAMPLEPER_SAMPLEPER_Field_16384US => 7);

   --  Sample period.
   type SAMPLEPER_Register is record
      --  Sample period.
      SAMPLEPER     : SAMPLEPER_SAMPLEPER_Field :=
                       NRF51_SVD.QDEC.SAMPLEPER_SAMPLEPER_Field_128US;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SAMPLEPER_Register use record
      SAMPLEPER     at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Number of samples to generate an EVENT_REPORTRDY.
   type REPORTPER_REPORTPER_Field is
     (
      --  10 samples per report.
      REPORTPER_REPORTPER_Field_10Smpl,
      --  40 samples per report.
      REPORTPER_REPORTPER_Field_40Smpl,
      --  80 samples per report.
      REPORTPER_REPORTPER_Field_80Smpl,
      --  120 samples per report.
      REPORTPER_REPORTPER_Field_120Smpl,
      --  160 samples per report.
      REPORTPER_REPORTPER_Field_160Smpl,
      --  200 samples per report.
      REPORTPER_REPORTPER_Field_200Smpl,
      --  240 samples per report.
      REPORTPER_REPORTPER_Field_240Smpl,
      --  280 samples per report.
      REPORTPER_REPORTPER_Field_280Smpl)
     with Size => 3;
   for REPORTPER_REPORTPER_Field use
     (REPORTPER_REPORTPER_Field_10Smpl => 0,
      REPORTPER_REPORTPER_Field_40Smpl => 1,
      REPORTPER_REPORTPER_Field_80Smpl => 2,
      REPORTPER_REPORTPER_Field_120Smpl => 3,
      REPORTPER_REPORTPER_Field_160Smpl => 4,
      REPORTPER_REPORTPER_Field_200Smpl => 5,
      REPORTPER_REPORTPER_Field_240Smpl => 6,
      REPORTPER_REPORTPER_Field_280Smpl => 7);

   --  Number of samples to generate an EVENT_REPORTRDY.
   type REPORTPER_Register is record
      --  Number of samples to generate an EVENT_REPORTRDY.
      REPORTPER     : REPORTPER_REPORTPER_Field :=
                       NRF51_SVD.QDEC.REPORTPER_REPORTPER_Field_10Smpl;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for REPORTPER_Register use record
      REPORTPER     at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Enable debounce input filters.
   type DBFEN_DBFEN_Field is
     (
      --  Debounce input filters disabled.
      Disabled,
      --  Debounce input filters enabled.
      Enabled)
     with Size => 1;
   for DBFEN_DBFEN_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable debouncer input filters.
   type DBFEN_Register is record
      --  Enable debounce input filters.
      DBFEN         : DBFEN_DBFEN_Field := NRF51_SVD.QDEC.Disabled;
      --  unspecified
      Reserved_1_31 : HAL.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DBFEN_Register use record
      DBFEN         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype LEDPRE_LEDPRE_Field is HAL.UInt9;

   --  Time LED is switched ON before the sample.
   type LEDPRE_Register is record
      --  Period in us the LED in switched on prior to sampling.
      LEDPRE        : LEDPRE_LEDPRE_Field := 16#10#;
      --  unspecified
      Reserved_9_31 : HAL.UInt23 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for LEDPRE_Register use record
      LEDPRE        at 0 range 0 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype ACCDBL_ACCDBL_Field is HAL.UInt4;

   --  Accumulated double (error) transitions register.
   type ACCDBL_Register is record
      --  Read-only. Accumulated double (error) transitions.
      ACCDBL        : ACCDBL_ACCDBL_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACCDBL_Register use record
      ACCDBL        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype ACCDBLREAD_ACCDBLREAD_Field is HAL.UInt4;

   --  Snapshot of ACCDBL register. Value generated by the TASKS_READCLEACC
   --  task.
   type ACCDBLREAD_Register is record
      --  Read-only. Snapshot of accumulated double (error) transitions.
      ACCDBLREAD    : ACCDBLREAD_ACCDBLREAD_Field;
      --  unspecified
      Reserved_4_31 : HAL.UInt28;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACCDBLREAD_Register use record
      ACCDBLREAD    at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

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
      POWER         : POWER_POWER_Field := NRF51_SVD.QDEC.Disabled;
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

   --  Rotary decoder.
   type QDEC_Peripheral is record
      --  Start the quadrature decoder.
      TASKS_START      : aliased HAL.UInt32;
      --  Stop the quadrature decoder.
      TASKS_STOP       : aliased HAL.UInt32;
      --  Transfers the content from ACC registers to ACCREAD registers, and
      --  clears the ACC registers.
      TASKS_READCLRACC : aliased HAL.UInt32;
      --  A new sample is written to the sample register.
      EVENTS_SAMPLERDY : aliased HAL.UInt32;
      --  REPORTPER number of samples accumulated in ACC register, and ACC
      --  register different than zero.
      EVENTS_REPORTRDY : aliased HAL.UInt32;
      --  ACC or ACCDBL register overflow.
      EVENTS_ACCOF     : aliased HAL.UInt32;
      --  Shortcuts for the QDEC.
      SHORTS           : aliased SHORTS_Register;
      --  Interrupt enable set register.
      INTENSET         : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR         : aliased INTENCLR_Register;
      --  Enable the QDEC.
      ENABLE           : aliased ENABLE_Register;
      --  LED output pin polarity.
      LEDPOL           : aliased LEDPOL_Register;
      --  Sample period.
      SAMPLEPER        : aliased SAMPLEPER_Register;
      --  Motion sample value.
      SAMPLE           : aliased HAL.UInt32;
      --  Number of samples to generate an EVENT_REPORTRDY.
      REPORTPER        : aliased REPORTPER_Register;
      --  Accumulated valid transitions register.
      ACC              : aliased HAL.UInt32;
      --  Snapshot of ACC register. Value generated by the TASKS_READCLEACC
      --  task.
      ACCREAD          : aliased HAL.UInt32;
      --  Pin select for LED output.
      PSELLED          : aliased HAL.UInt32;
      --  Pin select for phase A input.
      PSELA            : aliased HAL.UInt32;
      --  Pin select for phase B input.
      PSELB            : aliased HAL.UInt32;
      --  Enable debouncer input filters.
      DBFEN            : aliased DBFEN_Register;
      --  Time LED is switched ON before the sample.
      LEDPRE           : aliased LEDPRE_Register;
      --  Accumulated double (error) transitions register.
      ACCDBL           : aliased ACCDBL_Register;
      --  Snapshot of ACCDBL register. Value generated by the TASKS_READCLEACC
      --  task.
      ACCDBLREAD       : aliased ACCDBLREAD_Register;
      --  Peripheral power control.
      POWER            : aliased POWER_Register;
   end record
     with Volatile;

   for QDEC_Peripheral use record
      TASKS_START      at 16#0# range 0 .. 31;
      TASKS_STOP       at 16#4# range 0 .. 31;
      TASKS_READCLRACC at 16#8# range 0 .. 31;
      EVENTS_SAMPLERDY at 16#100# range 0 .. 31;
      EVENTS_REPORTRDY at 16#104# range 0 .. 31;
      EVENTS_ACCOF     at 16#108# range 0 .. 31;
      SHORTS           at 16#200# range 0 .. 31;
      INTENSET         at 16#304# range 0 .. 31;
      INTENCLR         at 16#308# range 0 .. 31;
      ENABLE           at 16#500# range 0 .. 31;
      LEDPOL           at 16#504# range 0 .. 31;
      SAMPLEPER        at 16#508# range 0 .. 31;
      SAMPLE           at 16#50C# range 0 .. 31;
      REPORTPER        at 16#510# range 0 .. 31;
      ACC              at 16#514# range 0 .. 31;
      ACCREAD          at 16#518# range 0 .. 31;
      PSELLED          at 16#51C# range 0 .. 31;
      PSELA            at 16#520# range 0 .. 31;
      PSELB            at 16#524# range 0 .. 31;
      DBFEN            at 16#528# range 0 .. 31;
      LEDPRE           at 16#540# range 0 .. 31;
      ACCDBL           at 16#544# range 0 .. 31;
      ACCDBLREAD       at 16#548# range 0 .. 31;
      POWER            at 16#FFC# range 0 .. 31;
   end record;

   --  Rotary decoder.
   QDEC_Periph : aliased QDEC_Peripheral
     with Import, Address => System'To_Address (16#40012000#);

end NRF51_SVD.QDEC;
