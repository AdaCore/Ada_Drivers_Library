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

package NRF51_SVD.SPIS is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between END event and the ACQUIRE task.
   type SHORTS_END_ACQUIRE_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_END_ACQUIRE_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcuts for SPIS.
   type SHORTS_Register is record
      --  unspecified
      Reserved_0_1  : HAL.UInt2 := 16#0#;
      --  Shortcut between END event and the ACQUIRE task.
      END_ACQUIRE   : SHORTS_END_ACQUIRE_Field := NRF51_SVD.SPIS.Disabled;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      END_ACQUIRE   at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Enable interrupt on END event.
   type INTENSET_END_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_END_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on END event.
   type INTENSET_END_Field_1 is
     (
      --  Reset value for the field
      Intenset_End_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_END_Field_1 use
     (Intenset_End_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on ACQUIRED event.
   type INTENSET_ACQUIRED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_ACQUIRED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on ACQUIRED event.
   type INTENSET_ACQUIRED_Field_1 is
     (
      --  Reset value for the field
      Intenset_Acquired_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_ACQUIRED_Field_1 use
     (Intenset_Acquired_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Enable interrupt on END event.
      END_k          : INTENSET_END_Field_1 := Intenset_End_Field_Reset;
      --  unspecified
      Reserved_2_9   : HAL.UInt8 := 16#0#;
      --  Enable interrupt on ACQUIRED event.
      ACQUIRED       : INTENSET_ACQUIRED_Field_1 :=
                        Intenset_Acquired_Field_Reset;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      END_k          at 0 range 1 .. 1;
      Reserved_2_9   at 0 range 2 .. 9;
      ACQUIRED       at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  Disable interrupt on END event.
   type INTENCLR_END_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_END_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on END event.
   type INTENCLR_END_Field_1 is
     (
      --  Reset value for the field
      Intenclr_End_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_END_Field_1 use
     (Intenclr_End_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on ACQUIRED event.
   type INTENCLR_ACQUIRED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_ACQUIRED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on ACQUIRED event.
   type INTENCLR_ACQUIRED_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Acquired_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_ACQUIRED_Field_1 use
     (Intenclr_Acquired_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Disable interrupt on END event.
      END_k          : INTENCLR_END_Field_1 := Intenclr_End_Field_Reset;
      --  unspecified
      Reserved_2_9   : HAL.UInt8 := 16#0#;
      --  Disable interrupt on ACQUIRED event.
      ACQUIRED       : INTENCLR_ACQUIRED_Field_1 :=
                        Intenclr_Acquired_Field_Reset;
      --  unspecified
      Reserved_11_31 : HAL.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      END_k          at 0 range 1 .. 1;
      Reserved_2_9   at 0 range 2 .. 9;
      ACQUIRED       at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  Semaphore status.
   type SEMSTAT_SEMSTAT_Field is
     (
      --  Semaphore is free.
      Free,
      --  Semaphore is assigned to the CPU.
      Cpu,
      --  Semaphore is assigned to the SPIS.
      Spis,
      --  Semaphore is assigned to the SPIS, but a handover to the CPU is
      --  pending.
      Cpupending)
     with Size => 2;
   for SEMSTAT_SEMSTAT_Field use
     (Free => 0,
      Cpu => 1,
      Spis => 2,
      Cpupending => 3);

   --  Semaphore status.
   type SEMSTAT_Register is record
      --  Read-only. Semaphore status.
      SEMSTAT       : SEMSTAT_SEMSTAT_Field;
      --  unspecified
      Reserved_2_31 : HAL.UInt30;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SEMSTAT_Register use record
      SEMSTAT       at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  TX buffer overread detected, and prevented.
   type STATUS_OVERREAD_Field is
     (
      --  Error not present.
      Notpresent,
      --  Error present.
      Present)
     with Size => 1;
   for STATUS_OVERREAD_Field use
     (Notpresent => 0,
      Present => 1);

   --  TX buffer overread detected, and prevented.
   type STATUS_OVERREAD_Field_1 is
     (
      --  Reset value for the field
      Status_Overread_Field_Reset,
      --  Clear on write.
      Clear)
     with Size => 1;
   for STATUS_OVERREAD_Field_1 use
     (Status_Overread_Field_Reset => 0,
      Clear => 1);

   --  RX buffer overflow detected, and prevented.
   type STATUS_OVERFLOW_Field is
     (
      --  Error not present.
      Notpresent,
      --  Error present.
      Present)
     with Size => 1;
   for STATUS_OVERFLOW_Field use
     (Notpresent => 0,
      Present => 1);

   --  RX buffer overflow detected, and prevented.
   type STATUS_OVERFLOW_Field_1 is
     (
      --  Reset value for the field
      Status_Overflow_Field_Reset,
      --  Clear on write.
      Clear)
     with Size => 1;
   for STATUS_OVERFLOW_Field_1 use
     (Status_Overflow_Field_Reset => 0,
      Clear => 1);

   --  Status from last transaction.
   type STATUS_Register is record
      --  TX buffer overread detected, and prevented.
      OVERREAD      : STATUS_OVERREAD_Field_1 := Status_Overread_Field_Reset;
      --  RX buffer overflow detected, and prevented.
      OVERFLOW      : STATUS_OVERFLOW_Field_1 := Status_Overflow_Field_Reset;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for STATUS_Register use record
      OVERREAD      at 0 range 0 .. 0;
      OVERFLOW      at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Enable or disable SPIS.
   type ENABLE_ENABLE_Field is
     (
      --  Disabled SPIS.
      Disabled,
      --  Enable SPIS.
      Enabled)
     with Size => 3;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 2);

   --  Enable SPIS.
   type ENABLE_Register is record
      --  Enable or disable SPIS.
      ENABLE        : ENABLE_ENABLE_Field := NRF51_SVD.SPIS.Disabled;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype MAXRX_MAXRX_Field is HAL.UInt8;

   --  Maximum number of bytes in the receive buffer.
   type MAXRX_Register is record
      --  Maximum number of bytes in the receive buffer.
      MAXRX         : MAXRX_MAXRX_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAXRX_Register use record
      MAXRX         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AMOUNTRX_AMOUNTRX_Field is HAL.UInt8;

   --  Number of bytes received in last granted transaction.
   type AMOUNTRX_Register is record
      --  Read-only. Number of bytes received in last granted transaction.
      AMOUNTRX      : AMOUNTRX_AMOUNTRX_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMOUNTRX_Register use record
      AMOUNTRX      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype MAXTX_MAXTX_Field is HAL.UInt8;

   --  Maximum number of bytes in the transmit buffer.
   type MAXTX_Register is record
      --  Maximum number of bytes in the transmit buffer.
      MAXTX         : MAXTX_MAXTX_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAXTX_Register use record
      MAXTX         at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AMOUNTTX_AMOUNTTX_Field is HAL.UInt8;

   --  Number of bytes transmitted in last granted transaction.
   type AMOUNTTX_Register is record
      --  Read-only. Number of bytes transmitted in last granted transaction.
      AMOUNTTX      : AMOUNTTX_AMOUNTTX_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMOUNTTX_Register use record
      AMOUNTTX      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Bit order.
   type CONFIG_ORDER_Field is
     (
      --  Most significant bit transmitted out first.
      Msbfirst,
      --  Least significant bit transmitted out first.
      Lsbfirst)
     with Size => 1;
   for CONFIG_ORDER_Field use
     (Msbfirst => 0,
      Lsbfirst => 1);

   --  Serial clock (SCK) phase.
   type CONFIG_CPHA_Field is
     (
      --  Sample on leading edge of the clock. Shift serial data on trailing
      --  edge.
      Leading,
      --  Sample on trailing edge of the clock. Shift serial data on leading
      --  edge.
      Trailing)
     with Size => 1;
   for CONFIG_CPHA_Field use
     (Leading => 0,
      Trailing => 1);

   --  Serial clock (SCK) polarity.
   type CONFIG_CPOL_Field is
     (
      --  Active high.
      Activehigh,
      --  Active low.
      Activelow)
     with Size => 1;
   for CONFIG_CPOL_Field use
     (Activehigh => 0,
      Activelow => 1);

   --  Configuration register.
   type CONFIG_Register is record
      --  Bit order.
      ORDER         : CONFIG_ORDER_Field := NRF51_SVD.SPIS.Msbfirst;
      --  Serial clock (SCK) phase.
      CPHA          : CONFIG_CPHA_Field := NRF51_SVD.SPIS.Leading;
      --  Serial clock (SCK) polarity.
      CPOL          : CONFIG_CPOL_Field := NRF51_SVD.SPIS.Activehigh;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      ORDER         at 0 range 0 .. 0;
      CPHA          at 0 range 1 .. 1;
      CPOL          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype DEF_DEF_Field is HAL.UInt8;

   --  Default character.
   type DEF_Register is record
      --  Default character.
      DEF           : DEF_DEF_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DEF_Register use record
      DEF           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype ORC_ORC_Field is HAL.UInt8;

   --  Over-read character.
   type ORC_Register is record
      --  Over-read character.
      ORC           : ORC_ORC_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ORC_Register use record
      ORC           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
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
      POWER         : POWER_POWER_Field := NRF51_SVD.SPIS.Disabled;
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

   --  SPI slave 1.
   type SPIS_Peripheral is record
      --  Acquire SPI semaphore.
      TASKS_ACQUIRE   : aliased HAL.UInt32;
      --  Release SPI semaphore.
      TASKS_RELEASE   : aliased HAL.UInt32;
      --  Granted transaction completed.
      EVENTS_END      : aliased HAL.UInt32;
      --  Semaphore acquired.
      EVENTS_ACQUIRED : aliased HAL.UInt32;
      --  Shortcuts for SPIS.
      SHORTS          : aliased SHORTS_Register;
      --  Interrupt enable set register.
      INTENSET        : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR        : aliased INTENCLR_Register;
      --  Semaphore status.
      SEMSTAT         : aliased SEMSTAT_Register;
      --  Status from last transaction.
      STATUS          : aliased STATUS_Register;
      --  Enable SPIS.
      ENABLE          : aliased ENABLE_Register;
      --  Pin select for SCK.
      PSELSCK         : aliased HAL.UInt32;
      --  Pin select for MISO.
      PSELMISO        : aliased HAL.UInt32;
      --  Pin select for MOSI.
      PSELMOSI        : aliased HAL.UInt32;
      --  Pin select for CSN.
      PSELCSN         : aliased HAL.UInt32;
      --  RX data pointer.
      RXDPTR          : aliased HAL.UInt32;
      --  Maximum number of bytes in the receive buffer.
      MAXRX           : aliased MAXRX_Register;
      --  Number of bytes received in last granted transaction.
      AMOUNTRX        : aliased AMOUNTRX_Register;
      --  TX data pointer.
      TXDPTR          : aliased HAL.UInt32;
      --  Maximum number of bytes in the transmit buffer.
      MAXTX           : aliased MAXTX_Register;
      --  Number of bytes transmitted in last granted transaction.
      AMOUNTTX        : aliased AMOUNTTX_Register;
      --  Configuration register.
      CONFIG          : aliased CONFIG_Register;
      --  Default character.
      DEF             : aliased DEF_Register;
      --  Over-read character.
      ORC             : aliased ORC_Register;
      --  Peripheral power control.
      POWER           : aliased POWER_Register;
   end record
     with Volatile;

   for SPIS_Peripheral use record
      TASKS_ACQUIRE   at 16#24# range 0 .. 31;
      TASKS_RELEASE   at 16#28# range 0 .. 31;
      EVENTS_END      at 16#104# range 0 .. 31;
      EVENTS_ACQUIRED at 16#128# range 0 .. 31;
      SHORTS          at 16#200# range 0 .. 31;
      INTENSET        at 16#304# range 0 .. 31;
      INTENCLR        at 16#308# range 0 .. 31;
      SEMSTAT         at 16#400# range 0 .. 31;
      STATUS          at 16#440# range 0 .. 31;
      ENABLE          at 16#500# range 0 .. 31;
      PSELSCK         at 16#508# range 0 .. 31;
      PSELMISO        at 16#50C# range 0 .. 31;
      PSELMOSI        at 16#510# range 0 .. 31;
      PSELCSN         at 16#514# range 0 .. 31;
      RXDPTR          at 16#534# range 0 .. 31;
      MAXRX           at 16#538# range 0 .. 31;
      AMOUNTRX        at 16#53C# range 0 .. 31;
      TXDPTR          at 16#544# range 0 .. 31;
      MAXTX           at 16#548# range 0 .. 31;
      AMOUNTTX        at 16#54C# range 0 .. 31;
      CONFIG          at 16#554# range 0 .. 31;
      DEF             at 16#55C# range 0 .. 31;
      ORC             at 16#5C0# range 0 .. 31;
      POWER           at 16#FFC# range 0 .. 31;
   end record;

   --  SPI slave 1.
   SPIS1_Periph : aliased SPIS_Peripheral
     with Import, Address => System'To_Address (16#40004000#);

end NRF51_SVD.SPIS;
