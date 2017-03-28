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

package NRF51_SVD.SPIM is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Enable interrupt on STOPPED event.
   type INTENSET_STOPPED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on STOPPED event.
   type INTENSET_STOPPED_Field_1 is
     (
      --  Reset value for the field
      Intenset_Stopped_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_STOPPED_Field_1 use
     (Intenset_Stopped_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on ENDRX event.
   type INTENSET_ENDRX_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_ENDRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on ENDRX event.
   type INTENSET_ENDRX_Field_1 is
     (
      --  Reset value for the field
      Intenset_Endrx_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_ENDRX_Field_1 use
     (Intenset_Endrx_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on ENDTX event.
   type INTENSET_ENDTX_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_ENDTX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on ENDTX event.
   type INTENSET_ENDTX_Field_1 is
     (
      --  Reset value for the field
      Intenset_Endtx_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_ENDTX_Field_1 use
     (Intenset_Endtx_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on STARTED event.
   type INTENSET_STARTED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_STARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on STARTED event.
   type INTENSET_STARTED_Field_1 is
     (
      --  Reset value for the field
      Intenset_Started_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_STARTED_Field_1 use
     (Intenset_Started_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Enable interrupt on STOPPED event.
      STOPPED        : INTENSET_STOPPED_Field_1 :=
                        Intenset_Stopped_Field_Reset;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Enable interrupt on ENDRX event.
      ENDRX          : INTENSET_ENDRX_Field_1 := Intenset_Endrx_Field_Reset;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Enable interrupt on ENDTX event.
      ENDTX          : INTENSET_ENDTX_Field_1 := Intenset_Endtx_Field_Reset;
      --  unspecified
      Reserved_9_18  : HAL.UInt10 := 16#0#;
      --  Enable interrupt on STARTED event.
      STARTED        : INTENSET_STARTED_Field_1 :=
                        Intenset_Started_Field_Reset;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      STOPPED        at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      ENDRX          at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      ENDTX          at 0 range 8 .. 8;
      Reserved_9_18  at 0 range 9 .. 18;
      STARTED        at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Disable interrupt on STOPPED event.
   type INTENCLR_STOPPED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on STOPPED event.
   type INTENCLR_STOPPED_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Stopped_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_STOPPED_Field_1 use
     (Intenclr_Stopped_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on ENDRX event.
   type INTENCLR_ENDRX_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_ENDRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on ENDRX event.
   type INTENCLR_ENDRX_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Endrx_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_ENDRX_Field_1 use
     (Intenclr_Endrx_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on ENDTX event.
   type INTENCLR_ENDTX_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_ENDTX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on ENDTX event.
   type INTENCLR_ENDTX_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Endtx_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_ENDTX_Field_1 use
     (Intenclr_Endtx_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on STARTED event.
   type INTENCLR_STARTED_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_STARTED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on STARTED event.
   type INTENCLR_STARTED_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Started_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_STARTED_Field_1 use
     (Intenclr_Started_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Disable interrupt on STOPPED event.
      STOPPED        : INTENCLR_STOPPED_Field_1 :=
                        Intenclr_Stopped_Field_Reset;
      --  unspecified
      Reserved_2_3   : HAL.UInt2 := 16#0#;
      --  Disable interrupt on ENDRX event.
      ENDRX          : INTENCLR_ENDRX_Field_1 := Intenclr_Endrx_Field_Reset;
      --  unspecified
      Reserved_5_7   : HAL.UInt3 := 16#0#;
      --  Disable interrupt on ENDTX event.
      ENDTX          : INTENCLR_ENDTX_Field_1 := Intenclr_Endtx_Field_Reset;
      --  unspecified
      Reserved_9_18  : HAL.UInt10 := 16#0#;
      --  Disable interrupt on STARTED event.
      STARTED        : INTENCLR_STARTED_Field_1 :=
                        Intenclr_Started_Field_Reset;
      --  unspecified
      Reserved_20_31 : HAL.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      STOPPED        at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      ENDRX          at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      ENDTX          at 0 range 8 .. 8;
      Reserved_9_18  at 0 range 9 .. 18;
      STARTED        at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  Enable or disable SPIM.
   type ENABLE_ENABLE_Field is
     (
      --  Disabled SPIM.
      Disabled,
      --  Enable SPIM.
      Enabled)
     with Size => 4;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 7);

   --  Enable SPIM.
   type ENABLE_Register is record
      --  Enable or disable SPIM.
      ENABLE        : ENABLE_ENABLE_Field := NRF51_SVD.SPIM.Disabled;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------------------------
   -- SPIM_PSEL cluster's Registers --
   -----------------------------------

   --  Pin select configuration.
   type SPIM_PSEL_Cluster is record
      --  Pin select for SCK.
      SCK  : aliased HAL.UInt32;
      --  Pin select for MOSI.
      MOSI : aliased HAL.UInt32;
      --  Pin select for MISO.
      MISO : aliased HAL.UInt32;
   end record
     with Volatile, Size => 96;

   for SPIM_PSEL_Cluster use record
      SCK  at 16#0# range 0 .. 31;
      MOSI at 16#4# range 0 .. 31;
      MISO at 16#8# range 0 .. 31;
   end record;

   ----------------------------------
   -- SPIM_RXD cluster's Registers --
   ----------------------------------

   subtype MAXCNT_RXD_MAXCNT_Field is HAL.UInt8;

   --  Maximum number of buffer bytes to receive.
   type MAXCNT_RXD_Register is record
      --  Maximum number of buffer bytes to receive.
      MAXCNT        : MAXCNT_RXD_MAXCNT_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAXCNT_RXD_Register use record
      MAXCNT        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AMOUNT_RXD_AMOUNT_Field is HAL.UInt8;

   --  Number of bytes received in the last transaction.
   type AMOUNT_RXD_Register is record
      --  Read-only. Number of bytes received in the last transaction.
      AMOUNT        : AMOUNT_RXD_AMOUNT_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMOUNT_RXD_Register use record
      AMOUNT        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  RXD EasyDMA configuration and status.
   type SPIM_RXD_Cluster is record
      --  Data pointer.
      PTR    : aliased HAL.UInt32;
      --  Maximum number of buffer bytes to receive.
      MAXCNT : aliased MAXCNT_RXD_Register;
      --  Number of bytes received in the last transaction.
      AMOUNT : aliased AMOUNT_RXD_Register;
   end record
     with Volatile, Size => 96;

   for SPIM_RXD_Cluster use record
      PTR    at 16#0# range 0 .. 31;
      MAXCNT at 16#4# range 0 .. 31;
      AMOUNT at 16#8# range 0 .. 31;
   end record;

   ----------------------------------
   -- SPIM_TXD cluster's Registers --
   ----------------------------------

   subtype MAXCNT_TXD_MAXCNT_Field is HAL.UInt8;

   --  Maximum number of buffer bytes to send.
   type MAXCNT_TXD_Register is record
      --  Maximum number of buffer bytes to send.
      MAXCNT        : MAXCNT_TXD_MAXCNT_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for MAXCNT_TXD_Register use record
      MAXCNT        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AMOUNT_TXD_AMOUNT_Field is HAL.UInt8;

   --  Number of bytes sent in the last transaction.
   type AMOUNT_TXD_Register is record
      --  Read-only. Number of bytes sent in the last transaction.
      AMOUNT        : AMOUNT_TXD_AMOUNT_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for AMOUNT_TXD_Register use record
      AMOUNT        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  TXD EasyDMA configuration and status.
   type SPIM_TXD_Cluster is record
      --  Data pointer.
      PTR    : aliased HAL.UInt32;
      --  Maximum number of buffer bytes to send.
      MAXCNT : aliased MAXCNT_TXD_Register;
      --  Number of bytes sent in the last transaction.
      AMOUNT : aliased AMOUNT_TXD_Register;
   end record
     with Volatile, Size => 96;

   for SPIM_TXD_Cluster use record
      PTR    at 16#0# range 0 .. 31;
      MAXCNT at 16#4# range 0 .. 31;
      AMOUNT at 16#8# range 0 .. 31;
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
      ORDER         : CONFIG_ORDER_Field := NRF51_SVD.SPIM.Msbfirst;
      --  Serial clock (SCK) phase.
      CPHA          : CONFIG_CPHA_Field := NRF51_SVD.SPIM.Leading;
      --  Serial clock (SCK) polarity.
      CPOL          : CONFIG_CPOL_Field := NRF51_SVD.SPIM.Activehigh;
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
      POWER         : POWER_POWER_Field := NRF51_SVD.SPIM.Disabled;
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

   --  SPI master with easyDMA 1.
   type SPIM_Peripheral is record
      --  Start SPI transaction.
      TASKS_START    : aliased HAL.UInt32;
      --  Stop SPI transaction.
      TASKS_STOP     : aliased HAL.UInt32;
      --  Suspend SPI transaction.
      TASKS_SUSPEND  : aliased HAL.UInt32;
      --  Resume SPI transaction.
      TASKS_RESUME   : aliased HAL.UInt32;
      --  SPI transaction has stopped.
      EVENTS_STOPPED : aliased HAL.UInt32;
      --  End of RXD buffer reached.
      EVENTS_ENDRX   : aliased HAL.UInt32;
      --  End of TXD buffer reached.
      EVENTS_ENDTX   : aliased HAL.UInt32;
      --  Transaction started.
      EVENTS_STARTED : aliased HAL.UInt32;
      --  Interrupt enable set register.
      INTENSET       : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR       : aliased INTENCLR_Register;
      --  Enable SPIM.
      ENABLE         : aliased ENABLE_Register;
      --  Pin select configuration.
      PSEL           : aliased SPIM_PSEL_Cluster;
      --  SPI frequency.
      FREQUENCY      : aliased HAL.UInt32;
      --  RXD EasyDMA configuration and status.
      RXD            : aliased SPIM_RXD_Cluster;
      --  TXD EasyDMA configuration and status.
      TXD            : aliased SPIM_TXD_Cluster;
      --  Configuration register.
      CONFIG         : aliased CONFIG_Register;
      --  Over-read character.
      ORC            : aliased ORC_Register;
      --  Peripheral power control.
      POWER          : aliased POWER_Register;
   end record
     with Volatile;

   for SPIM_Peripheral use record
      TASKS_START    at 16#10# range 0 .. 31;
      TASKS_STOP     at 16#14# range 0 .. 31;
      TASKS_SUSPEND  at 16#1C# range 0 .. 31;
      TASKS_RESUME   at 16#20# range 0 .. 31;
      EVENTS_STOPPED at 16#104# range 0 .. 31;
      EVENTS_ENDRX   at 16#110# range 0 .. 31;
      EVENTS_ENDTX   at 16#120# range 0 .. 31;
      EVENTS_STARTED at 16#14C# range 0 .. 31;
      INTENSET       at 16#304# range 0 .. 31;
      INTENCLR       at 16#308# range 0 .. 31;
      ENABLE         at 16#500# range 0 .. 31;
      PSEL           at 16#508# range 0 .. 95;
      FREQUENCY      at 16#524# range 0 .. 31;
      RXD            at 16#534# range 0 .. 95;
      TXD            at 16#544# range 0 .. 95;
      CONFIG         at 16#554# range 0 .. 31;
      ORC            at 16#5C0# range 0 .. 31;
      POWER          at 16#FFC# range 0 .. 31;
   end record;

   --  SPI master with easyDMA 1.
   SPIM1_Periph : aliased SPIM_Peripheral
     with Import, Address => System'To_Address (16#40004000#);

end NRF51_SVD.SPIM;
