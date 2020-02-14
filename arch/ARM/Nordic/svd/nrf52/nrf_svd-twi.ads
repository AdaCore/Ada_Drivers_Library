--  Copyright (c) 2010 - 2018, Nordic Semiconductor ASA
--
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without modification,
--  are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form, except as embedded into a Nordic
--  Semiconductor ASA integrated circuit in a product or a software update for
--  such product, must reproduce the above copyright notice, this list of
--  conditions and the following disclaimer in the documentation and/or other
--  materials provided with the distribution.
--
--  3. Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from this
--  software without specific prior written permission.
--
--  4. This software, with or without modification, must only be used with a
--  Nordic Semiconductor ASA integrated circuit.
--
--  5. Any software provided in binary form under this license must not be reverse
--  engineered, decompiled, modified and/or disassembled.
--
--  THIS SOFTWARE IS PROVIDED BY NORDIC SEMICONDUCTOR ASA "AS IS" AND ANY EXPRESS
--  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL NORDIC SEMICONDUCTOR ASA OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
--  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
--  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf52.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF_SVD.TWI is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between BB event and SUSPEND task
   type SHORTS_BB_SUSPEND_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_BB_SUSPEND_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between BB event and STOP task
   type SHORTS_BB_STOP_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_BB_STOP_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut register
   type SHORTS_Register is record
      --  Shortcut between BB event and SUSPEND task
      BB_SUSPEND    : SHORTS_BB_SUSPEND_Field := NRF_SVD.TWI.Disabled;
      --  Shortcut between BB event and STOP task
      BB_STOP       : SHORTS_BB_STOP_Field := NRF_SVD.TWI.Disabled;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      BB_SUSPEND    at 0 range 0 .. 0;
      BB_STOP       at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Write '1' to Enable interrupt for STOPPED event
   type INTENSET_STOPPED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for STOPPED event
   type INTENSET_STOPPED_Field_1 is
     (--  Reset value for the field
      Intenset_Stopped_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_STOPPED_Field_1 use
     (Intenset_Stopped_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for RXDREADY event
   type INTENSET_RXDREADY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RXDREADY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RXDREADY event
   type INTENSET_RXDREADY_Field_1 is
     (--  Reset value for the field
      Intenset_Rxdready_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RXDREADY_Field_1 use
     (Intenset_Rxdready_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for TXDSENT event
   type INTENSET_TXDSENT_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_TXDSENT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for TXDSENT event
   type INTENSET_TXDSENT_Field_1 is
     (--  Reset value for the field
      Intenset_Txdsent_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_TXDSENT_Field_1 use
     (Intenset_Txdsent_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for ERROR event
   type INTENSET_ERROR_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_ERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for ERROR event
   type INTENSET_ERROR_Field_1 is
     (--  Reset value for the field
      Intenset_Error_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_ERROR_Field_1 use
     (Intenset_Error_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for BB event
   type INTENSET_BB_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_BB_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for BB event
   type INTENSET_BB_Field_1 is
     (--  Reset value for the field
      Intenset_Bb_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_BB_Field_1 use
     (Intenset_Bb_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for SUSPENDED event
   type INTENSET_SUSPENDED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_SUSPENDED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for SUSPENDED event
   type INTENSET_SUSPENDED_Field_1 is
     (--  Reset value for the field
      Intenset_Suspended_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_SUSPENDED_Field_1 use
     (Intenset_Suspended_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Write '1' to Enable interrupt for STOPPED event
      STOPPED        : INTENSET_STOPPED_Field_1 :=
                        Intenset_Stopped_Field_Reset;
      --  Write '1' to Enable interrupt for RXDREADY event
      RXDREADY       : INTENSET_RXDREADY_Field_1 :=
                        Intenset_Rxdready_Field_Reset;
      --  unspecified
      Reserved_3_6   : HAL.UInt4 := 16#0#;
      --  Write '1' to Enable interrupt for TXDSENT event
      TXDSENT        : INTENSET_TXDSENT_Field_1 :=
                        Intenset_Txdsent_Field_Reset;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Write '1' to Enable interrupt for ERROR event
      ERROR          : INTENSET_ERROR_Field_1 := Intenset_Error_Field_Reset;
      --  unspecified
      Reserved_10_13 : HAL.UInt4 := 16#0#;
      --  Write '1' to Enable interrupt for BB event
      BB             : INTENSET_BB_Field_1 := Intenset_Bb_Field_Reset;
      --  unspecified
      Reserved_15_17 : HAL.UInt3 := 16#0#;
      --  Write '1' to Enable interrupt for SUSPENDED event
      SUSPENDED      : INTENSET_SUSPENDED_Field_1 :=
                        Intenset_Suspended_Field_Reset;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      STOPPED        at 0 range 1 .. 1;
      RXDREADY       at 0 range 2 .. 2;
      Reserved_3_6   at 0 range 3 .. 6;
      TXDSENT        at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      ERROR          at 0 range 9 .. 9;
      Reserved_10_13 at 0 range 10 .. 13;
      BB             at 0 range 14 .. 14;
      Reserved_15_17 at 0 range 15 .. 17;
      SUSPENDED      at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Write '1' to Disable interrupt for STOPPED event
   type INTENCLR_STOPPED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_STOPPED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for STOPPED event
   type INTENCLR_STOPPED_Field_1 is
     (--  Reset value for the field
      Intenclr_Stopped_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_STOPPED_Field_1 use
     (Intenclr_Stopped_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for RXDREADY event
   type INTENCLR_RXDREADY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RXDREADY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RXDREADY event
   type INTENCLR_RXDREADY_Field_1 is
     (--  Reset value for the field
      Intenclr_Rxdready_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RXDREADY_Field_1 use
     (Intenclr_Rxdready_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for TXDSENT event
   type INTENCLR_TXDSENT_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_TXDSENT_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for TXDSENT event
   type INTENCLR_TXDSENT_Field_1 is
     (--  Reset value for the field
      Intenclr_Txdsent_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_TXDSENT_Field_1 use
     (Intenclr_Txdsent_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for ERROR event
   type INTENCLR_ERROR_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_ERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for ERROR event
   type INTENCLR_ERROR_Field_1 is
     (--  Reset value for the field
      Intenclr_Error_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_ERROR_Field_1 use
     (Intenclr_Error_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for BB event
   type INTENCLR_BB_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_BB_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for BB event
   type INTENCLR_BB_Field_1 is
     (--  Reset value for the field
      Intenclr_Bb_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_BB_Field_1 use
     (Intenclr_Bb_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for SUSPENDED event
   type INTENCLR_SUSPENDED_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_SUSPENDED_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for SUSPENDED event
   type INTENCLR_SUSPENDED_Field_1 is
     (--  Reset value for the field
      Intenclr_Suspended_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_SUSPENDED_Field_1 use
     (Intenclr_Suspended_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  unspecified
      Reserved_0_0   : HAL.Bit := 16#0#;
      --  Write '1' to Disable interrupt for STOPPED event
      STOPPED        : INTENCLR_STOPPED_Field_1 :=
                        Intenclr_Stopped_Field_Reset;
      --  Write '1' to Disable interrupt for RXDREADY event
      RXDREADY       : INTENCLR_RXDREADY_Field_1 :=
                        Intenclr_Rxdready_Field_Reset;
      --  unspecified
      Reserved_3_6   : HAL.UInt4 := 16#0#;
      --  Write '1' to Disable interrupt for TXDSENT event
      TXDSENT        : INTENCLR_TXDSENT_Field_1 :=
                        Intenclr_Txdsent_Field_Reset;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Write '1' to Disable interrupt for ERROR event
      ERROR          : INTENCLR_ERROR_Field_1 := Intenclr_Error_Field_Reset;
      --  unspecified
      Reserved_10_13 : HAL.UInt4 := 16#0#;
      --  Write '1' to Disable interrupt for BB event
      BB             : INTENCLR_BB_Field_1 := Intenclr_Bb_Field_Reset;
      --  unspecified
      Reserved_15_17 : HAL.UInt3 := 16#0#;
      --  Write '1' to Disable interrupt for SUSPENDED event
      SUSPENDED      : INTENCLR_SUSPENDED_Field_1 :=
                        Intenclr_Suspended_Field_Reset;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      STOPPED        at 0 range 1 .. 1;
      RXDREADY       at 0 range 2 .. 2;
      Reserved_3_6   at 0 range 3 .. 6;
      TXDSENT        at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      ERROR          at 0 range 9 .. 9;
      Reserved_10_13 at 0 range 10 .. 13;
      BB             at 0 range 14 .. 14;
      Reserved_15_17 at 0 range 15 .. 17;
      SUSPENDED      at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Overrun error
   type ERRORSRC_OVERRUN_Field is
     (--  Read: no overrun occured
      Notpresent,
      --  Read: overrun occured
      Present)
     with Size => 1;
   for ERRORSRC_OVERRUN_Field use
     (Notpresent => 0,
      Present => 1);

   --  Overrun error
   type ERRORSRC_OVERRUN_Field_1 is
     (--  Reset value for the field
      Errorsrc_Overrun_Field_Reset,
      --  Write: clear error on writing '1'
      Clear)
     with Size => 1;
   for ERRORSRC_OVERRUN_Field_1 use
     (Errorsrc_Overrun_Field_Reset => 0,
      Clear => 1);

   --  NACK received after sending the address (write '1' to clear)
   type ERRORSRC_ANACK_Field is
     (--  Read: error not present
      Notpresent,
      --  Read: error present
      Present)
     with Size => 1;
   for ERRORSRC_ANACK_Field use
     (Notpresent => 0,
      Present => 1);

   --  NACK received after sending the address (write '1' to clear)
   type ERRORSRC_ANACK_Field_1 is
     (--  Reset value for the field
      Errorsrc_Anack_Field_Reset,
      --  Write: clear error on writing '1'
      Clear)
     with Size => 1;
   for ERRORSRC_ANACK_Field_1 use
     (Errorsrc_Anack_Field_Reset => 0,
      Clear => 1);

   --  NACK received after sending a data byte (write '1' to clear)
   type ERRORSRC_DNACK_Field is
     (--  Read: error not present
      Notpresent,
      --  Read: error present
      Present)
     with Size => 1;
   for ERRORSRC_DNACK_Field use
     (Notpresent => 0,
      Present => 1);

   --  NACK received after sending a data byte (write '1' to clear)
   type ERRORSRC_DNACK_Field_1 is
     (--  Reset value for the field
      Errorsrc_Dnack_Field_Reset,
      --  Write: clear error on writing '1'
      Clear)
     with Size => 1;
   for ERRORSRC_DNACK_Field_1 use
     (Errorsrc_Dnack_Field_Reset => 0,
      Clear => 1);

   --  Error source
   type ERRORSRC_Register is record
      --  Overrun error
      OVERRUN       : ERRORSRC_OVERRUN_Field_1 :=
                       Errorsrc_Overrun_Field_Reset;
      --  NACK received after sending the address (write '1' to clear)
      ANACK         : ERRORSRC_ANACK_Field_1 := Errorsrc_Anack_Field_Reset;
      --  NACK received after sending a data byte (write '1' to clear)
      DNACK         : ERRORSRC_DNACK_Field_1 := Errorsrc_Dnack_Field_Reset;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ERRORSRC_Register use record
      OVERRUN       at 0 range 0 .. 0;
      ANACK         at 0 range 1 .. 1;
      DNACK         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  Enable or disable TWI
   type ENABLE_ENABLE_Field is
     (--  Disable TWI
      Disabled,
      --  Enable TWI
      Enabled)
     with Size => 4;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 5);

   --  Enable TWI
   type ENABLE_Register is record
      --  Enable or disable TWI
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.TWI.Disabled;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype RXD_RXD_Field is HAL.UInt8;

   --  RXD register
   type RXD_Register is record
      --  Read-only. *** Reading this field has side effects on other resources
      --  ***. RXD register
      RXD           : RXD_RXD_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXD_Register use record
      RXD           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype TXD_TXD_Field is HAL.UInt8;

   --  TXD register
   type TXD_Register is record
      --  TXD register
      TXD           : TXD_TXD_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXD_Register use record
      TXD           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype ADDRESS_ADDRESS_Field is HAL.UInt7;

   --  Address used in the TWI transfer
   type ADDRESS_Register is record
      --  Address used in the TWI transfer
      ADDRESS       : ADDRESS_ADDRESS_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : HAL.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ADDRESS_Register use record
      ADDRESS       at 0 range 0 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  I2C compatible Two-Wire Interface 0
   type TWI_Peripheral is record
      --  Start TWI receive sequence
      TASKS_STARTRX    : aliased HAL.UInt32;
      --  Start TWI transmit sequence
      TASKS_STARTTX    : aliased HAL.UInt32;
      --  Stop TWI transaction
      TASKS_STOP       : aliased HAL.UInt32;
      --  Suspend TWI transaction
      TASKS_SUSPEND    : aliased HAL.UInt32;
      --  Resume TWI transaction
      TASKS_RESUME     : aliased HAL.UInt32;
      --  TWI stopped
      EVENTS_STOPPED   : aliased HAL.UInt32;
      --  TWI RXD byte received
      EVENTS_RXDREADY  : aliased HAL.UInt32;
      --  TWI TXD byte sent
      EVENTS_TXDSENT   : aliased HAL.UInt32;
      --  TWI error
      EVENTS_ERROR     : aliased HAL.UInt32;
      --  TWI byte boundary, generated before each byte that is sent or
      --  received
      EVENTS_BB        : aliased HAL.UInt32;
      --  TWI entered the suspended state
      EVENTS_SUSPENDED : aliased HAL.UInt32;
      --  Shortcut register
      SHORTS           : aliased SHORTS_Register;
      --  Enable interrupt
      INTENSET         : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR         : aliased INTENCLR_Register;
      --  Error source
      ERRORSRC         : aliased ERRORSRC_Register;
      --  Enable TWI
      ENABLE           : aliased ENABLE_Register;
      --  Pin select for SCL
      PSELSCL          : aliased HAL.UInt32;
      --  Pin select for SDA
      PSELSDA          : aliased HAL.UInt32;
      --  RXD register
      RXD              : aliased RXD_Register;
      --  TXD register
      TXD              : aliased TXD_Register;
      --  TWI frequency
      FREQUENCY        : aliased HAL.UInt32;
      --  Address used in the TWI transfer
      ADDRESS          : aliased ADDRESS_Register;
   end record
     with Volatile;

   for TWI_Peripheral use record
      TASKS_STARTRX    at 16#0# range 0 .. 31;
      TASKS_STARTTX    at 16#8# range 0 .. 31;
      TASKS_STOP       at 16#14# range 0 .. 31;
      TASKS_SUSPEND    at 16#1C# range 0 .. 31;
      TASKS_RESUME     at 16#20# range 0 .. 31;
      EVENTS_STOPPED   at 16#104# range 0 .. 31;
      EVENTS_RXDREADY  at 16#108# range 0 .. 31;
      EVENTS_TXDSENT   at 16#11C# range 0 .. 31;
      EVENTS_ERROR     at 16#124# range 0 .. 31;
      EVENTS_BB        at 16#138# range 0 .. 31;
      EVENTS_SUSPENDED at 16#148# range 0 .. 31;
      SHORTS           at 16#200# range 0 .. 31;
      INTENSET         at 16#304# range 0 .. 31;
      INTENCLR         at 16#308# range 0 .. 31;
      ERRORSRC         at 16#4C4# range 0 .. 31;
      ENABLE           at 16#500# range 0 .. 31;
      PSELSCL          at 16#508# range 0 .. 31;
      PSELSDA          at 16#50C# range 0 .. 31;
      RXD              at 16#518# range 0 .. 31;
      TXD              at 16#51C# range 0 .. 31;
      FREQUENCY        at 16#524# range 0 .. 31;
      ADDRESS          at 16#588# range 0 .. 31;
   end record;

   --  I2C compatible Two-Wire Interface 0
   TWI0_Periph : aliased TWI_Peripheral
     with Import, Address => TWI0_Base;

   --  I2C compatible Two-Wire Interface 1
   TWI1_Periph : aliased TWI_Peripheral
     with Import, Address => TWI1_Base;

end NRF_SVD.TWI;
