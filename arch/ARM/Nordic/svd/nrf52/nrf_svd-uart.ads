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

package NRF_SVD.UART is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between CTS event and STARTRX task
   type SHORTS_CTS_STARTRX_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_CTS_STARTRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between NCTS event and STOPRX task
   type SHORTS_NCTS_STOPRX_Field is
     (--  Disable shortcut
      Disabled,
      --  Enable shortcut
      Enabled)
     with Size => 1;
   for SHORTS_NCTS_STOPRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut register
   type SHORTS_Register is record
      --  unspecified
      Reserved_0_2  : HAL.UInt3 := 16#0#;
      --  Shortcut between CTS event and STARTRX task
      CTS_STARTRX   : SHORTS_CTS_STARTRX_Field := NRF_SVD.UART.Disabled;
      --  Shortcut between NCTS event and STOPRX task
      NCTS_STOPRX   : SHORTS_NCTS_STOPRX_Field := NRF_SVD.UART.Disabled;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      Reserved_0_2  at 0 range 0 .. 2;
      CTS_STARTRX   at 0 range 3 .. 3;
      NCTS_STOPRX   at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Write '1' to Enable interrupt for CTS event
   type INTENSET_CTS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_CTS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for CTS event
   type INTENSET_CTS_Field_1 is
     (--  Reset value for the field
      Intenset_Cts_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_CTS_Field_1 use
     (Intenset_Cts_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for NCTS event
   type INTENSET_NCTS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_NCTS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for NCTS event
   type INTENSET_NCTS_Field_1 is
     (--  Reset value for the field
      Intenset_Ncts_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_NCTS_Field_1 use
     (Intenset_Ncts_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for RXDRDY event
   type INTENSET_RXDRDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RXDRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RXDRDY event
   type INTENSET_RXDRDY_Field_1 is
     (--  Reset value for the field
      Intenset_Rxdrdy_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RXDRDY_Field_1 use
     (Intenset_Rxdrdy_Field_Reset => 0,
      Set => 1);

   --  Write '1' to Enable interrupt for TXDRDY event
   type INTENSET_TXDRDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_TXDRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for TXDRDY event
   type INTENSET_TXDRDY_Field_1 is
     (--  Reset value for the field
      Intenset_Txdrdy_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_TXDRDY_Field_1 use
     (Intenset_Txdrdy_Field_Reset => 0,
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

   --  Write '1' to Enable interrupt for RXTO event
   type INTENSET_RXTO_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENSET_RXTO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Enable interrupt for RXTO event
   type INTENSET_RXTO_Field_1 is
     (--  Reset value for the field
      Intenset_Rxto_Field_Reset,
      --  Enable
      Set)
     with Size => 1;
   for INTENSET_RXTO_Field_1 use
     (Intenset_Rxto_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt
   type INTENSET_Register is record
      --  Write '1' to Enable interrupt for CTS event
      CTS            : INTENSET_CTS_Field_1 := Intenset_Cts_Field_Reset;
      --  Write '1' to Enable interrupt for NCTS event
      NCTS           : INTENSET_NCTS_Field_1 := Intenset_Ncts_Field_Reset;
      --  Write '1' to Enable interrupt for RXDRDY event
      RXDRDY         : INTENSET_RXDRDY_Field_1 := Intenset_Rxdrdy_Field_Reset;
      --  unspecified
      Reserved_3_6   : HAL.UInt4 := 16#0#;
      --  Write '1' to Enable interrupt for TXDRDY event
      TXDRDY         : INTENSET_TXDRDY_Field_1 := Intenset_Txdrdy_Field_Reset;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Write '1' to Enable interrupt for ERROR event
      ERROR          : INTENSET_ERROR_Field_1 := Intenset_Error_Field_Reset;
      --  unspecified
      Reserved_10_16 : HAL.UInt7 := 16#0#;
      --  Write '1' to Enable interrupt for RXTO event
      RXTO           : INTENSET_RXTO_Field_1 := Intenset_Rxto_Field_Reset;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENSET_Register use record
      CTS            at 0 range 0 .. 0;
      NCTS           at 0 range 1 .. 1;
      RXDRDY         at 0 range 2 .. 2;
      Reserved_3_6   at 0 range 3 .. 6;
      TXDRDY         at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      ERROR          at 0 range 9 .. 9;
      Reserved_10_16 at 0 range 10 .. 16;
      RXTO           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Write '1' to Disable interrupt for CTS event
   type INTENCLR_CTS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_CTS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for CTS event
   type INTENCLR_CTS_Field_1 is
     (--  Reset value for the field
      Intenclr_Cts_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_CTS_Field_1 use
     (Intenclr_Cts_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for NCTS event
   type INTENCLR_NCTS_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_NCTS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for NCTS event
   type INTENCLR_NCTS_Field_1 is
     (--  Reset value for the field
      Intenclr_Ncts_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_NCTS_Field_1 use
     (Intenclr_Ncts_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for RXDRDY event
   type INTENCLR_RXDRDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RXDRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RXDRDY event
   type INTENCLR_RXDRDY_Field_1 is
     (--  Reset value for the field
      Intenclr_Rxdrdy_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RXDRDY_Field_1 use
     (Intenclr_Rxdrdy_Field_Reset => 0,
      Clear => 1);

   --  Write '1' to Disable interrupt for TXDRDY event
   type INTENCLR_TXDRDY_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_TXDRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for TXDRDY event
   type INTENCLR_TXDRDY_Field_1 is
     (--  Reset value for the field
      Intenclr_Txdrdy_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_TXDRDY_Field_1 use
     (Intenclr_Txdrdy_Field_Reset => 0,
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

   --  Write '1' to Disable interrupt for RXTO event
   type INTENCLR_RXTO_Field is
     (--  Read: Disabled
      Disabled,
      --  Read: Enabled
      Enabled)
     with Size => 1;
   for INTENCLR_RXTO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Write '1' to Disable interrupt for RXTO event
   type INTENCLR_RXTO_Field_1 is
     (--  Reset value for the field
      Intenclr_Rxto_Field_Reset,
      --  Disable
      Clear)
     with Size => 1;
   for INTENCLR_RXTO_Field_1 use
     (Intenclr_Rxto_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt
   type INTENCLR_Register is record
      --  Write '1' to Disable interrupt for CTS event
      CTS            : INTENCLR_CTS_Field_1 := Intenclr_Cts_Field_Reset;
      --  Write '1' to Disable interrupt for NCTS event
      NCTS           : INTENCLR_NCTS_Field_1 := Intenclr_Ncts_Field_Reset;
      --  Write '1' to Disable interrupt for RXDRDY event
      RXDRDY         : INTENCLR_RXDRDY_Field_1 := Intenclr_Rxdrdy_Field_Reset;
      --  unspecified
      Reserved_3_6   : HAL.UInt4 := 16#0#;
      --  Write '1' to Disable interrupt for TXDRDY event
      TXDRDY         : INTENCLR_TXDRDY_Field_1 := Intenclr_Txdrdy_Field_Reset;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Write '1' to Disable interrupt for ERROR event
      ERROR          : INTENCLR_ERROR_Field_1 := Intenclr_Error_Field_Reset;
      --  unspecified
      Reserved_10_16 : HAL.UInt7 := 16#0#;
      --  Write '1' to Disable interrupt for RXTO event
      RXTO           : INTENCLR_RXTO_Field_1 := Intenclr_Rxto_Field_Reset;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for INTENCLR_Register use record
      CTS            at 0 range 0 .. 0;
      NCTS           at 0 range 1 .. 1;
      RXDRDY         at 0 range 2 .. 2;
      Reserved_3_6   at 0 range 3 .. 6;
      TXDRDY         at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      ERROR          at 0 range 9 .. 9;
      Reserved_10_16 at 0 range 10 .. 16;
      RXTO           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Overrun error
   type ERRORSRC_OVERRUN_Field is
     (--  Read: error not present
      Notpresent,
      --  Read: error present
      Present)
     with Size => 1;
   for ERRORSRC_OVERRUN_Field use
     (Notpresent => 0,
      Present => 1);

   --  Parity error
   type ERRORSRC_PARITY_Field is
     (--  Read: error not present
      Notpresent,
      --  Read: error present
      Present)
     with Size => 1;
   for ERRORSRC_PARITY_Field use
     (Notpresent => 0,
      Present => 1);

   --  Framing error occurred
   type ERRORSRC_FRAMING_Field is
     (--  Read: error not present
      Notpresent,
      --  Read: error present
      Present)
     with Size => 1;
   for ERRORSRC_FRAMING_Field use
     (Notpresent => 0,
      Present => 1);

   --  Break condition
   type ERRORSRC_BREAK_Field is
     (--  Read: error not present
      Notpresent,
      --  Read: error present
      Present)
     with Size => 1;
   for ERRORSRC_BREAK_Field use
     (Notpresent => 0,
      Present => 1);

   --  Error source
   type ERRORSRC_Register is record
      --  Overrun error
      OVERRUN       : ERRORSRC_OVERRUN_Field := NRF_SVD.UART.Notpresent;
      --  Parity error
      PARITY        : ERRORSRC_PARITY_Field := NRF_SVD.UART.Notpresent;
      --  Framing error occurred
      FRAMING       : ERRORSRC_FRAMING_Field := NRF_SVD.UART.Notpresent;
      --  Break condition
      BREAK         : ERRORSRC_BREAK_Field := NRF_SVD.UART.Notpresent;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ERRORSRC_Register use record
      OVERRUN       at 0 range 0 .. 0;
      PARITY        at 0 range 1 .. 1;
      FRAMING       at 0 range 2 .. 2;
      BREAK         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Enable or disable UART
   type ENABLE_ENABLE_Field is
     (--  Disable UART
      Disabled,
      --  Enable UART
      Enabled)
     with Size => 4;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 4);

   --  Enable UART
   type ENABLE_Register is record
      --  Enable or disable UART
      ENABLE        : ENABLE_ENABLE_Field := NRF_SVD.UART.Disabled;
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
      --  ***. RX data received in previous transfers, double buffered
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
      --  Write-only. TX data to be transferred
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

   --  Hardware flow control
   type CONFIG_HWFC_Field is
     (--  Disabled
      Disabled,
      --  Enabled
      Enabled)
     with Size => 1;
   for CONFIG_HWFC_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Parity
   type CONFIG_PARITY_Field is
     (--  Exclude parity bit
      Excluded,
      --  Include parity bit
      Included)
     with Size => 3;
   for CONFIG_PARITY_Field use
     (Excluded => 0,
      Included => 7);

   --  Configuration of parity and hardware flow control
   type CONFIG_Register is record
      --  Hardware flow control
      HWFC          : CONFIG_HWFC_Field := NRF_SVD.UART.Disabled;
      --  Parity
      PARITY        : CONFIG_PARITY_Field := NRF_SVD.UART.Excluded;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      HWFC          at 0 range 0 .. 0;
      PARITY        at 0 range 1 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Universal Asynchronous Receiver/Transmitter
   type UART_Peripheral is record
      --  Start UART receiver
      TASKS_STARTRX : aliased HAL.UInt32;
      --  Stop UART receiver
      TASKS_STOPRX  : aliased HAL.UInt32;
      --  Start UART transmitter
      TASKS_STARTTX : aliased HAL.UInt32;
      --  Stop UART transmitter
      TASKS_STOPTX  : aliased HAL.UInt32;
      --  Suspend UART
      TASKS_SUSPEND : aliased HAL.UInt32;
      --  CTS is activated (set low). Clear To Send.
      EVENTS_CTS    : aliased HAL.UInt32;
      --  CTS is deactivated (set high). Not Clear To Send.
      EVENTS_NCTS   : aliased HAL.UInt32;
      --  Data received in RXD
      EVENTS_RXDRDY : aliased HAL.UInt32;
      --  Data sent from TXD
      EVENTS_TXDRDY : aliased HAL.UInt32;
      --  Error detected
      EVENTS_ERROR  : aliased HAL.UInt32;
      --  Receiver timeout
      EVENTS_RXTO   : aliased HAL.UInt32;
      --  Shortcut register
      SHORTS        : aliased SHORTS_Register;
      --  Enable interrupt
      INTENSET      : aliased INTENSET_Register;
      --  Disable interrupt
      INTENCLR      : aliased INTENCLR_Register;
      --  Error source
      ERRORSRC      : aliased ERRORSRC_Register;
      --  Enable UART
      ENABLE        : aliased ENABLE_Register;
      --  Pin select for RTS
      PSELRTS       : aliased HAL.UInt32;
      --  Pin select for TXD
      PSELTXD       : aliased HAL.UInt32;
      --  Pin select for CTS
      PSELCTS       : aliased HAL.UInt32;
      --  Pin select for RXD
      PSELRXD       : aliased HAL.UInt32;
      --  RXD register
      RXD           : aliased RXD_Register;
      --  TXD register
      TXD           : aliased TXD_Register;
      --  Baud rate
      BAUDRATE      : aliased HAL.UInt32;
      --  Configuration of parity and hardware flow control
      CONFIG        : aliased CONFIG_Register;
   end record
     with Volatile;

   for UART_Peripheral use record
      TASKS_STARTRX at 16#0# range 0 .. 31;
      TASKS_STOPRX  at 16#4# range 0 .. 31;
      TASKS_STARTTX at 16#8# range 0 .. 31;
      TASKS_STOPTX  at 16#C# range 0 .. 31;
      TASKS_SUSPEND at 16#1C# range 0 .. 31;
      EVENTS_CTS    at 16#100# range 0 .. 31;
      EVENTS_NCTS   at 16#104# range 0 .. 31;
      EVENTS_RXDRDY at 16#108# range 0 .. 31;
      EVENTS_TXDRDY at 16#11C# range 0 .. 31;
      EVENTS_ERROR  at 16#124# range 0 .. 31;
      EVENTS_RXTO   at 16#144# range 0 .. 31;
      SHORTS        at 16#200# range 0 .. 31;
      INTENSET      at 16#304# range 0 .. 31;
      INTENCLR      at 16#308# range 0 .. 31;
      ERRORSRC      at 16#480# range 0 .. 31;
      ENABLE        at 16#500# range 0 .. 31;
      PSELRTS       at 16#508# range 0 .. 31;
      PSELTXD       at 16#50C# range 0 .. 31;
      PSELCTS       at 16#510# range 0 .. 31;
      PSELRXD       at 16#514# range 0 .. 31;
      RXD           at 16#518# range 0 .. 31;
      TXD           at 16#51C# range 0 .. 31;
      BAUDRATE      at 16#524# range 0 .. 31;
      CONFIG        at 16#56C# range 0 .. 31;
   end record;

   --  Universal Asynchronous Receiver/Transmitter
   UART0_Periph : aliased UART_Peripheral
     with Import, Address => UART0_Base;

end NRF_SVD.UART;
