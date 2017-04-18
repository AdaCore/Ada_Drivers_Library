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

package NRF51_SVD.UART is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Shortcut between CTS event and STARTRX task.
   type SHORTS_CTS_STARTRX_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_CTS_STARTRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcut between NCTS event and STOPRX task.
   type SHORTS_NCTS_STOPRX_Field is
     (
      --  Shortcut disabled.
      Disabled,
      --  Shortcut enabled.
      Enabled)
     with Size => 1;
   for SHORTS_NCTS_STOPRX_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Shortcuts for UART.
   type SHORTS_Register is record
      --  unspecified
      Reserved_0_2  : HAL.UInt3 := 16#0#;
      --  Shortcut between CTS event and STARTRX task.
      CTS_STARTRX   : SHORTS_CTS_STARTRX_Field := NRF51_SVD.UART.Disabled;
      --  Shortcut between NCTS event and STOPRX task.
      NCTS_STOPRX   : SHORTS_NCTS_STOPRX_Field := NRF51_SVD.UART.Disabled;
      --  unspecified
      Reserved_5_31 : HAL.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SHORTS_Register use record
      Reserved_0_2  at 0 range 0 .. 2;
      CTS_STARTRX   at 0 range 3 .. 3;
      NCTS_STOPRX   at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  Enable interrupt on CTS event.
   type INTENSET_CTS_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_CTS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on CTS event.
   type INTENSET_CTS_Field_1 is
     (
      --  Reset value for the field
      Intenset_Cts_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_CTS_Field_1 use
     (Intenset_Cts_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on NCTS event.
   type INTENSET_NCTS_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_NCTS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on NCTS event.
   type INTENSET_NCTS_Field_1 is
     (
      --  Reset value for the field
      Intenset_Ncts_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_NCTS_Field_1 use
     (Intenset_Ncts_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on RXRDY event.
   type INTENSET_RXDRDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_RXDRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on RXRDY event.
   type INTENSET_RXDRDY_Field_1 is
     (
      --  Reset value for the field
      Intenset_Rxdrdy_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_RXDRDY_Field_1 use
     (Intenset_Rxdrdy_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on TXRDY event.
   type INTENSET_TXDRDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_TXDRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on TXRDY event.
   type INTENSET_TXDRDY_Field_1 is
     (
      --  Reset value for the field
      Intenset_Txdrdy_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_TXDRDY_Field_1 use
     (Intenset_Txdrdy_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on ERROR event.
   type INTENSET_ERROR_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_ERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on ERROR event.
   type INTENSET_ERROR_Field_1 is
     (
      --  Reset value for the field
      Intenset_Error_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_ERROR_Field_1 use
     (Intenset_Error_Field_Reset => 0,
      Set => 1);

   --  Enable interrupt on RXTO event.
   type INTENSET_RXTO_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENSET_RXTO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Enable interrupt on RXTO event.
   type INTENSET_RXTO_Field_1 is
     (
      --  Reset value for the field
      Intenset_Rxto_Field_Reset,
      --  Enable interrupt on write.
      Set)
     with Size => 1;
   for INTENSET_RXTO_Field_1 use
     (Intenset_Rxto_Field_Reset => 0,
      Set => 1);

   --  Interrupt enable set register.
   type INTENSET_Register is record
      --  Enable interrupt on CTS event.
      CTS            : INTENSET_CTS_Field_1 := Intenset_Cts_Field_Reset;
      --  Enable interrupt on NCTS event.
      NCTS           : INTENSET_NCTS_Field_1 := Intenset_Ncts_Field_Reset;
      --  Enable interrupt on RXRDY event.
      RXDRDY         : INTENSET_RXDRDY_Field_1 := Intenset_Rxdrdy_Field_Reset;
      --  unspecified
      Reserved_3_6   : HAL.UInt4 := 16#0#;
      --  Enable interrupt on TXRDY event.
      TXDRDY         : INTENSET_TXDRDY_Field_1 := Intenset_Txdrdy_Field_Reset;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Enable interrupt on ERROR event.
      ERROR          : INTENSET_ERROR_Field_1 := Intenset_Error_Field_Reset;
      --  unspecified
      Reserved_10_16 : HAL.UInt7 := 16#0#;
      --  Enable interrupt on RXTO event.
      RXTO           : INTENSET_RXTO_Field_1 := Intenset_Rxto_Field_Reset;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
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

   --  Disable interrupt on CTS event.
   type INTENCLR_CTS_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_CTS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on CTS event.
   type INTENCLR_CTS_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Cts_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_CTS_Field_1 use
     (Intenclr_Cts_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on NCTS event.
   type INTENCLR_NCTS_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_NCTS_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on NCTS event.
   type INTENCLR_NCTS_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Ncts_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_NCTS_Field_1 use
     (Intenclr_Ncts_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on RXRDY event.
   type INTENCLR_RXDRDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_RXDRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on RXRDY event.
   type INTENCLR_RXDRDY_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Rxdrdy_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_RXDRDY_Field_1 use
     (Intenclr_Rxdrdy_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on TXRDY event.
   type INTENCLR_TXDRDY_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_TXDRDY_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on TXRDY event.
   type INTENCLR_TXDRDY_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Txdrdy_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_TXDRDY_Field_1 use
     (Intenclr_Txdrdy_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on ERROR event.
   type INTENCLR_ERROR_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_ERROR_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on ERROR event.
   type INTENCLR_ERROR_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Error_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_ERROR_Field_1 use
     (Intenclr_Error_Field_Reset => 0,
      Clear => 1);

   --  Disable interrupt on RXTO event.
   type INTENCLR_RXTO_Field is
     (
      --  Interrupt disabled.
      Disabled,
      --  Interrupt enabled.
      Enabled)
     with Size => 1;
   for INTENCLR_RXTO_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Disable interrupt on RXTO event.
   type INTENCLR_RXTO_Field_1 is
     (
      --  Reset value for the field
      Intenclr_Rxto_Field_Reset,
      --  Disable interrupt on write.
      Clear)
     with Size => 1;
   for INTENCLR_RXTO_Field_1 use
     (Intenclr_Rxto_Field_Reset => 0,
      Clear => 1);

   --  Interrupt enable clear register.
   type INTENCLR_Register is record
      --  Disable interrupt on CTS event.
      CTS            : INTENCLR_CTS_Field_1 := Intenclr_Cts_Field_Reset;
      --  Disable interrupt on NCTS event.
      NCTS           : INTENCLR_NCTS_Field_1 := Intenclr_Ncts_Field_Reset;
      --  Disable interrupt on RXRDY event.
      RXDRDY         : INTENCLR_RXDRDY_Field_1 := Intenclr_Rxdrdy_Field_Reset;
      --  unspecified
      Reserved_3_6   : HAL.UInt4 := 16#0#;
      --  Disable interrupt on TXRDY event.
      TXDRDY         : INTENCLR_TXDRDY_Field_1 := Intenclr_Txdrdy_Field_Reset;
      --  unspecified
      Reserved_8_8   : HAL.Bit := 16#0#;
      --  Disable interrupt on ERROR event.
      ERROR          : INTENCLR_ERROR_Field_1 := Intenclr_Error_Field_Reset;
      --  unspecified
      Reserved_10_16 : HAL.UInt7 := 16#0#;
      --  Disable interrupt on RXTO event.
      RXTO           : INTENCLR_RXTO_Field_1 := Intenclr_Rxto_Field_Reset;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
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

   --  A start bit is received while the previous data still lies in RXD. (Data
   --  loss).
   type ERRORSRC_OVERRUN_Field is
     (
      --  Error not present.
      Notpresent,
      --  Error present.
      Present)
     with Size => 1;
   for ERRORSRC_OVERRUN_Field use
     (Notpresent => 0,
      Present => 1);

   --  A start bit is received while the previous data still lies in RXD. (Data
   --  loss).
   type ERRORSRC_OVERRUN_Field_1 is
     (
      --  Reset value for the field
      Errorsrc_Overrun_Field_Reset,
      --  Clear error on write.
      Clear)
     with Size => 1;
   for ERRORSRC_OVERRUN_Field_1 use
     (Errorsrc_Overrun_Field_Reset => 0,
      Clear => 1);

   --  A character with bad parity is received. Only checked if HW parity
   --  control is enabled.
   type ERRORSRC_PARITY_Field is
     (
      --  Error not present.
      Notpresent,
      --  Error present.
      Present)
     with Size => 1;
   for ERRORSRC_PARITY_Field use
     (Notpresent => 0,
      Present => 1);

   --  A character with bad parity is received. Only checked if HW parity
   --  control is enabled.
   type ERRORSRC_PARITY_Field_1 is
     (
      --  Reset value for the field
      Errorsrc_Parity_Field_Reset,
      --  Clear error on write.
      Clear)
     with Size => 1;
   for ERRORSRC_PARITY_Field_1 use
     (Errorsrc_Parity_Field_Reset => 0,
      Clear => 1);

   --  A valid stop bit is not detected on the serial data input after all bits
   --  in a character have been received.
   type ERRORSRC_FRAMING_Field is
     (
      --  Error not present.
      Notpresent,
      --  Error present.
      Present)
     with Size => 1;
   for ERRORSRC_FRAMING_Field use
     (Notpresent => 0,
      Present => 1);

   --  A valid stop bit is not detected on the serial data input after all bits
   --  in a character have been received.
   type ERRORSRC_FRAMING_Field_1 is
     (
      --  Reset value for the field
      Errorsrc_Framing_Field_Reset,
      --  Clear error on write.
      Clear)
     with Size => 1;
   for ERRORSRC_FRAMING_Field_1 use
     (Errorsrc_Framing_Field_Reset => 0,
      Clear => 1);

   --  The serial data input is '0' for longer than the length of a data frame.
   type ERRORSRC_BREAK_Field is
     (
      --  Error not present.
      Notpresent,
      --  Error present.
      Present)
     with Size => 1;
   for ERRORSRC_BREAK_Field use
     (Notpresent => 0,
      Present => 1);

   --  The serial data input is '0' for longer than the length of a data frame.
   type ERRORSRC_BREAK_Field_1 is
     (
      --  Reset value for the field
      Errorsrc_Break_Field_Reset,
      --  Clear error on write.
      Clear)
     with Size => 1;
   for ERRORSRC_BREAK_Field_1 use
     (Errorsrc_Break_Field_Reset => 0,
      Clear => 1);

   --  Error source. Write error field to 1 to clear error.
   type ERRORSRC_Register is record
      --  A start bit is received while the previous data still lies in RXD.
      --  (Data loss).
      OVERRUN       : ERRORSRC_OVERRUN_Field_1 :=
                       Errorsrc_Overrun_Field_Reset;
      --  A character with bad parity is received. Only checked if HW parity
      --  control is enabled.
      PARITY        : ERRORSRC_PARITY_Field_1 := Errorsrc_Parity_Field_Reset;
      --  A valid stop bit is not detected on the serial data input after all
      --  bits in a character have been received.
      FRAMING       : ERRORSRC_FRAMING_Field_1 :=
                       Errorsrc_Framing_Field_Reset;
      --  The serial data input is '0' for longer than the length of a data
      --  frame.
      BREAK         : ERRORSRC_BREAK_Field_1 := Errorsrc_Break_Field_Reset;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ERRORSRC_Register use record
      OVERRUN       at 0 range 0 .. 0;
      PARITY        at 0 range 1 .. 1;
      FRAMING       at 0 range 2 .. 2;
      BREAK         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  Enable or disable UART and acquire IOs.
   type ENABLE_ENABLE_Field is
     (
      --  UART disabled.
      Disabled,
      --  UART enabled.
      Enabled)
     with Size => 3;
   for ENABLE_ENABLE_Field use
     (Disabled => 0,
      Enabled => 4);

   --  Enable UART and acquire IOs.
   type ENABLE_Register is record
      --  Enable or disable UART and acquire IOs.
      ENABLE        : ENABLE_ENABLE_Field := NRF51_SVD.UART.Disabled;
      --  unspecified
      Reserved_3_31 : HAL.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENABLE_Register use record
      ENABLE        at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype RXD_RXD_Field is HAL.UInt8;

   --  RXD register. On read action the buffer pointer is displaced. Once read
   --  the character is consumed. If read when no character available, the UART
   --  will stop working.
   type RXD_Register is record
      --  Read-only. *** Reading this field has side effects on other resources
      --  ***. RX data from previous transfer. Double buffered.
      RXD           : RXD_RXD_Field;
      --  unspecified
      Reserved_8_31 : HAL.UInt24;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXD_Register use record
      RXD           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype TXD_TXD_Field is HAL.UInt8;

   --  TXD register.
   type TXD_Register is record
      --  Write-only. TX data for transfer.
      TXD           : TXD_TXD_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : HAL.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXD_Register use record
      TXD           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Hardware flow control.
   type CONFIG_HWFC_Field is
     (
      --  Hardware flow control disabled.
      Disabled,
      --  Hardware flow control enabled.
      Enabled)
     with Size => 1;
   for CONFIG_HWFC_Field use
     (Disabled => 0,
      Enabled => 1);

   --  Include parity bit.
   type CONFIG_PARITY_Field is
     (
      --  Parity bit excluded.
      Excluded,
      --  Parity bit included.
      Included)
     with Size => 3;
   for CONFIG_PARITY_Field use
     (Excluded => 0,
      Included => 7);

   --  Configuration of parity and hardware flow control register.
   type CONFIG_Register is record
      --  Hardware flow control.
      HWFC          : CONFIG_HWFC_Field := NRF51_SVD.UART.Disabled;
      --  Include parity bit.
      PARITY        : CONFIG_PARITY_Field := NRF51_SVD.UART.Excluded;
      --  unspecified
      Reserved_4_31 : HAL.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFIG_Register use record
      HWFC          at 0 range 0 .. 0;
      PARITY        at 0 range 1 .. 3;
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
      POWER         : POWER_POWER_Field := NRF51_SVD.UART.Disabled;
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

   --  Universal Asynchronous Receiver/Transmitter.
   type UART_Peripheral is record
      --  Start UART receiver.
      TASKS_STARTRX : aliased HAL.UInt32;
      --  Stop UART receiver.
      TASKS_STOPRX  : aliased HAL.UInt32;
      --  Start UART transmitter.
      TASKS_STARTTX : aliased HAL.UInt32;
      --  Stop UART transmitter.
      TASKS_STOPTX  : aliased HAL.UInt32;
      --  Suspend UART.
      TASKS_SUSPEND : aliased HAL.UInt32;
      --  CTS activated.
      EVENTS_CTS    : aliased HAL.UInt32;
      --  CTS deactivated.
      EVENTS_NCTS   : aliased HAL.UInt32;
      --  Data received in RXD.
      EVENTS_RXDRDY : aliased HAL.UInt32;
      --  Data sent from TXD.
      EVENTS_TXDRDY : aliased HAL.UInt32;
      --  Error detected.
      EVENTS_ERROR  : aliased HAL.UInt32;
      --  Receiver timeout.
      EVENTS_RXTO   : aliased HAL.UInt32;
      --  Shortcuts for UART.
      SHORTS        : aliased SHORTS_Register;
      --  Interrupt enable set register.
      INTENSET      : aliased INTENSET_Register;
      --  Interrupt enable clear register.
      INTENCLR      : aliased INTENCLR_Register;
      --  Error source. Write error field to 1 to clear error.
      ERRORSRC      : aliased ERRORSRC_Register;
      --  Enable UART and acquire IOs.
      ENABLE        : aliased ENABLE_Register;
      --  Pin select for RTS.
      PSELRTS       : aliased HAL.UInt32;
      --  Pin select for TXD.
      PSELTXD       : aliased HAL.UInt32;
      --  Pin select for CTS.
      PSELCTS       : aliased HAL.UInt32;
      --  Pin select for RXD.
      PSELRXD       : aliased HAL.UInt32;
      --  RXD register. On read action the buffer pointer is displaced. Once
      --  read the character is consumed. If read when no character available,
      --  the UART will stop working.
      RXD           : aliased RXD_Register;
      --  TXD register.
      TXD           : aliased TXD_Register;
      --  UART Baudrate.
      BAUDRATE      : aliased HAL.UInt32;
      --  Configuration of parity and hardware flow control register.
      CONFIG        : aliased CONFIG_Register;
      --  Peripheral power control.
      POWER         : aliased POWER_Register;
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
      POWER         at 16#FFC# range 0 .. 31;
   end record;

   --  Universal Asynchronous Receiver/Transmitter.
   UART0_Periph : aliased UART_Peripheral
     with Import, Address => System'To_Address (16#40002000#);

end NRF51_SVD.UART;
