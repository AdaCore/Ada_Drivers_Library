------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017-2019, AdaCore                      --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with System.Storage_Elements;

with HAL;      use HAL;
with HAL.UART; use HAL.UART;


private with System;

package SiFive.UART is

   type UART_Device
     (Base_Address : System.Storage_Elements.Integer_Address)
   is
   limited new HAL.UART.UART_Port with private;

   type Stop_Bits is (Stopbits_1, Stopbits_2);
   procedure Set_Stop_Bits (This : in out UART_Device; To : Stop_Bits);

   subtype Baud_Rates is UInt32;
   procedure Set_Baud_Rate (This          : in out UART_Device;
                            CPU_Frequency : UInt32;
                            To            : Baud_Rates);

   procedure Enable_RX (This : in out UART_Device);
   procedure Enable_TX (This : in out UART_Device);
   procedure Disable_RX (This : in out UART_Device);
   procedure Disable_TX (This : in out UART_Device);

   function RX_Interrupt_Pending (This : UART_Device) return Boolean;
   --  The interrupt flag is set when the RX fifo is strictly greater than the
   --  threshold (default to 0).
   --
   --  The flag is cleared by the hardware when enough data have been dequeued.

   function TX_Interrupt_Pending (This : UART_Device) return Boolean;
   --  The interrupt flag is set when the TX fifo is strictly less than the
   --  threshold (default to 0).
   --
   --  The flag is cleared by the hardware when enough data have been enqueued.

   procedure Enable_RX_Interrupt (This : in out UART_Device);
   procedure Enable_TX_Interrupt (This : in out UART_Device);
   procedure Disable_RX_Interrupt (This : in out UART_Device);
   procedure Disable_TX_Interrupt (This : in out UART_Device);

   procedure Set_Interrupt_Thresholds (This   : in out UART_Device;
                                       RX, TX : UInt3);

   ---------------
   --  HAL.GPIO --
   ---------------

   overriding
   function Data_Size (Port : UART_Device) return UART_Data_Size
   is (Data_Size_8b);
   --  FE310 UARTs are 8bits only

   overriding
   procedure Transmit
     (This    : in out UART_Device;
      Data    : UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Transmit
     (This    : in out UART_Device;
      Data    : UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out UART_Device;
      Data    : out UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out UART_Device;
      Data    : out UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

private

   ---------------
   -- Registers --
   ---------------

   subtype TXDATA_DATA_Field is HAL.UInt8;

   --  Transmit Data Register.
   type TXDATA_Register is record
      DATA          : TXDATA_DATA_Field := 16#0#;
      --  unspecified
      Reserved_8_30 : HAL.UInt23 := 16#0#;
      FULL          : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXDATA_Register use record
      DATA          at 0 range 0 .. 7;
      Reserved_8_30 at 0 range 8 .. 30;
      FULL          at 0 range 31 .. 31;
   end record;

   subtype RXDATA_DATA_Field is HAL.UInt8;

   --  Receive Data Register.
   type RXDATA_Register is record
      DATA          : RXDATA_DATA_Field := 16#0#;
      --  unspecified
      Reserved_8_30 : HAL.UInt23 := 16#0#;
      EMPTY         : Boolean := False;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXDATA_Register use record
      DATA          at 0 range 0 .. 7;
      Reserved_8_30 at 0 range 8 .. 30;
      EMPTY         at 0 range 31 .. 31;
   end record;

   subtype TXCTRL_TXCNT_Field is HAL.UInt3;

   --  Transmit Control Register.
   type TXCTRL_Register is record
      ENABLE         : Boolean := False;
      NSTOP          : Boolean := False;
      --  unspecified
      Reserved_2_15  : HAL.UInt14 := 16#0#;
      TXCNT          : TXCTRL_TXCNT_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXCTRL_Register use record
      ENABLE         at 0 range 0 .. 0;
      NSTOP          at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      TXCNT          at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype RXCTRL_RXCNT_Field is HAL.UInt3;

   --  Receive Control Register.
   type RXCTRL_Register is record
      ENABLE         : Boolean := False;
      --  unspecified
      Reserved_1_15  : HAL.UInt15 := 16#0#;
      RXCNT          : RXCTRL_RXCNT_Field := 16#0#;
      --  unspecified
      Reserved_19_31 : HAL.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXCTRL_Register use record
      ENABLE         at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      RXCNT          at 0 range 16 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  Interrupt Pending Register.
   type IP_Register is record
      TXWM          : Boolean := False;
      RXWM          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IP_Register use record
      TXWM          at 0 range 0 .. 0;
      RXWM          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Interrupt Enable Register.
   type IE_Register is record
      TXWM          : Boolean := False;
      RXWM          : Boolean := False;
      --  unspecified
      Reserved_2_31 : HAL.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for IE_Register use record
      TXWM          at 0 range 0 .. 0;
      RXWM          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype DIV_DIV_Field is HAL.UInt16;

   --  Baud Rate Divisor Register (BAUD = Fin / (DIV + 1)).
   type DIV_Register is record
      DIV            : DIV_DIV_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : HAL.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIV_Register use record
      DIV            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Universal Asynchronous Receiver/Transmitter.
   type UART_Peripheral is record
      --  Transmit Data Register.
      TXDATA : aliased TXDATA_Register;
      --  Receive Data Register.
      RXDATA : aliased RXDATA_Register;
      --  Transmit Control Register.
      TXCTRL : aliased TXCTRL_Register;
      --  Receive Control Register.
      RXCTRL : aliased RXCTRL_Register;
      --  Interrupt Pending Register.
      IP     : aliased IP_Register;
      --  Interrupt Enable Register.
      IE     : aliased IE_Register;
      --  Baud Rate Divisor Register (BAUD = Fin / (DIV + 1)).
      DIV    : aliased DIV_Register;
   end record
     with Volatile;

   for UART_Peripheral use record
      TXDATA at 16#0# range 0 .. 31;
      RXDATA at 16#4# range 0 .. 31;
      TXCTRL at 16#8# range 0 .. 31;
      RXCTRL at 16#C# range 0 .. 31;
      IP     at 16#10# range 0 .. 31;
      IE     at 16#14# range 0 .. 31;
      DIV    at 16#18# range 0 .. 31;
   end record;

   type UART_Device
     (Base_Address : System.Storage_Elements.Integer_Address)
   is
   limited new HAL.UART.UART_Port with null record;

end SiFive.UART;
