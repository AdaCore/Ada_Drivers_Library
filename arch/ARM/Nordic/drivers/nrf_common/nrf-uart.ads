------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2019-2020, AdaCore                      --
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

with HAL;           use HAL;
with HAL.UART;      use HAL.UART;
with nRF.GPIO;    use nRF.GPIO;
with NRF_SVD.UART;

package nRF.UART is

   type UART_Device (Periph : not null access NRF_SVD.UART.UART_Peripheral)
   is
     new HAL.UART.UART_Port with private;

   type Baud_Rate is (Baud1200, Baud2400, Baud4800, Baud9600, Baud14400,
                      Baud19200, Baud28800, Baud38400, Baud57600, Baud76800,
                      Baud115200, Baud230400, Baud250000, Baud460800,
                      Baud921600, Baud1M);

   procedure Configure (This   : in out UART_Device;
                        Baud   : Baud_Rate;
                        Parity : Boolean);

   procedure Enable (This   : in out UART_Device;
                     Tx, Rx : GPIO_Pin_Index);
   --  Connects the GPIOs and enable device

   procedure Disable (This : in out UART_Device);
   --  Disable the device and disconects the GPIOs

   procedure Enable_Flow_Control (This     : in out UART_Device;
                                  RTS, CTS : GPIO_Pin_Index);
   --  Connects the GPIOs used for flow control and enable it

   procedure Disable_Flow_Control (This     : in out UART_Device);
   --  Disable flow control and disconnects the GPIOs

   -- HAL.GPIO --

   overriding
   function Data_Size (Port : UART_Device) return UART_Data_Size
   is (HAL.UART.Data_Size_8b);
   --  Only 8-bit is supported

   overriding
   procedure Transmit
     (This    : in out UART_Device;
      Data    : UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out UART_Device;
      Data    : out UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Transmit
     (This    : in out UART_Device;
      Data    : UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);
   --  Will raise Program_Error

   overriding
   procedure Receive
     (This    : in out UART_Device;
      Data    : out UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);
   --  Will raise Program_Error

private

   type UART_Device (Periph : not null access NRF_SVD.UART.UART_Peripheral)
   is
     new HAL.UART.UART_Port with record
      Do_Stop_Sequence : Boolean := True;
   end record;

   for Baud_Rate use
     (Baud1200   => 16#0004F000#,
      Baud2400   => 16#0009D000#,
      Baud4800   => 16#0013B000#,
      Baud9600   => 16#00275000#,
      Baud14400  => 16#003B0000#,
      Baud19200  => 16#004EA000#,
      Baud28800  => 16#0075F000#,
      Baud38400  => 16#009D5000#,
      Baud57600  => 16#00EBF000#,
      Baud76800  => 16#013A9000#,
      Baud115200 => 16#01D7E000#,
      Baud230400 => 16#03AFB000#,
      Baud250000 => 16#04000000#,
      Baud460800 => 16#075F7000#,
      Baud921600 => 16#0EBEDFA4#,
      Baud1M     => 16#10000000#);

end nRF.UART;
