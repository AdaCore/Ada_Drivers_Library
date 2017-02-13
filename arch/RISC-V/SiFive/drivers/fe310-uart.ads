------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with HAL.UART; use HAL.UART;

private with FE310_SVD.UART;

package FE310.UART is

   type Internal_UART is limited private;

   type UART_Port (Periph : not null access Internal_UART) is
     limited new HAL.UART.UART_Port with private;

   type Stop_Bits is (Stopbits_1, Stopbits_2);
   procedure Set_Stop_Bits (This : in out UART_Port; To : Stop_Bits);

   subtype Baud_Rates is UInt32;
   procedure Set_Baud_Rate (This : in out UART_Port;  To : Baud_Rates);

   procedure Enable_RX (This : in out UART_Port);
   procedure Enable_TX (This : in out UART_Port);
   procedure Disable_RX (This : in out UART_Port);
   procedure Disable_TX (This : in out UART_Port);

   function RX_Interrupt_Pending (This : UART_Port) return Boolean;
   --  The interrupt flag is set when the RX fifo is strictly greater than the
   --  threshold (default to 0).
   --
   --  The flag is cleared by the hardware when enough data have been dequeued.

   function TX_Interrupt_Pending (This : UART_Port) return Boolean;
   --  The interrupt flag is set when the TX fifo is strictly less than the
   --  threshold (default to 0).
   --
   --  The flag is cleared by the hardware when enough data have been enqueued.

   procedure Enable_RX_Interrupt (This : in out UART_Port);
   procedure Enable_TX_Interrupt (This : in out UART_Port);
   procedure Disable_RX_Interrupt (This : in out UART_Port);
   procedure Disable_TX_Interrupt (This : in out UART_Port);

   procedure Set_Interrupt_Thresholds (This   : in out UART_Port;
                                       RX, TX : UInt3);

   ---------------
   --  HAL.GPIO --
   ---------------

   overriding
   function Data_Size (Port : UART_Port) return UART_Data_Size
   is (Data_Size_8b);
   --  FE310 UARTs are 8bits only

   overriding
   procedure Transmit
     (This    : in out UART_Port;
      Data    : UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Transmit
     (This    : in out UART_Port;
      Data    : UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out UART_Port;
      Data    : out UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out UART_Port;
      Data    : out UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000);

private

   type Internal_UART is new FE310_SVD.UART.UART_Peripheral;

   type UART_Port (Periph : not null access Internal_UART) is
     limited new HAL.UART.UART_Port with null record;

end FE310.UART;
