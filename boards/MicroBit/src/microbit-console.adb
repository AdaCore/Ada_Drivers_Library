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

with HAL;          use HAL;
with HAL.UART;     use HAL.UART;
with nRF.Device;
with nRF.UART;   use nRF.UART;

package body MicroBit.Console is

   UART : nRF.UART.UART_Device renames nRF.Device.UART_0;

   ---------
   -- Get --
   ---------

   procedure Get (C : out Character) is
      Data   : UART_Data_8b (1 .. 1);
      Status : UART_Status;
   begin
      UART.Receive (Data, Status);
      C := Character'Val (Data (Data'First));
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
      Data   : constant UART_Data_8b (1 .. 1) :=
        (1 => UInt8 (Character'Pos (C)));

      Status : UART_Status;
   begin
      UART.Transmit (Data, Status);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Str : String) is
      Data   : UART_Data_8b (Str'First .. Str'Last);

      Status : UART_Status;
   begin
      for Index in Str'Range loop
         Data (Index) := UInt8 (Character'Pos (Str (Index)));
      end loop;
      UART.Transmit (Data, Status);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Str : String) is
      Data   : UART_Data_8b (Str'First .. Str'Last + 2);

      Status : UART_Status;
   begin
      for Index in Str'Range loop
         Data (Index) := UInt8 (Character'Pos (Str (Index)));
      end loop;
      Data (Data'Last - 1) := Character'Pos (ASCII.CR);
      Data (Data'Last) := Character'Pos (ASCII.LF);
      UART.Transmit (Data, Status);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Put (ASCII.CR & ASCII.LF);
   end New_Line;

begin
   UART.Configure (nRF.UART.Baud115200, Parity => False);
   UART.Enable (MB_UART_TX.Pin, MB_UART_RX.Pin);
end MicroBit.Console;

