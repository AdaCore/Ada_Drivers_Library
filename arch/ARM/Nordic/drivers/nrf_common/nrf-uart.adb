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

with NRF_SVD.UART; use NRF_SVD.UART;

package body nRF.UART is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (This : in out UART_Device; Baud : Baud_Rate; Parity : Boolean)
   is
   begin
      This.Periph.BAUDRATE := Baud'Enum_Rep;
      This.Periph.CONFIG.PARITY := (if Parity then Included else Excluded);
   end Configure;

   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out UART_Device;
                     Tx, Rx : GPIO_Pin_Index) is
   begin
      This.Periph.PSELTXD := UInt32 (Tx);
      This.Periph.PSELRXD := UInt32 (Rx);
      This.Periph.ENABLE.ENABLE := Enabled;

      --  Start TX and RX
      This.Periph.TASKS_STARTRX := 1;
      This.Periph.TASKS_STARTTX := 1;

      --  Send a first character to start the TXREADY events (See nRF Series
      --  Reference Manual Version 3.0 Figure 68: UART transmission).
      This.Periph.TXD.TXD := 0;
   end Enable;
   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out UART_Device) is
   begin
      This.Periph.ENABLE.ENABLE := Disabled;
      This.Periph.PSELTXD := 16#FFFF_FFFF#;
      This.Periph.PSELRXD := 16#FFFF_FFFF#;

      --  Stop TX and RX
      This.Periph.TASKS_STOPTX := 1;
      This.Periph.TASKS_STOPRX := 1;
   end Disable;

   -------------------------
   -- Enable_Flow_Control --
   -------------------------

   procedure Enable_Flow_Control
     (This     : in out UART_Device;
      RTS, CTS : GPIO_Pin_Index)
   is
   begin
      This.Periph.PSELRTS := UInt32 (RTS);
      This.Periph.PSELCTS := UInt32 (CTS);
      This.Periph.CONFIG.HWFC := Enabled;
   end Enable_Flow_Control;

   --------------------------
   -- Disable_Flow_Control --
   --------------------------

   procedure Disable_Flow_Control (This : in out UART_Device) is
   begin
      This.Periph.CONFIG.HWFC := Disabled;
      This.Periph.PSELRTS := 16#FFFF_FFFF#;
      This.Periph.PSELCTS := 16#FFFF_FFFF#;
   end Disable_Flow_Control;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out UART_Device;
      Data    :        UART_Data_8b;
      Status  :    out UART_Status;
      Timeout :        Natural := 1_000)
   is
      pragma Unreferenced (Timeout);
   begin
      if Data'Length = 0 then
         Status := HAL.UART.Ok;
         return;
      end if;

      for C of Data loop
         --  Wait for TX Ready event
         while UART0_Periph.EVENTS_TXDRDY = 0 loop
            null;
         end loop;

            --  Clear the event
         This.Periph.EVENTS_TXDRDY := 0;

         --  Send a character
         This.Periph.TXD.TXD := C;
      end loop;

      Status := HAL.UART.Ok;
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out UART_Device;
      Data    :    out UART_Data_8b;
      Status  :    out UART_Status;
      Timeout :        Natural := 1_000)
   is
      pragma Unreferenced (Timeout);
   begin
      if Data'Length = 0 then
         Status := HAL.UART.Ok;
         return;
      end if;

      for C of Data loop
         --  Wait for RX Ready event
         while UART0_Periph.EVENTS_RXDRDY = 0 loop
            null;
         end loop;

         --  Read a character
         C := This.Periph.RXD.RXD;

         --  Clear the RX event for the character we just received
         UART0_Periph.EVENTS_RXDRDY := 0;
      end loop;

      Status := HAL.UART.Ok;
   end Receive;

   --------------
   -- Transmit --
   --------------

   overriding
   procedure Transmit
     (This    : in out UART_Device;
      Data    :        UART_Data_9b;
      Status  : out    UART_Status;
      Timeout :        Natural := 1_000)
   is
   begin
      raise Program_Error;
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out UART_Device;
      Data    :    out UART_Data_9b;
      Status  :    out UART_Status;
      Timeout :        Natural := 1_000)
   is
   begin
      raise Program_Error;
   end Receive;

end nRF.UART;
