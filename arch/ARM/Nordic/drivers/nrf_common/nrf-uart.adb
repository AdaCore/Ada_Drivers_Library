------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
                     Tx, Rx : GPIO_Pin_Index) is separate;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out UART_Device) is separate;

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
   is separate;

   -------------
   -- Receive --
   -------------

   overriding
   procedure Receive
     (This    : in out UART_Device;
      Data    :    out UART_Data_8b;
      Status  :    out UART_Status;
      Timeout :        Natural := 1_000)
   is separate;

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
