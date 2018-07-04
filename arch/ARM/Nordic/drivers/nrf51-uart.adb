------------------------------------------------------------------------------
--                                                                          --
--            Copyright (C) 2018, AdaCore and other contributors            --
--                                                                          --
--      See github.com/AdaCore/Ada_Drivers_Library/graphs/contributors      --
--                           for more information                           --
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
------------------------------------------------------------------------------`

with NRF51_SVD.UART;  use NRF51_SVD.UART;

package body nRF51.UART is
   
   ------------
   -- Enable --
   ------------

   procedure Enable (This : in out UART) is
   begin
      This.Periph.ENABLE.ENABLE := Enabled;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (This : in out UART) is
   begin
      This.Periph.ENABLE.ENABLE := Disabled;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (This : UART) return Boolean is
   begin
      return This.Periph.ENABLE.ENABLE = Enabled;
   end Enabled;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (This   : in out UART;
                        RX_Pin : GPIO_Pin_Index;
                        TX_Pin : GPIO_Pin_Index;
                        Speed  : UART_Speed) is
   begin
      This.Periph.PSELRXD := UInt32 (RX_Pin);
      This.Periph.PSELTXD := UInt32 (TX_Pin);
      This.Periph.BAUDRATE := (case Speed is
                                   when UART_1200_Baud => 16#0004_F000#,
                                   when UART_2400_Baud => 16#0009_D000#,
                                   when UART_4800_Baud => 16#0013_B000#,
                                   when UART_9600_Baud => 16#0027_5000#,
                                   when UART_14400_Baud => 16#003B_0000#,
                                   when UART_19200_Baud => 16#004E_A000#,
                                   when UART_28800_Baud => 16#0075_F000#,
                                   when UART_38400_Baud => 16#009D_5000#,
                                   when UART_57600_Baud => 16#00EB_F000#,
                                   when UART_76800_Baud => 16#013A_9000#,
                                   when UART_115200_Baud => 16#01D7_E000#,
                                   when UART_230400_Baud => 16#03AF_B000#,
                                   when UART_250000_Baud => 16#0400_0000#,
                                   when UART_460800_Baud => 16#075F_7000#,
                                   when UART_921600_Baud => 16#0EBE_DFA4#,
                                   when UART_1M_Baud => 16#1000_0000#);
   end Configure;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (This : in out UART) is
   begin
      This.Periph.PSELRXD := 16#FFFF_FFFF#;
      This.Periph.PSELTXD := 16#FFFF_FFFF#;
   end Disconnect;

   ---------------
   -- Data_Size --
   ---------------

   overriding function Data_Size (Port : UART) return UART_Data_Size is
      pragma Unreferenced (Port);
   begin
      return Data_Size_8b;
   end Data_Size;

   --------------
   -- Transmit --
   --------------

   overriding procedure Transmit
     (This    : in out UART;
      Data    : UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
      Index : Integer := Data'First + 1;
      Err_Src : ERRORSRC_Register with Unreferenced;
   begin

      if Data'Length = 0 then
         Status := Ok;
         return;
      end if;
      
      --  Clear errors
      This.Periph.ERRORSRC.OVERRUN := Errorsrc_Overrun_Field_Reset;
      This.Periph.ERRORSRC.PARITY  := Errorsrc_Parity_Field_Reset;
      This.Periph.ERRORSRC.FRAMING := Errorsrc_Framing_Field_Reset;
      This.Periph.ERRORSRC.BREAK   := Errorsrc_Break_Field_Reset;

      --  Prepare first byte
      This.Periph.TXD.TXD := Data (Data'First);
      
      --  Start TX sequence
      This.Periph.TASKS_STARTTX := 1;

      loop
         
         loop

            if This.Periph.EVENTS_ERROR /= 0 then
               Err_Src := This.Periph.ERRORSRC;
               Status := Err_Error;
               --  Clear the error
               This.Periph.EVENTS_ERROR := 0;

               --  Stop sequence
               This.Periph.TASKS_STOPTX := 1;

               return;
            end if;

            exit when This.Periph.EVENTS_TXDRDY /= 0;
         end loop;

         --  Clear the event
         This.Periph.EVENTS_TXDRDY := 0;

         exit when Index > Data'Last;

         This.Periph.TXD.TXD := Data (Index);
         Index := Index + 1;
      end loop;
      
      Status := Ok;
   end Transmit;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (This    : in out UART;
      Data    : out UART_Data_8b;
      Status  : out UART_Status;
      Timeout : Natural := 1000)
   is
      pragma Unreferenced (Timeout);
      Err_Src : ERRORSRC_Register with Unreferenced;
   begin
      
      if Data'Length = 0 then
         Status := Ok;
         return;
      end if;

      --  Clear errors
      This.Periph.ERRORSRC.OVERRUN := Errorsrc_Overrun_Field_Reset;
      This.Periph.ERRORSRC.PARITY  := Errorsrc_Parity_Field_Reset;
      This.Periph.ERRORSRC.FRAMING := Errorsrc_Framing_Field_Reset;
      This.Periph.ERRORSRC.BREAK   := Errorsrc_Break_Field_Reset;

      --  Start RX sequence
      This.Periph.TASKS_STARTRX := 1;

      for Index in Data'Range loop

         loop

            if This.Periph.EVENTS_ERROR /= 0 then
               Err_Src := This.Periph.ERRORSRC;
               Status := Err_Error;
               --  Clear the error
               This.Periph.EVENTS_ERROR := 0;

               return;
            end if;

            exit when This.Periph.EVENTS_RXDRDY /= 0;
         end loop;

         --  Clear the event
         This.Periph.EVENTS_RXDRDY := 0;

         Data (Index) := This.Periph.RXD.RXD;
      end loop;

      Status := Ok;
   end Receive;
   
   --------------
   -- Transmit --
   --------------

   overriding procedure Transmit
     (This    : in out UART;
      Data    : UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000) is
   begin
      Status := Err_Error;
   end Transmit;
   
   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (This    : in out UART;
      Data    : out UART_Data_9b;
      Status  : out UART_Status;
      Timeout : Natural := 1000) is
   begin
      Status := Err_Error;
   end Receive;
   
end nRF51.UART;
