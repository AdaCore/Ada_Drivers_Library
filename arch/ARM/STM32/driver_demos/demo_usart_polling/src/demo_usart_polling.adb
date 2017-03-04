------------------------------------------------------------------------------
--                                                                          --
--                 Copyright (C) 2015-2016, AdaCore                         --
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

--  The file declares the main procedure for the demonstration.

with HAL;           use HAL;
with STM32.GPIO;    use STM32.GPIO;
with STM32.USARTs;  use STM32.USARTs;

with STM32.Device;  use STM32.Device;

procedure Demo_USART_Polling is

   TX_Pin : constant GPIO_Point := PB7;
   RX_Pin : constant GPIO_Point := PB6;

   procedure Initialize_UART_GPIO;

   procedure Initialize;

   procedure Await_Send_Ready (This : USART) with Inline;

   procedure Put_Blocking (This : in out USART;  Data : UInt16);

   --------------------------
   -- Initialize_UART_GPIO --
   --------------------------

   procedure Initialize_UART_GPIO is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (USART_1);
      Enable_Clock (RX_Pin & TX_Pin);

      Configuration.Mode := Mode_AF;
      Configuration.Speed := Speed_50MHz;
      Configuration.Output_Type := Push_Pull;
      Configuration.Resistors := Pull_Up;

      Configure_IO (RX_Pin & TX_Pin, Configuration);

      Configure_Alternate_Function (RX_Pin & TX_Pin, AF => GPIO_AF_USART1_7);
   end Initialize_UART_GPIO;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize_UART_GPIO;

      Disable (USART_1);

      Set_Baud_Rate    (USART_1, 115_200);
      Set_Mode         (USART_1, Tx_Rx_Mode);
      Set_Stop_Bits    (USART_1, Stopbits_1);
      Set_Word_Length  (USART_1, Word_Length_8);
      Set_Parity       (USART_1, No_Parity);
      Set_Flow_Control (USART_1, No_Flow_Control);

      Enable (USART_1);
   end Initialize;

   ----------------------
   -- Await_Send_Ready --
   ----------------------

   procedure Await_Send_Ready (This : USART) is
   begin
      loop
         exit when Tx_Ready (This);
      end loop;
   end Await_Send_Ready;

   ------------------
   -- Put_Blocking --
   ------------------

   procedure Put_Blocking (This : in out USART;  Data : UInt16) is
   begin
      Await_Send_Ready (This);
      Transmit (This, UInt9 (Data));
   end Put_Blocking;

begin
   Initialize;
   loop
      for Next_Char in Character range 'a' .. 'z' loop  -- arbitrary
         Put_Blocking (USART_1, Character'Pos (Next_Char));
      end loop;
   end loop;
end Demo_USART_Polling;


