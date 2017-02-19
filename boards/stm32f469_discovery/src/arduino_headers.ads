------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2017, AdaCore                           --
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

with STM32.Generic_UNO_Header;
with STM32.Device; use STM32.Device;

package Arduino_Headers is new STM32.Generic_UNO_Header
  (D0_Pt  => PG9'Access,
   D1_Pt  => PG14'Access,
   D2_Pt  => PG13'Access,
   D3_Pt  => PA1'Access,
   D4_Pt  => PG12'Access,
   D5_Pt  => PA2'Access,
   D6_Pt  => PA6'Access,
   D7_Pt  => PG11'Access,
   D8_Pt  => PG10'Access,
   D9_Pt  => PA7'Access,
   D10_Pt => PH6'Access,
   D11_Pt => PB15'Access,
   D12_Pt => PB14'Access,
   D13_Pt => PD3'Access,
   D14_Pt => PB9'Access,
   D15_Pt => PB8'Access,
   A0_Pt  => PB1'Access,
   A1_Pt  => PC2'Access,
   A2_Pt  => PC3'Access,
   A3_Pt  => PC4'Access,
   A4_Pt  => PC5'Access,
   A5_Pt  => PA4'Access,

   SCL_AF => GPIO_AF_4_I2C1,
   SDA_AF => GPIO_AF_4_I2C1,

   I2C_Port => I2C_1'Access,

   UART_TX_AF => GPIO_AF_8_USART6,
   UART_RX_AF => GPIO_AF_8_USART6,

   UART_Port => USART_6'Access);
