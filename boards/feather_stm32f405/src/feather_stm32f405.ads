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

with STM32.GPIO;   use STM32.GPIO;
with STM32.Device;

package Feather_STM32F405 is

   A0    : GPIO_Point renames STM32.Device.PA4;
   A1    : GPIO_Point renames STM32.Device.PA5;
   A2    : GPIO_Point renames STM32.Device.PA6;
   A3    : GPIO_Point renames STM32.Device.PA7;
   A4    : GPIO_Point renames STM32.Device.PC4;
   A5    : GPIO_Point renames STM32.Device.PC5;
   D5    : GPIO_Point renames STM32.Device.PC7;
   D6    : GPIO_Point renames STM32.Device.PC6;
   D9    : GPIO_Point renames STM32.Device.PB8;
   D10   : GPIO_Point renames STM32.Device.PB9;
   D11   : GPIO_Point renames STM32.Device.PC3;
   D12   : GPIO_Point renames STM32.Device.PC2;
   D13   : GPIO_Point renames STM32.Device.PC1;
   SCL   : GPIO_Point renames STM32.Device.PB6;
   SDA   : GPIO_Point renames STM32.Device.PB7;
   SCK   : GPIO_Point renames STM32.Device.PB13;
   MISO  : GPIO_Point renames STM32.Device.PB14;
   MOSI  : GPIO_Point renames STM32.Device.PB15;
   RX_D0 : GPIO_Point renames STM32.Device.PB11;
   TX_D1 : GPIO_Point renames STM32.Device.PB10;

end Feather_STM32F405;
