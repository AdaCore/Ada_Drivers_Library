------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016-2020, AdaCore                      --
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

with nRF.Device;
with nRF.GPIO;

package NRF52_DK is

   --  DK Headers are "Arduino compatible" these names reflect the
   --  Arduino locations of these peripherals

   DK_SCK  : nRF.GPIO.GPIO_Point renames nRF.Device.P25;
   DK_MISO : nRF.GPIO.GPIO_Point renames nRF.Device.P24;
   DK_MOSI : nRF.GPIO.GPIO_Point renames nRF.Device.P23;

   DK_SCL  : nRF.GPIO.GPIO_Point renames nRF.Device.P31;
   DK_SDA  : nRF.GPIO.GPIO_Point renames nRF.Device.P30;

   DK_UART_TX : nRF.GPIO.GPIO_Point renames nRF.Device.P12;
   DK_UART_RX : nRF.GPIO.GPIO_Point renames nRF.Device.P11;

end NRF52_DK;
