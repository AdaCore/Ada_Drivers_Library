------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with nRF51.Device;
with nRF51.GPIO;

package MicroBit is

   --  http://tech.microbit.org/hardware/edgeconnector_ds/

   MB_P0   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P03;  --  0 pad on edge connector
   MB_P1   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P02;  --  1 pad on edge connector
   MB_P2   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P01;  --  2 pad on edge connector
   MB_P3   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P04;  --  Display column 1
   MB_P4   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P05;  --  Display column 2
   MB_P5   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P17;  --  Button A
   MB_P6   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P12;  --  Display column 9
   MB_P7   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P11;  --  Display column 8
   MB_P8   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P18;
   MB_P9   : nRF51.GPIO.GPIO_Point renames nRF51.Device.P10;  --  Display column 7
   MB_P10  : nRF51.GPIO.GPIO_Point renames nRF51.Device.P06;  --  Display column 3
   MB_P11  : nRF51.GPIO.GPIO_Point renames nRF51.Device.P26;  --  Button B
   MB_P12  : nRF51.GPIO.GPIO_Point renames nRF51.Device.P20;
   MB_P13  : nRF51.GPIO.GPIO_Point renames nRF51.Device.P23;  --  SCK
   MB_P14  : nRF51.GPIO.GPIO_Point renames nRF51.Device.P22;  --  MISO
   MB_P15  : nRF51.GPIO.GPIO_Point renames nRF51.Device.P21;  --  MOSI
   MB_P16  : nRF51.GPIO.GPIO_Point renames nRF51.Device.P16;
   MB_P19  : nRF51.GPIO.GPIO_Point renames nRF51.Device.P00;  --  SCL
   MB_P20  : nRF51.GPIO.GPIO_Point renames nRF51.Device.P30;  --  SDA

   MB_SCK  : nRF51.GPIO.GPIO_Point renames MB_P13;
   MB_MISO : nRF51.GPIO.GPIO_Point renames MB_P14;
   MB_MOSI : nRF51.GPIO.GPIO_Point renames MB_P15;

   MB_SCL  : nRF51.GPIO.GPIO_Point renames MB_P19;
   MB_SDA  : nRF51.GPIO.GPIO_Point renames MB_P20;


end MicroBit;
