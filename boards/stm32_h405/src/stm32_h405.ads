------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

--  Olimex STM32-H405
--  Digi-Key part no. 1188-1167-ND
--  https://www.olimex.com/Products/ARM/ST/STM32-H405/resources/STM32-H405_sch.pdf

with STM32.GPIO;   use STM32.GPIO;
with STM32.Device;

package STM32_H405 is

   EXT1_1  : GPIO_Point renames STM32.Device.PA11;
   EXT1_2  : GPIO_Point renames STM32.Device.PA8;
   EXT1_3  : GPIO_Point renames STM32.Device.PA12;
   EXT1_4  : GPIO_Point renames STM32.Device.PA9;
   --  EXT1_5  : 3.3V
   --  EXT1_6  : GND
   EXT1_7  : GPIO_Point renames STM32.Device.PA10;
   EXT1_8  : GPIO_Point renames STM32.Device.PC10;
   EXT1_9  : GPIO_Point renames STM32.Device.PC11;
   EXT1_10 : GPIO_Point renames STM32.Device.PC12;
   EXT1_11 : GPIO_Point renames STM32.Device.PD2;
   EXT1_12 : GPIO_Point renames STM32.Device.PB5;
   EXT1_13 : GPIO_Point renames STM32.Device.PB6;
   EXT1_14 : GPIO_Point renames STM32.Device.PA6;
   EXT1_15 : GPIO_Point renames STM32.Device.PB7;
   EXT1_16 : GPIO_Point renames STM32.Device.PB8;
   EXT1_17 : GPIO_Point renames STM32.Device.PB9;
   EXT1_18 : GPIO_Point renames STM32.Device.PA5;
   EXT1_19 : GPIO_Point renames STM32.Device.PC0;
   EXT1_20 : GPIO_Point renames STM32.Device.PC1;
   EXT1_21 : GPIO_Point renames STM32.Device.PB0;
   EXT1_22 : GPIO_Point renames STM32.Device.PA7;
   --  EXT1_23 : VBAT
   EXT1_24 : GPIO_Point renames STM32.Device.PC13;
   --  EXT1_25 : NRST
   EXT1_26 : GPIO_Point renames STM32.Device.PB1;

   --  EXT2_1  : VDDA
   EXT2_2  : GPIO_Point renames STM32.Device.PC2;
   --  EXT2_3  : GNDA
   EXT2_4  : GPIO_Point renames STM32.Device.PA0;
   --  EXT2_5  : 3.3V
   --  EXT2_6  : GND
   EXT2_7  : GPIO_Point renames STM32.Device.PA2;
   EXT2_8  : GPIO_Point renames STM32.Device.PA1;
   EXT2_9  : GPIO_Point renames STM32.Device.PC3;
   EXT2_10 : GPIO_Point renames STM32.Device.PA3;
   EXT2_11 : GPIO_Point renames STM32.Device.PA4;
   EXT2_12 : GPIO_Point renames STM32.Device.PC4;
   EXT2_13 : GPIO_Point renames STM32.Device.PC5;
   EXT2_14 : GPIO_Point renames STM32.Device.PB10;
   EXT2_15 : GPIO_Point renames STM32.Device.PB11;
   EXT2_16 : GPIO_Point renames STM32.Device.PB13;
   EXT2_17 : GPIO_Point renames STM32.Device.PB12;
   EXT2_18 : GPIO_Point renames STM32.Device.PB14;
   EXT2_19 : GPIO_Point renames STM32.Device.PB15;
   EXT2_20 : GPIO_Point renames STM32.Device.PC6;
   EXT2_21 : GPIO_Point renames STM32.Device.PC7;
   EXT2_22 : GPIO_Point renames STM32.Device.PC8;
   --  EXT2_23 : VUSB
   EXT2_24 : GPIO_Point renames STM32.Device.PC9;
   --  EXT2_25 : GND
   --  EXT2_26 : VIN

   OSC32_IN  : GPIO_Point renames STM32.Device.PC14;
   OSC32_OUT : GPIO_Point renames STM32.Device.PC15;
   OSC8M_IN  : GPIO_Point renames STM32.Device.PD0;
   OSC8M_OUT : GPIO_Point renames STM32.Device.PD1;

   TMS  : GPIO_Point renames STM32.Device.PA13;
   TCK  : GPIO_Point renames STM32.Device.PA14;
   TDI  : GPIO_Point renames STM32.Device.PA15;
   TDO  : GPIO_Point renames STM32.Device.PB3;
   TRST : GPIO_Point renames STM32.Device.PB4;

   Button   : GPIO_Point renames STM32.Device.PA0;
   User_LED : GPIO_Point renames STM32.Device.PC12;

   USB_DM : GPIO_Point renames STM32.Device.PA11;
   USB_DP : GPIO_Point renames STM32.Device.PA12;

end STM32_H405;
