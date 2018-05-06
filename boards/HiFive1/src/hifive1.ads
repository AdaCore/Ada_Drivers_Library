------------------------------------------------------------------------------
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with FE310.Device; use FE310.Device;
with FE310.GPIO;   use FE310.GPIO;

package HiFive1 is

   HF1_Pin_0  : GPIO_Point renames P16; -- IOF 0: UART 0 RX
   HF1_Pin_1  : GPIO_Point renames P17; -- IOF 0: UART 0 TX
   HF1_Pin_2  : GPIO_Point renames P18;
   HF1_Pin_3  : GPIO_Point renames P19; --                    | IOF 1 : PWM 1
   HF1_Pin_4  : GPIO_Point renames P20; --                    | IOF 1 : PWM 1
   HF1_Pin_5  : GPIO_Point renames P21; --                    | IOF 1 : PWM 1
   HF1_Pin_6  : GPIO_Point renames P22; --                    | IOF 1 : PWM 1
   HF1_Pin_7  : GPIO_Point renames P23;
   HF1_Pin_8  : GPIO_Point renames P00; --                    | IOF 1 : PWM 0
   HF1_Pin_9  : GPIO_Point renames P01; --                    | IOF 1 : PWM 0
   HF1_Pin_10 : GPIO_Point renames P02; --                    | IOF 1 : PWM 0
   HF1_Pin_11 : GPIO_Point renames P03; -- IOF 0: SPI 1 MOSI  | IOF 1 : PWM 0
   HF1_Pin_12 : GPIO_Point renames P04; -- IOF 0: SPI 1 MISO
   HF1_Pin_13 : GPIO_Point renames P05; -- IOF 0: SPI 1 SCK
   --  HF1_Pin_14 is not connected
   HF1_Pin_15 : GPIO_Point renames P09;
   HF1_Pin_16 : GPIO_Point renames P10; --                    | IOF 1 : PWM 2
   HF1_Pin_17 : GPIO_Point renames P11; --                    | IOF 1 : PWM 2
   HF1_Pin_18 : GPIO_Point renames P12; --                    | IOF 1 : PWM 2
   HF1_Pin_19 : GPIO_Point renames P13; --                    | IOF 1 : PWM 2

end HiFive1;
