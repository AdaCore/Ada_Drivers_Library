------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

package SAM.DMAC.Sources is

   DISABLE        : constant Trigger_Source := 16#00#;
   RTC_TIMESTAMP  : constant Trigger_Source := 16#01#;
   DSU_DCC0       : constant Trigger_Source := 16#02#;
   DSU_DCC1       : constant Trigger_Source := 16#03#;
   SERCOM0_RX     : constant Trigger_Source := 16#04#;
   SERCOM0_TX     : constant Trigger_Source := 16#05#;
   SERCOM1_RX     : constant Trigger_Source := 16#06#;
   SERCOM1_TX     : constant Trigger_Source := 16#07#;
   SERCOM2_RX     : constant Trigger_Source := 16#08#;
   SERCOM2_TX     : constant Trigger_Source := 16#09#;
   SERCOM3_RX     : constant Trigger_Source := 16#0A#;
   SERCOM3_TX     : constant Trigger_Source := 16#0B#;
   SERCOM4_RX     : constant Trigger_Source := 16#0C#;
   SERCOM4_TX     : constant Trigger_Source := 16#0D#;
   SERCOM5_RX     : constant Trigger_Source := 16#0E#;
   SERCOM5_TX     : constant Trigger_Source := 16#0F#;
   SERCOM6_RX     : constant Trigger_Source := 16#10#;
   SERCOM6_TX     : constant Trigger_Source := 16#11#;
   SERCOM7_RX     : constant Trigger_Source := 16#12#;
   SERCOM7_TX     : constant Trigger_Source := 16#13#;
   CAN0_DEBUG     : constant Trigger_Source := 16#14#;
   CAN1_DEBUG     : constant Trigger_Source := 16#15#;
   TCC0_OVF       : constant Trigger_Source := 16#16#;
   TCC0_MC0       : constant Trigger_Source := 16#17#;
   TCC0_MC1       : constant Trigger_Source := 16#18#;
   TCC0_MC2       : constant Trigger_Source := 16#19#;
   TCC0_MC3       : constant Trigger_Source := 16#1A#;
   TCC0_MC4       : constant Trigger_Source := 16#1B#;
   TCC0_MC5       : constant Trigger_Source := 16#1C#;
   TCC1_OVF       : constant Trigger_Source := 16#1D#;
   TCC1_MC0       : constant Trigger_Source := 16#1E#;
   TCC1_MC1       : constant Trigger_Source := 16#1F#;
   TCC1_MC2       : constant Trigger_Source := 16#20#;
   TCC1_MC3       : constant Trigger_Source := 16#21#;
   TCC2_OVF       : constant Trigger_Source := 16#22#;
   TCC2_MC0       : constant Trigger_Source := 16#23#;
   TCC2_MC1       : constant Trigger_Source := 16#24#;
   TCC2_MC2       : constant Trigger_Source := 16#25#;
   TCC3_OVF       : constant Trigger_Source := 16#26#;
   TCC3_MC0       : constant Trigger_Source := 16#27#;
   TCC3_MC1       : constant Trigger_Source := 16#28#;
   TCC4_OVF       : constant Trigger_Source := 16#29#;
   TCC4_MC0       : constant Trigger_Source := 16#2A#;
   TCC4_MC1       : constant Trigger_Source := 16#2B#;
   TC0_OVF        : constant Trigger_Source := 16#2C#;
   TC0_MC0        : constant Trigger_Source := 16#2D#;
   TC0_MC1        : constant Trigger_Source := 16#2E#;
   TC1_OVF        : constant Trigger_Source := 16#2F#;
   TC1_MC0        : constant Trigger_Source := 16#30#;
   TC1_MC1        : constant Trigger_Source := 16#31#;
   TC2_OVF        : constant Trigger_Source := 16#32#;
   TC2_MC0        : constant Trigger_Source := 16#33#;
   TC2_MC1        : constant Trigger_Source := 16#34#;
   TC3_OVF        : constant Trigger_Source := 16#35#;
   TC3_MC0        : constant Trigger_Source := 16#36#;
   TC3_MC1        : constant Trigger_Source := 16#37#;
   TC4_OVF        : constant Trigger_Source := 16#38#;
   TC4_MC0        : constant Trigger_Source := 16#39#;
   TC4_MC1        : constant Trigger_Source := 16#3A#;
   TC5_OVF        : constant Trigger_Source := 16#3B#;
   TC5_MC0        : constant Trigger_Source := 16#3C#;
   TC5_MC1        : constant Trigger_Source := 16#3D#;
   TC6_OVF        : constant Trigger_Source := 16#3E#;
   TC6_MC0        : constant Trigger_Source := 16#3F#;
   TC6_MC1        : constant Trigger_Source := 16#40#;
   TC7_OVF        : constant Trigger_Source := 16#41#;
   TC7_MC0        : constant Trigger_Source := 16#42#;
   TC7_MC1        : constant Trigger_Source := 16#43#;
   ADC0_RESRDY    : constant Trigger_Source := 16#44#;
   ADC0_SEQ       : constant Trigger_Source := 16#45#;
   ADC1_RESRDY    : constant Trigger_Source := 16#46#;
   ADC1_SEQ       : constant Trigger_Source := 16#47#;
   DAC_EMPTY_0    : constant Trigger_Source := 16#48#;
   DAC_EMPTY_1    : constant Trigger_Source := 16#49#;
   DAC_RESRDY_0   : constant Trigger_Source := 16#4A#;
   DAC_RESRDY_1   : constant Trigger_Source := 16#4B#;
   I2S_RX_Ready_0 : constant Trigger_Source := 16#4C#;
   I2S_RX_Ready_1 : constant Trigger_Source := 16#4D#;
   I2S_TX_Ready_0 : constant Trigger_Source := 16#4E#;
   I2S_TX_Ready_1 : constant Trigger_Source := 16#4F#;
   PCC_RX         : constant Trigger_Source := 16#50#;
   AES_WR         : constant Trigger_Source := 16#51#;
   AES_RD         : constant Trigger_Source := 16#52#;
   QSPI_RX        : constant Trigger_Source := 16#53#;
   QSPI_TX        : constant Trigger_Source := 16#54#;

end SAM.DMAC.Sources;
