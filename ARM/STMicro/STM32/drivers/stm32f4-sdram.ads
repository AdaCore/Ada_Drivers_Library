------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_sdram.h                                         --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   Header file of DMA HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32F4.FMC; use STM32F4.FMC;

package STM32F4.SDRAM is

   Bank_Address : constant := 16#D000_0000#;

   procedure Initialize;

private

   SDRAM_MEMORY_WIDTH : constant := FMC_SDMemory_Width_16b;
   SDRAM_CAS_LATENCY  : constant := FMC_CAS_Latency_3;
   SDCLOCK_PERIOD     : constant := FMC_SDClock_Period_2;
   SDRAM_READBURST    : constant := FMC_Read_Burst_Disable;

   SDRAM_MODEREG_BURST_LENGTH_1             : constant := 16#0000#;
   SDRAM_MODEREG_BURST_LENGTH_2             : constant := 16#0001#;
   SDRAM_MODEREG_BURST_LENGTH_4             : constant := 16#0002#;
   SDRAM_MODEREG_BURST_LENGTH_8             : constant := 16#0004#;
   SDRAM_MODEREG_BURST_TYPE_SEQUENTIAL      : constant := 16#0000#;
   SDRAM_MODEREG_BURST_TYPE_INTERLEAVED     : constant := 16#0008#;
   SDRAM_MODEREG_CAS_LATENCY_2              : constant := 16#0020#;
   SDRAM_MODEREG_CAS_LATENCY_3              : constant := 16#0030#;
   SDRAM_MODEREG_OPERATING_MODE_STANDARD    : constant := 16#0000#;
   SDRAM_MODEREG_WRITEBURST_MODE_PROGRAMMED : constant := 16#0000#;
   SDRAM_MODEREG_WRITEBURST_MODE_SINGLE     : constant := 16#0200#;

end STM32F4.SDRAM;
