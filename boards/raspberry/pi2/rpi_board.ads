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

with HAL;

with RPi.Framebuffer; use RPi.Framebuffer;
with RPi.DMA;         use RPi.DMA;
with RPi.SDMMC;       use RPi.SDMMC;
with RPi.Regs.DMA;

package Rpi_Board is

   DMA_0   : aliased DMA_Controller (RPi.Regs.DMA.DMA_0'Access);
   DMA_1   : aliased DMA_Controller (RPi.Regs.DMA.DMA_1'Access);
   DMA_2   : aliased DMA_Controller (RPi.Regs.DMA.DMA_2'Access);
   DMA_3   : aliased DMA_Controller (RPi.Regs.DMA.DMA_3'Access);
   DMA_4   : aliased DMA_Controller (RPi.Regs.DMA.DMA_4'Access);
   DMA_5   : aliased DMA_Controller (RPi.Regs.DMA.DMA_5'Access);
   DMA_6   : aliased DMA_Controller (RPi.Regs.DMA.DMA_6'Access);
   DMA_7   : aliased DMA_Controller (RPi.Regs.DMA.DMA_7'Access);
   DMA_8   : aliased DMA_Controller (RPi.Regs.DMA.DMA_8'Access);
   DMA_9   : aliased DMA_Controller (RPi.Regs.DMA.DMA_9'Access);
   DMA_10  : aliased DMA_Controller (RPi.Regs.DMA.DMA_10'Access);
   DMA_11  : aliased DMA_Controller (RPi.Regs.DMA.DMA_11'Access);
   DMA_12  : aliased DMA_Controller (RPi.Regs.DMA.DMA_12'Access);
   DMA_13  : aliased DMA_Controller (RPi.Regs.DMA.DMA_13'Access);
   DMA_14  : aliased DMA_Controller (RPi.Regs.DMA.DMA_14'Access);

   Display : Framebuffer_Display;

   Display_Width  : constant := 800;
   Display_Height : constant := 480;

   function Sqrt (X : Float) return Float
     with Import, Convention => Intrinsic, External_Name => "__builtin_sqrtf";

   --  The EMMC driver
   EMMC_Driver : aliased SDCard_Driver;

   function Get_EMMC_Clock return HAL.UInt32;

end Rpi_Board;
