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
------------------------------------------------------------------------------
pragma Restrictions (No_Elaboration_Code);

package STM32.RCC is

--     type RCC_System_Clocks is record
--        SYSCLK  : Word;
--        HCLK    : Word;
--        PCLK1   : Word;
--        PCLK2   : Word;
--        TIMCLK1 : Word;
--        TIMCLK2 : Word;
--     end record;
--
--     function System_Clock_Frequencies return RCC_System_Clocks;

   --  Part below is obsolete and should be moved to the corresponding driver.

   procedure CRC_Clock_Enable with Inline;
   procedure BKPSRAM_Clock_Enable with Inline;
--     procedure CCMDATARAMEN_Clock_Enable with Inline;
--     procedure DMA2D_Clock_Enable with Inline;
   procedure WWDG_Clock_Enable with Inline;

--     procedure SDIO_Clock_Enable with Inline;
   procedure SYSCFG_Clock_Enable with Inline;

   procedure AHB1_Force_Reset with Inline;
   procedure AHB1_Release_Reset with Inline;
   procedure AHB2_Force_Reset with Inline;
   procedure AHB2_Release_Reset with Inline;
   procedure APB1_Force_Reset with Inline;
   procedure APB1_Release_Reset with Inline;
   procedure APB2_Force_Reset with Inline;
   procedure APB2_Release_Reset with Inline;

   procedure CRC_Force_Reset with Inline;
   procedure CRC_Release_Reset with Inline;

--     procedure DMA2D_Force_Reset with Inline;
--     procedure DMA2D_Release_Reset with Inline;

   procedure OTGFS_Force_Reset with Inline;
   procedure OTGFS_Release_Reset with Inline;

   procedure WWDG_Force_Reset with Inline;
   procedure WWDG_Release_Reset with Inline;

--     procedure SDIO_Force_Reset with Inline;
--     procedure SDIO_Release_Reset with Inline;

   procedure SYSCFG_Force_Reset with Inline;
   procedure SYSCFG_Release_Reset with Inline;

end STM32.RCC;
