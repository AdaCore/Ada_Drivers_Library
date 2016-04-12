------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2016, AdaCore                           --
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
--   @file    stm32f469i_discovery_lcd.c                                          --
--   @author  MCD Application Team                                          --
--   @version V1.0.2                                                        --
--   @date    13-January-2016                                              --
--                                                                          --
--   COPYRIGHT(c) 2016 STMicroelectronics                                   --
------------------------------------------------------------------------------

with STM32.DSI;

package STM32.LCDInit is

   type LCD_Orientation is
     (Portrait,
      Landscape);
   Default_Orientation : constant LCD_Orientation := Landscape;

   LCD_WIDTH  : constant := 800;
   LCD_HEIGHT : constant := 480;
   VSYNC      : constant := 12;
   VBP        : constant := 12;
   VFP        : constant := 12;
   HSYNC      : constant := 120;
   HBP        : constant := 120;
   HFP        : constant := 120;

   PLLSAIN     : constant := 384;
   PLLSAIR     : constant := 5;
   PLLSAI_DIVR : constant := 4;

   DSI_LCD_Color_Mode : constant STM32.DSI.DSI_Color_Mode := STM32.DSI.RGB888;

   procedure Pre_LTDC_Initialize;

   procedure Post_LTDC_Initialize (Orientation : LCD_Orientation);

   procedure Default_Postinit;

end STM32.LCDInit;
