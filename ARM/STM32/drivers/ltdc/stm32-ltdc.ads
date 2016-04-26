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
--   @file    stm32f429i_discovery_lcd.h                                    --
--   @author  MCD Application Team                                          --
--   @version V1.1.0                                                        --
--   @date    19-June-2014                                                  --
--   @brief   This file contains all the functions prototypes for the       --
--            stm32f429i_discovery_lcd.c driver.                            --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

with System;

with STM32.Device; use STM32.Device;

package STM32.LTDC is

   type LCD_Layer is (Layer1, Layer2);

   --  These bits defines the color format
   type Pixel_Format is
     (Pixel_Fmt_ARGB8888,
      Pixel_Fmt_RGB888,
      Pixel_Fmt_RGB565,
      Pixel_Fmt_ARGB1555,
      Pixel_Fmt_ARGB4444,
      Pixel_Fmt_L8, --  8bit luminance
      Pixel_Fmt_AL44, --  4-bit Alpha, 4-bit Luniunance
      Pixel_Fmt_AL88) --  8-bit Alpha, 8-bit Luniunance
     with Size => 3;

   function Bytes_Per_Pixel (Fmt : Pixel_Format) return Natural
     with Inline;

   subtype Frame_Buffer_Access is System.Address;

   type Blending_Factor is
     (BF_Constant_Alpha,
      BF_Pixel_Alpha_X_Constant_Alpha);

   procedure Initialize (Width         : Positive;
                         Height        : Positive;
                         H_Sync        : Natural;
                         H_Back_Porch  : Natural;
                         H_Front_Porch : Natural;
                         V_Sync        : Natural;
                         V_Back_Porch  : Natural;
                         V_Front_Porch : Natural;
                         PLLSAI_N      : UInt9;
                         PLLSAI_R      : UInt3;
                         DivR          : Natural);

   function Initialized return Boolean;
   procedure Start;
   procedure Stop;

   procedure Set_Background (R, G, B : Byte);

   procedure Layer_Init
     (Layer          : LCD_Layer;
      Config         : Pixel_Format;
      Buffer         : System.Address;
      X, Y           : Natural;
      W, H           : Positive;
      Constant_Alpha : Byte := 255;
      BF             : Blending_Factor := BF_Pixel_Alpha_X_Constant_Alpha);

   procedure Set_Layer_State
     (Layer   : LCD_Layer;
      Enabled : Boolean);

   procedure Set_Frame_Buffer
     (Layer : LCD_Layer; Addr : Frame_Buffer_Access);

   function Get_Frame_Buffer
     (Layer : LCD_Layer)
      return Frame_Buffer_Access;

   procedure Reload_Config (Immediate : Boolean := False);

private

   function Bytes_Per_Pixel (Fmt : Pixel_Format) return Natural
   is (case Fmt is
          when Pixel_Fmt_ARGB8888 => 4,
          when Pixel_Fmt_RGB888 => 3,
          when Pixel_Fmt_RGB565 | Pixel_Fmt_ARGB1555 | Pixel_Fmt_ARGB4444 => 2,
          when Pixel_Fmt_L8 | Pixel_Fmt_AL44 => 1,
          when Pixel_Fmt_AL88 => 2);

end STM32.LTDC;
