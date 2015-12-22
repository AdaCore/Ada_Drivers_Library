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

generic
   LCD_Width  : Positive;
   LCD_Height : Positive;

   LCD_HSync  : Natural;
   LCD_HBP    : Natural;
   LCD_HFP    : Natural;
   LCD_VSync  : Natural;
   LCD_VBP    : Natural;
   LCD_VFP    : Natural;

   PLLSAI_R   : UInt3;
   DivR       : Natural;

   with procedure Initialize_LCD;

package STM32.LTDC is

   type LCD_Layer is (Layer1, Layer2);

   --  These bits defines the color format
   type Pixel_Format is
     (Pixel_Fmt_ARGB8888,
      Pixel_Fmt_RGB888,
      Pixel_Fmt_RGB565,
      Pixel_Fmt_ARGB1555,
      Pixel_Fmt_ARGB4444)
     with Size => 3;

   function Get_Pixel_Fmt  return Pixel_Format;
   function Bytes_Per_Pixel (Fmt : Pixel_Format) return Natural
     with Inline;

   subtype Frame_Buffer_Access is System.Address;

   type Layer_State is (Enabled, Disabled);

   procedure Initialize (Pixel_Fmt : Pixel_Format := Pixel_Fmt_ARGB1555);

   procedure Set_Background (R, G, B : Byte);

   procedure Set_Layer_State
     (Layer : LCD_Layer;
      State : Layer_State);

   function Current_Frame_Buffer
     (Layer : LCD_Layer)
      return Frame_Buffer_Access;

   type Orientation_Mode is
     (Portrait,
      Landscape);

   procedure Set_Orientation (Orientation : Orientation_Mode);
   --  The hardware does not support such translation natively.
   --  Setting an orientation that is different from the standard one
   --  will affect the behavior of Set_Pixel and the various subprograms of
   --  DMA2D.

   function Get_Orientation return Orientation_Mode;
   function SwapXY return Boolean;
   function Pixel_Width return Natural;
   function Pixel_Height return Natural;

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural;
      Value : Word)
     with Pre => Get_Pixel_Fmt = Pixel_Fmt_ARGB8888;

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural;
      Value : UInt24)
     with Pre => Get_Pixel_Fmt = Pixel_Fmt_RGB888;

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural;
      Value : Half_Word)
     with Pre =>
       Get_Pixel_Fmt /= Pixel_Fmt_ARGB8888 and then
       Get_Pixel_Fmt /= Pixel_Fmt_RGB888;

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural)
      return Word
     with Pre => Get_Pixel_Fmt = Pixel_Fmt_ARGB8888;

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural)
      return UInt24
     with Pre => Get_Pixel_Fmt = Pixel_Fmt_RGB888;

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Natural;
      Y     : Natural)
      return Half_Word
     with Pre =>
       Get_Pixel_Fmt /= Pixel_Fmt_ARGB8888 and then
       Get_Pixel_Fmt /= Pixel_Fmt_RGB888;

   procedure Reload_Config (Immediate : Boolean := True);

private

   function Bytes_Per_Pixel (Fmt : Pixel_Format) return Natural
   is (case Fmt is
          when Pixel_Fmt_ARGB8888 => 4,
          when Pixel_Fmt_RGB888 => 3,
          when others => 2);

end STM32.LTDC;
