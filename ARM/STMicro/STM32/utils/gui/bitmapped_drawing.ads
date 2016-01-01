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

--  This package provides routines for drawing shapes, characters, and strings
--  on a bit-mapped device. Any bit-mapped device will suffice that supplies a
--  routine to set a pixel, as shown by the formal subprogram.
--
--  For example, you can use this package in combination with the low-level LCD
--  component driver, the STM32F4.STM32F4.ILI9341, as shown in the following:
--
--     package LCD_Drawing is new Bitmapped_Drawing
--       (Color     => STM32F4.ILI9341.Colors,
--        Set_Pixel => STM32F4.ILI9341.Set_Pixel);
--
--  Such an instantiation would then allow you to write on the LCD:
--
--     use LCD_Drawing;
--
--  begin
--     Draw_String
--       (Start      => (0, 0),
--        Msg        => Chars,
--        Font       => Font16x24,
--        Foreground => White,
--        Background => Black);
--
--     Draw_Circle
--       (Center => (40, 220),
--        Radius => 20,
--        Hue    => Blue,
--        Fill   => True);
--
--  There is no control over colors or screen orientation. If that is required,
--  see the LCD_Std_Out package.

with BMP_Fonts;   use BMP_Fonts;
with STM32.DMA2D; use STM32.DMA2D;

package Bitmapped_Drawing is

   subtype Colors is DMA2D_Color;

   Black       : constant Colors := (255, 0, 0, 0);
   Blue        : constant Colors := (255, 0, 0, 255);
   Light_Blue  : constant Colors := (255, 173, 216, 230);
   Brown       : constant Colors := (255, 165, 42, 42);
   Cyan        : constant Colors := (255, 0, 255, 255);
   Gray        : constant Colors := (255, 190, 190, 190);
   Light_Gray  : constant Colors := (255, 211, 211, 211);
   Green       : constant Colors := (255, 0, 255, 0);
   Light_Green : constant Colors := (255, 144, 238, 144);
   Magenta     : constant Colors := (255, 255, 0, 255);
   Red         : constant Colors := (255, 255, 0, 0);
   Orange      : constant Colors := (255, 255, 69, 0);
   Violet      : constant Colors := (255, 238, 130, 238);
   Yellow      : constant Colors := (255, 255, 255, 0);
   White       : constant Colors := (255, 255, 255, 255);

   type Display_Point is record
      X : Natural;
      Y : Natural;
   end record;

   function Screen_Buffer return DMA2D_Buffer;
   --  Returns the LCD Frame buffer for Layer 1

   procedure Draw_Line
     (Buffer      : DMA2D_Buffer;
      Start, Stop : Display_Point;
      Hue         : DMA2D_Color;
      Thickness   : Natural := 1);

   procedure Draw_Rectangle
     (Buffer      : DMA2D_Buffer;
      Start, Stop : Display_Point;
      Hue         : DMA2D_Color;
      Thickness   : Natural := 1);

   procedure Fill_Rectangle
     (Buffer      : DMA2D_Buffer;
      Start, Stop : Display_Point;
      Hue         : DMA2D_Color);

   procedure Cubic_Bezier
     (Buffer         : DMA2D_Buffer;
      P1, P2, P3, P4 : Display_Point;
      Hue            : DMA2D_Color;
      N              : Positive := 20;
      Thickness      : Natural := 1);

   procedure Draw_Circle
     (Buffer : DMA2D_Buffer;
      Center : Display_Point;
      Radius : Natural;
      Hue    : DMA2D_Color);

   procedure Fill_Circle
     (Buffer : DMA2D_Buffer;
      Center : Display_Point;
      Radius : Natural;
      Hue    : DMA2D_Color);

   procedure Draw_Char
     (Buffer     : DMA2D_Buffer;
      Start      : Display_Point;
      Char       : Character;
      Font       : BMP_Font;
      Foreground : DMA2D_Color;
      Background : DMA2D_Color);

   procedure Draw_String
     (Buffer     : DMA2D_Buffer;
      Start      : Display_Point;
      Msg        : String;
      Font       : BMP_Font;
      Foreground : DMA2D_Color;
      Background : DMA2D_Color);

end Bitmapped_Drawing;

