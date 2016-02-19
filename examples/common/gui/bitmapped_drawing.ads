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

with Interfaces;  use Interfaces;
with BMP_Fonts;   use BMP_Fonts;
with STM32.DMA2D; use STM32.DMA2D;

package Bitmapped_Drawing is

   subtype Graphics_Color is DMA2D_Color;
   subtype Graphics_Buffer is DMA2D_Buffer;

   Black       : constant Graphics_Color := (255, 0, 0, 0);
   Blue        : constant Graphics_Color := (255, 0, 0, 255);
   Light_Blue  : constant Graphics_Color := (255, 173, 216, 230);
   Brown       : constant Graphics_Color := (255, 165, 42, 42);
   Cyan        : constant Graphics_Color := (255, 0, 255, 255);
   Gray        : constant Graphics_Color := (255, 190, 190, 190);
   Light_Gray  : constant Graphics_Color := (255, 211, 211, 211);
   Green       : constant Graphics_Color := (255, 0, 255, 0);
   Light_Green : constant Graphics_Color := (255, 144, 238, 144);
   Magenta     : constant Graphics_Color := (255, 255, 0, 255);
   Red         : constant Graphics_Color := (255, 255, 0, 0);
   Orange      : constant Graphics_Color := (255, 255, 69, 0);
   Violet      : constant Graphics_Color := (255, 238, 130, 238);
   Yellow      : constant Graphics_Color := (255, 255, 255, 0);
   White       : constant Graphics_Color := (255, 255, 255, 255);
   Transparent : constant Graphics_Color := (0, 0, 0, 0);

   type Point is record
      X : Natural;
      Y : Natural;
   end record;

   type Rect is record
      Position : Point;
      Width    : Natural;
      Height   : Natural;
   end record;

   function Screen_Buffer return Graphics_Buffer;
   --  Returns the LCD Frame buffer for Layer 1

   procedure Put_Pixel
     (Buffer   : Graphics_Buffer;
      Position : Point;
      Hue      : Graphics_Color);

   procedure Put_Pixel
     (Buffer   : Graphics_Buffer;
      Position : Point;
      Hue      : Unsigned_32);

   function Get_Pixel
     (Buffer   : Graphics_Buffer;
      Position : Point) return Graphics_Color;

   function Get_Pixel
     (Buffer   : Graphics_Buffer;
      Position : Point) return Unsigned_32;

   procedure Draw_Line
     (Buffer      : Graphics_Buffer;
      Start, Stop : Point;
      Hue         : Graphics_Color;
      Thickness   : Natural := 1;
      Fast        : Boolean := True);
   --  If fast is set, then the line thickness uses squares to draw, while
   --  if not set, then the line will be composed of circles, much slower to
   --  draw but providing nicer line cap.

   procedure Draw_Rectangle
     (Buffer    : Graphics_Buffer;
      Area      : Rect;
      Hue       : Graphics_Color;
      Thickness : Natural := 1);

   procedure Draw_Rounded_Rectangle
     (Buffer    : Graphics_Buffer;
      Area      : Rect;
      Radius    : Natural;
      Hue       : Graphics_Color;
      Thickness : Natural := 1);

   procedure Fill
     (Buffer      : Graphics_Buffer;
      Hue         : Graphics_Color;
      Synchronous : Boolean := False)
      renames STM32.DMA2D.DMA2D_Fill;

   procedure Fill_Rectangle
     (Buffer : Graphics_Buffer;
      Area   : Rect;
      Hue    : Graphics_Color);

   procedure Fill_Rounded_Rectangle
     (Buffer : Graphics_Buffer;
      Area   : Rect;
      Radius : Natural;
      Hue    : Graphics_Color);

   procedure Copy_Rectangle
     (Src      : Graphics_Buffer;
      Src_Area : Rect;
      Dst      : Graphics_Buffer;
      Dst_Pos  : Point);

   procedure Copy_Rectangle_Blend
     (Src      : Graphics_Buffer;
      Src_Area : Rect;
      Dst      : Graphics_Buffer;
      Dst_Pos  : Point);

   procedure Cubic_Bezier
     (Buffer         : Graphics_Buffer;
      P1, P2, P3, P4 : Point;
      Hue            : Graphics_Color;
      N              : Positive := 20;
      Thickness      : Natural := 1);

   procedure Draw_Circle
     (Buffer : Graphics_Buffer;
      Center : Point;
      Radius : Natural;
      Hue    : Graphics_Color);

   procedure Fill_Circle
     (Buffer : Graphics_Buffer;
      Center : Point;
      Radius : Natural;
      Hue    : Graphics_Color);

   procedure Draw_Char
     (Buffer     : Graphics_Buffer;
      Start      : Point;
      Char       : Character;
      Font       : BMP_Font;
      Foreground : Graphics_Color;
      Background : Graphics_Color);

   procedure Draw_String
     (Buffer     : Graphics_Buffer;
      Start      : Point;
      Msg        : String;
      Font       : BMP_Font;
      Foreground : DMA2D_Color;
      Background : DMA2D_Color);

end Bitmapped_Drawing;

