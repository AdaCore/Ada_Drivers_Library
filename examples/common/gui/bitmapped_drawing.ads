------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides routines for drawing shapes, characters, and strings
--  on a bit-mapped device or graphical buffer.

with Interfaces;    use Interfaces;
with BMP_Fonts;     use BMP_Fonts;
with Hershey_Fonts; use Hershey_Fonts;
with STM32.DMA2D;   use STM32.DMA2D;

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
      Foreground : Graphics_Color;
      Background : Graphics_Color);

   procedure Draw_String
     (Buffer     : Graphics_Buffer;
      Start      : Point;
      Msg        : String;
      Font       : Hershey_Font;
      Height     : Natural;
      Bold       : Boolean;
      Foreground : DMA2D_Color;
      Fast       : Boolean := True);

   procedure Draw_String
     (Buffer     : Graphics_Buffer;
      Area       : Rect;
      Msg        : String;
      Font       : Hershey_Font;
      Bold       : Boolean;
      Outline    : Boolean;
      Foreground : Graphics_Color;
      Fast       : Boolean := True);

end Bitmapped_Drawing;

