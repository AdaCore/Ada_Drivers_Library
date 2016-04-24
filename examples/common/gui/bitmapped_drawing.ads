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
with HAL.Bitmap;    use HAL.Bitmap;

package Bitmapped_Drawing is

   type Point is record
      X : Natural;
      Y : Natural;
   end record;

   function "+" (P1, P2 : Point) return Point
     is ((P1.X + P2.X, P1.Y + P2.Y));

   function "-" (P1, P2 : Point) return Point
     is ((P1.X - P2.X, P1.Y - P2.Y));

   type Rect is record
      Position : Point;
      Width    : Natural;
      Height   : Natural;
   end record;

   procedure Draw_Line
     (Buffer      : Bitmap_Buffer'Class;
      Start, Stop : Point;
      Hue         : Unsigned_32;
      Thickness   : Natural := 1;
      Fast        : Boolean := True);
   procedure Draw_Line
     (Buffer      : Bitmap_Buffer'Class;
      Start, Stop : Point;
      Hue         : Bitmap_Color;
      Thickness   : Natural := 1;
      Fast        : Boolean := True);
   --  If fast is set, then the line thickness uses squares to draw, while
   --  if not set, then the line will be composed of circles, much slower to
   --  draw but providing nicer line cap.

   procedure Draw_Rectangle
     (Buffer    : Bitmap_Buffer'Class;
      Area      : Rect;
      Hue       : Bitmap_Color;
      Thickness : Natural := 1);

   procedure Draw_Rounded_Rectangle
     (Buffer    : Bitmap_Buffer'Class;
      Area      : Rect;
      Radius    : Natural;
      Hue       : Bitmap_Color;
      Thickness : Natural := 1);

   procedure Fill_Rounded_Rectangle
     (Buffer : Bitmap_Buffer'Class;
      X      : Natural;
      Y      : Natural;
      Width  : Positive;
      Height : Positive;
      Radius : Natural;
      Hue    : Bitmap_Color);

   procedure Cubic_Bezier
     (Buffer         : Bitmap_Buffer'Class;
      P1, P2, P3, P4 : Point;
      Hue            : Bitmap_Color;
      N              : Positive := 20;
      Thickness      : Natural := 1);

   procedure Draw_Circle
     (Buffer : Bitmap_Buffer'Class;
      Center : Point;
      Radius : Natural;
      Hue    : Unsigned_32);
   procedure Draw_Circle
     (Buffer : Bitmap_Buffer'Class;
      Center : Point;
      Radius : Natural;
      Hue    : Bitmap_Color);

   procedure Fill_Circle
     (Buffer : Bitmap_Buffer'Class;
      Center : Point;
      Radius : Natural;
      Hue    : Unsigned_32);
   procedure Fill_Circle
     (Buffer : Bitmap_Buffer'Class;
      Center : Point;
      Radius : Natural;
      Hue    : Bitmap_Color);

   procedure Draw_Char
     (Buffer     : Bitmap_Buffer'Class;
      Start      : Point;
      Char       : Character;
      Font       : BMP_Font;
      Foreground : Unsigned_32;
      Background : Unsigned_32);

   procedure Draw_String
     (Buffer     : Bitmap_Buffer'Class;
      Start      : Point;
      Msg        : String;
      Font       : BMP_Font;
      Foreground : Bitmap_Color;
      Background : Bitmap_Color);

   procedure Draw_String
     (Buffer     : Bitmap_Buffer'Class;
      Start      : Point;
      Msg        : String;
      Font       : Hershey_Font;
      Height     : Natural;
      Bold       : Boolean;
      Foreground : Bitmap_Color;
      Fast       : Boolean := True);

   procedure Draw_String
     (Buffer     : Bitmap_Buffer'Class;
      Area       : Rect;
      Msg        : String;
      Font       : Hershey_Font;
      Bold       : Boolean;
      Outline    : Boolean;
      Foreground : Bitmap_Color;
      Fast       : Boolean := True);

end Bitmapped_Drawing;

