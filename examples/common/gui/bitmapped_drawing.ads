
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

