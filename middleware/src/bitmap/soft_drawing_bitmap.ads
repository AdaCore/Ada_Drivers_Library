------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

--  This package provides a software implementation of the HAL.Bitmap drawing
--  primitives.

with HAL;        use HAL;
with HAL.Bitmap; use HAL.Bitmap;

package Soft_Drawing_Bitmap is

   subtype Parent is Bitmap_Buffer;

   type Soft_Drawing_Bitmap_Buffer is abstract new Parent with null record;

   type Any_Soft_Drawing_Bitmap_Buffer is
     access all Soft_Drawing_Bitmap_Buffer'Class;

   overriding
   procedure Set_Source (Buffer : in out Soft_Drawing_Bitmap_Buffer;
                         ARGB   : Bitmap_Color);

   overriding
   function Source
     (Buffer : Soft_Drawing_Bitmap_Buffer)
      return Bitmap_Color;

   overriding
   procedure Set_Pixel
     (Buffer  : in out Soft_Drawing_Bitmap_Buffer;
      Pt      : Point;
      Color   : Bitmap_Color);

   overriding
   procedure Set_Pixel
     (Buffer  : in out Soft_Drawing_Bitmap_Buffer;
      Pt      : Point;
      Native  : UInt32);

   overriding
   function Pixel
     (Buffer : Soft_Drawing_Bitmap_Buffer;
      Pt     : Point)
      return Bitmap_Color;

   overriding
   procedure Draw_Line
     (Buffer      : in out Soft_Drawing_Bitmap_Buffer;
      Start, Stop : Point;
      Thickness   : Natural := 1;
      Fast        : Boolean := True);

   overriding
   procedure Fill
     (Buffer : in out Soft_Drawing_Bitmap_Buffer);
   --  Fill the entire buffer with the source color

   overriding
   procedure Fill_Rect
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Area   : Rect);
   --  Fill the specified area of the buffer with the source color

   overriding
   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Soft_Drawing_Bitmap_Buffer;
      Dst_Pt      : Point;
      Bg_Buffer   : Bitmap_Buffer'Class;
      Bg_Pt       : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean);

   overriding
   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Soft_Drawing_Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean);

   overriding
   procedure Copy_Rect_Blend
     (Src_Buffer  : Soft_Drawing_Bitmap_Buffer;
      Src_Pt      : Point;
      Dst_Buffer  : in out Bitmap_Buffer'Class;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean);

   overriding
   procedure Draw_Vertical_Line
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Pt     : Point;
      Height : Integer);

   overriding
   procedure Draw_Horizontal_Line
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Pt     : Point;
      Width  : Integer);

   overriding
   procedure Draw_Rect
     (Buffer    : in out Soft_Drawing_Bitmap_Buffer;
      Area      : Rect;
      Thickness : Natural := 1);
   --  Draws a rectangle

   overriding
   procedure Draw_Rounded_Rect
     (Buffer    : in out Soft_Drawing_Bitmap_Buffer;
      Area      : Rect;
      Radius    : Natural;
      Thickness : Natural := 1);

   overriding
   procedure Fill_Rounded_Rect
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Area   : Rect;
      Radius : Natural);

   overriding
   procedure Draw_Circle
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Center : Point;
      Radius : Natural);

   overriding
   procedure Fill_Circle
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Center : Point;
      Radius : Natural);

   overriding
   procedure Cubic_Bezier
     (Buffer         : in out Soft_Drawing_Bitmap_Buffer;
      P1, P2, P3, P4 : Point;
      N              : Positive := 20;
      Thickness      : Natural := 1);

   overriding
   procedure Bezier
     (Buffer         : in out Soft_Drawing_Bitmap_Buffer;
      Input_Points   : Point_Array;
      N              : Positive := 20;
      Thickness      : Natural := 1);

private

   type Natural_Array is array (Natural range <>) of Natural;

   procedure Cubic_Bezier_Points
     (P1, P2, P3, P4 : Point;
      N              : Positive;
      Points         : in out Point_Array);

   procedure Bezier_Points
     (Input_Points : Point_Array;
      N            : Positive;
      Points       : in out Point_Array);

   procedure Binomial_Coefficients
     (Outputs : in out Natural_Array);

end Soft_Drawing_Bitmap;
