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

with HAL;                 use HAL;
with HAL.Bitmap;          use HAL.Bitmap;
with Soft_Drawing_Bitmap; use Soft_Drawing_Bitmap;
with System;

package Memory_Mapped_Bitmap is

   subtype Parent is Soft_Drawing_Bitmap_Buffer;

   type Memory_Mapped_Bitmap_Buffer is new Parent with record
      Addr              : System.Address;

      Actual_Width      : Natural;
      Actual_Height     : Natural;
      --  Width and Height of the buffer. Note that it's the user-visible width
      --  (see below for the meaning of the Swapped value).

      Actual_Color_Mode : Bitmap_Color_Mode;
      --  The buffer color mode. Note that not all color modes are supported by
      --  the hardware acceleration (if any), so you need to check your actual
      --  hardware to optimize buffer transfers.

      Currently_Swapped : Boolean := False;
      --  If Swap is set, then operations on this buffer will consider:
      --  Width0 = Height
      --  Height0 = Width
      --  Y0 = Buffer.Width - X - 1
      --  X0 = Y
      --
      --  As an example, the Bitmap buffer that corresponds to a 240x320
      --  swapped display (to display images in landscape mode) with have
      --  the following values:
      --  Width => 320
      --  Height => 240
      --  Swapped => True
      --  So Put_Pixel (Buffer, 30, 10, Color) will place the pixel at
      --  Y0 = 320 - 30 - 1 = 289
      --  X0 = 10

      Native_Source : UInt32 := 0;
      --  Source color in native format
   end record;

   type Any_Memory_Mapped_Bitmap_Buffer is access all Memory_Mapped_Bitmap_Buffer'Class;

   overriding
   function Width (Buffer : Memory_Mapped_Bitmap_Buffer) return Natural is
     (Buffer.Actual_Width);

   overriding
   function Height (Buffer : Memory_Mapped_Bitmap_Buffer) return Natural is
     (Buffer.Actual_Height);

   overriding
   function Swapped (Buffer : Memory_Mapped_Bitmap_Buffer) return Boolean is
     (Buffer.Currently_Swapped);

   overriding
   function Color_Mode (Buffer : Memory_Mapped_Bitmap_Buffer) return Bitmap_Color_Mode is
     (Buffer.Actual_Color_Mode);

   overriding
   function Mapped_In_RAM (Buffer : Memory_Mapped_Bitmap_Buffer) return Boolean is
     (True);

   overriding
   function Memory_Address (Buffer : Memory_Mapped_Bitmap_Buffer) return System.Address is
     (Buffer.Addr);

   overriding
   procedure Set_Source (Buffer : in out Memory_Mapped_Bitmap_Buffer;
                         ARGB   : Bitmap_Color);

   overriding
   procedure Set_Source (Buffer : in out Memory_Mapped_Bitmap_Buffer;
                         Native : UInt32);

   overriding
   function Source (Buffer : Memory_Mapped_Bitmap_Buffer)
                    return Bitmap_Color;

   overriding
   function Source (Buffer : Memory_Mapped_Bitmap_Buffer)
                    return UInt32;

   overriding
   procedure Set_Pixel
     (Buffer  : in out Memory_Mapped_Bitmap_Buffer;
      Pt      : Point);

   overriding
   procedure Set_Pixel
     (Buffer  : in out Memory_Mapped_Bitmap_Buffer;
      Pt      : Point;
      Color   : Bitmap_Color);

   overriding
   procedure Set_Pixel
     (Buffer  : in out Memory_Mapped_Bitmap_Buffer;
      Pt      : Point;
      Raw     : UInt32);

   overriding
   procedure Set_Pixel_Blend
     (Buffer : in out Memory_Mapped_Bitmap_Buffer;
      Pt     : Point);

   overriding
   function Pixel
     (Buffer : Memory_Mapped_Bitmap_Buffer;
      Pt     : Point)
      return Bitmap_Color;

   overriding
   function Pixel
     (Buffer : Memory_Mapped_Bitmap_Buffer;
      Pt     : Point)
      return UInt32;

   overriding
   function Buffer_Size (Buffer : Memory_Mapped_Bitmap_Buffer) return Natural;

end Memory_Mapped_Bitmap;
