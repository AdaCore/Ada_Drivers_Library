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

with System;
with System.Storage_Elements;

with HAL;                 use HAL;
with HAL.Bitmap;          use HAL.Bitmap;
with Soft_Drawing_Bitmap; use Soft_Drawing_Bitmap;

package Memory_Mapped_Bitmap is

   subtype Parent is Soft_Drawing_Bitmap_Buffer;

   type Memory_Mapped_Bitmap_Buffer is new Parent with record
      Addr    : System.Address;
      Swapped : Boolean := False;
      --  Set to True if the X/Y coordinates meaning are reversed in the
      --  buffer representation. This is meant to accomodate portrait mode
      --  on landscape-only display, or the reverse.
   end record;

   type Any_Memory_Mapped_Bitmap_Buffer is access all Memory_Mapped_Bitmap_Buffer'Class;

   overriding function Kind
     (Buffer : Memory_Mapped_Bitmap_Buffer) return Bitmap_Buffer_Kind
   is (Memory_Mapped);

   overriding
   procedure Set_Pixel
     (Buffer  : in out Memory_Mapped_Bitmap_Buffer;
      Pt      : Point;
      Value   : Bitmap_Color);

   overriding
   procedure Set_Pixel
     (Buffer  : in out Memory_Mapped_Bitmap_Buffer;
      Pt      : Point;
      Value   : UInt32);

   overriding
   procedure Set_Pixel_Blend
     (Buffer : in out Memory_Mapped_Bitmap_Buffer;
      Pt      : Point;
      Value  : Bitmap_Color);

   overriding
   procedure Set_Pixel_Blend
     (Buffer : in out Memory_Mapped_Bitmap_Buffer;
      Pt     : Point;
      Value  : UInt32);

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

   overriding procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Memory_Mapped_Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean := True;
      Clean_Cache : Boolean := True);
   --  Copy the area denoted by Src_Pt, Width, Height from Src_Buffer to
   --  Dst_Buffer.
   --  In case of hardware (e.g. DMA) acceleration, Synchronous, if set,
   --  ensures that the operation is completed upon return.
   --  In case the CPU supports data memory cache, setting Clean_Cache
   --  to True will ensure that the source buffers are visible to both the
   --  CPU and the DMA. Has no effect on software rendered operation.

   overriding procedure Copy_Rect_Blend
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Memory_Mapped_Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean := True;
      Clean_Cache : Boolean := True);
   --  Blends the area denoted by Src_Pt, Width, Height from Src_Buffer with
   --  Dst_Buffer in the area Dst_Pt, Width, Height.
   --  In case of hardware (e.g. DMA) acceleration, Synchronous, if set,
   --  ensures that the operation is completed upon return.
   --  In case the CPU supports data memory cache, setting Clean_Cache
   --  to True will ensure that the source buffers are visible to both the
   --  CPU and the DMA. Has no effect on software rendered operation.

   overriding function Buffer_Size
     (Buffer : Memory_Mapped_Bitmap_Buffer) return Natural;

   function Swap
     (Buffer : Memory_Mapped_Bitmap_Buffer'Class;
      Pt     : Point) return Point
     with Inline_Always;
   --  Swaps X/Y to translate Pt from respectively Protrait/Landscape into
   --  Lanscape/Portrait modes, if indicated by Buffer.

   function Pixel_Offset
     (Buffer : Memory_Mapped_Bitmap_Buffer'Class;
      Pt     : Point) return System.Storage_Elements.Storage_Offset
     with Inline_Always;
   --  Offset is pixels of Pt in Buffer natural order

   overriding procedure Sync (Buffer : Memory_Mapped_Bitmap_Buffer) is null;

end Memory_Mapped_Bitmap;
