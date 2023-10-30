------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2023, AdaCore                     --
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

--  This file provides the bitmap interface to the ILI9341 RAM.

with HAL;
with HAL.Bitmap;
with Soft_Drawing_Bitmap;
with System;

generic
   with procedure Write_Pixels
     (This    : ILI9341_Connector;
      Mode    : HAL.Bitmap.Bitmap_Color_Mode;
      Address : System.Address;
      Count   : Positive;
      Repeat  : Positive) is null;
   --  Transfer to the display memory information about Count pixels located at
   --  the specified Address, taking into account the representation format
   --  Mode. Repeat the transfer the specified number of times.

   with procedure Read_Pixels
     (This    : ILI9341_Connector;
      Cmd     : HAL.UInt8;
      Address : System.Address;
      Count   : Positive) is null;
   --  Read from the display memory information about Count pixels into the
   --  location at the specified Address in RGB_888 color mode.

package ILI9341.Device.Bitmap is

   subtype Parent is Soft_Drawing_Bitmap.Soft_Drawing_Bitmap_Buffer;

   type Bitmap_Buffer
     (Device : not null access ILI9341_Device) is new Parent with private;

   function Get_Bitmap
     (This : not null access ILI9341_Device) return Bitmap_Buffer;

private

   type Bitmap_Buffer
     (Device : not null access ILI9341_Device) is new Parent with
   record
      Source : HAL.UInt32;
   end record;

   overriding
   function Source (This : Bitmap_Buffer) return HAL.UInt32 is (This.Source);

   overriding
   function Width (This : Bitmap_Buffer) return Natural is
     (if This.Device.Orientation in Portrait_1 | Portrait_2 then 240
      else 320);

   overriding
   function Height (This : Bitmap_Buffer) return Natural is
     (if This.Device.Orientation in Portrait_1 | Portrait_2 then 320
      else 240);

   overriding
   function Swapped (This : Bitmap_Buffer) return Boolean is (False);

   overriding
   function Color_Mode
     (This : Bitmap_Buffer) return HAL.Bitmap.Bitmap_Color_Mode is
       (HAL.Bitmap.RGB_888);

   overriding
   function Mapped_In_RAM (This : Bitmap_Buffer) return Boolean is (False);

   overriding
   function Memory_Address (This : Bitmap_Buffer) return System.Address is
     (System.Null_Address);

   overriding
   procedure Set_Source (This : in out Bitmap_Buffer; Native : HAL.UInt32);

   overriding
   procedure Set_Pixel
     (This  : in out Bitmap_Buffer;
      Point : HAL.Bitmap.Point);

   overriding
   procedure Set_Pixel_Blend
     (This  : in out Bitmap_Buffer;
      Point : HAL.Bitmap.Point) renames Set_Pixel;

   overriding
   function Pixel
     (This  : Bitmap_Buffer;
      Point : HAL.Bitmap.Point) return HAL.UInt32;

   overriding
   function Pixel
     (This  : Bitmap_Buffer;
      Point : HAL.Bitmap.Point)
      return HAL.Bitmap.Bitmap_Color;

   overriding
   function Buffer_Size (This : Bitmap_Buffer) return Natural is
     (This.Width * This.Height * 18 / 8);

   overriding
   procedure Fill (This : in out Bitmap_Buffer);

   overriding
   procedure Copy_Rect
     (Src_Buffer  : HAL.Bitmap.Bitmap_Buffer'Class;
      Src_Pt      : HAL.Bitmap.Point;
      Dst_Buffer  : in out Bitmap_Buffer;
      Dst_Pt      : HAL.Bitmap.Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean);

end ILI9341.Device.Bitmap;
