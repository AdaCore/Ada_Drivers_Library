------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

--  Take advantage of the DMA2D to accelerate some graphical operations on
--  bitmap surfaces

with System;
with HAL.Bitmap;
with STM32.DMA2D;

package STM32.DMA2D_Bitmap is

   type DMA2D_Bitmap_Buffer is new HAL.Bitmap.Bitmap_Buffer with null record;

   overriding procedure Set_Pixel
     (Buffer : DMA2D_Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : UInt32);

   overriding procedure Set_Pixel_Blend
     (Buffer : DMA2D_Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : HAL.Bitmap.Bitmap_Color);

   overriding function Get_Pixel
     (Buffer : DMA2D_Bitmap_Buffer;
      X      : Natural;
      Y      : Natural) return UInt32;

   overriding procedure Fill
     (Buffer : DMA2D_Bitmap_Buffer;
      Color  : UInt32);

   overriding procedure Fill_Rect
     (Buffer : DMA2D_Bitmap_Buffer;
      Color  : UInt32;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);

   overriding procedure Copy_Rect
     (Src_Buffer  : HAL.Bitmap.Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : DMA2D_Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Bg_Buffer   : HAL.Bitmap.Bitmap_Buffer'Class;
      X_Bg        : Natural;
      Y_Bg        : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
     with Pre =>
       Dst_Buffer.Color_Mode in HAL.Bitmap.ARGB_8888 .. HAL.Bitmap.ARGB_4444;

   overriding procedure Wait_Transfer (Buffer : DMA2D_Bitmap_Buffer);

   Null_Buffer : constant DMA2D_Bitmap_Buffer :=
                   (Addr       => System.Null_Address,
                    Width      => 0,
                    Height     => 0,
                    Color_Mode => HAL.Bitmap.L_8,
                    Swapped    => False);

   function To_DMA2D_Buffer
     (Buffer : HAL.Bitmap.Bitmap_Buffer'Class) return STM32.DMA2D.DMA2D_Buffer
     with Inline;

end STM32.DMA2D_Bitmap;
