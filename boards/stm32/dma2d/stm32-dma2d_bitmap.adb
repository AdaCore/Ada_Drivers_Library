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

with Ada.Unchecked_Conversion;
with Cortex_M.Cache;      use Cortex_M.Cache;

with Soft_Drawing_Bitmap; use Soft_Drawing_Bitmap;
with STM32.DMA2D;         use STM32.DMA2D;

package body STM32.DMA2D_Bitmap is

   function To_DMA2D_CM is new Ada.Unchecked_Conversion
     (HAL.Bitmap.Bitmap_Color_Mode, STM32.DMA2D.DMA2D_Color_Mode);

   function To_DMA2D_Color is new Ada.Unchecked_Conversion
     (HAL.Bitmap.Bitmap_Color, STM32.DMA2D.DMA2D_Color);

   ---------------------
   -- To_DMA2D_Buffer --
   ---------------------

   function To_DMA2D_Buffer
     (Buffer : Memory_Mapped_Bitmap_Buffer'Class) return STM32.DMA2D.DMA2D_Buffer
   is
      Color_Mode : constant DMA2D_Color_Mode :=
                     To_DMA2D_CM (Buffer.Color_Mode);
      Ret : DMA2D_Buffer (Color_Mode);
   begin
      Ret.Addr   := Buffer.Addr;
      if not Buffer.Swapped then
         Ret.Width  := Buffer.Width;
         Ret.Height := Buffer.Height;
      else
         Ret.Width  := Buffer.Height;
         Ret.Height := Buffer.Width;
      end if;
      return Ret;
   end To_DMA2D_Buffer;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   overriding procedure Set_Pixel_Blend
     (Buffer : in out DMA2D_Bitmap_Buffer;
      Pt     : Point;
      Value  : HAL.Bitmap.Bitmap_Color)
   is
      DMA_Buf : constant DMA2D_Buffer := To_DMA2D_Buffer (Buffer);
   begin
      if To_DMA2D_CM (Buffer.Color_Mode) in DMA2D_Dst_Color_Mode then
         if not Buffer.Swapped then
            DMA2D_Set_Pixel_Blend
              (Buffer => DMA_Buf,
               X      => Pt.X,
               Y      => Pt.Y,
               Color  => To_DMA2D_Color (Value));
         else
            DMA2D_Set_Pixel_Blend
              (Buffer => DMA_Buf,
               X      => Pt.Y,
               Y      => Buffer.Width - Pt.X - 1,
               Color  => To_DMA2D_Color (Value));
         end if;
      else
         Parent (Buffer).Set_Pixel_Blend (Pt, Value);
      end if;
   end Set_Pixel_Blend;

   ----------
   -- Fill --
   ----------

   overriding procedure Fill
     (Buffer : in out DMA2D_Bitmap_Buffer;
      Color  : UInt32)
   is
   begin
      if To_DMA2D_CM (Buffer.Color_Mode) in DMA2D_Dst_Color_Mode then
         DMA2D_Fill (To_DMA2D_Buffer (Buffer), Color, True);
      else
         Parent (Buffer).Fill (Color);
      end if;
   end Fill;

   ---------------
   -- Fill_Rect --
   ---------------

   overriding procedure Fill_Rect
     (Buffer      : in out DMA2D_Bitmap_Buffer;
      Color       : UInt32;
      Area        : Rect;
      Synchronous : Boolean := True)
   is
      DMA_Buf : constant DMA2D_Buffer := To_DMA2D_Buffer (Buffer);
   begin
      if To_DMA2D_CM (Buffer.Color_Mode) in DMA2D_Dst_Color_Mode then
         if not Buffer.Swapped then
            DMA2D_Fill_Rect
              (DMA_Buf,
               Color       => Color,
               X           => Area.Position.X,
               Y           => Area.Position.Y,
               Width       => Area.Width,
               Height      => Area.Height,
               Synchronous => Synchronous);
         else
            DMA2D_Fill_Rect
              (DMA_Buf,
               Color       => Color,
               X           => Area.Position.Y,
               Y           => Buffer.Width - Area.Position.X - Area.Width,
               Width       => Area.Height,
               Height      => Area.Width,
               Synchronous => Synchronous);
         end if;
      else
         Parent (Buffer).Fill_Rect (Color, Area, Synchronous);
      end if;
   end Fill_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   overriding procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out DMA2D_Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean := True;
      Clean_Cache : Boolean := True)
   is
   begin
      if Src_Buffer.Kind /= Memory_Mapped
        or else Memory_Mapped_Bitmap_Buffer (Src_Buffer).Swapped /= Dst_Buffer.Swapped
      then
         Soft_Drawing_Bitmap.Copy_Rect
           (Src_Buffer, Src_Pt,
            Soft_Drawing_Bitmap_Buffer (Dst_Buffer), Dst_Pt,
            Width, Height, Synchronous, Clean_Cache);
         return;
      end if;

      declare
         use type System.Address;
         S_Buf       : constant Memory_Mapped_Bitmap_Buffer'Class :=
                         Memory_Mapped_Bitmap_Buffer'Class (Src_Buffer);
         DMA_Buf_Src : constant DMA2D_Buffer := To_DMA2D_Buffer (S_Buf);
         DMA_Buf_Dst : constant DMA2D_Buffer := To_DMA2D_Buffer (Dst_Buffer);

      begin
         if Clean_Cache then
            Cortex_M.Cache.Clean_DCache
              (S_Buf.Addr, S_Buf.Buffer_Size);
         end if;

         if Dst_Buffer.Swapped then
            DMA2D_Copy_Rect
              (DMA_Buf_Src, Src_Pt.Y, Src_Pt.X,
               DMA_Buf_Dst, Dst_Pt.Y, Dst_Buffer.Width - Dst_Pt.X - Width,
               STM32.DMA2D.Null_Buffer, 0, 0,
               Height, Width,
            Synchronous => Synchronous);
         else
            DMA2D_Copy_Rect
              (DMA_Buf_Src, Src_Pt.X, Src_Pt.Y,
               DMA_Buf_Dst, Dst_Pt.X, Dst_Pt.Y,
               STM32.DMA2D.Null_Buffer, 0, 0,
               Width, Height,
            Synchronous => Synchronous);
         end if;


      end;
   end Copy_Rect;

   ---------------------
   -- Copy_Rect_Blend --
   ---------------------

   overriding procedure Copy_Rect_Blend
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out DMA2D_Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean := True;
      Clean_Cache : Boolean := True)
   is
   begin
      if Src_Buffer.Kind /= Memory_Mapped
        or else Memory_Mapped_Bitmap_Buffer (Src_Buffer).Swapped /= Dst_Buffer.Swapped
      then
         Soft_Drawing_Bitmap.Copy_Rect
           (Src_Buffer, Src_Pt,
            Soft_Drawing_Bitmap_Buffer (Dst_Buffer), Dst_Pt,
            Width, Height, Synchronous, Clean_Cache);
         return;
      end if;

      declare
         use type System.Address;
         S_Buf       : constant Memory_Mapped_Bitmap_Buffer'Class :=
                         Memory_Mapped_Bitmap_Buffer'Class (Src_Buffer);
         DMA_Buf_Src : constant DMA2D_Buffer := To_DMA2D_Buffer (S_Buf);
         DMA_Buf_Dst : constant DMA2D_Buffer := To_DMA2D_Buffer (Dst_Buffer);
         Src_Pt0     : constant Point := Swap (S_Buf, Src_Pt);
         Dst_Pt0     : constant Point := Swap (Dst_Buffer, Dst_Pt);
         W           : Natural := Width;
         H           : Natural := Height;

      begin
         if Dst_Buffer.Swapped then
            W := Height;
            H := Width;
         end if;

         if Clean_Cache then
            Cortex_M.Cache.Clean_DCache
              (S_Buf.Addr, S_Buf.Buffer_Size);
         end if;

         DMA2D_Copy_Rect
           (DMA_Buf_Src, Src_Pt0.X, Src_Pt0.Y,
            DMA_Buf_Dst, Dst_Pt0.X, Dst_Pt0.Y,
            DMA_Buf_Dst, Dst_Pt0.X, Dst_Pt0.Y,
            W, H,
            Synchronous => Synchronous);
      end;
   end Copy_Rect_Blend;

   ----------
   -- Sync --
   ----------

   overriding procedure Sync (Buffer : DMA2D_Bitmap_Buffer)
   is
   begin
      DMA2D_Wait_Transfer;
      Cortex_M.Cache.Clean_Invalidate_DCache
        (Start => Buffer.Addr,
         Len   => Buffer.Buffer_Size);
   end Sync;

end STM32.DMA2D_Bitmap;
