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
with Cortex_M.Cache; use Cortex_M.Cache;

with STM32.DMA2D;    use STM32.DMA2D;

package body STM32.DMA2D_Bitmap is

   function To_DMA2D_CM is new Ada.Unchecked_Conversion
     (HAL.Bitmap.Bitmap_Color_Mode, STM32.DMA2D.DMA2D_Color_Mode);

   function To_DMA2D_Color is new Ada.Unchecked_Conversion
     (HAL.Bitmap.Bitmap_Color, STM32.DMA2D.DMA2D_Color);

   ---------------------
   -- To_DMA2D_Buffer --
   ---------------------

   function To_DMA2D_Buffer
     (Buffer : HAL.Bitmap.Bitmap_Buffer'Class) return DMA2D_Buffer
   is
      Color_Mode : constant DMA2D_Color_Mode :=
        To_DMA2D_CM (Buffer.Color_Mode);
      Ret : DMA2D_Buffer (Color_Mode);
   begin
      Ret.Addr := Buffer.Memory_Address;
      if not Buffer.Swapped then
         Ret.Width := Buffer.Width;
         Ret.Height := Buffer.Height;
      else
         Ret.Width := Buffer.Height;
         Ret.Height := Buffer.Width;
      end if;
      return Ret;
   end To_DMA2D_Buffer;

   ---------------
   -- Set_Pixel --
   ---------------

   overriding procedure Set_Pixel
     (Buffer : in out DMA2D_Bitmap_Buffer;
      Pt     : Point)
   is
   begin
      DMA2D_Wait_Transfer;
      Parent (Buffer).Set_Pixel (Pt);
   end Set_Pixel;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   overriding procedure Set_Pixel_Blend
     (Buffer : in out DMA2D_Bitmap_Buffer;
      Pt     : Point)
   is
      DMA_Buf : constant DMA2D_Buffer := To_DMA2D_Buffer (Buffer);
      Value   : constant HAL.Bitmap.Bitmap_Color := Buffer.Source;
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
         Parent (Buffer).Set_Pixel_Blend (Pt);
      end if;
   end Set_Pixel_Blend;

   ---------------
   -- Get_Pixel --
   ---------------

   overriding function Pixel
     (Buffer : DMA2D_Bitmap_Buffer;
      Pt     : Point) return Bitmap_Color
   is
   begin
      DMA2D_Wait_Transfer;
      return Parent (Buffer).Pixel (Pt);
   end Pixel;

   ----------
   -- Fill --
   ----------

   overriding procedure Fill
     (Buffer : in out DMA2D_Bitmap_Buffer)
   is
   begin
      if To_DMA2D_CM (Buffer.Color_Mode) in DMA2D_Dst_Color_Mode then
         DMA2D_Fill (To_DMA2D_Buffer (Buffer), Buffer.Native_Source, True);
      else
         Parent (Buffer).Fill;
      end if;
   end Fill;

   ---------------
   -- Fill_Rect --
   ---------------

   overriding procedure Fill_Rect
     (Buffer : in out DMA2D_Bitmap_Buffer;
      Area   : Rect)
   is
      DMA_Buf : constant DMA2D_Buffer := To_DMA2D_Buffer (Buffer);
   begin
      if To_DMA2D_CM (Buffer.Color_Mode) in DMA2D_Dst_Color_Mode then
         if not Buffer.Swapped then
            DMA2D_Fill_Rect
              (DMA_Buf,
               Color  => Buffer.Native_Source,
               X      => Area.Position.X,
               Y      => Area.Position.Y,
               Width  => Area.Width,
               Height => Area.Height);
         else
            DMA2D_Fill_Rect
              (DMA_Buf,
               Color  => Buffer.Native_Source,
               X      => Area.Position.Y,
               Y      => Buffer.Width - Area.Position.X - Area.Width,
               Width  => Area.Height,
               Height => Area.Width);
         end if;
      else
         Parent (Buffer).Fill_Rect (Area);
      end if;
   end Fill_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   overriding procedure Copy_Rect
     (Src_Buffer  : HAL.Bitmap.Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out DMA2D_Bitmap_Buffer;
      Dst_Pt      : Point;
      Bg_Buffer   : HAL.Bitmap.Bitmap_Buffer'Class;
      Bg_Pt       : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
      use type System.Address;
      DMA_Buf_Src : constant DMA2D_Buffer := To_DMA2D_Buffer (Src_Buffer);
      DMA_Buf_Dst : constant DMA2D_Buffer := To_DMA2D_Buffer (Dst_Buffer);
      DMA_Buf_Bg  : DMA2D_Buffer := To_DMA2D_Buffer (Bg_Buffer);
      X0_Src      : Natural := Src_Pt.X;
      Y0_Src      : Natural := Src_Pt.Y;
      X0_Dst      : Natural := Dst_Pt.X;
      Y0_Dst      : Natural := Dst_Pt.Y;
      X0_Bg       : Natural := Bg_Pt.X;
      Y0_Bg       : Natural := Bg_Pt.Y;
      W           : Natural := Width;
      H           : Natural := Height;
   begin
      if Src_Buffer.Swapped then
         X0_Src := Src_Pt.Y;
         Y0_Src := Src_Buffer.Width - Src_Pt.X - Width;
      end if;

      if Dst_Buffer.Swapped then
         X0_Dst := Dst_Pt.Y;
         Y0_Dst := Dst_Buffer.Width - Dst_Pt.X - Width;
         W := Height;
         H := Width;
      end if;

      if Bg_Buffer.Memory_Address = System.Null_Address then
         DMA_Buf_Bg := STM32.DMA2D.Null_Buffer;
         X0_Bg := 0;
         Y0_Bg := 0;
      elsif Bg_Buffer.Swapped then
         X0_Bg := Bg_Pt.Y;
         Y0_Bg := Bg_Buffer.Width - Bg_Pt.X - Width;
      end if;

      Cortex_M.Cache.Clean_DCache (Src_Buffer.Memory_Address, Src_Buffer.Buffer_Size);

      DMA2D_Copy_Rect
        (DMA_Buf_Src, X0_Src, Y0_Src,
         DMA_Buf_Dst, X0_Dst, Y0_Dst,
         DMA_Buf_Bg, X0_Bg, Y0_Bg,
         W, H,
         Synchronous => Synchronous);
   end Copy_Rect;

   overriding procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out DMA2D_Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
   begin
      Copy_Rect
        (Src_Buffer  => Src_Buffer,
         Src_Pt      => Src_Pt,
         Dst_Buffer  => Dst_Buffer,
         Dst_Pt      => Dst_Pt,
         Bg_Buffer   => Null_Buffer,
         Bg_Pt       => (0, 0),
         Width       => Width,
         Height      => Height,
         Synchronous => Synchronous);
   end Copy_Rect;

   -------------------
   -- Wait_Transfer --
   -------------------

   procedure Wait_Transfer (Buffer : DMA2D_Bitmap_Buffer)
   is
   begin
      DMA2D_Wait_Transfer;
      Cortex_M.Cache.Clean_Invalidate_DCache
        (Start => Buffer.Addr,
         Len   => Buffer.Buffer_Size);
   end Wait_Transfer;

end STM32.DMA2D_Bitmap;
