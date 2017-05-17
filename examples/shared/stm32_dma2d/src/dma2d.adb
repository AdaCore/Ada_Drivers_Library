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

with Last_Chance_Handler; pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with STM32.Board;         use STM32.Board;
with STM32.DMA2D;         use STM32.DMA2D;
with STM32.DMA2D_Bitmap;  use STM32.DMA2D_Bitmap;
with HAL;                 use HAL;
with HAL.Bitmap;          use HAL.Bitmap;

procedure Dma2d
is

   function Bitmap_Buffer return not null Any_Bitmap_Buffer;
   function Buffer return DMA2D_Buffer;

   -------------------
   -- Bitmap_Buffer --
   -------------------

   function Bitmap_Buffer return not null Any_Bitmap_Buffer is
   begin
      if Display.Hidden_Buffer (1).all not in DMA2D_Bitmap_Buffer then
         raise Program_Error with "We expect a DM2D buffer here";
      end if;

      return Display.Hidden_Buffer (1);
   end Bitmap_Buffer;

   ------------
   -- Buffer --
   ------------

   function Buffer return DMA2D_Buffer is
   begin
      return To_DMA2D_Buffer (Display.Hidden_Buffer (1).all);
   end Buffer;

   Width  : Natural;
   Height : Natural;
   X, Y   : Natural;

   L4_CLUT : array (UInt4) of DMA2D_Color;
   L8_CLUT : array (UInt8) of DMA2D_Color;

   type L4_Bitmap is array (UInt4) of UInt4 with Pack;
   type L8_Bitmap is array (UInt8) of UInt8 with Pack;

   L4_Data : L4_Bitmap with Size => 16 * 4;
   L8_Data : L8_Bitmap with Size => 256 * 8;

   L4_Buffer : constant DMA2D_Buffer :=
     (Color_Mode      => L4,
      Addr            => L4_Data (0)'Address,
      Width           => 4,
      Height          => 4,
      CLUT_Color_Mode => ARGB8888,
      CLUT_Addr       => L4_CLUT (0)'Address);
   L8_Buffer : constant DMA2D_Buffer :=
     (Color_Mode      => L8,
      Addr            => L8_Data (0)'Address,
      Width           => 16,
      Height          => 16,
      CLUT_Color_Mode => ARGB8888,
      CLUT_Addr       => L8_CLUT (0)'Address);
begin
   --  Initialize LCD
   Display.Initialize;
   Display.Initialize_Layer (1, HAL.Bitmap.ARGB_8888);

   Width := Display.Hidden_Buffer (1).Width;
   Height := Display.Hidden_Buffer (1).Height;

   loop
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Dark_Green);
      Bitmap_Buffer.Fill;

      --  Draw blue filled rectangle in the upper left corner
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Blue);
      Bitmap_Buffer.Fill_Rect ((Position => (0, 0),
                                Width    => Width / 2,
                                Height   => Height / 2));

      --  Drawn yellow rectangle outline in the lower left corner
      Bitmap_Buffer.Set_Source (HAL.Bitmap.Yellow);
      Bitmap_Buffer.Draw_Rect ((Position => (0, Height / 2),
                                Width  => Width / 2,
                                Height => Height / 2));

      --  Draw 10 red lines in the blue rectangle
      X := 0;
      Y := 0;
      while X < Width / 2 and then Y < ((Height / 2) - 10) loop
         for Cnt in 0 .. 10 loop
            Bitmap_Buffer.Set_Pixel ((X, Y + Cnt), HAL.Bitmap.Red);
         end loop;
         X := X + 1;
         Y := Y + 1;
      end loop;

      --  Draw 10 red blended lines in the yellow rectangle
      X := 0;
      Y := Height / 2;
      while X < Width / 2 and then Y < Height - 10 loop
         for Cnt in 0 .. 10 loop
            Bitmap_Buffer.Set_Source ((100, 255, 0, 0));
            Bitmap_Buffer.Set_Pixel_Blend ((X, Y + Cnt));
         end loop;
         X := X + 1;
         Y := Y + 1;
      end loop;

      --  Copy half of the screen to the other half
      Copy_Rect (Src_Buffer  => Bitmap_Buffer.all,
                 Src_Pt      => (0, 0),
                 Dst_Buffer  => Bitmap_Buffer.all,
                 Dst_Pt      => (Width / 2, 0),
                 Bg_Buffer   => STM32.DMA2D_Bitmap.Null_Buffer,
                 Bg_Pt       => (0, 0),
                 Width       => Width / 2,
                 Height      => Height,
                 Synchronous => True);


      --  Fill L4 CLUT
      for Index in UInt4 loop
         L4_CLUT (Index) := (255, 0, 0, UInt8 (Index) * 16);
      end loop;

      --  Fill L4 bitmap
      for Index in L4_Data'Range loop
         L4_Data (Index) := Index;
      end loop;

      --  Fill L8 CLUT
      for Index in UInt8 loop
         L8_CLUT (Index) := (255, 0, Index, 0);
      end loop;

      --  Fill L8 bitmap
      for Index in L8_Data'Range loop
         L8_Data (Index) := Index;
      end loop;


      for X in 0 .. 4 loop
         for Y in 0 .. 4 loop
            STM32.DMA2D.DMA2D_Copy_Rect
              (Src_Buffer  => L4_Buffer,
               X_Src       => 0,
               Y_Src       => 0,
               Dst_Buffer  => Buffer,
               X_Dst       => L4_Buffer.Width * X,
               Y_Dst       => (L4_Buffer.Height * Y),
               Bg_Buffer   => STM32.DMA2D.Null_Buffer,
               X_Bg        => 0,
               Y_Bg        => 0,
               Width       => L4_Buffer.Width,
               Height      => L4_Buffer.Height,
               Synchronous => True);

            STM32.DMA2D.DMA2D_Copy_Rect
              (Src_Buffer  => L8_Buffer,
               X_Src       => 0,
               Y_Src       => 0,
               Dst_Buffer  => Buffer,
               X_Dst       => L8_Buffer.Width * X,
               Y_Dst       => (L8_Buffer.Height * Y) + Height / 2,
               Bg_Buffer   => STM32.DMA2D.Null_Buffer,
               X_Bg        => 0,
               Y_Bg        => 0,
               Width       => L8_Buffer.Width,
               Height      => L8_Buffer.Height,
               Synchronous => True);
         end loop;
      end loop;

      Display.Update_Layers;
   end loop;
end Dma2d;
