--  Take advantage of the DMA2D to accelerate some graphical operations on
--  bitmap surfaces

with System;
with HAL.Bitmap;

package STM32.DMA2D_Bitmap is

   type DMA2D_Bitmap_Buffer is new HAL.Bitmap.Bitmap_Buffer with null record;

   overriding procedure Set_Pixel
     (Buffer : DMA2D_Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : Word);

   overriding procedure Set_Pixel_Blend
     (Buffer : DMA2D_Bitmap_Buffer;
      X      : Natural;
      Y      : Natural;
      Value  : HAL.Bitmap.Bitmap_Color);

   overriding function Get_Pixel
     (Buffer : DMA2D_Bitmap_Buffer;
      X      : Natural;
      Y      : Natural) return Word;

   overriding procedure Fill
     (Buffer : DMA2D_Bitmap_Buffer;
      Color  : Word);

   overriding procedure Fill_Rect
     (Buffer : DMA2D_Bitmap_Buffer;
      Color  : Word;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);

   overriding procedure Copy_Rect
     (Src_Buffer : HAL.Bitmap.Bitmap_Buffer'Class;
      X_Src      : Natural;
      Y_Src      : Natural;
      Dst_Buffer : DMA2D_Bitmap_Buffer;
      X_Dst      : Natural;
      Y_Dst      : Natural;
      Bg_Buffer  : HAL.Bitmap.Bitmap_Buffer'Class;
      X_Bg       : Natural;
      Y_Bg       : Natural;
      Width      : Natural;
      Height     : Natural)
     with Pre =>
       Dst_Buffer.Color_Mode in HAL.Bitmap.ARGB_8888 .. HAL.Bitmap.ARGB_4444;

   overriding procedure Wait_Transfer (Buffer : DMA2D_Bitmap_Buffer);

   Null_Buffer : constant DMA2D_Bitmap_Buffer :=
                   (Addr       => System.Null_Address,
                    Width      => 0,
                    Height     => 0,
                    Color_Mode => HAL.Bitmap.L_8,
                    Swapped    => False);

end STM32.DMA2D_Bitmap;
