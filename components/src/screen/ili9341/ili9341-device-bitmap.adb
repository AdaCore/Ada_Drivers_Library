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

pragma Ada_2022;

with ILI9341.Regs; use ILI9341.Regs;
with Bitmap_Color_Conversion;
with System.Storage_Elements;

package body ILI9341.Device.Bitmap is
   use HAL;

   procedure Set_Cursor_Position
     (From : HAL.Bitmap.Point;
      To   : HAL.Bitmap.Point);

   ---------------
   -- Copy_Rect --
   ---------------

   overriding
   procedure Copy_Rect
     (Src_Buffer  : HAL.Bitmap.Bitmap_Buffer'Class;
      Src_Pt      : HAL.Bitmap.Point;
      Dst_Buffer  : in out Bitmap_Buffer;
      Dst_Pt      : HAL.Bitmap.Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
      use type HAL.Bitmap.Bitmap_Color_Mode;
      use type System.Storage_Elements.Storage_Offset;

      Mode : constant HAL.Bitmap.Bitmap_Color_Mode := Src_Buffer.Color_Mode;
      Pixel_Size : constant Positive := HAL.Bitmap.Bits_Per_Pixel (Mode);
      Src_Width : constant Natural := Src_Buffer.Width;
   begin
      --  TODO: speed up if Src_Buffet=This by r/w a bunch of pixels

      if Pixel_Size < 8
        or Mode = HAL.Bitmap.A_8
        or not Src_Buffer.Mapped_In_RAM
      then
         --  Fallback to the slow copying
         Soft_Drawing_Bitmap.Copy_Rect
           (Src_Buffer, Src_Pt, Parent (Dst_Buffer), Dst_Pt,
            Width, Height, Synchronous);

         return;
      end if;

      if Width = Src_Buffer.Width then
         --  Copy rectangle in one go
         Set_Cursor_Position
           (Dst_Pt,
            (Dst_Pt.X + Width - 1, Dst_Pt.Y + Height - 1));

         Send_Command (Connector, ILI9341_GRAM, []);

         Write_Pixels
           (This    => Connector,
            Mode    => Src_Buffer.Color_Mode,
            Address => Src_Buffer.Memory_Address +
              System.Storage_Elements.Storage_Offset
                (Src_Pt.Y * Width * Pixel_Size / 8),
            Count   => Width * Height,
            Repeat  => 1);
      else
         for DY in 0 .. 0 + Height - 1 loop
            Set_Cursor_Position
              ((Y => Dst_Pt.Y + DY, X => Dst_Pt.X),
               (Y => Dst_Pt.Y + DY, X => Dst_Pt.X + Width - 1));

            Send_Command (Connector, ILI9341_GRAM, []);

            Write_Pixels
              (This    => Connector,
               Mode    => Src_Buffer.Color_Mode,
               Address => Src_Buffer.Memory_Address +
                 System.Storage_Elements.Storage_Offset
                   ((Src_Pt.Y + DY) * Src_Width * Pixel_Size / 8),
               Count   => Width,
               Repeat  => 1);
         end loop;
      end if;
   end Copy_Rect;

   ----------
   -- Fill --
   ----------

   overriding
   procedure Fill (This : in out Bitmap_Buffer) is
   begin
      Set_Cursor_Position ((0, 0), (This.Width - 1, This.Height - 1));
      Send_Command (Connector, ILI9341_GRAM, []);

      Write_Pixels
        (This    => Connector,
         Mode    => HAL.Bitmap.RGB_888,
         Address => This.Source'Address,
         Count   => 1,
         Repeat  => This.Width * This.Height);
   end Fill;

   ----------------
   -- Get_Bitmap --
   ----------------

   function Get_Bitmap
     (This : not null access ILI9341_Device) return Bitmap_Buffer is
   begin
      return (This, Source => 0);
   end Get_Bitmap;

   -----------
   -- Pixel --
   -----------

   overriding
   function Pixel
     (This  : Bitmap_Buffer;
      Point : HAL.Bitmap.Point) return UInt32 is
   begin
      return Bitmap_Color_Conversion.Bitmap_Color_To_Word
        (HAL.Bitmap.RGB_888, This.Pixel (Point));
   end Pixel;

   -----------
   -- Pixel --
   -----------

   overriding
   function Pixel
     (This  : Bitmap_Buffer;
      Point : HAL.Bitmap.Point)
      return HAL.Bitmap.Bitmap_Color
   is
      Color : HAL.Bitmap.Bitmap_Color := (HAL.Bitmap.White);
   begin
      Set_Cursor_Position (Point, Point);

      Read_Pixels
        (This    => Connector,
         Cmd     => ILI9341_RAMRD,
         Address => Color'Address,
         Count   => 1);

      Color.Alpha := 255;

      return Color;
   end Pixel;

   -------------------------
   -- Set_Cursor_Position --
   -------------------------

   procedure Set_Cursor_Position
     (From : HAL.Bitmap.Point;
      To   : HAL.Bitmap.Point)
   is
      X1_High : constant UInt8 := UInt8 (Shift_Right (UInt16 (From.X), 8));
      X1_Low  : constant UInt8 := UInt8 (UInt16 (From.X) and 16#FF#);
      X2_High : constant UInt8 := UInt8 (Shift_Right (UInt16 (To.X), 8));
      X2_Low  : constant UInt8 := UInt8 (UInt16 (To.X) and 16#FF#);

      Y1_High : constant UInt8 := UInt8 (Shift_Right (UInt16 (From.Y), 8));
      Y1_Low  : constant UInt8 := UInt8 (UInt16 (From.Y) and 16#FF#);
      Y2_High : constant UInt8 := UInt8 (Shift_Right (UInt16 (To.Y), 8));
      Y2_Low  : constant UInt8 := UInt8 (UInt16 (To.Y) and 16#FF#);
   begin
      Send_Command
        (Connector,
         ILI9341_COLUMN_ADDR,
         [X1_High, X1_Low, X2_High, X2_Low]);

      Send_Command
        (Connector,
         ILI9341_PAGE_ADDR,
         [Y1_High, Y1_Low, Y2_High, Y2_Low]);
   end Set_Cursor_Position;

   ---------------
   -- Set_Pixel --
   ---------------

   overriding
   procedure Set_Pixel
     (This  : in out Bitmap_Buffer;
      Point : HAL.Bitmap.Point)
   is
   begin
      Set_Cursor_Position (Point, Point);
      Send_Command (Connector, ILI9341_GRAM, []);

      Write_Pixels
        (This    => Connector,
         Mode    => HAL.Bitmap.RGB_888,
         Address => This.Source'Address,
         Count   => 1,
         Repeat  => 1);
   end Set_Pixel;

   ----------------
   -- Set_Source --
   ----------------

   overriding
   procedure Set_Source
     (This : in out Bitmap_Buffer; Native : UInt32) is
   begin
      This.Source := Native;
   end Set_Source;

end ILI9341.Device.Bitmap;
