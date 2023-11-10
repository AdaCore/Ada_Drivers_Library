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

package body ILI9341.Device.Bitmap is
   use HAL;

   procedure Set_Cursor_Position
     (From : HAL.Bitmap.Point;
      To   : HAL.Bitmap.Point);

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
