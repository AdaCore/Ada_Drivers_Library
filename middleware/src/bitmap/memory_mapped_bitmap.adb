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

with System.Storage_Elements; use System.Storage_Elements;
with Bitmap_Color_Conversion; use Bitmap_Color_Conversion;

package body Memory_Mapped_Bitmap is

   procedure Handle_Swap
     (Buffer : Bitmap_Buffer'Class;
      X      : in out Natural;
      Y      : in out Natural);

   -----------------
   -- Handle_Swap --
   -----------------

   procedure Handle_Swap
     (Buffer : Bitmap_Buffer'Class;
      X      : in out Natural;
      Y      : in out Natural)
   is
      Tmp : Natural;
   begin
      if not Buffer.Swapped then
         return;
      end if;

      Tmp := X;
      X := Y;
      Y := Buffer.Width - Tmp - 1;
   end Handle_Swap;

   ---------------
   -- Set_Pixel --
   ---------------

   overriding
   procedure Set_Pixel
     (Buffer : in out Memory_Mapped_Bitmap_Buffer;
      Pt     : Point;
      Value  : Bitmap_Color)
   is
      Col : constant UInt32 :=
              Bitmap_Color_To_Word (Buffer.Color_Mode, Value);
   begin
      Set_Pixel (Bitmap_Buffer'Class (Buffer), Pt, Col);
   end Set_Pixel;

   ---------------
   -- Set_Pixel --
   ---------------

   overriding
   procedure Set_Pixel
     (Buffer : in out Memory_Mapped_Bitmap_Buffer;
      Pt     : Point;
      Value  : UInt32)
   is
      X0 : Natural := Pt.X;
      Y0 : Natural := Pt.Y;
      Offset : Natural;

   begin
      if Pt.X >= Buffer.Width
        or else Pt.Y >= Buffer.Height
      then
         return;
      end if;

      if Buffer.Swapped then
         Handle_Swap (Buffer, X0, Y0);
         Offset := X0 + Y0 * Buffer.Height;

      else
         Offset := Pt.X + Pt.Y * Buffer.Width;
      end if;

      case Buffer.Color_Mode is
         when ARGB_8888 =>
            declare
               Pixel : aliased UInt32
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 4);
            begin
               Pixel := Value;
            end;

         when RGB_888 =>
            declare
               Pixel_B : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3);
               Pixel_G : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 1);
               Pixel_R : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 2);
               R       : constant UInt8 :=
                           UInt8 (Shift_Right (Value and 16#FF0000#, 16));
               G       : constant UInt8 :=
                           UInt8 (Shift_Right (Value and 16#FF00#, 8));
               B       : constant UInt8 := UInt8 (Value and 16#FF#);
            begin
               Pixel_B := B;
               Pixel_G := G;
               Pixel_R := R;
            end;

         when ARGB_1555 | ARGB_4444 | RGB_565 | AL_88 =>
            declare
               Pixel : aliased UInt16
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 2);
            begin
               Pixel := UInt16 (Value and 16#FF_FF#);
            end;

         when L_8 | AL_44 | A_8 =>
            declare
               Pixel : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset);
            begin
               Pixel := UInt8 (Value and 16#FF#);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset / 2);
            begin
               if Offset mod 2 = 0 then
                  Pixel :=
                    (Pixel and 16#0F#) or
                    Shift_Left (UInt8 (Value and 16#0F#), 4);
               else
                  Pixel := (Pixel and 16#F0#) or UInt8 (Value and 16#0F#);
               end if;
            end;

      end case;
   end Set_Pixel;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   overriding
   procedure Set_Pixel_Blend
     (Buffer : in out Memory_Mapped_Bitmap_Buffer;
      Pt     : Point;
      Value  : Bitmap_Color)
   is
      Col : Bitmap_Color;
      FgA, FgR, FgG, FgB : Float;
      BgA, BgR, BgG, BgB : Float;
      RA, RR, RG, RB     : Float;

   begin
      if Value.Alpha = 255 then
         Set_Pixel (Memory_Mapped_Bitmap_Buffer'Class (Buffer), Pt, Value);
      else
         Col := Pixel (Bitmap_Buffer'Class (Buffer), Pt);
         BgA := Float (Col.Alpha) / 255.0;
         BgR := Float (Col.Red) / 255.0;
         BgG := Float (Col.Green) / 255.0;
         BgB := Float (Col.Blue) / 255.0;

         FgA := Float (Value.Alpha) / 255.0;
         FgR := Float (Value.Red) / 255.0;
         FgG := Float (Value.Green) / 255.0;
         FgB := Float (Value.Blue) / 255.0;

         RA := 1.0 - (1.0 - FgA) * (1.0 - FgB);
         RR := FgR * FgA / RA + BgR * BgA * (1.0 - FgA) / RA;
         RG := FgG * FgA / RA + BgG * BgA * (1.0 - FgA) / RA;
         RB := FgB * FgA / RA + BgB * BgA * (1.0 - FgA) / RA;

         Col := (Alpha => UInt8 (RA * 255.0),
                 Red   => UInt8 (RR * 255.0),
                 Green => UInt8 (RG * 255.0),
                 Blue  => UInt8 (RB * 255.0));
         Set_Pixel (Bitmap_Buffer'Class (Buffer), Pt, Col);
      end if;
   end Set_Pixel_Blend;

   -----------
   -- Pixel --
   -----------

   overriding
   function Pixel
     (Buffer : Memory_Mapped_Bitmap_Buffer;
      Pt     : Point)
      return Bitmap_Color
   is
      Native_Color : UInt32;
   begin
      Native_Color := Pixel (Bitmap_Buffer'Class (Buffer), Pt);

      return Word_To_Bitmap_Color (Buffer.Color_Mode, Native_Color);
   end Pixel;

   -----------
   -- Pixel --
   -----------

   overriding
   function Pixel
     (Buffer : Memory_Mapped_Bitmap_Buffer;
      Pt     : Point)
      return UInt32
   is
      X0 : Natural := Pt.X;
      Y0 : Natural := Pt.Y;
      Offset : Natural;

   begin
      if Buffer.Swapped then
         Handle_Swap (Buffer, X0, Y0);
         Offset := X0 + Y0 * Buffer.Height;

      else
         Offset := Pt.X + Pt.Y * Buffer.Width;
      end if;

      case Buffer.Color_Mode is
         when ARGB_8888 =>
            declare
               Pixel : aliased UInt32
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 4);
            begin
               return Pixel;
            end;

         when RGB_888 =>
            declare
               Pixel_B : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3);
               Pixel_G : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 1);
               Pixel_R : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 2);
            begin
               return Shift_Left (UInt32 (Pixel_R), 16)
                 or Shift_Left (UInt32 (Pixel_G), 8) or UInt32 (Pixel_B);
            end;

         when ARGB_1555 | ARGB_4444 | RGB_565 | AL_88 =>
            declare
               Pixel : aliased UInt16
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 2);
            begin
               return UInt32 (Pixel);
            end;

         when L_8 | AL_44 | A_8 =>
            declare
               Pixel : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset);
            begin
               return UInt32 (Pixel);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset / 2);
            begin
               if Offset mod 2 = 0 then
                  return UInt32 (Shift_Right (Pixel and 16#F0#, 4));
               else
                  return UInt32 (Pixel and 16#0F#);
               end if;
            end;
      end case;
   end Pixel;

   -----------------
   -- Buffer_Size --
   -----------------

   overriding
   function Buffer_Size (Buffer : Memory_Mapped_Bitmap_Buffer) return Natural
   is
   begin
      return Bits_Per_Pixel (Buffer.Color_Mode) *
        Buffer.Width * Buffer.Height / 8;
   end Buffer_Size;
end Memory_Mapped_Bitmap;
