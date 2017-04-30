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

   function Blend_Colors
     (Fg, Bg : Bitmap_Color) return Bitmap_Color;

   ------------------
   -- Blend_Colors --
   ------------------

   function Blend_Colors
     (Fg, Bg : Bitmap_Color) return Bitmap_Color
   is
      FgA, FgR, FgG, FgB : Float;
      BgA, BgR, BgG, BgB : Float;
      RA, RR, RG, RB     : Float;

   begin
      if Fg.Alpha = 255 then
         return Fg;
      elsif Fg.Alpha = 0 then
         return Bg;
      end if;

      BgA := Float (Bg.Alpha) / 255.0;
      BgR := Float (Bg.Red) / 255.0;
      BgG := Float (Bg.Green) / 255.0;
      BgB := Float (Bg.Blue) / 255.0;

      FgA := Float (Fg.Alpha) / 255.0;
      FgR := Float (Fg.Red) / 255.0;
      FgG := Float (Fg.Green) / 255.0;
      FgB := Float (Fg.Blue) / 255.0;

      RA := 1.0 - (1.0 - FgA) * (1.0 - FgB);
      RR := FgR * FgA + BgR * BgA * (1.0 - FgA);
      RG := FgG * FgA + BgG * BgA * (1.0 - FgA);
      RB := FgB * FgA + BgB * BgA * (1.0 - FgA);

      return (Alpha => UInt8 (RA * 255.0),
              Red   => UInt8 (RR * 255.0),
              Green => UInt8 (RG * 255.0),
              Blue  => UInt8 (RB * 255.0));
   end Blend_Colors;

   ----------
   -- Swap --
   ----------

   function Swap
     (Buffer : Memory_Mapped_Bitmap_Buffer'Class;
      Pt     : Point) return Point
   is
   begin
      if not Buffer.Swapped then
         return Pt;
      else
         return (Pt.Y, Buffer.Width - 1 - Pt.X);
      end if;
   end Swap;

   ------------------
   -- Pixel_Offset --
   ------------------

   function Pixel_Offset
     (Buffer : Memory_Mapped_Bitmap_Buffer'Class;
      Pt     : Point) return Storage_Offset
   is
      Pt0 : Point;
   begin
      if not Buffer.Swapped then
         return Storage_Offset (Pt.X + Pt.Y * Buffer.Width);
      else
         Pt0 := Swap (Buffer, Pt);
         return Storage_Offset (Pt0.X + Pt0.Y * Buffer.Height);
      end if;
   end Pixel_Offset;

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
      Offset : constant Storage_Offset := Pixel_Offset (Buffer, Pt);

   begin
      if Pt.X >= Buffer.Width
        or else Pt.Y >= Buffer.Height
      then
         return;
      end if;

      case Buffer.Color_Mode is
         when ARGB_8888 =>
            declare
               Pixel : aliased UInt32
                 with Import,
                      Address => Buffer.Addr + Offset * 4;
            begin
               Pixel := Value;
            end;

         when RGB_888 =>
            declare
               Pixel_B : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Offset * 3;
               Pixel_G : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Offset * 3 + 1;
               Pixel_R : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Offset * 3 + 2;
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
                      Address => Buffer.Addr + Offset * 2;
            begin
               Pixel := UInt16 (Value and 16#FF_FF#);
            end;

         when L_8 | AL_44 | A_8 =>
            declare
               Pixel : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Offset;
            begin
               Pixel := UInt8 (Value and 16#FF#);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Offset / 2;
            begin
               if Offset mod 2 = 0 then
                  Pixel :=
                    (Pixel and 16#0F#) or
                    Shift_Left (UInt8 (Value and 16#0F#), 4);
               else
                  Pixel := (Pixel and 16#F0#) or UInt8 (Value and 16#0F#);
               end if;
            end;

         when M_1 =>
            declare
               type Mono_BM is array (Natural range <>) of Bit with Pack;

               BM : aliased Mono_BM (0 .. (Buffer.Height * Buffer.Width) - 1)
                 with Import, Address => Buffer.Addr;
            begin
               BM (Natural (Offset)) := (if Value /= 0 then 1 else 0);
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

   begin
      if Value.Alpha = 255 then
         Set_Pixel (Memory_Mapped_Bitmap_Buffer'Class (Buffer), Pt, Value);
      else
         Col := Blend_Colors
           (Fg => Value,
            Bg => Pixel (Bitmap_Buffer'Class (Buffer), Pt));
         Set_Pixel (Bitmap_Buffer'Class (Buffer), Pt, Col);
      end if;
   end Set_Pixel_Blend;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   overriding
   procedure Set_Pixel_Blend
     (Buffer : in out Memory_Mapped_Bitmap_Buffer;
      Pt     : Point;
      Value  : UInt32)
   is
      Col : constant Bitmap_Color :=
              Word_To_Bitmap_Color (Buffer.Color_Mode, Value);
   begin
      Set_Pixel_Blend (Bitmap_Buffer'Class (Buffer), Pt, Col);
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
      Offset : constant Storage_Offset := Pixel_Offset (Buffer, Pt);

   begin
      case Buffer.Color_Mode is
         when ARGB_8888 =>
            declare
               Pixel : aliased UInt32
                 with Import,
                      Address => Buffer.Addr + Offset * 4;
            begin
               return Pixel;
            end;

         when RGB_888 =>
            declare
               Pixel_B : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Offset * 3;
               Pixel_G : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + (Offset * 3 + 1);
               Pixel_R : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + (Offset * 3 + 2);
            begin
               return Shift_Left (UInt32 (Pixel_R), 16)
                 or Shift_Left (UInt32 (Pixel_G), 8) or UInt32 (Pixel_B);
            end;

         when ARGB_1555 | ARGB_4444 | RGB_565 | AL_88 =>
            declare
               Pixel : aliased UInt16
                 with Import,
                      Address => Buffer.Addr + Offset * 2;
            begin
               return UInt32 (Pixel);
            end;

         when L_8 | AL_44 | A_8 =>
            declare
               Pixel : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Offset;
            begin
               return UInt32 (Pixel);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased UInt8
                 with Import,
                      Address => Buffer.Addr + Offset / 2;
            begin
               if Offset mod 2 = 0 then
                  return UInt32 (Shift_Right (Pixel and 16#F0#, 4));
               else
                  return UInt32 (Pixel and 16#0F#);
               end if;
            end;
         when M_1 =>
            declare
               type Mono_BM is array (Natural range <>) of Bit with Pack;

               BM : aliased Mono_BM (0 .. (Buffer.Height * Buffer.Width) - 1)
                 with Import, Address => Buffer.Addr;
            begin
               return UInt32 (BM (Natural (Offset)));
            end;
      end case;
   end Pixel;

   ---------------
   -- Copy_Rect --
   ---------------

   overriding procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Memory_Mapped_Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean := True;
      Clean_Cache : Boolean := True)
   is
      use type System.Address;
      procedure Copy_Rect_Pixels;
      --  Copies the two buffers pixel by pixel

      ----------------------
      -- Copy_Rect_Pixels --
      ----------------------

      procedure Copy_Rect_Pixels
      is
      begin
         if Src_Buffer.Color_Mode = Dst_Buffer.Color_Mode then
            --  Device to memory copy: need to use Src_Buffer.Pixel abstraction
            --  No need for color conversion
            declare
               Pix : UInt32;
            begin
               for X in 0 .. Width - 1 loop
                  for Y in 0 .. Height - 1 loop
                     --  Simple Copy_Rect
                     Pix := Src_Buffer.Pixel (Src_Pt + (X, Y));
                     Set_Pixel
                       (Memory_Mapped_Bitmap_Buffer'Class (Dst_Buffer),
                        Dst_Pt + (X, Y),
                        Pix);
                  end loop;
               end loop;
            end;

         else
            --  Device to memory copy: need to use Src_Buffer.Pixel abstraction
            --  Requires color conversion
            declare
               Pix : Bitmap_Color;
            begin
               for X in 0 .. Width - 1 loop
                  for Y in 0 .. Height - 1 loop
                     --  Simple Copy_Rect
                     Pix := Src_Buffer.Pixel (Src_Pt + (X, Y));
                     Set_Pixel
                       (Memory_Mapped_Bitmap_Buffer'Class (Dst_Buffer),
                        Dst_Pt + (X, Y),
                        Pix);
                  end loop;
               end loop;
            end;
         end if;
      end Copy_Rect_Pixels;

   begin
      if Src_Pt.X + Width > Src_Buffer.Width
        or else Src_Pt.Y + Height > Src_Buffer.Height
        or else Dst_Pt.X + Width > Dst_Buffer.Width
        or else Dst_Pt.Y + Height > Dst_Buffer.Height
      then
         raise Constraint_Error with "X, Y not in range in call to Copy_Rect";
      end if;

      if Src_Buffer.Kind = Memory_Mapped
        and then Src_Buffer.Color_Mode = Dst_Buffer.Color_Mode
        and then Bits_Per_Pixel (Src_Buffer.Color_Mode) >= 8
      then
         declare
            S          : Memory_Mapped_Bitmap_Buffer'Class :=
                           Memory_Mapped_Bitmap_Buffer'Class (Src_Buffer);
            D          : Memory_Mapped_Bitmap_Buffer := Dst_Buffer;
            S_Pt, D_Pt : Point;
         begin
            if S.Swapped and then Dst_Buffer.Swapped then
               --  The two buffers are swapped: let's revert the logic
               S_Pt := Swap (S, Src_Pt);
               D_Pt := Swap (Dst_Buffer, Dst_Pt);
               S.Width := Src_Buffer.Height;
               S.Height := Src_Buffer.Width;
               S.Swapped := False;
               D.Width := Dst_Buffer.Height;
               D.Height := Dst_Buffer.Width;
               D.Swapped := False;
               Copy_Rect
                 (S, S_Pt, D, D_Pt,
                  Width       => Height,
                  Height      => Width,
                  Synchronous => Synchronous,
                  Clean_Cache => Clean_Cache);

            elsif S.Swapped or else D.Swapped then
               --  Only one of the buffer is swapped: no optimisation is
               --  possible, so let's fallback to pixel by pixel copy
               Copy_Rect_Pixels;

            else
               --  The two buffers are memory buffers, non-swapped, and have
               --  the same color mode, with byte-aligned pixels: let's
               --  optimize the memory transfer.
               for Y in 0 .. Height - 1 loop
                  declare
                     Bytes    : constant Natural :=
                                  Bits_Per_Pixel (S.Color_Mode) * Width / 8;
                     S_Offset : constant Storage_Offset :=
                                  Storage_Offset (Bits_Per_Pixel (S.Color_Mode)) *
                                  Pixel_Offset (S, (Src_Pt.X, Y + Src_Pt.Y)) /
                                  8;
                     D_Offset : constant Storage_Offset :=
                                  Storage_Offset (Bits_Per_Pixel (S.Color_Mode)) *
                                  Pixel_Offset (D, (Dst_Pt.X, Y + Dst_Pt.Y)) /
                                  8;
                     S_Line   : constant UInt8_Array (1 .. Bytes)
                       with Import, Address => S.Addr + S_Offset;
                     D_Line   : UInt8_Array (1 .. Bytes)
                       with Import, Address => D.Addr + D_Offset;
                  begin
                     D_Line := S_Line;
                  end;
               end loop;
            end if;
         end;

      else
         Copy_Rect_Pixels;
      end if;
   end Copy_Rect;

   ---------------------
   -- Copy_Rect_Blend --
   ---------------------

   overriding procedure Copy_Rect_Blend
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Memory_Mapped_Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean := True;
      Clean_Cache : Boolean := True)
   is
      pragma Unreferenced (Synchronous, Clean_Cache);
      Pix : Bitmap_Color;

   begin
      if Src_Pt.X + Width > Src_Buffer.Width
        or else Src_Pt.Y + Height > Src_Buffer.Height
        or else Dst_Pt.X + Width > Dst_Buffer.Width
        or else Dst_Pt.Y + Height > Dst_Buffer.Height
      then
         raise Constraint_Error with "X, Y not in range in call to Copy_Rect";
      end if;

      for X in 0 .. Width - 1 loop
         for Y in 0 .. Height - 1 loop
            Pix := Src_Buffer.Pixel (Src_Pt + (X, Y));
            Set_Pixel_Blend
              (Memory_Mapped_Bitmap_Buffer'Class (Dst_Buffer),
               Dst_Pt + (X, Y),
               Pix);
         end loop;
      end loop;
   end Copy_Rect_Blend;

   -----------------
   -- Buffer_Size --
   -----------------

   overriding
   function Buffer_Size (Buffer : Memory_Mapped_Bitmap_Buffer) return Natural
   is
   begin
      return
        Bits_Per_Pixel (Buffer.Color_Mode) * Buffer.Width * Buffer.Height / 8;
   end Buffer_Size;

end Memory_Mapped_Bitmap;
