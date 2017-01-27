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

with Interfaces; use Interfaces;

package body Bitmap_Color_Conversion is

   --------------------------
   -- Bitmap_Color_To_Word --
   --------------------------

   function Bitmap_Color_To_Word
     (Mode : Bitmap_Color_Mode; Col : Bitmap_Color)
      return UInt32
   is
      Ret : UInt32 := 0;

      procedure Add_Byte
        (Value : Byte; Pos : Natural; Size : Positive) with Inline;

      function Luminance return Byte;

      --------------
      -- Add_Byte --
      --------------

      procedure Add_Byte
        (Value : Byte; Pos : Natural; Size : Positive)
      is
         Val : constant UInt32 :=
                 Shift_Left
                   (UInt32
                      (Shift_Right (Value,
                                    abs (Integer (Size) - 8))),
                    Pos);
      begin
         Ret := Ret or Val;
      end Add_Byte;

      ---------------
      -- Luminance --
      ---------------

      function Luminance return Byte
      is
      begin
         return Byte
           (Shift_Right
              (UInt32 (Col.Red) * 3 + UInt32 (Col.Blue) + UInt32 (Col.Green) * 4,
               3));
      end Luminance;

   begin
      case Mode is
         when ARGB_8888 =>
            Add_Byte (Col.Alpha, 24, 8);
            Add_Byte (Col.Red,   16, 8);
            Add_Byte (Col.Green,  8, 8);
            Add_Byte (Col.Blue,   0, 8);

         when RGB_888 =>
            Add_Byte (Col.Red,   16, 8);
            Add_Byte (Col.Green,  8, 8);
            Add_Byte (Col.Blue,   0, 8);

         when RGB_565 =>
            Add_Byte (Col.Red,   11, 5);
            Add_Byte (Col.Green,  5, 6);
            Add_Byte (Col.Blue,   0, 5);

         when ARGB_1555 =>
            Add_Byte (Col.Alpha, 15, 1);
            Add_Byte (Col.Red,   10, 5);
            Add_Byte (Col.Green,  5, 5);
            Add_Byte (Col.Blue,   0, 5);

         when ARGB_4444 =>
            Add_Byte (Col.Alpha, 12, 4);
            Add_Byte (Col.Red,    8, 4);
            Add_Byte (Col.Green,  4, 4);
            Add_Byte (Col.Blue,   0, 4);

         when L_8 =>
            Add_Byte (Luminance, 0, 8);

         when AL_44 =>
            Add_Byte (Col.Alpha, 4, 4);
            Add_Byte (Luminance, 0, 4);

         when AL_88 =>
            Add_Byte (Col.Alpha, 8, 8);
            Add_Byte (Luminance, 0, 8);

         when L_4 =>
            Add_Byte (Luminance, 0, 4);

         when A_8 =>
            Add_Byte (Col.Alpha, 0, 8);

         when A_4 =>
            Add_Byte (Col.Alpha, 0, 4);
      end case;

      return Ret;
   end Bitmap_Color_To_Word;

   --------------------------
   -- Word_To_Bitmap_Color --
   --------------------------

   function Word_To_Bitmap_Color
     (Mode : Bitmap_Color_Mode; Col : UInt32)
      return Bitmap_Color
   is

      function Get_Byte
        (Pos : Natural; Size : Positive) return Byte with Inline;

      --------------
      -- Get_Byte --
      --------------

      function Get_Byte
        (Pos : Natural; Size : Positive) return Byte
      is
         Ret : Byte;
         Mask : constant UInt32 := Shift_Left (2 ** Size - 1, Pos);
      begin
         Ret := Byte (Shift_Right (Col and Mask, Pos));

         if Size = 8 then
            return Ret;
         elsif Size = 1 then
            return (if Ret > 0 then 255 else 0);
         elsif Size >= 4 then
            --  return [7..3] => Ret[4 .. 0], [2 .. 0] => Ret[4 .. 2]
            return Shift_Left (Ret, 8 - Size) or
              Shift_Right (Ret, 2 * Size - 8);
         else
            raise Constraint_Error with "Unsupported color component size";
         end if;
      end Get_Byte;

      A, R, G, B : Byte;
   begin
      case Mode is
         when ARGB_8888 =>
            A := Get_Byte (24, 8);
            R := Get_Byte (16, 8);
            G := Get_Byte (8, 8);
            B := Get_Byte (0, 8);

         when RGB_888 =>
            A := 255;
            R := Get_Byte (16, 8);
            G := Get_Byte (8, 8);
            B := Get_Byte (0, 8);

         when RGB_565 =>
            A := 255;
            R := Get_Byte (11, 5);
            G := Get_Byte (5, 6);
            B := Get_Byte (0, 5);

         when ARGB_1555 =>
            A := Get_Byte (15, 1);
            R := Get_Byte (10, 5);
            G := Get_Byte (5, 5);
            B := Get_Byte (0, 5);

         when ARGB_4444 =>
            A := Get_Byte (12, 4);
            R := Get_Byte (8, 4);
            G := Get_Byte (4, 4);
            B := Get_Byte (0, 4);

         when L_8 =>
            A := 255;
            R := Get_Byte (0, 8);
            G := R;
            B := R;

         when AL_44 =>
            A := Get_Byte (4, 4);
            R := Get_Byte (0, 4);
            G := R;
            B := R;

         when AL_88 =>
            A := Get_Byte (8, 8);
            R := Get_Byte (0, 8);
            G := R;
            B := R;

         when L_4 =>
            A := 255;
            R := Get_Byte (0, 4);
            G := R;
            B := R;

         when A_8 =>
            A := Get_Byte (0, 8);
            R := 255;
            G := 255;
            B := 255;

         when A_4 =>
            A := Get_Byte (0, 4);
            R := 255;
            G := 255;
            B := 255;
      end case;

      return (Alpha => A,
              Red   => R,
              Green => G,
              Blue  => B);
   end Word_To_Bitmap_Color;

end Bitmap_Color_Conversion;
