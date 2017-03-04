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

package body Bitmap_Color_Conversion is

   --------------------------
   -- Bitmap_Color_To_Word --
   --------------------------

   function Bitmap_Color_To_Word
     (Mode : Bitmap_Color_Mode; Col : Bitmap_Color)
      return UInt32
   is
      Ret : UInt32 := 0;

      procedure Add_UInt8
        (Value : UInt8; Pos : Natural; Size : Positive) with Inline;

      function Luminance return UInt8;

      --------------
      -- Add_UInt8 --
      --------------

      procedure Add_UInt8
        (Value : UInt8; Pos : Natural; Size : Positive)
      is
         Val : constant UInt32 :=
                 Shift_Left
                   (UInt32
                      (Shift_Right (Value,
                                    abs (Integer (Size) - 8))),
                    Pos);
      begin
         Ret := Ret or Val;
      end Add_UInt8;

      ---------------
      -- Luminance --
      ---------------

      function Luminance return UInt8
      is
      begin
         return UInt8
           (Shift_Right
              (UInt32 (Col.Red) * 3 + UInt32 (Col.Blue) + UInt32 (Col.Green) * 4,
               3));
      end Luminance;

   begin
      case Mode is
         when ARGB_8888 =>
            Add_UInt8 (Col.Alpha, 24, 8);
            Add_UInt8 (Col.Red,   16, 8);
            Add_UInt8 (Col.Green,  8, 8);
            Add_UInt8 (Col.Blue,   0, 8);

         when RGB_888 =>
            Add_UInt8 (Col.Red,   16, 8);
            Add_UInt8 (Col.Green,  8, 8);
            Add_UInt8 (Col.Blue,   0, 8);

         when RGB_565 =>
            Add_UInt8 (Col.Red,   11, 5);
            Add_UInt8 (Col.Green,  5, 6);
            Add_UInt8 (Col.Blue,   0, 5);

         when ARGB_1555 =>
            Add_UInt8 (Col.Alpha, 15, 1);
            Add_UInt8 (Col.Red,   10, 5);
            Add_UInt8 (Col.Green,  5, 5);
            Add_UInt8 (Col.Blue,   0, 5);

         when ARGB_4444 =>
            Add_UInt8 (Col.Alpha, 12, 4);
            Add_UInt8 (Col.Red,    8, 4);
            Add_UInt8 (Col.Green,  4, 4);
            Add_UInt8 (Col.Blue,   0, 4);

         when L_8 =>
            Add_UInt8 (Luminance, 0, 8);

         when AL_44 =>
            Add_UInt8 (Col.Alpha, 4, 4);
            Add_UInt8 (Luminance, 0, 4);

         when AL_88 =>
            Add_UInt8 (Col.Alpha, 8, 8);
            Add_UInt8 (Luminance, 0, 8);

         when L_4 =>
            Add_UInt8 (Luminance, 0, 4);

         when A_8 =>
            Add_UInt8 (Col.Alpha, 0, 8);

         when A_4 =>
            Add_UInt8 (Col.Alpha, 0, 4);
         when M_1 =>
            Ret := (if Luminance > 127 then 1 else 0);
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

      function Get_UInt8
        (Pos : Natural; Size : Positive) return UInt8 with Inline;

      --------------
      -- Get_UInt8 --
      --------------

      function Get_UInt8
        (Pos : Natural; Size : Positive) return UInt8
      is
         Ret : UInt8;
         Mask : constant UInt32 := Shift_Left (2 ** Size - 1, Pos);
      begin
         Ret := UInt8 (Shift_Right (Col and Mask, Pos));

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
      end Get_UInt8;

      A, R, G, B : UInt8;
   begin
      case Mode is
         when ARGB_8888 =>
            A := Get_UInt8 (24, 8);
            R := Get_UInt8 (16, 8);
            G := Get_UInt8 (8, 8);
            B := Get_UInt8 (0, 8);

         when RGB_888 =>
            A := 255;
            R := Get_UInt8 (16, 8);
            G := Get_UInt8 (8, 8);
            B := Get_UInt8 (0, 8);

         when RGB_565 =>
            A := 255;
            R := Get_UInt8 (11, 5);
            G := Get_UInt8 (5, 6);
            B := Get_UInt8 (0, 5);

         when ARGB_1555 =>
            A := Get_UInt8 (15, 1);
            R := Get_UInt8 (10, 5);
            G := Get_UInt8 (5, 5);
            B := Get_UInt8 (0, 5);

         when ARGB_4444 =>
            A := Get_UInt8 (12, 4);
            R := Get_UInt8 (8, 4);
            G := Get_UInt8 (4, 4);
            B := Get_UInt8 (0, 4);

         when L_8 =>
            A := 255;
            R := Get_UInt8 (0, 8);
            G := R;
            B := R;

         when AL_44 =>
            A := Get_UInt8 (4, 4);
            R := Get_UInt8 (0, 4);
            G := R;
            B := R;

         when AL_88 =>
            A := Get_UInt8 (8, 8);
            R := Get_UInt8 (0, 8);
            G := R;
            B := R;

         when L_4 =>
            A := 255;
            R := Get_UInt8 (0, 4);
            G := R;
            B := R;

         when A_8 =>
            A := Get_UInt8 (0, 8);
            R := 255;
            G := 255;
            B := 255;

         when A_4 =>
            A := Get_UInt8 (0, 4);
            R := 255;
            G := 255;
            B := 255;
         when M_1 =>
            A := 255;
            if Col /= 0 then
               R := 255;
               G := 255;
               B := 255;
            else
               A := 0;
               R := 0;
               G := 0;
            end if;

      end case;

      return (Alpha => A,
              Red   => R,
              Green => G,
              Blue  => B);
   end Word_To_Bitmap_Color;

end Bitmap_Color_Conversion;
