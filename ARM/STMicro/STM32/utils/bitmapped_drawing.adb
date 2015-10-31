------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with STM32F4; use STM32F4;

package body Bitmapped_Drawing is

   ---------------
   -- Draw_Char --
   ---------------

   procedure Draw_Char
     (Start      : Display_Point;
      Char       : Character;
      Font       : BMP_Font;
      Foreground : Color;
      Background : Color)
   is
   begin
      for H in 0 .. Char_Height (Font) - 1 loop
         for W in 0 .. Char_Width (Font) - 1 loop
            if (Data (Font, Char, H) and Mask (Font, W)) /= 0 then
               Set_Pixel (Start.X + W, Start.Y + H, Foreground);
            else
               Set_Pixel (Start.X + W, Start.Y + H, Background);
            end if;
         end loop;
      end loop;
   end Draw_Char;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Start          : Display_Point;
      Msg        : String;
      Font       : BMP_Font;
      Foreground : Color;
      Background : Color)
   is
      Count : Natural := 0;
   begin
      for C of Msg loop
         Draw_Char
           ((Start.X + Count * Char_Width (Font), Start.Y),
            C,
            Font,
            Foreground,
            Background);
         Count := Count + 1;
      end loop;
   end Draw_String;

   --  http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#Ada
   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (Start, Stop : Display_Point;
      Hue         : Color;
      Thickness   : Natural := 1)
   is
      DX     : constant Float := abs Float (Stop.X - Start.X);
      DY     : constant Float := abs Float (Stop.Y - Start.Y);
      Err    : Float;
      X      : Natural := Start.X;
      Y      : Natural := Start.Y;
      Step_X : Integer := 1;
      Step_Y : Integer := 1;

      ----------------
      -- Draw_Point --
      ----------------

      procedure Draw_Point (P : Display_Point) is
      begin
         if Thickness /= 1 then
            Draw_Line ((P.X - (Thickness / 2), P.Y),
                  (P.X + (Thickness / 2), P.Y),
                  Hue, 1);
            Draw_Line ((P.X, P.Y -(Thickness / 2)),
                  (P.X, P.Y + (Thickness / 2)),
                  Hue, 1);
         else
            Set_Pixel (P.X, P.Y, Hue);
         end if;
      end Draw_Point;
      pragma Inline (Draw_Point);

   begin
      if Start.X > Stop.X then
         Step_X := -1;
      end if;

      if Start.Y > Stop.Y then
         Step_Y := -1;
      end if;

      if DX > DY then
         Err := DX / 2.0;
         while X /= Stop.X loop
            Draw_Point ((X, Y));
            Err := Err - DY;
            if Err < 0.0 then
               Y := Y + Step_Y;
               Err := Err + DX;
            end if;
            X := X + Step_X;
         end loop;
      else
         Err := DY / 2.0;
         while Y /= Stop.Y loop
            Draw_Point ((X, Y));
            Err := Err - DX;
            if Err < 0.0 then
               X := X + Step_X;
               Err := Err + DY;
            end if;
            Y := Y + Step_Y;
         end loop;
      end if;

      Draw_Point ((X, Y));
   end Draw_Line;

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle
     (Start, Stop : Display_Point;
      Hue         : Color;
      Thickness   : Natural := 1)
   is
   begin
      Draw_Line (Start, (Stop.X, Start.Y), Hue, Thickness);
      Draw_Line ((Stop.X, Start.Y), Stop, Hue, Thickness);
      Draw_Line (Stop, (Start.X, Stop.Y), Hue, Thickness);
      Draw_Line ((Start.X, Stop.Y), Start, Hue, Thickness);
   end Draw_Rectangle;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle
     (Start, Stop : Display_Point;
      Hue         : Color)
   is
      P1 :  Display_Point := Start;
      P2 : Display_Point := (Start.X, Stop.Y);
   begin
      loop
         Draw_Line (P2, P1, Hue);
         exit when P2.X = Stop.X;
         P1.X := P1.X + 1;
         P2.X := P2.X + 1;
      end loop;
   end Fill_Rectangle;

   --  http://rosettacode.org/wiki/Bitmap/B%C3%A9zier_curves/Cubic
   ------------------
   -- Cubic_Bezier --
   ------------------

   procedure Cubic_Bezier
     (P1, P2, P3, P4 : Display_Point;
      Hue            : Color;
      N              : Positive := 20;
      Thickness      : Natural := 1)
   is
      Points : array (0 .. N) of Display_Point;
   begin
      for I in Points'Range loop
         declare
            T : constant Float := Float (I) / Float (N);
            A : constant Float := (1.0 - T)**3;
            B : constant Float := 3.0 * T * (1.0 - T)**2;
            C : constant Float := 3.0 * T**2 * (1.0 - T);
            D : constant Float := T**3;
         begin
            Points (I).X := Natural (A * Float (P1.X) +
                                    B * Float (P2.X) +
                                    C * Float (P3.X) +
                                    D * Float (P4.X));
            Points (I).Y := Natural (A * Float (P1.Y) +
                                    B * Float (P2.Y) +
                                    C * Float (P3.Y) +
                                    D * Float (P4.Y));
         end;
      end loop;
      for I in Points'First .. Points'Last - 1 loop
         Draw_Line (Points (I), Points (I + 1), Hue, Thickness => Thickness);
      end loop;
   end Cubic_Bezier;

   --  http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm
   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (Center : Display_Point;
      Radius : Natural;
      Hue    : Color;
      Fill   : Boolean := False)
   is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X     : Integer := 0;
      Y     : Integer := Radius;
   begin
      if Fill then
         for Cnt in 1 .. Radius loop
            Draw_Circle (Center, Cnt, Hue, False);
         end loop;
         return;
      end if;

      Set_Pixel (Center.X, Center.Y + Radius, Hue);
      Set_Pixel (Center.X, Center.Y - Radius, Hue);
      Set_Pixel (Center.X + Radius, Center.Y, Hue);
      Set_Pixel (Center.X - Radius, Center.Y, Hue);
      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;
         Set_Pixel (Center.X + X, Center.Y + Y, Hue);
         Set_Pixel (Center.X - X, Center.Y + Y, Hue);
         Set_Pixel (Center.X + X, Center.Y - Y, Hue);
         Set_Pixel (Center.X - X, Center.Y - Y, Hue);
         Set_Pixel (Center.X + Y, Center.Y + X, Hue);
         Set_Pixel (Center.X - Y, Center.Y + X, Hue);
         Set_Pixel (Center.X + Y, Center.Y - X, Hue);
         Set_Pixel (Center.X - Y, Center.Y - X, Hue);
      end loop;
   end Draw_Circle;

end Bitmapped_Drawing;
