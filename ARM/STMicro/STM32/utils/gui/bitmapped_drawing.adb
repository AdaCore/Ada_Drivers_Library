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

with Interfaces; use Interfaces;

with STM32.LCD;

package body Bitmapped_Drawing is

   function Screen_Buffer return DMA2D_Buffer
   is
   begin
      return (Addr       => STM32.LCD.Current_Frame_Buffer (STM32.LCD.Layer1),
              Width      => STM32.LCD.Pixel_Width,
              Height     => STM32.LCD.Pixel_Height,
              Color_Mode => STM32.LCD.Get_Pixel_Fmt,
              Swap_X_Y   => STM32.LCD.SwapXY);
   end Screen_Buffer;

   ---------------
   -- Draw_Char --
   ---------------

   procedure Draw_Char
     (Buffer     : DMA2D_Buffer;
      Start      : Display_Point;
      Char       : Character;
      Font       : BMP_Font;
      Foreground : DMA2D_Color;
      Background : DMA2D_Color)
   is
   begin
      for H in 0 .. Char_Height (Font) - 1 loop
         for W in 0 .. Char_Width (Font) - 1 loop
            if (Data (Font, Char, H) and Mask (Font, W)) /= 0 then
               if Foreground.Alpha /= 255 then
                  DMA2D_Set_Pixel_Blend
                    (Buffer, Start.X + W, Start.Y + H, Foreground);
               else
                  DMA2D_Set_Pixel
                    (Buffer, Start.X + W, Start.Y + H, Foreground);
               end if;
            else
               if Background.Alpha /= 255 then
                  DMA2D_Set_Pixel_Blend
                    (Buffer, Start.X + W, Start.Y + H, Background);
               else
                  DMA2D_Set_Pixel
                    (Buffer, Start.X + W, Start.Y + H, Background);
               end if;
            end if;
         end loop;
      end loop;
   end Draw_Char;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Buffer     : DMA2D_Buffer;
      Start      : Display_Point;
      Msg        : String;
      Font       : BMP_Font;
      Foreground : DMA2D_Color;
      Background : DMA2D_Color)
   is
      Count : Natural := 0;
   begin
      for C of Msg loop
         exit when Start.X > Buffer.Width;
         Draw_Char
           (Buffer,
            (Start.X + Count * Char_Width (Font), Start.Y),
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
     (Buffer      : DMA2D_Buffer;
      Start, Stop : Display_Point;
      Hue         : DMA2D_Color;
      Thickness   : Natural := 1)
   is
      DX     : constant Float := abs Float (Stop.X - Start.X);
      DY     : constant Float := abs Float (Stop.Y - Start.Y);
      Err    : Float;
      X      : Natural := Start.X;
      Y      : Natural := Start.Y;
      Step_X : Integer := 1;
      Step_Y : Integer := 1;

      procedure Draw_Point (P : Display_Point) with Inline;

      ----------------
      -- Draw_Point --
      ----------------

      procedure Draw_Point (P : Display_Point) is
      begin
         if Thickness /= 1 then
            DMA2D_Fill_Rect
              (Buffer, Hue,
               P.X - (Thickness / 2),
               P.Y - (Thickness / 2),
               Thickness,
               Thickness);
         else
            DMA2D_Set_Pixel (Buffer, P.X, P.Y, Hue);
         end if;
      end Draw_Point;

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
     (Buffer      : DMA2D_Buffer;
      Start, Stop : Display_Point;
      Hue         : DMA2D_Color;
      Thickness   : Natural := 1)
   is
      X0, Y0, X1, Y1 : Natural;
   begin
      X0 := Natural'Min (Start.X, Stop.X);
      Y0 := Natural'Min (Start.Y, Stop.Y);
      X1 := Natural'Max (Start.X, Stop.X);
      Y1 := Natural'Max (Start.Y, Stop.Y);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         X0 - Thickness / 2, Y0,
         Thickness, Y1 - Y0);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         X1 - Thickness / 2, Y0,
         Thickness, Y1 - Y0);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         X0, Y0 - Thickness / 2,
         X1 - X0, Thickness);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         X0, Y1 - Thickness / 2,
         X1 - X0, Thickness);
   end Draw_Rectangle;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle
     (Buffer      : DMA2D_Buffer;
      Start, Stop : Display_Point;
      Hue         : DMA2D_Color)
   is
      X0, Y0, X1, Y1 : Natural;
   begin
      X0 := Natural'Min (Start.X, Stop.X);
      Y0 := Natural'Min (Start.Y, Stop.Y);
      X1 := Natural'Max (Start.X, Stop.X);
      Y1 := Natural'Max (Start.Y, Stop.Y);
      DMA2D_Fill_Rect (Buffer, Hue,
                       X0, Y0, X1 - X0 + 1, Y1 - Y0 + 1);
   end Fill_Rectangle;

   --  http://rosettacode.org/wiki/Bitmap/B%C3%A9zier_curves/Cubic
   ------------------
   -- Cubic_Bezier --
   ------------------

   procedure Cubic_Bezier
     (Buffer         : DMA2D_Buffer;
      P1, P2, P3, P4 : Display_Point;
      Hue            : DMA2D_Color;
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
         Draw_Line (Buffer, Points (I), Points (I + 1), Hue,
                    Thickness => Thickness);
      end loop;
   end Cubic_Bezier;

   --  http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm
   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (Buffer : DMA2D_Buffer;
      Center : Display_Point;
      Radius : Natural;
      Hue    : DMA2D_Color)
   is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X     : Integer := 0;
      Y     : Integer := Radius;
   begin
      DMA2D_Set_Pixel (Buffer, Center.X, Center.Y + Radius, Hue);
      DMA2D_Set_Pixel (Buffer, Center.X, Center.Y - Radius, Hue);
      DMA2D_Set_Pixel (Buffer, Center.X + Radius, Center.Y, Hue);
      DMA2D_Set_Pixel (Buffer, Center.X - Radius, Center.Y, Hue);
      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;
         DMA2D_Set_Pixel (Buffer, Center.X + X, Center.Y + Y, Hue);
         DMA2D_Set_Pixel (Buffer, Center.X - X, Center.Y + Y, Hue);
         DMA2D_Set_Pixel (Buffer, Center.X + X, Center.Y - Y, Hue);
         DMA2D_Set_Pixel (Buffer, Center.X - X, Center.Y - Y, Hue);
         DMA2D_Set_Pixel (Buffer, Center.X + Y, Center.Y + X, Hue);
         DMA2D_Set_Pixel (Buffer, Center.X - Y, Center.Y + X, Hue);
         DMA2D_Set_Pixel (Buffer, Center.X + Y, Center.Y - X, Hue);
         DMA2D_Set_Pixel (Buffer, Center.X - Y, Center.Y - X, Hue);
      end loop;
   end Draw_Circle;

   -----------------
   -- Fill_Circle --
   -----------------

   procedure Fill_Circle
     (Buffer : DMA2D_Buffer;
      Center : Display_Point;
      Radius : Natural;
      Hue    : DMA2D_Color)
   is
      procedure Draw_Horizontal_Line (X, Y : Integer; Width : Natural);
      procedure Draw_Vertical_Line (X, Y : Integer; Height : Natural);

      --------------------------
      -- Draw_Horizontal_Line --
      --------------------------

      procedure Draw_Horizontal_Line (X, Y : Integer; Width : Natural)
      is
         X1, W1 : Natural;
      begin
         if Y < 0 or else Y >= STM32.LCD.Pixel_Height then
            return;
         end if;

         if X + Width < 0 or else X >= STM32.LCD.Pixel_Width then
            return;
         end if;

         if X < 0 then
            X1 := 0;
            W1 := Width + X;
         else
            X1 := X;
            W1 := Width;
         end if;

         if X1 + W1 >= STM32.LCD.Pixel_Width then
            W1 := Stm32.LCD.Pixel_Width - X1 - 1;
         end if;

         DMA2D_Fill_Rect (Buffer, Hue, X1, Y, W1, 1);
      end Draw_Horizontal_Line;

      ------------------------
      -- Draw_Vertical_Line --
      ------------------------

      procedure Draw_Vertical_Line (X, Y : Integer; Height : Natural)
      is
         Y1, H1 : Natural;
      begin
         if X < 0 or else X >= STM32.LCD.Pixel_Width then
            return;
         end if;

         if Y + Height < 0 or else Y >= STM32.LCD.Pixel_Height then
            return;
         end if;

         if Y < 0 then
            Y1 := 0;
            H1 := Height + Y;
         else
            Y1 := Y;
            H1 := Height;
         end if;

         if Y1 + H1 >= STM32.LCD.Pixel_Height then
            H1 := Stm32.LCD.Pixel_Height - Y1 - 1;
         end if;

         DMA2D_Fill_Rect (Buffer, Hue, X, Y1, 1, H1);
      end Draw_Vertical_Line;

      F     : Integer := 1 - Radius;
      ddF_X : Integer := 1;
      ddF_Y : Integer := -(2 * Radius);
      X     : Integer := 0;
      Y     : Integer := Radius;

   begin
      Draw_Vertical_Line
        (Center.X,
         Center.Y - Radius,
         2 * Radius);
      Draw_Horizontal_Line
        (Center.X - Radius,
         Center.Y,
         2 * Radius);

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X;

         Draw_Horizontal_Line (Center.X - X, Center.Y + Y, 2 * X);
         Draw_Horizontal_Line (Center.X - X, Center.Y - Y, 2 * X);
         Draw_Horizontal_Line (Center.X - Y, Center.Y + X, 2 * Y);
         Draw_Horizontal_Line (Center.X - Y, Center.Y - X, 2 * Y);
      end loop;
   end Fill_Circle;

end Bitmapped_Drawing;
