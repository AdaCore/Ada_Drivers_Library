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

with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

with STM32.LCD;               use STM32.LCD;

package body Bitmapped_Drawing is

   -------------------
   -- Screen_Buffer --
   -------------------

   function Screen_Buffer return Graphics_Buffer
   is
   begin
      return
        (Addr       => STM32.LCD.Current_Frame_Buffer (STM32.LCD.Layer1),
         Width      => STM32.LCD.Pixel_Width,
         Height     => STM32.LCD.Pixel_Height,
         Color_Mode => STM32.LCD.Get_Pixel_Fmt,
         Swap_X_Y   => STM32.LCD.SwapXY);
   end Screen_Buffer;

   ---------------
   -- Put_Pixel --
   ---------------

   procedure Put_Pixel
     (Buffer   : Graphics_Buffer;
      Position : Point;
      Hue      : Graphics_Color)
   is
   begin
      DMA2D_Set_Pixel (Buffer, Position.X, Position.Y, Hue);
   end Put_Pixel;

   ---------------
   -- Put_Pixel --
   ---------------

   procedure Put_Pixel
     (Buffer   : Graphics_Buffer;
      Position : Point;
      Hue      : Unsigned_32)
   is
   begin
      DMA2D_Set_Pixel (Buffer, Position.X, Position.Y, Hue);
   end Put_Pixel;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel
     (Buffer   : Graphics_Buffer;
      Position : Point) return Graphics_Color
   is
   begin
      return Word_To_DMA2D_Color (Buffer, Get_Pixel (Buffer, Position));
   end Get_Pixel;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel
     (Buffer   : Graphics_Buffer;
      Position : Point) return Unsigned_32
   is
      Pix_Size : constant Natural := Bytes_Per_Pixel (Buffer.Color_Mode);
      Offset   : constant Natural :=
                   Pix_Size * (Position.X + Buffer.Width * Position.Y);
      Pix_Addr : constant System.Address :=
                   Buffer.Addr + Storage_Offset (Offset);

   begin
      DMA2D_Wait_Transfer;
      case Pix_Size is
         when 2 =>
            declare
               Val : Unsigned_16 with Address => Pix_Addr;
            begin
               return Unsigned_32 (Val);
            end;
         when 3 =>
            declare
               Val : Unsigned_24 with Address => Pix_Addr;
            begin
               return Unsigned_32 (Val);
            end;
         when 4 =>
            declare
               Val : Unsigned_32 with Address => Pix_Addr;
            begin
               return Val;
            end;
         when others =>
            raise Constraint_Error with "Unexpected pixel size";
      end case;
   end Get_Pixel;


   ---------------
   -- Draw_Char --
   ---------------

   procedure Draw_Char
     (Buffer     : Graphics_Buffer;
      Start      : Point;
      Char       : Character;
      Font       : BMP_Font;
      Foreground : DMA2D_Color;
      Background : DMA2D_Color)
   is
   begin
      for H in 0 .. Char_Height (Font) - 1 loop
         for W in 0 .. Char_Width (Font) - 1 loop
            if (Data (Font, Char, H) and Mask (Font, W)) /= 0 then
               DMA2D_Set_Pixel
                 (Buffer, Start.X + W, Start.Y + H, Foreground);
            else
               DMA2D_Set_Pixel
                 (Buffer, Start.X + W, Start.Y + H, Background);
            end if;
         end loop;
      end loop;
   end Draw_Char;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Buffer     : Graphics_Buffer;
      Start      : Point;
      Msg        : String;
      Font       : BMP_Font;
      Foreground : DMA2D_Color;
      Background : DMA2D_Color)
   is
      Count : Natural := 0;
   begin
      for C of Msg loop
         exit when Start.X + Count * Char_Width (Font) > Buffer.Width;
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
     (Buffer      : Graphics_Buffer;
      Start, Stop : Point;
      Hue         : Graphics_Color;
      Thickness   : Natural := 1;
      Fast        : Boolean := True)
   is
      DX     : constant Float := abs Float (Stop.X - Start.X);
      DY     : constant Float := abs Float (Stop.Y - Start.Y);
      Err    : Float;
      X      : Natural := Start.X;
      Y      : Natural := Start.Y;
      Step_X : Integer := 1;
      Step_Y : Integer := 1;

      procedure Draw_Point (P : Point) with Inline;

      ----------------
      -- Draw_Point --
      ----------------

      procedure Draw_Point (P : Point) is
      begin
         if Thickness /= 1 then
            if not Fast then
               Fill_Circle (Buffer,
                            Center => P,
                            Radius => Thickness / 2,
                            Hue    => Hue);
            else
               DMA2D_Fill_Rect
                 (Buffer, Hue,
                  P.X - (Thickness / 2),
                  P.Y - (Thickness / 2),
                  Thickness,
                  Thickness);
            end if;
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
     (Buffer    : Graphics_Buffer;
      Area      : Rect;
      Hue       : Graphics_Color;
      Thickness : Natural := 1)
   is
      X0, Y0, X1, Y1 : Natural;
   begin
      X0 := Area.Position.X;
      Y0 := Area.Position.Y;
      X1 := Area.Position.X + Area.Width - 1;
      Y1 := Area.Position.Y + Area.Height - 1;
      DMA2D_Fill_Rect
        (Buffer, Hue,
         X0 - Thickness / 2, Y0,
         Thickness,
         Area.Height + Thickness / 2);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         X1 - Thickness / 2, Y0,
         Thickness,
         Area.Height + Thickness / 2);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         X0,
         Y0 - Thickness / 2,
         Area.Width + Thickness / 2,
         Thickness);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         X0,
         Y1 - Thickness / 2,
         Area.Width + Thickness / 2,
         Thickness);
   end Draw_Rectangle;

   ----------------------------
   -- Draw_Rounded_Rectangle --
   ----------------------------

   procedure Draw_Rounded_Rectangle
     (Buffer    : Graphics_Buffer;
      Area      : Rect;
      Radius    : Natural;
      Hue       : Graphics_Color;
      Thickness : Natural := 1)
   is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X     : Integer := 0;
      Y     : Integer := Radius;
      Center_Top : constant Natural := Area.Position.Y + Radius;
      Center_Bot : constant Natural := Area.Position.Y + Area.Height - 1 - Radius;
      Center_Lft : constant Natural := Area.Position.X + Radius;
      Center_Rgt : constant Natural := Area.Position.X + Area.Width - 1 - Radius;

      procedure Draw_Point (X, Y : Natural) with Inline;

      ----------------
      -- Draw_Point --
      ----------------

      procedure Draw_Point (X, Y : Natural) is
      begin
         if Thickness /= 1 then
            DMA2D_Fill_Rect
              (Buffer, Hue,
               X - (Thickness / 2),
               Y - (Thickness / 2),
               Thickness,
               Thickness);
         else
            DMA2D_Set_Pixel (Buffer, X, Y, Hue);
         end if;
      end Draw_Point;

   begin
      if Radius = 0 then
         Draw_Rectangle (Buffer, Area, Hue, Thickness);
         return;
      end if;

      DMA2D_Fill_Rect
        (Buffer, Hue,
         Area.Position.X - Thickness / 2,
         Area.Position.Y + Radius,
         Thickness,
         Area.Height - 2 * Radius);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         Area.Position.X + Area.Width - Thickness / 2 - 1,
         Area.Position.Y + Radius,
         Thickness,
         Area.Height - 2 * Radius);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         Area.Position.X + Radius,
         Area.Position.Y - Thickness / 2,
         Area.Width - 2 * Radius,
         Thickness);
      DMA2D_Fill_Rect
        (Buffer, Hue,
         Area.Position.X + Radius,
         Area.Position.Y + Area.Height - Thickness / 2 - 1,
         Area.Width - 2 * Radius,
         Thickness);

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;


         Draw_Point (Center_Rgt + X, Center_Bot + Y);
         Draw_Point (Center_Lft - X, Center_Bot + Y);
         Draw_Point (Center_Rgt + X, Center_Top - Y);
         Draw_Point (Center_Lft - X, Center_Top - Y);
         Draw_Point (Center_Rgt + Y, Center_Bot + X);
         Draw_Point (Center_Lft - Y, Center_Bot + X);
         Draw_Point (Center_Rgt + Y, Center_Top - X);
         Draw_Point (Center_Lft - Y, Center_Top - X);

      end loop;
   end Draw_Rounded_Rectangle;

   ----------------------------
   -- Fill_Rounded_Rectangle --
   ----------------------------

   procedure Fill_Rounded_Rectangle
     (Buffer : Graphics_Buffer;
      Area   : Rect;
      Radius : Natural;
      Hue    : Graphics_Color)
   is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X     : Integer := 0;
      Y     : Integer := Radius;
      Center_Top : constant Natural := Area.Position.Y + Radius;
      Center_Bot : constant Natural := Area.Position.Y + Area.Height - 1 - Radius;
      Center_Lft : constant Natural := Area.Position.X + Radius;

   begin
      if Radius = 0 then
         Fill_Rectangle (Buffer, Area, Hue);
         return;
      end if;

      DMA2D_Fill_Rect
        (Buffer, Hue,
         Area.Position.X, Center_Top,
         Area.Width, Area.Height - 2 * Radius);

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;

         DMA2D_Draw_Horizontal_Line
           (Buffer, Hue,
            Center_Lft - X, Center_Bot + Y,
            Area.Width - 2 * Radius + 2 * X);
         DMA2D_Draw_Horizontal_Line
           (Buffer, Hue,
            Center_Lft - X, Center_Top - Y,
            Area.Width - 2 * Radius + 2 * X);
         DMA2D_Draw_Horizontal_Line
           (Buffer, Hue,
            Center_Lft - Y, Center_Bot + X,
            Area.Width - 2 * Radius + 2 * Y);
         DMA2D_Draw_Horizontal_Line
           (Buffer, Hue,
            Center_Lft - Y, Center_Top - X,
            Area.Width - 2 * Radius + 2 * Y);
      end loop;
   end Fill_Rounded_Rectangle;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle
     (Buffer : Graphics_Buffer;
      Area   : Rect;
      Hue    : Graphics_Color)
   is
   begin
      DMA2D_Fill_Rect
        (Buffer, Hue,
         Area.Position.X, Area.Position.Y, Area.Width, Area.Height);
   end Fill_Rectangle;

   --------------------
   -- Copy_Rectangle --
   --------------------

   procedure Copy_Rectangle
     (Src      : Graphics_Buffer;
      Src_Area : Rect;
      Dst      : Graphics_Buffer;
      Dst_Pos  : Point)
   is
   begin
      DMA2D_Copy_Rect
        (Src_Buffer  => Src,
         X_Src       => Src_Area.Position.X,
         Y_Src       => Src_Area.Position.Y,
         Dst_Buffer  => Dst,
         X_Dst       => Dst_Pos.X,
         Y_Dst       => Dst_Pos.Y,
         Bg_Buffer   => Null_Buffer,
         X_Bg        => 0,
         Y_Bg        => 0,
         Width       => Src_Area.Width,
         Height      => Src_Area.Height);
   end Copy_Rectangle;

   --------------------------
   -- Copy_Rectangle_Blend --
   --------------------------

   procedure Copy_Rectangle_Blend
     (Src      : Graphics_Buffer;
      Src_Area : Rect;
      Dst      : Graphics_Buffer;
      Dst_Pos  : Point)
   is
   begin
      DMA2D_Copy_Rect
        (Src_Buffer  => Src,
         X_Src       => Src_Area.Position.X,
         Y_Src       => Src_Area.Position.Y,
         Dst_Buffer  => Dst,
         X_Dst       => Dst_Pos.X,
         Y_Dst       => Dst_Pos.Y,
         Bg_Buffer   => Dst,
         X_Bg        => Dst_Pos.X,
         Y_Bg        => Dst_Pos.Y,
         Width       => Src_Area.Width,
         Height      => Src_Area.Height);
   end Copy_Rectangle_Blend;

   --  http://rosettacode.org/wiki/Bitmap/B%C3%A9zier_curves/Cubic
   ------------------
   -- Cubic_Bezier --
   ------------------

   procedure Cubic_Bezier
     (Buffer         : Graphics_Buffer;
      P1, P2, P3, P4 : Point;
      Hue            : Graphics_Color;
      N              : Positive := 20;
      Thickness      : Natural := 1)
   is
      Points : array (0 .. N) of Point;
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
     (Buffer : Graphics_Buffer;
      Center : Point;
      Radius : Natural;
      Hue    : Graphics_Color)
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
     (Buffer : Graphics_Buffer;
      Center : Point;
      Radius : Natural;
      Hue    : Graphics_Color)
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
         if Y < 0 or else Y >= Buffer.Height then
            return;
         end if;

         if X + Width < 0 or else X >= Buffer.Width then
            return;
         end if;

         if X < 0 then
            X1 := 0;
            W1 := Width + X;
         else
            X1 := X;
            W1 := Width;
         end if;

         if X1 + W1 >= Buffer.Width then
            W1 := Buffer.Width - X1 - 1;
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
         if X < 0 or else X >= Buffer.Width then
            return;
         end if;

         if Y + Height < 0 or else Y >= Buffer.Height then
            return;
         end if;

         if Y < 0 then
            Y1 := 0;
            H1 := Height + Y;
         else
            Y1 := Y;
            H1 := Height;
         end if;

         if Y1 + H1 >= Buffer.Height then
            H1 := Buffer.Height - Y1 - 1;
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
