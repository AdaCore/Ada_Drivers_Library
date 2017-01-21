------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

with HAL; use HAL;

package body Bitmapped_Drawing is

   ---------------
   -- Draw_Char --
   ---------------

   procedure Draw_Char
     (Buffer     : in out Bitmap_Buffer'Class;
      Start      : Point;
      Char       : Character;
      Font       : BMP_Font;
      Foreground : Unsigned_32;
      Background : Unsigned_32)
   is
   begin
      for H in 0 .. Char_Height (Font) - 1 loop
         for W in 0 .. Char_Width (Font) - 1 loop
            if (Data (Font, Char, H) and Mask (Font, W)) /= 0 then
               Buffer.Set_Pixel
                 (Start.X + W, Start.Y + H, Foreground);
            else
               Buffer.Set_Pixel
                 (Start.X + W, Start.Y + H, Background);
            end if;
         end loop;
      end loop;
   end Draw_Char;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Buffer     : in out Bitmap_Buffer'Class;
      Start      : Point;
      Msg        : String;
      Font       : BMP_Font;
      Foreground : Bitmap_Color;
      Background : Bitmap_Color)
   is
      Count : Natural := 0;
      FG    : constant Unsigned_32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                            Foreground);
      BG    : constant Unsigned_32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                            Background);
   begin
      for C of Msg loop
         exit when Start.X + Count * Char_Width (Font) > Buffer.Width;
         Draw_Char
           (Buffer,
            (Start.X + Count * Char_Width (Font), Start.Y),
            C,
            Font,
            FG,
            BG);
         Count := Count + 1;
      end loop;
   end Draw_String;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Buffer     : in out Bitmap_Buffer'Class;
      Start      : Point;
      Msg        : String;
      Font       : Hershey_Font;
      Height     : Natural;
      Bold       : Boolean;
      Foreground : Bitmap_Color;
      Fast       : Boolean := True)
   is
      FG    : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                     Foreground);

      procedure Internal_Draw_Line
        (X0, Y0, X1, Y1 : Natural;
         Width          : Positive);

      procedure Internal_Draw_Line
        (X0, Y0, X1, Y1 : Natural;
         Width          : Positive)
      is
      begin
         Draw_Line (Buffer,
                    (X0, Y0),
                    (X1, Y1),
                    FG,
                    Width,
                    Fast => Fast);
      end Internal_Draw_Line;

      procedure Draw_Glyph is new Hershey_Fonts.Draw_Glyph
        (Internal_Draw_Line);

      Current : Point := Start;

   begin
      for C of Msg loop
         exit when Current.X > Buffer.Width;
         Draw_Glyph
           (Fnt    => Font,
            C      => C,
            X      => Current.X,
            Y      => Current.Y,
            Height => Height,
            Bold   => Bold);
      end loop;
   end Draw_String;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Buffer     : in out Bitmap_Buffer'Class;
      Area       : Rect;
      Msg        : String;
      Font       : Hershey_Font;
      Bold       : Boolean;
      Outline    : Boolean;
      Foreground : Bitmap_Color;
      Fast       : Boolean := True)
   is
      Length  : constant Natural :=
                  Hershey_Fonts.Strlen (Msg, Font, Area.Height);
      Ratio   : Float;
      Current : Point := (0, 0);
      Prev    : Unsigned_32;
      FG      : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                       Foreground);
      Blk     : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                       Black);

      procedure Internal_Draw_Line
        (X0, Y0, X1, Y1 : Natural;
         Width          : Positive);

      procedure Internal_Draw_Line
        (X0, Y0, X1, Y1 : Natural;
         Width          : Positive)
      is
      begin
         Draw_Line (Buffer,
                    (Area.Position.X + Natural (Float (X0) * Ratio),
                     Area.Position.Y + Y0),
                    (Area.Position.X + Natural (Float (X1) * Ratio),
                     Area.Position.Y + Y1),
                    Foreground,
                    Width,
                    Fast);
      end Internal_Draw_Line;

      procedure Draw_Glyph is new Hershey_Fonts.Draw_Glyph
        (Internal_Draw_Line);

   begin
      if Length > Area.Width then
         Ratio := Float (Area.Width) / Float (Length);
      else
         Ratio := 1.0;
         Current.X := (Area.Width - Length) / 2;
      end if;

      for C of Msg loop
         Draw_Glyph
           (Fnt    => Font,
            C      => C,
            X      => Current.X,
            Y      => Current.Y,
            Height => Area.Height,
            Bold   => Bold);
      end loop;

      if Outline and then Area.Height > 40 then
         for Y in Area.Position.Y + 1 .. Area.Position.Y + Area.Height loop
            Prev := Buffer.Pixel (Area.Position.X, Y);
            if Prev = FG then
               Buffer.Set_Pixel (Area.Position.X, Y, Black);
            end if;

            for X in Area.Position.X + 1 .. Area.Position.X + Area.Width loop
               declare
                  Col : constant Unsigned_32 := Buffer.Pixel (X, Y);
                  Top : constant Unsigned_32 := Buffer.Pixel (X, Y - 1);
               begin

                  if Prev /= FG
                    and then Col = FG
                  then
                     Buffer.Set_Pixel (X, Y, Blk);

                  elsif Prev = FG
                    and then Col /= FG
                  then
                     Buffer.Set_Pixel (X - 1, Y, Blk);

                  elsif Top /= FG
                    and then Top /= Blk
                    and then Col = FG
                  then
                     Buffer.Set_Pixel (X, Y, Blk);

                  elsif Top = FG
                    and then Col /= FG
                  then
                     Buffer.Set_Pixel (X, Y - 1, Blk);
                  end if;

                  Prev := Col;
               end;
            end loop;
         end loop;
      end if;
   end Draw_String;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (Buffer      : in out Bitmap_Buffer'Class;
      Start, Stop : Point;
      Hue         : Bitmap_Color;
      Thickness   : Natural := 1;
      Fast        : Boolean := True)
   is
      Col : constant Unsigned_32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                          Hue);
   begin
      Draw_Line (Buffer, Start, Stop, Col, Thickness, Fast);
   end Draw_Line;

   --  http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#Ada
   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line
     (Buffer      : in out Bitmap_Buffer'Class;
      Start, Stop : Point;
      Hue         : Unsigned_32;
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
               Buffer.Fill_Rect
                 (Hue,
                  P.X - (Thickness / 2),
                  P.Y - (Thickness / 2),
                  Thickness,
                  Thickness);
            end if;
         else
            Buffer.Set_Pixel (P.X, P.Y, Hue);
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
     (Buffer    : in out Bitmap_Buffer'Class;
      Area      : Rect;
      Hue       : Bitmap_Color;
      Thickness : Natural := 1)
   is
      X0, Y0, X1, Y1 : Natural;
   begin
      X0 := Area.Position.X;
      Y0 := Area.Position.Y;
      X1 := Area.Position.X + Area.Width - 1;
      Y1 := Area.Position.Y + Area.Height - 1;
      Buffer.Fill_Rect
        (Hue,
         X0 - Thickness / 2, Y0,
         Thickness,
         Area.Height + Thickness / 2);
      Buffer.Fill_Rect
        (Hue,
         X1 - Thickness / 2, Y0,
         Thickness,
         Area.Height + Thickness / 2);
      Buffer.Fill_Rect
        (Hue,
         X0,
         Y0 - Thickness / 2,
         Area.Width + Thickness / 2,
         Thickness);
      Buffer.Fill_Rect
        (Hue,
         X0,
         Y1 - Thickness / 2,
         Area.Width + Thickness / 2,
         Thickness);
   end Draw_Rectangle;

   ----------------------------
   -- Draw_Rounded_Rectangle --
   ----------------------------

   procedure Draw_Rounded_Rectangle
     (Buffer    : in out Bitmap_Buffer'Class;
      Area      : Rect;
      Radius    : Natural;
      Hue       : Bitmap_Color;
      Thickness : Natural := 1)
   is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X     : Integer := 0;
      Y     : Integer := Radius;
      Center_Top : constant Natural :=
        Area.Position.Y + Radius;
      Center_Bot : constant Natural :=
        Area.Position.Y + Area.Height - 1 - Radius;
      Center_Lft : constant Natural :=
        Area.Position.X + Radius;
      Center_Rgt : constant Natural :=
        Area.Position.X + Area.Width - 1 - Radius;

      procedure Draw_Point (X, Y : Natural) with Inline;

      ----------------
      -- Draw_Point --
      ----------------

      procedure Draw_Point (X, Y : Natural) is
      begin
         if Thickness /= 1 then
            Buffer.Fill_Rect
              (Hue,
               X - (Thickness / 2),
               Y - (Thickness / 2),
               Thickness,
               Thickness);
         else
            Buffer.Set_Pixel (X, Y, Hue);
         end if;
      end Draw_Point;

   begin
      if Radius = 0 then
         Draw_Rectangle (Buffer, Area, Hue, Thickness);
         return;
      end if;

      Buffer.Fill_Rect
        (Hue,
         Area.Position.X - Thickness / 2,
         Area.Position.Y + Radius,
         Thickness,
         Area.Height - 2 * Radius);
      Buffer.Fill_Rect
        (Hue,
         Area.Position.X + Area.Width - Thickness / 2 - 1,
         Area.Position.Y + Radius,
         Thickness,
         Area.Height - 2 * Radius);
      Buffer.Fill_Rect
        (Hue,
         Area.Position.X + Radius,
         Area.Position.Y - Thickness / 2,
         Area.Width - 2 * Radius,
         Thickness);
      Buffer.Fill_Rect
        (Hue,
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
     (Buffer : in out Bitmap_Buffer'Class;
      X      : Natural;
      Y      : Natural;
      Width  : Positive;
      Height : Positive;
      Radius : Natural;
      Hue    : Bitmap_Color)
   is
      Col   : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode, Hue);
      F          : Integer := 1 - Radius;
      ddF_X      : Integer := 0;
      ddF_Y      : Integer := (-2) * Radius;
      X0         : Integer := 0;
      Y0         : Integer := Radius;
      Center_Top : constant Natural := Y + Radius;
      Center_Bot : constant Natural := Y + Height - 1 - Radius;
      Center_Lft : constant Natural := X + Radius;

   begin
      if Radius = 0 then
         Buffer.Fill_Rect
           (Color  => Col,
            X      => X,
            Y      => Y,
            Width  => Width,
            Height => Height);
         return;
      end if;

      Buffer.Fill_Rect
        (Col,
         X, Center_Top,
         Width, Height - 2 * Radius);

      while X0 < Y0 loop
         if F >= 0 then
            Y0 := Y0 - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;

         X0 := X0 + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;

         Buffer.Draw_Horizontal_Line
           (Col,
            Center_Lft - X0, Center_Bot + Y0,
            Width - 2 * Radius + 2 * X0);
         Buffer.Draw_Horizontal_Line
           (Col,
            Center_Lft - X0, Center_Top - Y0,
            Width - 2 * Radius + 2 * X0);
         Buffer.Draw_Horizontal_Line
           (Col,
            Center_Lft - Y0, Center_Bot + X0,
            Width - 2 * Radius + 2 * Y0);
         Buffer.Draw_Horizontal_Line
           (Col,
            Center_Lft - Y0, Center_Top - X0,
            Width - 2 * Radius + 2 * Y0);
      end loop;
   end Fill_Rounded_Rectangle;

   --  http://rosettacode.org/wiki/Bitmap/B%C3%A9zier_curves/Cubic
   ------------------
   -- Cubic_Bezier --
   ------------------

   procedure Cubic_Bezier
     (Buffer         : in out Bitmap_Buffer'Class;
      P1, P2, P3, P4 : Point;
      Hue            : Bitmap_Color;
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

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (Buffer : in out Bitmap_Buffer'Class;
      Center : Point;
      Radius : Natural;
      Hue    : Bitmap_Color)
   is
   begin
      Draw_Circle
        (Buffer, Center, Radius,
         Bitmap_Color_To_Word (Buffer.Color_Mode, Hue));
   end Draw_Circle;

   --  http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm
   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (Buffer : in out Bitmap_Buffer'Class;
      Center : Point;
      Radius : Natural;
      Hue    : UInt32)
   is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X     : Integer := 0;
      Y     : Integer := Radius;
   begin
      Buffer.Set_Pixel (Center.X, Center.Y + Radius, Hue);
      Buffer.Set_Pixel (Center.X, Center.Y - Radius, Hue);
      Buffer.Set_Pixel (Center.X + Radius, Center.Y, Hue);
      Buffer.Set_Pixel (Center.X - Radius, Center.Y, Hue);

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;
         Buffer.Set_Pixel (Center.X + X, Center.Y + Y, Hue);
         Buffer.Set_Pixel (Center.X - X, Center.Y + Y, Hue);
         Buffer.Set_Pixel (Center.X + X, Center.Y - Y, Hue);
         Buffer.Set_Pixel (Center.X - X, Center.Y - Y, Hue);
         Buffer.Set_Pixel (Center.X + Y, Center.Y + X, Hue);
         Buffer.Set_Pixel (Center.X - Y, Center.Y + X, Hue);
         Buffer.Set_Pixel (Center.X + Y, Center.Y - X, Hue);
         Buffer.Set_Pixel (Center.X - Y, Center.Y - X, Hue);
      end loop;
   end Draw_Circle;

   -----------------
   -- Fill_Circle --
   -----------------

   procedure Fill_Circle
     (Buffer : in out Bitmap_Buffer'Class;
      Center : Point;
      Radius : Natural;
      Hue    : Bitmap_Color)
   is
      Col : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                   Hue);
   begin
      Fill_Circle (Buffer, Center, Radius, Col);
   end Fill_Circle;

   procedure Fill_Circle
     (Buffer : in out Bitmap_Buffer'Class;
      Center : Point;
      Radius : Natural;
      Hue    : Unsigned_32)
   is
      procedure Draw_Horizontal_Line (X, Y : Integer; Width : Natural);
      ------------------------
      -- Draw_Vertical_Line --
      ------------------------

      procedure Draw_Vertical_Line (X, Y : Integer; Height : Natural);

      --------------------------
      -- Draw_Horizontal_Line --
      --------------------------

      procedure Draw_Horizontal_Line (X, Y : Integer; Width : Natural)
      is
         X1, W1 : Natural;
      begin
         if Width = 0 then
            return;

         elsif Y < 0 or else Y >= Buffer.Height then
            return;

         elsif X + Width < 0 or else X >= Buffer.Width then
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

         if W1 = 0 then
            return;
         end if;

         Buffer.Fill_Rect (Hue, X1, Y, W1, 1);
      end Draw_Horizontal_Line;

      ------------------------
      -- Draw_Vertical_Line --
      ------------------------

      procedure Draw_Vertical_Line (X, Y : Integer; Height : Natural)
      is
         Y1, H1 : Natural;
      begin
         if Height = 0 then
            return;

         elsif X < 0 or else X >= Buffer.Width then
            return;

         elsif Y + Height < 0 or else Y >= Buffer.Height then
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

         if H1 = 0 then
            return;
         end if;

         Buffer.Fill_Rect (Hue, X, Y1, 1, H1);
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
