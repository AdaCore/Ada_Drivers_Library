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

with Bitmap_Color_Conversion; use Bitmap_Color_Conversion;

package body Soft_Drawing_Bitmap is

   subtype Dispatch is Soft_Drawing_Bitmap_Buffer'Class;

   overriding
   procedure Draw_Line
     (Buffer      : in out Soft_Drawing_Bitmap_Buffer;
      Color       : UInt32;
      Start, Stop : Point;
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
               Dispatch (Buffer). Fill_Circle (Color  => Color,
                                               Center => P,
                                               Radius => Thickness / 2);
            else
               Dispatch (Buffer).Fill_Rect
                 (Color,
                  ((P.X - (Thickness / 2), P.Y - (Thickness / 2)),
                   Thickness,
                   Thickness));
            end if;
         else
            Dispatch (Buffer).Set_Pixel ((P.X, P.Y), Color);
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

   overriding
   procedure Draw_Line
     (Buffer      : in out Soft_Drawing_Bitmap_Buffer;
      Color       : Bitmap_Color;
      Start, Stop : Point;
      Thickness   : Natural := 1;
      Fast        : Boolean := True)
   is
      Col : constant UInt32 :=
        Bitmap_Color_To_Word (Dispatch (Buffer).Color_Mode, Color);
   begin
      Dispatch (Buffer).Draw_Line (Col, Start, Stop, Thickness, Fast);
   end Draw_Line;

   ----------
   -- Fill --
   ----------

   overriding
   procedure Fill
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : Bitmap_Color)
   is
      Col : constant UInt32 := Bitmap_Color_To_Word (Dispatch (Buffer).Color_Mode, Color);
   begin
      Fill (Dispatch (Buffer), Col);
   end Fill;

   ----------
   -- Fill --
   ----------

   overriding
   procedure Fill
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : UInt32)
   is
   begin
      for Y in 0 .. Dispatch (Buffer).Height - 1 loop
         for X in 0 .. Dispatch (Buffer).Width - 1 loop
            Dispatch (Buffer).Set_Pixel ((X, Y), Color);
         end loop;
      end loop;
   end Fill;

   ---------------
   -- Fill_Rect --
   ---------------

   overriding
   procedure Fill_Rect
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : Bitmap_Color;
      Area   : Rect)
   is
   begin
      Fill_Rect
        (Dispatch (Buffer),
         Bitmap_Color_To_Word (Dispatch (Buffer).Color_Mode, Color),
         Area);
   end Fill_Rect;

   ---------------
   -- Fill_Rect --
   ---------------

   overriding
   procedure Fill_Rect
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : UInt32;
      Area   : Rect)
   is
   begin
      for Y0 in Area.Position.Y .. Area.Position.Y + Area.Height - 1 loop
         for X0 in Area.Position.X .. Area.Position.X + Area.Width - 1 loop
            Dispatch (Buffer).Set_Pixel ((X0, Y0), Color);
         end loop;
      end loop;
   end Fill_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   overriding
   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Soft_Drawing_Bitmap_Buffer;
      Dst_Pt      : Point;
      Bg_Buffer   : Bitmap_Buffer'Class;
      Bg_Pt       : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
   begin
      raise Constraint_Error with "Not implemented yet.";
   end Copy_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   overriding
   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Soft_Drawing_Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
      pragma Unreferenced (Synchronous);
      Pix : UInt32;
   begin
      if Src_Pt.X + Width > Src_Buffer.Width
        or else
         Src_Pt.Y + Height > Src_Buffer.Height
      then
         raise Constraint_Error with "invalid copy area for source bitmap";
      end if;

      if Dst_Pt.X + Width > Dispatch (Dst_Buffer).Width
        or else
         Dst_Pt.Y + Height > Dispatch (Dst_Buffer).Height
      then
         raise Constraint_Error with "invalid copy area for destination bitmap";
      end if;

      for X in 0 .. Width - 1 loop
         for Y in 0 .. Height - 1 loop
            Pix := Src_Buffer.Pixel (Src_Pt + (X, Y));
            Dispatch (Dst_Buffer).Set_Pixel (Dst_Pt + (X, Y), Pix);
         end loop;
      end loop;
   end Copy_Rect;

   ---------------------
   -- Copy_Rect_Blend --
   ---------------------

   overriding
   procedure Copy_Rect_Blend
     (Src_Buffer  : Soft_Drawing_Bitmap_Buffer;
      Src_Pt      : Point;
      Dst_Buffer  : in out Bitmap_Buffer'Class;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
   begin
      Copy_Rect
        (Src_Buffer  => Dispatch (Src_Buffer),
         Src_Pt      => Src_Pt,
         Dst_Buffer  => Dst_Buffer,
         Dst_Pt      => Dst_Pt,
         Bg_Buffer   => Dst_Buffer,
         Bg_Pt       => Dst_Pt,
         Width       => Width,
         Height      => Height,
         Synchronous => Synchronous);
   end Copy_Rect_Blend;

   ------------------------
   -- Draw_Vertical_Line --
   ------------------------

   overriding
   procedure Draw_Vertical_Line
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : UInt32;
      Pt     : Point;
      Height : Integer)
   is
   begin
      Fill_Rect (Dispatch (Buffer), Color,
                 (Position => Pt,
                  Width    => 1,
                  Height   => Height));
   end Draw_Vertical_Line;

   ------------------------
   -- Draw_Vertical_Line --
   ------------------------

   overriding
   procedure Draw_Vertical_Line
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : Bitmap_Color;
      Pt     : Point;
      Height : Integer)
   is
   begin
      Fill_Rect (Dispatch (Buffer), Color,
                 (Position => Pt,
                  Width    => 1,
                  Height   => Height));
   end Draw_Vertical_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   overriding
   procedure Draw_Horizontal_Line
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : UInt32;
      Pt     : Point;
      Width  : Integer)
   is
   begin
      Fill_Rect (Dispatch (Buffer), Color,
                 (Position => Pt,
                  Width    => Width,
                  Height   => 1));
   end Draw_Horizontal_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   overriding
   procedure Draw_Horizontal_Line
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : Bitmap_Color;
      Pt     : Point;
      Width  : Integer)
   is
   begin
      Fill_Rect (Dispatch (Buffer), Color,
                 (Position => Pt,
                  Width    => Width,
                  Height   => 1));
   end Draw_Horizontal_Line;

   ---------------
   -- Draw_Rect --
   ---------------

   overriding
   procedure Draw_Rect
     (Buffer    : in out Soft_Drawing_Bitmap_Buffer;
      Color     : Bitmap_Color;
      Area      : Rect;
      Thickness : Natural := 1)
   is
      X0, Y0, X1, Y1 : Natural;
   begin
      X0 := Area.Position.X;
      Y0 := Area.Position.Y;
      X1 := Area.Position.X + Area.Width - 1;
      Y1 := Area.Position.Y + Area.Height - 1;
      Dispatch (Buffer).Fill_Rect
        (Color,
         (Position => (X0 - Thickness / 2, Y0),
          Width    => Thickness,
          Height   => Area.Height + Thickness / 2));
      Dispatch (Buffer).Fill_Rect
        (Color,
         (Position => (X1 - Thickness / 2, Y0),
          Width    => Thickness,
          Height   => Area.Height + Thickness / 2));
      Dispatch (Buffer).Fill_Rect
        (Color,
         (Position => (X0, Y0 - Thickness / 2),
          Width    => Area.Width + Thickness / 2,
          Height   => Thickness));
      Dispatch (Buffer).Fill_Rect
        (Color,
         (Position => (X0, Y1 - Thickness / 2),
          Width    => Area. Width + Thickness / 2,
          Height   => Thickness));
   end Draw_Rect;

   -----------------------
   -- Draw_Rounded_Rect --
   -----------------------

   overriding
   procedure Draw_Rounded_Rect
     (Buffer    : in out Soft_Drawing_Bitmap_Buffer;
      Color     : Bitmap_Color;
      Area      : Rect;
      Radius    : Natural;
      Thickness : Natural := 1)
   is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X0     : Integer := 0;
      Y0     : Integer := Radius;
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
            Dispatch (Buffer).Fill_Rect
              (Color,
               (Position => (X - (Thickness / 2), Y - (Thickness / 2)),
                Width    => Thickness,
                Height   => Thickness));
         else
            Dispatch (Buffer).Set_Pixel ((X, Y), Color);
         end if;
      end Draw_Point;

   begin
      if Radius = 0 then
         Draw_Rect (Buffer, Color, Area, Thickness);
         return;
      end if;

      Dispatch (Buffer).Fill_Rect
        (Color,
         (Position => (Area.Position.X - Thickness / 2, Area.Position.Y + Radius),
          Width    => Thickness,
          Height   => Area.Height - 2 * Radius));
      Dispatch (Buffer).Fill_Rect
        (Color,
         (Position => (Area.Position.X + Area.Width - Thickness / 2 - 1, Area.Position.Y + Radius),
          Width    => Thickness,
          Height   => Area.Height - 2 * Radius));
      Dispatch (Buffer).Fill_Rect
        (Color,
         (Position => (Area.Position.X + Radius, Area.Position.Y - Thickness / 2),
          Width    => Area.Width - 2 * Radius,
          Height   => Thickness));
      Dispatch (Buffer).Fill_Rect
        (Color,
         (Position => (Area.Position.X + Radius, Area.Position.Y + Area.Height - Thickness / 2 - 1),
          Width    => Area.Width - 2 * Radius,
          Height   => Thickness));

      while X0 < Y0 loop
         if F >= 0 then
            Y0 := Y0 - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X0 := X0 + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;


         Draw_Point (Center_Rgt + X0, Center_Bot + Y0);
         Draw_Point (Center_Lft - X0, Center_Bot + Y0);
         Draw_Point (Center_Rgt + X0, Center_Top - Y0);
         Draw_Point (Center_Lft - X0, Center_Top - Y0);
         Draw_Point (Center_Rgt + Y0, Center_Bot + X0);
         Draw_Point (Center_Lft - Y0, Center_Bot + X0);
         Draw_Point (Center_Rgt + Y0, Center_Top - X0);
         Draw_Point (Center_Lft - Y0, Center_Top - X0);
      end loop;
   end Draw_Rounded_Rect;

   -----------------------
   -- Fill_Rounded_Rect --
   -----------------------

   overriding
   procedure Fill_Rounded_Rect
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : Bitmap_Color;
      Area   : Rect;
      Radius : Natural)
   is
      Col   : constant UInt32 := Bitmap_Color_To_Word (Dispatch (Buffer).Color_Mode, Color);
      F          : Integer := 1 - Radius;
      ddF_X      : Integer := 0;
      ddF_Y      : Integer := (-2) * Radius;
      X0         : Integer := 0;
      Y0         : Integer := Radius;
      Center_Top : constant Natural := Area.Position.Y + Radius;
      Center_Bot : constant Natural := Area.Position.Y + Area.Height - 1 - Radius;
      Center_Lft : constant Natural := Area.Position.X + Radius;

   begin
      if Radius = 0 then
         Dispatch (Buffer).Fill_Rect (Col, Area'Update (Position => (X0, Y0)));
         return;
      end if;

      Dispatch (Buffer).Fill_Rect
        (Col,
         (Position => (Area.Position.X, Center_Top),
          Width    => Area.Width,
          Height   => Area.Height - 2 * Radius));

      while X0 < Y0 loop
         if F >= 0 then
            Y0 := Y0 - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;

         X0 := X0 + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;

         Dispatch (Buffer).Draw_Horizontal_Line
           (Col,
            (Center_Lft - X0, Center_Bot + Y0),
            Area.Width - 2 * Radius + 2 * X0);
         Dispatch (Buffer).Draw_Horizontal_Line
           (Col,
            (Center_Lft - X0, Center_Top - Y0),
            Area.Width - 2 * Radius + 2 * X0);
         Dispatch (Buffer).Draw_Horizontal_Line
           (Col,
            (Center_Lft - Y0, Center_Bot + X0),
            Area.Width - 2 * Radius + 2 * Y0);
         Dispatch (Buffer).Draw_Horizontal_Line
           (Col,
            (Center_Lft - Y0, Center_Top - X0),
            Area.Width - 2 * Radius + 2 * Y0);
      end loop;
   end Fill_Rounded_Rect;

   -----------------
   -- Draw_Circle --
   -----------------

   overriding
   procedure Draw_Circle
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : Bitmap_Color;
      Center : Point;
      Radius : Natural)
   is
   begin
      Draw_Circle
        (Dispatch (Buffer),
         Bitmap_Color_To_Word (Dispatch (Buffer).Color_Mode, Color),
         Center,
         Radius);
   end Draw_Circle;

   --  http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm
   -----------------
   -- Draw_Circle --
   -----------------

   overriding
   procedure Draw_Circle
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : UInt32;
      Center : Point;
      Radius : Natural)
   is
      F     : Integer := 1 - Radius;
      ddF_X : Integer := 0;
      ddF_Y : Integer := (-2) * Radius;
      X     : Integer := 0;
      Y     : Integer := Radius;
   begin
      Dispatch (Buffer).Set_Pixel ((Center.X, Center.Y + Radius), Color);
      Dispatch (Buffer).Set_Pixel ((Center.X, Center.Y - Radius), Color);
      Dispatch (Buffer).Set_Pixel ((Center.X + Radius, Center.Y), Color);
      Dispatch (Buffer).Set_Pixel ((Center.X - Radius, Center.Y), Color);

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;
         Dispatch (Buffer).Set_Pixel ((Center.X + X, Center.Y + Y), Color);
         Dispatch (Buffer).Set_Pixel ((Center.X - X, Center.Y + Y), Color);
         Dispatch (Buffer).Set_Pixel ((Center.X + X, Center.Y - Y), Color);
         Dispatch (Buffer).Set_Pixel ((Center.X - X, Center.Y - Y), Color);
         Dispatch (Buffer).Set_Pixel ((Center.X + Y, Center.Y + X), Color);
         Dispatch (Buffer).Set_Pixel ((Center.X - Y, Center.Y + X), Color);
         Dispatch (Buffer).Set_Pixel ((Center.X + Y, Center.Y - X), Color);
         Dispatch (Buffer).Set_Pixel ((Center.X - Y, Center.Y - X), Color);
      end loop;
   end Draw_Circle;

   -----------------
   -- Fill_Circle --
   -----------------

   overriding
   procedure Fill_Circle
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : Bitmap_Color;
      Center : Point;
      Radius : Natural)
   is
      U32_Color : constant UInt32 :=
        Bitmap_Color_To_Word (Dispatch (Buffer).Color_Mode, Color);
   begin
      Dispatch (Buffer).Fill_Circle (U32_Color, Center, Radius);
   end Fill_Circle;

   -----------------
   -- Fill_Circle --
   -----------------

   overriding
   procedure Fill_Circle
     (Buffer : in out Soft_Drawing_Bitmap_Buffer;
      Color  : UInt32;
      Center : Point;
      Radius : Natural)
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

         elsif Y < 0 or else Y >= Dispatch (Buffer).Height then
            return;

         elsif X + Width < 0 or else X >= Dispatch (Buffer).Width then
            return;
         end if;

         if X < 0 then
            X1 := 0;
            W1 := Width + X;
         else
            X1 := X;
            W1 := Width;
         end if;

         if X1 + W1 >= Dispatch (Buffer).Width then
            W1 := Dispatch (Buffer).Width - X1 - 1;
         end if;

         if W1 = 0 then
            return;
         end if;

         Dispatch (Buffer).Fill_Rect (Color, ((X1, Y), W1, 1));
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

         elsif X < 0 or else X >= Dispatch (Buffer).Width then
            return;

         elsif Y + Height < 0 or else Y >= Dispatch (Buffer).Height then
            return;
         end if;

         if Y < 0 then
            Y1 := 0;
            H1 := Height + Y;
         else
            Y1 := Y;
            H1 := Height;
         end if;

         if Y1 + H1 >= Dispatch (Buffer).Height then
            H1 := Dispatch (Buffer).Height - Y1 - 1;
         end if;

         if H1 = 0 then
            return;
         end if;

         Dispatch (Buffer).Fill_Rect (Color, ((X, Y1), 1, H1));
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

   ------------------
   -- Cubic_Bezier --
   ------------------

   overriding
   procedure Cubic_Bezier
     (Buffer         : in out Soft_Drawing_Bitmap_Buffer;
      Color          : Bitmap_Color;
      P1, P2, P3, P4 : Point;
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
         Dispatch (Buffer).Draw_Line (Color, Points (I), Points (I + 1),
                                      Thickness => Thickness);
      end loop;
   end Cubic_Bezier;

end Soft_Drawing_Bitmap;
