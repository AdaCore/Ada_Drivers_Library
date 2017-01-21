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

with System.Storage_Elements; use System.Storage_Elements;
with Interfaces;              use Interfaces;

package body HAL.Bitmap is

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

   -------------------
   -- Mapped_In_RAM --
   -------------------

   function Mapped_In_RAM (Buffer : Bitmap_Buffer) return Boolean is
     (True);

   -------------------
   -- Memory_Addres --
   -------------------

   function Memory_Addres (Buffer : Bitmap_Buffer) return System.Address is
     (Buffer.Addr);

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Buffer : in out Bitmap_Buffer;
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

   procedure Set_Pixel
     (Buffer : in out Bitmap_Buffer;
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
               Pixel_B : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3);
               Pixel_G : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 1);
               Pixel_R : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 2);
               R       : constant Byte :=
                           Byte (Shift_Right (Value and 16#FF0000#, 16));
               G       : constant Byte :=
                           Byte (Shift_Right (Value and 16#FF00#, 8));
               B       : constant Byte := Byte (Value and 16#FF#);
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
               Pixel : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset);
            begin
               Pixel := Byte (Value and 16#FF#);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset / 2);
            begin
               if Offset mod 2 = 0 then
                  Pixel :=
                    (Pixel and 16#0F#) or
                    Shift_Left (Byte (Value and 16#0F#), 4);
               else
                  Pixel := (Pixel and 16#F0#) or Byte (Value and 16#0F#);
               end if;
            end;

      end case;
   end Set_Pixel;

   ---------------------
   -- Set_Pixel_Blend --
   ---------------------

   procedure Set_Pixel_Blend
     (Buffer : in out Bitmap_Buffer;
      Pt     : Point;
      Value  : Bitmap_Color)
   is
      Col : Bitmap_Color;
      FgA, FgR, FgG, FgB : Float;
      BgA, BgR, BgG, BgB : Float;
      RA, RR, RG, RB     : Float;

   begin
      if Value.Alpha = 255 then
         Set_Pixel (Bitmap_Buffer'Class (Buffer), Pt, Value);
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

         Col := (Alpha => Byte (RA * 255.0),
                 Red   => Byte (RR * 255.0),
                 Green => Byte (RG * 255.0),
                 Blue  => Byte (RB * 255.0));
         Set_Pixel (Bitmap_Buffer'Class (Buffer), Pt, Col);
      end if;
   end Set_Pixel_Blend;

   ---------------
   -- Get_Pixel --
   ---------------

   function Pixel
     (Buffer : Bitmap_Buffer;
      Pt     : Point)
      return Bitmap_Color
   is
      Native_Color : UInt32;
   begin
      Native_Color := Pixel (Bitmap_Buffer'Class (Buffer), Pt);

      return Word_To_Bitmap_Color (Buffer.Color_Mode, Native_Color);
   end Pixel;

   ---------------
   -- Get_Pixel --
   ---------------

   function Pixel
     (Buffer : Bitmap_Buffer;
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
               Pixel_B : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3);
               Pixel_G : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset * 3 + 1);
               Pixel_R : aliased Byte
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
               Pixel : aliased Byte
                 with Import,
                      Address => Buffer.Addr + Storage_Offset (Offset);
            begin
               return UInt32 (Pixel);
            end;

         when L_4 | A_4 =>
            declare
               Pixel : aliased Byte
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

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Buffer : in out Bitmap_Buffer;
      Color  : Bitmap_Color)
   is
      Col : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode, Color);
   begin
      Fill (Bitmap_Buffer'Class (Buffer), Col);
   end Fill;

   ----------
   -- Fill --
   ----------

   procedure Fill
     (Buffer : in out Bitmap_Buffer;
      Color  : UInt32)
   is
   begin
      for Y in 0 .. Buffer.Height - 1 loop
         for X in 0 .. Buffer.Width - 1 loop
            Set_Pixel (Buffer, (X, Y), Color);
         end loop;
      end loop;
   end Fill;

   ---------------
   -- Fill_Rect --
   ---------------

   procedure Fill_Rect
     (Buffer : in out Bitmap_Buffer;
      Color  : Bitmap_Color;
      Area   : Rect)
   is
   begin
      Fill_Rect
        (Bitmap_Buffer'Class (Buffer),
         Bitmap_Color_To_Word (Buffer.Color_Mode, Color),
         Area);
   end Fill_Rect;

   ---------------
   -- Fill_Rect --
   ---------------

   procedure Fill_Rect
     (Buffer : in out Bitmap_Buffer;
      Color  : UInt32;
      Area   : Rect)
   is
   begin
      for Y0 in Area.Position.Y .. Area.Position.Y + Area.Height - 1 loop
         for X0 in Area.Position.X .. Area.Position.X + Area.Width - 1 loop
            Set_Pixel (Buffer, (X0, Y0), Color);
         end loop;
      end loop;
   end Fill_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Bitmap_Buffer;
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

   procedure Copy_Rect
     (Src_Buffer  : Bitmap_Buffer'Class;
      Src_Pt      : Point;
      Dst_Buffer  : in out Bitmap_Buffer;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
      Null_Buffer : Bitmap_Buffer'Class := Src_Buffer;
   begin
      Null_Buffer.Addr := System.Null_Address;
      Null_Buffer.Width := 0;
      Null_Buffer.Height := 0;
      Null_Buffer.Swapped := False;

      Copy_Rect
        (Src_Buffer  => Src_Buffer,
         Src_Pt      => Src_Pt,
         Dst_Buffer  => Bitmap_Buffer'Class (Dst_Buffer),
         Dst_Pt      => Dst_Pt,
         Bg_Buffer   => Null_Buffer,
         Bg_Pt       => (0, 0),
         Width       => Width,
         Height      => Height,
         Synchronous => Synchronous);
   end Copy_Rect;

   ---------------------
   -- Copy_Rect_Blend --
   ---------------------

   procedure Copy_Rect_Blend
     (Src_Buffer  : Bitmap_Buffer;
      Src_Pt      : Point;
      Dst_Buffer  : in out Bitmap_Buffer'Class;
      Dst_Pt      : Point;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean)
   is
   begin
      Copy_Rect
        (Src_Buffer  => Bitmap_Buffer'Class (Src_Buffer),
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

   procedure Draw_Vertical_Line
     (Buffer : in out Bitmap_Buffer;
      Color  : UInt32;
      Pt     : Point;
      Height : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color,
                 (Position => Pt,
                  Width    => 1,
                  Height   => Height));
   end Draw_Vertical_Line;

   ------------------------
   -- Draw_Vertical_Line --
   ------------------------

   procedure Draw_Vertical_Line
     (Buffer : in out Bitmap_Buffer;
      Color  : Bitmap_Color;
      Pt     : Point;
      Height : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color,
                 (Position => Pt,
                  Width    => 1,
                  Height   => Height));
   end Draw_Vertical_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   procedure Draw_Horizontal_Line
     (Buffer : in out Bitmap_Buffer;
      Color  : UInt32;
      Pt     : Point;
      Width  : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color,
                 (Position => Pt,
                  Width    => Width,
                  Height   => 1));
   end Draw_Horizontal_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   procedure Draw_Horizontal_Line
     (Buffer : in out Bitmap_Buffer;
      Color  : Bitmap_Color;
      Pt     : Point;
      Width  : Integer)
   is
   begin
      Fill_Rect (Bitmap_Buffer'Class (Buffer), Color,
                 (Position => Pt,
                  Width    => Width,
                  Height   => 1));
   end Draw_Horizontal_Line;

   ---------------
   -- Draw_Rect --
   ---------------

   procedure Draw_Rect
     (Buffer    : in out Bitmap_Buffer;
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
      Buffer.Fill_Rect
        (Color,
         (Position => (X0 - Thickness / 2, Y0),
          Width    => Thickness,
          Height   => Area.Height + Thickness / 2));
      Buffer.Fill_Rect
        (Color,
         (Position => (X1 - Thickness / 2, Y0),
          Width    => Thickness,
          Height   => Area.Height + Thickness / 2));
      Buffer.Fill_Rect
        (Color,
         (Position => (X0, Y0 - Thickness / 2),
          Width    => Area.Width + Thickness / 2,
          Height   => Thickness));
      Buffer.Fill_Rect
        (Color,
         (Position => (X0, Y1 - Thickness / 2),
          Width    => Area. Width + Thickness / 2,
          Height   => Thickness));
   end Draw_Rect;

   -----------------------
   -- Draw_Rounded_Rect --
   -----------------------

   procedure Draw_Rounded_Rect
     (Buffer    : in out Bitmap_Buffer;
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
            Buffer.Fill_Rect
              (Color,
               (Position => (X - (Thickness / 2), Y - (Thickness / 2)),
                Width    => Thickness,
                Height   => Thickness));
         else
            Buffer.Set_Pixel ((X, Y), Color);
         end if;
      end Draw_Point;

   begin
      if Radius = 0 then
         Draw_Rect (Buffer, Color, Area, Thickness);
         return;
      end if;

      Buffer.Fill_Rect
        (Color,
         (Position => (Area.Position.X - Thickness / 2, Area.Position.Y + Radius),
          Width    => Thickness,
          Height   => Area.Height - 2 * Radius));
      Buffer.Fill_Rect
        (Color,
         (Position => (Area.Position.X + Area.Width - Thickness / 2 - 1, Area.Position.Y + Radius),
          Width    => Thickness,
          Height   => Area.Height - 2 * Radius));
      Buffer.Fill_Rect
        (Color,
         (Position => (Area.Position.X + Radius, Area.Position.Y - Thickness / 2),
          Width    => Area.Width - 2 * Radius,
          Height   => Thickness));
      Buffer.Fill_Rect
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

   procedure Fill_Rounded_Rect
     (Buffer : in out Bitmap_Buffer;
      Color  : Bitmap_Color;
      Area   : Rect;
      Radius : Natural)
   is
      Col   : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode, Color);
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
         Buffer.Fill_Rect (Col, Area'Update (Position => (X0, Y0)));
         return;
      end if;

      Buffer.Fill_Rect
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

         Buffer.Draw_Horizontal_Line
           (Col,
            (Center_Lft - X0, Center_Bot + Y0),
            Area.Width - 2 * Radius + 2 * X0);
         Buffer.Draw_Horizontal_Line
           (Col,
            (Center_Lft - X0, Center_Top - Y0),
            Area.Width - 2 * Radius + 2 * X0);
         Buffer.Draw_Horizontal_Line
           (Col,
            (Center_Lft - Y0, Center_Bot + X0),
            Area.Width - 2 * Radius + 2 * Y0);
         Buffer.Draw_Horizontal_Line
           (Col,
            (Center_Lft - Y0, Center_Top - X0),
            Area.Width - 2 * Radius + 2 * Y0);
      end loop;
   end Fill_Rounded_Rect;

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (Buffer : in out Bitmap_Buffer;
      Color  : Bitmap_Color;
      Center : Point;
      Radius : Natural)
   is
   begin
      Draw_Circle
        (Buffer,
         Bitmap_Color_To_Word (Buffer.Color_Mode, Color),
         Center,
         Radius);
   end Draw_Circle;

   --  http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm
   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle
     (Buffer : in out Bitmap_Buffer;
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
      Buffer.Set_Pixel ((Center.X, Center.Y + Radius), Color);
      Buffer.Set_Pixel ((Center.X, Center.Y - Radius), Color);
      Buffer.Set_Pixel ((Center.X + Radius, Center.Y), Color);
      Buffer.Set_Pixel ((Center.X - Radius, Center.Y), Color);

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;
         Buffer.Set_Pixel ((Center.X + X, Center.Y + Y), Color);
         Buffer.Set_Pixel ((Center.X - X, Center.Y + Y), Color);
         Buffer.Set_Pixel ((Center.X + X, Center.Y - Y), Color);
         Buffer.Set_Pixel ((Center.X - X, Center.Y - Y), Color);
         Buffer.Set_Pixel ((Center.X + Y, Center.Y + X), Color);
         Buffer.Set_Pixel ((Center.X - Y, Center.Y + X), Color);
         Buffer.Set_Pixel ((Center.X + Y, Center.Y - X), Color);
         Buffer.Set_Pixel ((Center.X - Y, Center.Y - X), Color);
      end loop;
   end Draw_Circle;

   -----------------
   -- Fill_Circle --
   -----------------

   procedure Fill_Circle
     (Buffer : in out Bitmap_Buffer;
      Color  : Bitmap_Color;
      Center : Point;
      Radius : Natural)
   is
      U32_Color : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                           Color);
   begin
      Fill_Circle (Buffer, U32_Color, Center, Radius);
   end Fill_Circle;

   -----------------
   -- Fill_Circle --
   -----------------

   procedure Fill_Circle
     (Buffer : in out Bitmap_Buffer;
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

         Buffer.Fill_Rect (Color, ((X1, Y), W1, 1));
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

         Buffer.Fill_Rect (Color, ((X, Y1), 1, H1));
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

   -----------------
   -- Buffer_Size --
   -----------------

   function Buffer_Size (Buffer : Bitmap_Buffer) return Natural
   is
   begin
      return Bits_Per_Pixel (Buffer.Color_Mode) *
        Buffer.Width * Buffer.Height / 8;
   end Buffer_Size;

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

   -------------------
   -- Wait_Transfer --
   -------------------

   procedure Wait_Transfer (Buffer : Bitmap_Buffer)
   is null;

end HAL.Bitmap;
